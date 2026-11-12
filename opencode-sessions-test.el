;;; opencode-sessions-test.el --- Session rendering tests  -*- lexical-binding: t; -*-
;;; Code:

(require 'ert)
(require 'opencode)
(require 'opencode-sessions)

(defmacro with-opencode-session-test-buffer (&rest body)
  "Run BODY in a temporary `opencode-session-mode' buffer.
BODY can refer to the buffer's process as `process'.  The default directory
is the opencode source directory, and session state is bound so that events
for session \"session\" render here."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (setq default-directory
           (file-name-directory (locate-library "opencode-sessions")))
     (opencode-session-mode)
     (let ((process (start-process "opencode-test" (current-buffer) nil))
           (opencode-session-buffers (make-hash-table :test 'equal))
           (opencode--tool-calls (make-hash-table :test 'equal)))
       (setq opencode-session-id "session")
       (puthash "session" (current-buffer) opencode-session-buffers)
       (unwind-protect
           (progn ,@body)
         (delete-process process)))))

(defmacro opencode-session-test--with-stubbed-api (active &rest body)
  "Run BODY with api requests answered, and toasts and alerts discarded.
Requests for active sessions return ACTIVE, and others a session."
  (declare (indent 1))
  `(let ((opencode-toast-function #'ignore)
         (opencode-alerted-sessions nil)
         (opencode-file-edited-functions nil)
         (opencode-files-finished-editing-functions nil)
         (opencode-project-files-finished-editing-functions nil)
         (opencode--session-control-buffers nil))
     (cl-letf (((symbol-function 'opencode-api--request)
                (lambda (_method path &rest args)
                  (funcall (plist-get args :then)
                           (if (equal path "/api/session/active")
                               ,active
                             '((id . "session") (title . "Title")))))))
       ,@body)))

(defconst opencode-session-test--edit
  '((path . "opencode-sessions.el") (oldString . "old") (newString . "new"))
  "Input of an edit tool call.")

(defconst opencode-session-test--questions
  '((questions . [((question . "One?")) ((question . "Two?")) ((question . "Three?"))]))
  "Input of a question tool call.")

(defun opencode-session-test--assistant (content &optional id)
  "Return a completed assistant message with CONTENT, and ID or \"msg_1\"."
  `((id . ,(or id "msg_1")) (type . "assistant") (agent . "build")
    (time . ((created . 1) (completed . 2)))
    (tokens . ((input . 0) (output . 0) (reasoning . 0)
               (cache . ((read . 0) (write . 0)))))
    (content . ,content)))

(defun opencode-session-test--output ()
  "Return the session's output, without pending input."
  (buffer-substring-no-properties
   (point-min) (process-mark (get-buffer-process (current-buffer)))))

(defun opencode-session-test--at-prompt-p ()
  "Return non-nil if the session's output ends with a prompt."
  (string-suffix-p "❯ " (opencode-session-test--output)))

(defun opencode-session-test--margin-p (pos)
  "Return non-nil if POS has a margin."
  (seq-some (lambda (ov) (overlay-get ov 'line-prefix)) (overlays-at pos)))

(defun opencode-session-test--refontify ()
  "Refontify the buffer, checking it keeps its text, process mark and point."
  (let ((text (buffer-substring-no-properties (point-min) (point-max)))
        (mark (marker-position (process-mark (get-buffer-process (current-buffer)))))
        (position (point)))
    (font-lock-ensure)
    ;; Refontifying code blocks must not clear earlier inline
    ;; Markdown, even with normal JIT font-lock enabled.
    (font-lock-flush)
    (font-lock-ensure)
    (should (equal text (buffer-substring-no-properties (point-min) (point-max))))
    (should (= mark (process-mark (get-buffer-process (current-buffer)))))
    (should (= position (point)))))

(defun opencode-session-test--folded-p (text)
  "Return non-nil if the block whose first line contains TEXT is folded."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (eq 'hs (get-char-property (pos-eol) 'invisible))))

(defun opencode-session-test--error-face-p (text)
  "Return non-nil if TEXT has the `error' face."
  (save-excursion
    (goto-char (point-min))
    (and (search-forward text nil t)
         (eq 'error (get-char-property (match-beginning 0) 'face)))))

(defun opencode-session-test--error-styled-p (text)
  "Return non-nil if TEXT has the `error' face and margin, and is visible."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (let ((pos (match-beginning 0)))
      (and (eq 'error (get-char-property pos 'face))
           (not (invisible-p pos))
           (seq-some (lambda (ov)
                       (when-let* ((prefix (overlay-get ov 'line-prefix))
                                   (display (get-text-property 0 'display prefix)))
                         (eq 'error (get-text-property 0 'face (cadr display)))))
                     (overlays-at pos))))))

(defun opencode-session-test--delta (type delta &optional ordinal)
  "Output DELTA of the block of TYPE and ORDINAL, through the event dispatcher."
  (opencode--handle-message
   `((type . ,(format "session.%s.delta" type))
     (data . ((sessionID . "session")
              (assistantMessageID . "message")
              (ordinal . ,(or ordinal 0))
              (delta . ,delta))))))

(defun opencode-session-test--ended (type text &optional ordinal)
  "End the block of TYPE and ORDINAL with TEXT, through the event dispatcher."
  (opencode--handle-message
   `((type . ,(format "session.%s.ended" type))
     (data . ((sessionID . "session")
              (assistantMessageID . "message")
              (ordinal . ,(or ordinal 0))
              (text . ,text))))))

(defun opencode-session-test--tool (tool status input &optional output call-id exit)
  "Send the events of the TOOL call with STATUS, INPUT, OUTPUT and EXIT status.
OUTPUT is the error message when STATUS is \"error\".  CALL-ID defaults to TOOL."
  (let* ((id (or call-id tool))
         (data `((sessionID . "session") (assistantMessageID . "message") (id . ,id))))
    (pcase status
      ("running"
       (opencode--handle-message
        `((type . "session.tool.input.started")
          (data . ((name . ,tool) ,@data))))
       (opencode--handle-message
        `((type . "session.tool.called")
          (data . ((input . ,input) (executed . :false) ,@data)))))
      ("completed"
       (opencode--handle-message
        `((type . "session.tool.success")
          (data . ((content . (((type . "text") (text . ,output))))
                   (metadata . ((exit . ,exit)))
                   (executed . :false)
                   ,@data)))))
      ("error"
       (opencode--handle-message
        `((type . "session.tool.failed")
          (data . ((error . ((type . "unknown") (message . ,output)))
                   (executed . :false)
                   ,@data))))))))

(ert-deftest opencode-read-tool-file-location-button-test ()
  "Format read offsets as working file-and-line buttons."
  (let* ((file-directory (file-name-directory
                          (locate-library "opencode-sessions")))
         (default-directory file-directory)
         (formatted (opencode--format-tool-call
                     "read" '((path . "opencode-sessions.el")
                              (offset . 440)
                              (limit . 35)))))
    (with-temp-buffer
      (setq default-directory file-directory)
      (insert formatted)
      (opencode--buttonize-file-references (point-min) (point-max))
      (goto-char (point-min))
      (search-forward "opencode-sessions.el:440")
      (let ((button (button-at (1- (point)))))
        (should button)
        (should (equal (button-get button 'button-data)
                       (cons (expand-file-name "opencode-sessions.el"
                                               file-directory)
                             440)))))))

(ert-deftest opencode-session-markdown-fontification-test ()
  "Stream Markdown without replacing text or losing session decorations."
  (with-opencode-session-test-buffer
    (font-lock-mode 1)
    (opencode--output "# Heading\n\nSee `opencode-sessions")
    (opencode--output ".el` and *emphasis*.\n\n")
    (opencode--insert-tool-block "edit" opencode-session-test--edit)
    (opencode--output "**Testing button behavior**\n\n")
    (opencode--insert-block-with-margin
     (concat "emacs_eval_elisp:\n"
             (opencode--fenced-block "(+ 1 2)" "elisp"))
     'opencode-tool-margin-highlight)
    (opencode--maybe-insert-block-spacing)
    (opencode--show-prompt)
    (insert "pending input")
    (opencode-session-test--refontify)
    (save-excursion
      (goto-char (point-min))
      (search-forward "Heading")
      (should (memq 'markdown-ts-heading-1
                    (get-text-property (1- (point)) 'face)))
      (goto-char (point-min))
      (search-forward "opencode-sessions.el")
      (let ((button (button-at (1- (point)))))
        (should button)
        (should (equal (car (button-get button 'button-data))
                       (expand-file-name "opencode-sessions.el"))))
      (goto-char (point-min))
      (search-forward "-old")
      (should (memq 'diff-removed
                    (get-text-property (1- (point)) 'face)))
      (should (opencode-session-test--margin-p (1- (point))))
      (search-forward "```")
      (should-not (opencode-session-test--margin-p (match-beginning 0)))
      (search-forward "**Testing button behavior**")
      (should (memq 'markdown-ts-bold
                    (get-text-property (match-beginning 0) 'face)))
      (should (get-char-property (match-beginning 0) 'invisible))
      (search-forward "(+ 1 2)")
      (should (opencode-session-test--margin-p (1- (point))))
      (search-forward "```")
      (should-not (opencode-session-test--margin-p (match-beginning 0)))
      (should (opencode-session-test--at-prompt-p))
      (search-forward "❯ ")
      (should (get-char-property (match-beginning 0) 'invisible))
      (should (opencode-session-test--margin-p (match-beginning 0))))))

(ert-deftest opencode-session-input-markup-test ()
  "Show the markup of pending input, and hide it once sent."
  (with-opencode-session-test-buffer
    (font-lock-mode 1)
    (opencode--show-prompt)
    (let ((start (point)))
      (insert "> **bold**")
      (font-lock-ensure)
      (should (memq 'markdown-ts-bold
                    (get-text-property (+ start 4) 'face)))
      (should-not (text-property-not-all start (point-max) 'invisible nil))
      (opencode--replay-input "")
      (font-lock-ensure)
      (should (get-char-property start 'invisible))
      (should (opencode-session-test--margin-p start)))))

(ert-deftest opencode-session-label-test ()
  "Show the label name, and send it as code along with the label's part."
  (dolist (case '(("AGENTS.md" . "`AGENTS.md`")
                  ("a`b" . "``a`b``")
                  ("`x`" . "`` `x` ``")))
    (with-opencode-session-test-buffer
      (opencode--output "Hi!\n\n")
      (opencode--show-prompt)
      (let ((start (point))
            (part (list (cons 'name (car case)))))
        (insert "see ")
        (opencode--insert-label (car case) part)
        (insert " now")
        (font-lock-ensure)
        (should (equal (car case)
                       (get-char-property (+ start 4) 'display)))
        (should (equal (cons (concat "see " (cdr case) " now") (list part))
                       (opencode--input-text-and-parts start (point))))
        (delete-region (+ start 4) (+ start 5))
        (should (equal (cons "see  now" nil)
                       (opencode--input-text-and-parts start (point))))))))

(ert-deftest opencode-session-label-typing-test ()
  "Text typed right after a label is sent as text, not as copies of the label."
  (with-opencode-session-test-buffer
    (opencode--output "Hi!\n\n")
    (opencode--show-prompt)
    (let ((start (point))
          (part '((name . "AGENTS.md"))))
      (opencode--insert-label "AGENTS.md" part)
      (dolist (char (string-to-list "hi"))
        (let ((last-command-event char))
          (self-insert-command 1)))
      (should (equal (cons "`AGENTS.md`hi" (list part))
                     (opencode--input-text-and-parts start (point)))))))

(ert-deftest opencode-session-prompt-input-test ()
  "A sent prompt's input is copied editable and with working labels."
  (with-opencode-session-test-buffer
    (opencode--output "Hi!\n\n")
    (opencode--show-prompt)
    (let ((part '((name . "AGENTS.md"))))
      (insert "see ")
      (opencode--insert-label "AGENTS.md" part)
      (insert " now")
      (let ((comint-input-sender #'opencode--highlight-input))
        (comint-send-input))
      (opencode--output "Reply\n\n")
      (let ((input (save-excursion
                     (search-backward "Reply")
                     (opencode--prompt-input))))
        (should (equal "see ￼ now" (substring-no-properties input)))
        (should-not (text-property-not-all 0 4 'read-only nil input))
        (should-not (get-text-property 4 'read-only input))
        (opencode--show-prompt)
        (goto-char (point-max))
        (let ((start (point)))
          (insert input)
          (should (equal (cons "see `AGENTS.md` now" (list part))
                         (opencode--input-text-and-parts start (point)))))))))

(ert-deftest opencode-session-prompt-input-replayed-test ()
  "A replayed prompt's input is copied without the newline replay adds."
  (with-opencode-session-test-buffer
    (opencode--output "Hi!\n\n")
    (opencode--replay-input "line one\nline two")
    (opencode--output "Reply\n\n")
    (dolist (text '("Reply" "line two"))
      (save-excursion
        (search-backward text)
        (should (equal "line one\nline two" (opencode--prompt-input)))))))

(ert-deftest opencode-session-diff-refinement-test ()
  "Refine entire synthetic hunks without touching input or earlier output."
  (with-opencode-session-test-buffer
    (let ((diff-refine 'font-lock))
      (opencode--output "Earlier output\n")
      (insert "pending input")
      (opencode--insert-tool-block
       "patch"
       '((patchText . "*** Begin Patch\n*** Update File: example.el\n@@\n-old first value\n-old second value\n+new first value\n+new second value\n@@\n-before last value\n+after last value\n*** End Patch")))
      (opencode-session-test--refontify)
      (should (equal "pending input"
                     (buffer-substring-no-properties (process-mark process)
                                                     (point-max))))
      (save-excursion
        (goto-char (point-min))
        (should-not (get-char-property (point) 'invisible))
        (dolist (change '(("old second" . diff-refine-removed)
                          ("new second" . diff-refine-added)
                          ("before last" . diff-refine-removed)
                          ("after last" . diff-refine-added)))
          (search-forward (car change))
          (should (cl-some
                   (lambda (overlay)
                     (eq (overlay-get overlay 'face) (cdr change)))
                   (overlays-at (match-beginning 0)))))
        (goto-char (point-min))
        (while (search-forward "@@ -0 +0 @@" nil t)
          (should (get-char-property (match-beginning 0) 'invisible)))))))

(ert-deftest opencode-session-diff-line-background-test ()
  "Show diff faces' backgrounds, keeping the code block background elsewhere."
  (with-opencode-session-test-buffer
    (opencode--output "```diff\n-old\n+new\n```\n\n```python\nx = 1\n```\n\n")
    (opencode-session-test--refontify)
    (cl-flet ((check-diff-faces ()
                (dolist (change '(("old" . diff-removed)
                                  ("new" . diff-added)))
                  (goto-char (point-min))
                  (search-forward (car change))
                  (let ((face (get-char-property (1- (point)) 'face)))
                    (should (memq (cdr change) (ensure-list face)))))))
      (check-diff-faces)
      ;; A chunk with only a fence line resets the overlay it doesn't touch.
      (dolist (fence '("```diff" "+new\n```"))
        (goto-char (point-min))
        (search-forward fence)
        (opencode--fontify-markdown (pos-bol) (pos-eol))
        (check-diff-faces)))
    (goto-char (point-min))
    (search-forward "x = 1")
    (should (eq 'markdown-ts-code-block-markup-hidden
                (get-char-property (1- (point)) 'face)))))

(ert-deftest opencode-session-hidden-fence-lines-test ()
  "Hide whole fence lines, including their newlines."
  (with-opencode-session-test-buffer
    (opencode--output "Text:\n\n```python\nx = 1\n```\n\nAfter\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "```python")
    (should (get-char-property (pos-bol) 'invisible))
    (should (get-char-property (pos-eol) 'invisible))
    (search-forward "```")
    (should (get-char-property (pos-eol) 'invisible))
    (search-forward "After")
    (should-not (get-char-property (1- (pos-bol)) 'invisible))))

(ert-deftest opencode-session-yank-code-block-test ()
  "Copy only code from a fenced block, including embedded fences."
  (with-opencode-session-test-buffer
    (insert (opencode--fenced-block "```\ntext\n" "text") "\n")
    (goto-char (point-min))
    (search-forward "text" nil nil 2)
    (opencode-yank-code-block)
    (should (equal (current-kill 0 t) "```\ntext\n"))))

(ert-deftest opencode-session-completed-tables-test ()
  "Align completed streamed tables once without disturbing pending input."
  (with-opencode-session-test-buffer
    (let ((align (symbol-function 'opencode--align-output-table))
          (calls 0))
      (cl-letf (((symbol-function 'opencode--align-output-table)
                 (lambda (beg end)
                   (cl-incf calls)
                   (funcall align beg end))))
        (insert "pending input")
        (opencode-session-test--delta 'text "| File | Value |\n")
        (opencode-session-test--delta 'text "| :--- | ---: |\n")
        (opencode-session-test--delta 'text "| `opencode-sessions.el` | 7 |\n")
        (should (= calls 0))
        ;; An incomplete following line cannot yet establish the boundary.
        (opencode-session-test--delta 'text "# Following")
        (should (= calls 0))
        (opencode-session-test--delta 'text " prose.\n")
        (should (= calls 1))
        (let ((output (opencode-session-test--output)))
          (should (string-match-p "| +7|" output))
          (should (string-suffix-p "Following prose.\n" output)))
        (should (equal (buffer-substring-no-properties
                        (process-mark process) (point-max))
                       "pending input"))
        (should (= (point) (point-max)))
        (save-excursion
          (goto-char (point-min))
          (search-forward "opencode-sessions.el")
          (should (button-at (1- (point))))
          (search-forward "7|")
          (should (eq (get-text-property (match-beginning 0) 'field) 'output)))
        ;; A fenced table is code and must never be aligned.
        (opencode-session-test--delta
         'text "\n```markdown\n| Code | Value |\n| --- | --- |\n| x | y |\n```\n\n")
        (should (= calls 1))
        ;; A blank line completes a table before the text part ends.
        (opencode-session-test--delta
         'text "| Name | Value |\n| --- | --- |\n| x | y |\n\n")
        (should (= calls 2))
        (opencode-session-test--delta 'text "More prose.\n\n")
        (should (= calls 2))
        ;; Flush a final table without a trailing newline on part completion.
        (opencode-session-test--delta
         'text "| Last | Value |\n| --- | --- |\n| x | y |")
        (should (= calls 2))
        (opencode-session-test--ended 'text "ignored, as the block had deltas")
        (should (= calls 3))
        (should-not opencode--table-scan-marker)
        (should (eq (char-before (process-mark process)) ?|))
        (should (equal (buffer-substring-no-properties
                        (process-mark process) (point-max))
                       "pending input"))))))

(ert-deftest opencode-session-reasoning-leading-newline-test ()
  "A streamed reasoning part starting with a newline adds no blank line or margin."
  (with-opencode-session-test-buffer
    (opencode--output "request\n\n")
    (opencode-session-test--delta 'reasoning "\n")
    (opencode-session-test--delta 'reasoning "Thinking")
    (should (equal (opencode-session-test--output) "request\n\nThinking"))
    (dolist (pos (number-sequence (point-min) (+ (point-min) 8)))
      (should-not (opencode-session-test--margin-p pos)))
    (should (opencode-session-test--margin-p (+ (point-min) 9)))))

(ert-deftest opencode-session-folding-test ()
  "Fold reasoning, edit diffs and tool output, but not user shell output."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(shell-non-zero error))
          (comint-prompt-read-only t)
          (edit opencode-session-test--edit)
          (grep '((pattern . "foo")))
          (shell '((command . "ls")))
          (agent-shell '((command . "uname")))
          (eval '((code . "(progn\n  nil)"))))
      (opencode--output "request\n\n")
      (opencode-session-test--delta 'reasoning "Thinking\nmore thoughts")
      (should-not (opencode-session-test--folded-p "Thinking"))
      (opencode-session-test--tool "edit" "running" edit)
      (should (opencode-session-test--folded-p "Thinking"))
      (should (opencode-session-test--folded-p "edit opencode-sessions.el:"))
      (opencode-session-test--tool "edit" "completed" edit "Edit applied")
      (should-not (string-search "Edit applied" (buffer-string)))
      ;; Parallel calls: each output goes after its own call.
      (opencode-session-test--tool "grep" "running" grep)
      (opencode-session-test--tool "shell" "running" shell)
      (opencode-session-test--tool "shell" "running" agent-shell nil "agent-shell")
      (opencode-session-test--tool "eval" "running" eval)
      (opencode-session-test--tool "grep" "completed" grep "a.el:1\nb.el:2")
      (opencode-session-test--tool "shell" "completed" shell "ls-file")
      (opencode-session-test--tool "shell" "completed" agent-shell "agent-file"
                                   "agent-shell")
      (opencode-session-test--tool "eval" "completed" eval "result")
      (should (opencode-session-test--folded-p "grep \"foo\""))
      (should (opencode-session-test--folded-p "\nls"))
      (should (opencode-session-test--folded-p "\nuname"))
      (should (opencode-session-test--folded-p "  nil)"))
      (should-not (opencode-session-test--folded-p "eval\n"))
      (should (string-suffix-p
               (concat "grep \"foo\"\n```text\na.el:1\nb.el:2\n```\n\n"
                       "```bash\nls\n```\n```text\nls-file\n```\n\n"
                       "```bash\nuname\n```\n```text\nagent-file\n```\n\n"
                       "eval\n(progn\n  nil)\n```text\nresult\n```\n\n")
               (opencode-session-test--output)))
      (should (eq (get-text-property (point-min) 'field) 'output))
      (goto-char (point-min))
      (search-forward "b.el")
      (should (eq (get-text-property (point) 'field) 'output))
      (should (get-text-property (point) 'read-only))
      (opencode-session-test--delta 'text "Done.")
      (should (opencode-session-test--folded-p "Thinking"))
      (should (opencode-session-test--folded-p "edit opencode-sessions.el:"))
      ;; A user's shell command output isn't folded.
      (opencode--output "\n\n")
      (opencode--insert-shell-output "user-file" nil)
      (goto-char (point-min))
      (search-forward "user-file")
      (should-not (invisible-p (1- (point))))
      ;; Toggle a fold from its header line.
      (goto-char (point-min))
      (search-forward "Thinking")
      (hs-toggle-hiding)
      (should-not (opencode-session-test--folded-p "Thinking"))
      (hs-toggle-hiding)
      (should (opencode-session-test--folded-p "Thinking"))
      ;; Refold from inside an expanded body.
      (hs-toggle-hiding)
      (search-forward "more")
      (hs-toggle-hiding)
      (should (opencode-session-test--folded-p "Thinking"))
      ;; Outside any fold, toggling does nothing.
      (goto-char (point-min))
      (hs-toggle-hiding)
      (should (opencode-session-test--folded-p "Thinking")))))

(defun opencode-session-test--fold-label (text)
  "Return the label of the fold whose first line contains TEXT."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (overlay-get (seq-find (lambda (ov) (overlay-get ov 'hs))
                           (overlays-in (pos-eol) (1+ (pos-eol))))
                 'before-string)))

(ert-deftest opencode-session-fold-label-test ()
  "Fold labels count visible lines, not the fence lines Markdown hides."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs nil)
          (grep '((pattern . "foo")))
          (shell '((command . "ls")))
          (eval '((code . "(progn\n  nil)"))))
      (opencode--output "request\n\n")
      (opencode-session-test--delta 'reasoning "Thinking\nmore thoughts")
      (opencode-session-test--tool "grep" "running" grep)
      (opencode-session-test--tool "shell" "running" shell)
      (opencode-session-test--tool "eval" "running" eval)
      (opencode-session-test--tool "grep" "completed" grep "a.el:1\nb.el:2")
      (opencode-session-test--tool "shell" "completed" shell "ls-file")
      (opencode-session-test--tool "eval" "completed" eval "result")
      (dolist (fold '(("Thinking" . "1 line")
                      ("grep \"foo\"" . "2 lines")
                      ("\nls" . "1 line")
                      ("  nil)" . "1 line")))
        (let ((label (opencode-session-test--fold-label (car fold))))
          (should (string-match-p (format "\\` %s\\b" (cdr fold)) label))
          ;; The count stays clickable, like the ellipsis after it.
          (should (get-text-property 1 'keymap label)))))))

(ert-deftest opencode-session-shown-outputs-test ()
  "`opencode-shown-outputs' picks what starts unfolded; all of it can be folded."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(reasoning grep edit))
          (patch '((patchText . "*** Update File: a.el\n@@\n-old\n+new"))))
      (opencode--output "request\n\n")
      (opencode-session-test--delta 'reasoning "Thinking\nmore thoughts")
      (opencode-session-test--tool "edit" "running" opencode-session-test--edit)
      (opencode-session-test--tool "patch" "running" patch)
      (opencode-session-test--tool "grep" "running" '((pattern . "foo")))
      (opencode-session-test--tool "glob" "running" '((pattern . "*.el")))
      (opencode-session-test--tool "grep" "completed" '((pattern . "foo"))
                                   "a.el:1\nb.el:2")
      (opencode-session-test--tool "glob" "completed" '((pattern . "*.el"))
                                   "a.el\nb.el")
      (should-not (opencode-session-test--folded-p "Thinking"))
      (should-not (opencode-session-test--folded-p "edit opencode-sessions.el:"))
      (should (opencode-session-test--folded-p "patch:"))
      (should-not (opencode-session-test--folded-p "grep \"foo\""))
      (should (opencode-session-test--folded-p "glob \"*.el\""))
      ;; Shown blocks can still be folded, and unfolded again.
      (dolist (header '("Thinking" "edit opencode-sessions.el:" "grep \"foo\""))
        (goto-char (point-min))
        (search-forward header)
        (hs-toggle-hiding)
        (should (opencode-session-test--folded-p header))
        (hs-toggle-hiding)
        (should-not (opencode-session-test--folded-p header))))))

(ert-deftest opencode-session-shown-outputs-shell-test ()
  "`shell-non-zero' shows only failed commands, `shell' shows all of them."
  (with-opencode-session-test-buffer
    (let ((run (lambda (command output exit)
                 (let ((input `((command . ,command))))
                   (opencode-session-test--tool "shell" "running" input nil command)
                   (opencode-session-test--tool "shell" "completed" input output
                                                command exit)))))
      (opencode--output "request\n\n")
      (let ((opencode-shown-outputs '(shell-non-zero)))
        (funcall run "good" "out1" 0)
        (funcall run "bad" "out2" 1)
        (funcall run "killed" "out3" nil))
      (let ((opencode-shown-outputs '(shell)))
        (funcall run "good2" "out5" 0))
      (let ((opencode-shown-outputs nil))
        (funcall run "bad2" "out6" 1))
      ;; Each search stops at the first match, before any later "2" command.
      (should (opencode-session-test--folded-p "\ngood"))
      (should-not (opencode-session-test--folded-p "\nbad"))
      (should (opencode-session-test--folded-p "\nkilled"))
      (should-not (opencode-session-test--folded-p "\ngood2"))
      (should (opencode-session-test--folded-p "\nbad2"))
      ;; Only non-zero exits are styled as errors.
      (should (opencode-session-test--error-styled-p "out2"))
      (should-not (opencode-session-test--error-styled-p "out5")))))

(ert-deftest opencode-session-shown-outputs-replay-test ()
  "Replayed reasoning follows `opencode-shown-outputs'."
  (with-opencode-session-test-buffer
    (let ((replay
           (lambda (text)
             (opencode--replay-session-messages
              (list (opencode-session-test--assistant
                     `(((type . "reasoning")
                        (text . ,(concat text "\nmore thoughts"))))))))))
      (let ((opencode-shown-outputs '(reasoning)))
        (funcall replay "Shown"))
      (let ((opencode-shown-outputs nil))
        (funcall replay "Hidden"))
      (should-not (opencode-session-test--folded-p "Shown"))
      (should (opencode-session-test--folded-p "Hidden")))))

(ert-deftest opencode-session-replay-idle-test ()
  "Replayed failed and interrupted executions show an error."
  (with-opencode-session-test-buffer
    (opencode--replay-session-messages
     '(((id . "msg_1") (type . "idle") (outcome . "succeeded"))
       ((id . "msg_2") (type . "idle") (outcome . "failed")
        (error . ((type . "unknown") (message . "It broke"))))
       ((id . "msg_3") (type . "idle") (outcome . "interrupted"))))
    (should (opencode-session-test--error-face-p "It broke"))
    (should (opencode-session-test--error-face-p "Interrupted"))))

(ert-deftest opencode-session-tool-error-test ()
  "Failed calls show their error, including tools that normally hide output.
Unless `error' is not in `opencode-shown-outputs', when they start folded."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(error))
          (eval '((expression . "(car 1)")))
          (folded-eval '((expression . "(car 2)"))))
      (opencode--output "request\n\n")
      (opencode-session-test--tool "emacs_eval_elisp" "running" eval)
      (opencode-session-test--tool "edit" "running" opencode-session-test--edit)
      (opencode-session-test--tool "emacs_eval_elisp" "error" eval "Wrong type")
      (opencode-session-test--tool "edit" "error" opencode-session-test--edit
                                   "oldString not found")
      (should-not (opencode-session-test--folded-p "(car 1)"))
      (should (opencode-session-test--error-styled-p "Wrong type"))
      ;; The edit's diff is folded, but its error stays visible below it.
      (should (opencode-session-test--folded-p "edit opencode-sessions.el:"))
      (should (opencode-session-test--error-styled-p "oldString not found"))
      (let ((opencode-shown-outputs nil))
        (opencode-session-test--tool "emacs_eval_elisp" "running" folded-eval nil "folded")
        (opencode-session-test--tool "emacs_eval_elisp" "error" folded-eval "Wrong type"
                                     "folded"))
      (should (opencode-session-test--folded-p "(car 2)")))))

(ert-deftest opencode-session-tool-error-replay-test ()
  "Replayed failed calls show their error."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(error)))
      (opencode--replay-session-messages
       (list (opencode-session-test--assistant
              '(((type . "tool")
                 (id . "call")
                 (name . "emacs_eval_elisp")
                 (state . ((status . "error")
                           (input . ((expression . "(car 1)")))
                           (error . ((type . "unknown")
                                     (message . "Wrong type"))))))))))
      (should-not (opencode-session-test--folded-p "(car 1)"))
      (should (opencode-session-test--error-styled-p "Wrong type")))))

(ert-deftest opencode-conversation-tool-error-test ()
  "Exported conversations include the error of failed tool calls."
  (should (string-search
           "Error:\n\n```text\nWrong type\n```"
           (opencode--conversation-format-tool-item
            '((name . "emacs_eval_elisp")
              (state . ((status . "error")
                        (error . ((type . "unknown") (message . "Wrong type"))))))))))

(ert-deftest opencode-session-tool-json-output-test ()
  "Tool output that is a JSON object or array is fenced as JSON."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(execute)))
      (opencode-session-test--tool "execute" "completed" '((code . "a"))
                                   "{\"a\": [1, true]}" "c1")
      (opencode-session-test--tool "execute" "completed" '((code . "b"))
                                   "{not json" "c2")
      (should (string-search "```json\n{\n  \"a\": [\n    1,\n    true\n  ]\n}\n```"
                             (buffer-string)))
      (should (string-search "```text\n{not json\n```" (buffer-string))))))

(ert-deftest opencode-session-tool-output-margin-after-fenced-call-test ()
  "Output's margin covers a fenced call's closing fence, but only once it's there.
The first visible output line starts at that hidden fence."
  (with-opencode-session-test-buffer
    (let ((input '((code . "1 + 2")))
          (fence-margin-p
           (lambda ()
             (save-excursion
               (goto-char (point-min))
               (search-forward "1 + 2")
               (search-forward "```")
               (opencode-session-test--margin-p (match-beginning 0))))))
      (opencode--output "request\n\n")
      (opencode-session-test--tool "execute" "running" input)
      (should-not (funcall fence-margin-p))
      (opencode-session-test--tool "execute" "completed" input "3")
      (should (funcall fence-margin-p)))))

(ert-deftest opencode-session-table-alignment-context-test ()
  "Table alignment must not schedule a context mode in the session buffer."
  (with-opencode-session-test-buffer
    (insert "| Name | Value |\n| --- | --- |\n| x | y |\n")
    (let ((markdown-ts-enable-table-mode t)
          (markdown-ts-enable-code-block-context-mode t)
          (timer (symbol-function 'run-with-timer))
          context-timers)
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (seconds repeat function &rest args)
                   (if (memq function '(markdown-ts--enable-in-table-mode
                                        markdown-ts--enable-code-block-in-context-mode))
                       (push function context-timers)
                     (apply timer seconds repeat function args)))))
        (opencode--align-output-table (point-min) (point-max)))
      (should-not context-timers)
      (should-not markdown-ts-table-mode)
      (should-not markdown-ts-in-table-mode)
      (should (eq (key-binding (kbd "RET")) 'comint-send-input)))))

(ert-deftest opencode-session-history-table-alignment-test ()
  "Align replayed assistant tables, preserving user requests and code fences."
  (with-opencode-session-test-buffer
    (let* ((table "| Name | Value |\n| --- | ---: |\n| x | y |")
           (fenced (concat "```markdown\n" table "\n```"))
           (messages
            `(((id . "msg_1") (type . "user") (text . ,table))
              ,(opencode-session-test--assistant
                `(((type . "text") (text . ,table))
                  ((type . "text") (text . ,fenced)))
                "msg_2"))))
      ;; Capture the request separately to verify it reaches the user
      ;; renderer without going through the table alignment path.
      (cl-letf (((symbol-function 'opencode--replay-user-request)
                 (lambda (message)
                   (opencode--output (alist-get 'text message))
                   (opencode--output "\n\n"))))
        (opencode--replay-session-messages messages))
      (let ((output (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-prefix-p (concat table "\n\n") output))
        (should (string-match-p "|x +| +y|" output))
        (should (string-suffix-p (concat fenced "\n\n") output)))
      (should-not opencode--table-scan-marker)
      (should (= (process-mark process) (point-max))))))

(ert-deftest opencode-session-event-text-deltas-test ()
  "Text is output from its deltas, trimming only the start of the block."
  (with-opencode-session-test-buffer
    (opencode--output "request\n\n")
    (opencode-session-test--delta 'text "\n\n  Hello")
    (opencode-session-test--delta 'text " world\n")
    (opencode-session-test--ended 'text "\n\n  Hello world\n")
    (should (equal "request\n\nHello world\n" (opencode-session-test--output)))))

(ert-deftest opencode-session-event-only-ended-text-test ()
  "A block that only has final text, and no deltas, is output once."
  (with-opencode-session-test-buffer
    (opencode--output "request\n\n")
    (opencode-session-test--ended 'text "First.")
    (opencode-session-test--ended 'text "Second." 1)
    (opencode-session-test--ended 'text "  \n" 2)
    (should (equal "request\n\nFirst.\n\nSecond." (opencode-session-test--output)))))

(ert-deftest opencode-session-event-reasoning-then-text-test ()
  "Text and reasoning number their ordinals separately, so are different blocks."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(reasoning)))
      (opencode--output "request\n\n")
      (opencode-session-test--delta 'reasoning "thinking")
      (opencode-session-test--ended 'reasoning "thinking")
      (opencode-session-test--ended 'text "answer")
      (should (equal "request\n\nthinking\n\nanswer" (opencode-session-test--output)))
      (should opencode-last-response-start))))

(ert-deftest opencode-session-event-parallel-tools-test ()
  "Parallel tools finishing out of order put their output after their own call.
Call ids are only unique within a step, so they're keyed with the message too."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(grep)))
      (opencode--output "request\n\n")
      (opencode-session-test--tool "grep" "running" '((pattern . "one")) nil "a")
      (opencode-session-test--tool "grep" "running" '((pattern . "two")) nil "b")
      (opencode-session-test--tool "grep" "completed" '((pattern . "two")) "out-b" "b")
      (opencode-session-test--tool "grep" "completed" '((pattern . "one")) "out-a" "a")
      (should (string-suffix-p
               (concat "grep \"one\"\n```text\nout-a\n```\n\n"
                       "🔍 grep \"two\"\n```text\nout-b\n```\n\n")
               (opencode-session-test--output)))
      (should (zerop (hash-table-count opencode--tool-calls))))))

(ert-deftest opencode-session-event-tool-failed-without-called-test ()
  "A tool call that fails with malformed input is never called, but still shown."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(error)))
      (opencode--output "request\n\n")
      (opencode--handle-message
       '((type . "session.tool.input.started")
         (data . ((sessionID . "session") (assistantMessageID . "message")
                  (id . "call") (name . "edit")))))
      (opencode-session-test--tool "edit" "error" nil "Malformed input" "call")
      (should (string-match-p "\\`request\n\nedit\n```text\nMalformed input\n```"
                              (opencode-session-test--output)))
      (should (opencode-session-test--error-styled-p "Malformed input")))))

(ert-deftest opencode-session-event-question-tool-test ()
  "A question tool call is a block, its answers shown from its metadata."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(question)))
      (opencode--output "request\n\n")
      (opencode-session-test--tool
       "question" "running" opencode-session-test--questions nil "call")
      (opencode--handle-message
       '((type . "session.tool.success")
         (data . ((sessionID . "session") (assistantMessageID . "message") (id . "call")
                  (content . (((type . "text") (text . "User has answered"))))
                  (metadata . ((answers . [["A"] ["B" "C"] []])))
                  (executed . :false)))))
      (let ((text (opencode-session-test--output)))
        (should (string-search (concat "❓ One?\n❓ Two?\n❓ Three?\n"
                                       "```text\n→ A\n→ B, C\n→ Unanswered\n```")
                               text))
        (should-not (string-search "User has answered" text)))
      (should-not (invisible-p (save-excursion
                                 (goto-char (point-min))
                                 (search-forward "→ A")))))))

(ert-deftest opencode-session-event-question-tool-dismissed-test ()
  "A dismissed question tool call shows its error."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(error)))
      (opencode--output "request\n\n")
      (opencode-session-test--tool
       "question" "running" opencode-session-test--questions nil "call")
      (opencode-session-test--tool "question" "error" nil "dismissed" "call")
      (should (string-search "❓ Three?\n```text\ndismissed\n```"
                             (opencode-session-test--output)))
      (should (opencode-session-test--error-styled-p "dismissed")))))

(ert-deftest opencode-session-event-question-after-reasoning-test ()
  "Reasoning before a question tool call is folded without the question."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(question)))
      (opencode--output "request\n\n")
      (opencode-session-test--delta 'reasoning "thinking")
      (opencode-session-test--tool
       "question" "running" opencode-session-test--questions nil "call")
      (opencode-session-test--delta 'reasoning "more thinking" 1)
      (should-not (invisible-p (save-excursion
                                 (goto-char (point-min))
                                 (search-forward "❓ One?")))))))

(ert-deftest opencode-session-event-execution-test ()
  "Ending an execution goes back to a prompt, unless it'll resume after a shutdown.
A failure or user interrupt shows an error."
  (dolist (case '(("session.execution.succeeded" nil t)
                  ("session.execution.interrupted" ((reason . "user")) t "Interrupted")
                  ("session.execution.interrupted" ((reason . "shutdown")) t)
                  ("session.execution.failed"
                   ((error . ((type . "unknown") (message . "It broke")))) t "It broke")))
    (with-opencode-session-test-buffer
      (opencode-session-test--with-stubbed-api nil
        (opencode--output "request\n\n")
        (setq opencode-session-status "busy")
        (opencode--handle-message
         `((type . ,(car case))
           (data . ((sessionID . "session") ,@(cadr case)))))
        (should (equal opencode-session-status "idle"))
        (should (eq (opencode-session-test--at-prompt-p) (caddr case)))
        (if-let* ((error (cadddr case)))
            (should (opencode-session-test--error-face-p error))
          (should-not (string-search "Interrupted" (buffer-string)))))))
  ;; A real shutdown leaves the drain running, to resume after the restart.
  (with-opencode-session-test-buffer
    (opencode-session-test--with-stubbed-api '((session . ((type . "running"))))
      (opencode--output "request\n\n")
      (setq opencode-session-status "busy")
      (opencode--handle-message
       '((type . "session.execution.interrupted")
         (data . ((sessionID . "session") (reason . "shutdown")))))
      (should (equal opencode-session-status "busy"))
      (should-not (opencode-session-test--at-prompt-p)))))

(ert-deftest opencode-session-event-shell-test ()
  "A user's shell command isn't an execution, so it goes back to a prompt itself."
  (with-opencode-session-test-buffer
    (opencode--output "request\n\n")
    (opencode--handle-message
     '((type . "session.shell.ended")
       (data . ((sessionID . "session")
                (shell . ((status . "exited") (exit . 2)))
                (output . ((output . "bad output\n")))))))
    (should (opencode-session-test--at-prompt-p))
    (should (opencode-session-test--error-styled-p "bad output"))))

(ert-deftest opencode-session-event-selection-test ()
  "Selections made elsewhere are followed, and ours don't disturb local state."
  (with-opencode-session-test-buffer
    (setq opencode-session-agents (list '((id . "build") (name . "Build"))
                                        '((id . "plan") (name . "Plan")))
          opencode-session-agent (car opencode-session-agents)
          opencode--server-agent "build")
    (opencode--handle-message
     '((type . "session.agent.selected")
       (data . ((sessionID . "session") (agent . "plan")))))
    (should (equal "plan" (alist-get 'id opencode-session-agent)))
    (opencode--handle-message
     '((type . "session.model.selected")
       (data . ((sessionID . "session")
                (model . ((id . "m") (providerID . "p")))))))
    (should (equal "m" (map-nested-elt opencode-session-agent '(model id))))
    ;; An unsent local change isn't undone by an event for what's selected.
    (setq opencode-session-agent (car opencode-session-agents))
    (opencode--handle-message
     '((type . "session.agent.selected")
       (data . ((sessionID . "session") (agent . "plan")))))
    (should (equal "build" (alist-get 'id opencode-session-agent)))))

(ert-deftest opencode-session-status-indicator-test ()
  "The mode line never shows nil for an agent.
The server reports no variant as \"default\", which is no selection."
  (with-opencode-session-test-buffer
    (cl-flet ((indicator (agent)
                (let ((opencode-session-agent agent))
                  (opencode--session-status-indicator))))
      (should (string-search "🤖 Plan" (indicator '((id . "plan") (name . "Plan")))))
      (should (string-search "🤖 plan" (indicator '((id . "plan")))))
      (should (string-search "🤖 -" (indicator '((model . ((id . "m")))))))
      (should-not (string-search "nil" (indicator nil)))
      (cl-letf (((symbol-function 'opencode--current-model)
                 (lambda () '((name . "Model"))))
                ((symbol-function 'window-width) (lambda () 200)))
        (should-not (string-search
                     "default"
                     (indicator '((id . "a") (model . ((id . "m") (variant . "default")))))))
        (should (string-search
                 "Model high"
                 (indicator '((id . "a") (model . ((id . "m") (variant . "high")))))))))))

(ert-deftest opencode-open-project-buffers-test ()
  "A control buffer is named after the main checkout of its project."
  (let (opencode--session-control-buffers buffers)
    (cl-letf* (((symbol-function 'opencode-sessions-redisplay) #'ignore)
               ((symbol-function 'switch-to-buffer)
                (lambda (buffer &rest _) (push buffer buffers)))
               ((symbol-function 'opencode-api--request)
                (lambda (_method _path &rest args)
                  (funcall
                   (plist-get args :then)
                   (pcase default-directory
                     ((or "/src/repo/" "/src/worktree/")
                      '((project . ((id . "repo") (canonical . "/src/repo")))))
                     ("/other/repo/"
                      '((project . ((id . "other") (canonical . "/other/repo")))))
                     ("/scratch/"
                      '((project . ((id . "global") (canonical . "/"))))))))))
      (unwind-protect
          (progn
            (opencode-open-project "/src/repo")
            (should (equal "*OpenCode Sessions: repo*" (buffer-name (car buffers))))
            ;; A worktree shares the buffer, and new sessions go in the worktree.
            (opencode-open-project "/src/worktree/")
            (should (eq (car buffers) (cadr buffers)))
            (should (equal "/src/worktree/"
                           (buffer-local-value 'default-directory (car buffers))))
            ;; Same name, different project: it shows the one last opened.
            (opencode-open-project "/other/repo/")
            (should (eq (car buffers) (cadr buffers)))
            (should (equal "other"
                           (buffer-local-value 'opencode-session-control-project-id
                                               (car buffers))))
            (opencode-open-project "/scratch/")
            (should (equal "*OpenCode Sessions: global*" (buffer-name (car buffers)))))
        (mapc #'kill-buffer (delete-dups buffers))))))

(ert-deftest opencode-session-sync-only-what-differs-test ()
  "Switching agent always records a message, so is only done if it differs."
  (with-opencode-session-test-buffer
    (let (requests sent)
      (cl-letf (((symbol-function 'opencode-api--request)
                 (lambda (_method path &rest args)
                   (push path requests)
                   (funcall (plist-get args :then) nil))))
        (setq opencode-session-agent '((id . "build")
                                       (model . ((id . "m") (providerID . "p"))))
              opencode--server-agent "build")
        (opencode--sync-agent-and-model (lambda () (setq sent t)))
        (should sent)
        (should (equal requests '("/api/session/session/model")))
        (setq requests nil
              opencode--server-agent "plan")
        (opencode--sync-agent-and-model #'ignore)
        (should (equal requests '("/api/session/session/model"
                                  "/api/session/session/agent")))
        (should (equal opencode--server-agent "build"))))))

(ert-deftest opencode-session-load-selection-test ()
  "Opening a session selects what it has, or what its messages used if migrated."
  (with-opencode-session-test-buffer
    (let ((opencode-session-agents '(((id . "build") (name . "Build"))
                                     ((id . "plan") (name . "Plan"))))
          (opencode-agents '(((id . "build") (name . "Build") (mode . "primary")))))
      (cl-flet ((load-session (session messages)
                  (cl-letf (((symbol-function 'opencode-api--fetch-all-messages)
                             (lambda (_id callback) (funcall callback messages)))
                            ((symbol-function 'opencode--replay-session-messages) #'ignore)
                            ((symbol-function 'opencode-api--request) #'ignore))
                    (setq opencode-session-agent (car opencode-session-agents))
                    (opencode--load-session `((id . "session") ,@session))))
                (assistant (agent)
                  `((id . "m") (type . "assistant") (agent . ,agent))))
        (load-session '((agent . "plan") (model . ((id . "m") (providerID . "p"))))
                      (list (assistant "build")))
        (should (equal "plan" (alist-get 'id opencode-session-agent)))
        (should (equal "m" (map-nested-elt opencode-session-agent '(model id))))
        (load-session nil (list (assistant "build") (assistant "plan")))
        (should (equal "plan" opencode--server-agent))
        (should (equal "plan" (alist-get 'id opencode-session-agent)))
        ;; A new session keeps the local selection, and has the default on the server.
        (load-session nil nil)
        (should (equal "build" opencode--server-agent))
        (should (equal "build" (alist-get 'id opencode-session-agent)))))))

(ert-deftest opencode-session-replay-test ()
  "Replay prompts, a staged revert, and calls that later events finish."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs '(grep)))
      (opencode--replay-session-messages
       '(((id . "msg_1") (type . "user") (text . "first"))
         ((id . "msg_2") (type . "shell") (shellID . "sh") (command . "ls")
          (status . "exited") (exit . 0) (output . ((output . "listing"))))
         ((id . "msg_3") (type . "agent-switched") (agent . "plan"))
         ((id . "msg_4") (type . "synthetic") (text . "not shown"))
         ((id . "msg_5") (type . "assistant") (agent . "plan")
          (model . ((id . "m") (providerID . "p")))
          (time . ((created . 1)))
          (content . (((type . "text") (text . "partial"))
                      ((type . "tool") (id . "call") (name . "grep")
                       (state . ((status . "running")
                                 (input . ((pattern . "foo")))
                                 (metadata))))
                      ((type . "tool") (id . "later") (name . "grep")
                       (state . ((status . "streaming") (input . "{")))))))
         ((id . "msg_6") (type . "user") (text . "reverted")))
       "msg_6")
      (let ((output (buffer-string)))
        (should (string-search "first" output))
        (should (string-search "!ls" output))
        (should (string-search "listing" output))
        (should-not (string-search "not shown" output))
        (should-not (string-search "reverted" output)))
      ;; The running call is finished by a later event.
      (opencode--handle-message
       '((type . "session.tool.success")
         (data . ((sessionID . "session") (assistantMessageID . "msg_5") (id . "call")
                  (content . (((type . "text") (text . "matched")))) (executed . t)))))
      (should (string-search "grep \"foo\"\n```text\nmatched" (buffer-string)))
      ;; The call replayed while streaming keeps its name once called.
      (opencode--handle-message
       '((type . "session.tool.called")
         (data . ((sessionID . "session") (assistantMessageID . "msg_5") (id . "later")
                  (input . ((pattern . "bar")))))))
      (should (string-search "grep \"bar\"" (buffer-string)))
      ;; The partly replayed block isn't repeated when it ends.
      (opencode--handle-message
       '((type . "session.text.ended")
         (data . ((sessionID . "session") (assistantMessageID . "msg_5") (ordinal . 0)
                  (text . "partial and the rest")))))
      (should-not (string-search "the rest" (buffer-string))))))

(ert-deftest opencode-session-replay-migrated-tools-test ()
  "Sessions migrated from v1 have old tool and input names."
  (with-opencode-session-test-buffer
    (let ((opencode-shown-outputs nil))
      (opencode--replay-session-messages
       '(((id . "msg_1") (type . "assistant") (agent . "build")
          (time . ((created . 1) (completed . 2)))
          (content . (((type . "tool") (id . "a") (name . "bash")
                       (state . ((status . "completed")
                                 (input . ((command . "ls")))
                                 (content . (((type . "text") (text . "out"))))
                                 (metadata . ((exit . 0))))))
                      ((type . "tool") (id . "b") (name . "read")
                       (state . ((status . "completed")
                                 (input . ((filePath . "x.el")))
                                 (content . (((type . "text") (text . "out"))))))))))))
      (should (string-search "```bash\nls\n```" (buffer-string)))
      (should (string-search "read x.el" (buffer-string))))))

(ert-deftest opencode-session-prompt-body-test ()
  "Split label parts into the fields of a prompt, omitting empty ones."
  (should (equal '((text . "hi")) (opencode--prompt-body "hi" nil)))
  (let ((file (opencode--file-attachment "/tmp/a b.txt" "a b.txt"))
        (text (opencode--text-attachment "é" "region"))
        (agent '(agents . ((name . "explore")))))
    (should (equal "file:///tmp/a%20b.txt" (alist-get 'uri (cdr file))))
    (should (equal "data:text/plain;base64,w6k=" (alist-get 'uri (cdr text))))
    (should (equal `((text . "hi")
                     (files . ,(vector (cdr file) (cdr text)))
                     (agents . ,(vector (cdr agent))))
                   (opencode--prompt-body "hi" (list file agent text))))))

(ert-deftest opencode-session-attachable-file-test ()
  "Files that look like binaries the server would silently drop can't be attached."
  (let ((file (make-temp-file "opencode-test")))
    (unwind-protect
        (cl-flet ((with-bytes (bytes)
                    (let ((coding-system-for-write 'no-conversion))
                      (write-region bytes nil file nil 'silent))
                    (opencode--attachable-file-p file)))
          (should (with-bytes "plain text é\n"))
          (should (with-bytes ""))
          (should (with-bytes "\x89PNG\r\n\x1a\n\0\0"))
          (should (with-bytes "%PDF-1.7\0"))
          (should (with-bytes "RIFF\0\0\0\0WEBPVP8 "))
          (should (with-bytes "\xff\xd8\xff\0"))
          (should-not (with-bytes "binary\0data"))
          (should-not (with-bytes "RIFF\0\0\0\0WAVE"))
          (should-not (with-bytes "\xff\xfe\xfd invalid utf-8"))
          ;; A multibyte character cut in half by the size limit, in any position.
          (dolist (padding '(0 1 2 3))
            (should (with-bytes (concat (make-string (+ padding (* 1024 1024)) ?a)
                                        "\xf0\x9f\x98\x80")))
            (should (with-bytes (concat (make-string (- (* 1024 1024) padding) ?a)
                                        "\xf0\x9f\x98\x80")))))
      (delete-file file)))
  (should (opencode--attachable-file-p temporary-file-directory)))

(defmacro opencode-forms-test--with-server (&rest body)
  "Run BODY with an empty form queue, binding REQUESTS and TOASTS.
REQUESTS collects (METHOD PATH DIRECTORY BODY-JSON) of API requests, latest
first, and ELSE-HANDLERS their error handlers.  Menus aren't really shown and
MENUS collects their selections.  Timers run immediately."
  (declare (indent 0))
  `(let (opencode-forms--queue opencode-forms--current
                               opencode-forms--fields opencode-forms--answers
                               requests else-handlers toasts menus)
     (cl-letf (((symbol-function 'opencode-api--request)
                (lambda (method path &rest args)
                  (push (plist-get args :else) else-handlers)
                  (push (list method path default-directory
                              (and (plist-member args :body)
                                   (json-encode (plist-get args :body))))
                        requests)))
               ((symbol-function 'opencode-forms--show-menu)
                (lambda (selection) (push selection menus) t))
               ((symbol-function 'run-at-time)
                (lambda (_time _repeat function &rest args) (apply function args)))
               ((symbol-function 'opencode-open-session) #'ignore))
       (let ((opencode-toast-function (lambda (toast) (push toast toasts)))
             (default-directory "/project/"))
         ,@body))))

(defconst opencode-forms-test--websearch
  '((id . "frm_1") (sessionID . "global") (title . "Web Search")
    (metadata . ((kind . "websearch.provider")))
    (fields . (((key . "choice") (description . "Allow?") (type . "string")
                (required . t) (custom . :false)
                (options . (((value . "allow") (label . "Allow"))
                            ((value . "disable") (label . "Disable")))))))))

(ert-deftest opencode-forms-global-reply-test ()
  "A global form is shown right away and replied to from its own directory."
  (opencode-forms-test--with-server
    (opencode--form-request opencode-forms-test--websearch)
    (should (equal '(nil) menus))
    (should-not opencode-forms--queue)
    (let ((default-directory "/elsewhere/"))
      (opencode-forms--answer "allow"))
    (should (equal '((post "/api/session/global/form/frm_1/reply" "/project/"
                      "{\"answer\":{\"choice\":\"allow\"}}"))
                   requests))
    (should-not opencode-forms--current)))

(ert-deftest opencode-forms-duplicate-and-settled-test ()
  "A form is only queued once, and forgotten once settled."
  (opencode-forms-test--with-server
    (let ((form (append '((id . "frm_2") (sessionID . "ses_1")) (cddr opencode-forms-test--websearch)))
          (opencode-session-buffers (make-hash-table :test #'equal))
          (buffer (generate-new-buffer " *form-test*")))
      (unwind-protect
          (progn
            (puthash "ses_1" buffer opencode-session-buffers)
            (opencode--form-request form)
            (opencode--form-request form)
            (should (length= opencode-forms--queue 1))
            (should (length= toasts 1))
            (should (equal "Allow?" (alist-get 'message (car toasts))))
            (should-not menus)
            (opencode--form-settled "frm_2")
            (should-not opencode-forms--queue))
        (kill-buffer buffer)))))

(ert-deftest opencode-forms-unsupported-test ()
  "A form with an unknown field type is cancelled so nothing hangs."
  (opencode-forms-test--with-server
    (opencode--form-request '((id . "frm_1") (sessionID . "global") (title . "Login")
                              (fields . (((key . "x") (type . "hologram"))))))
    (should (eq 'delete (caar requests)))
    (should (string-match-p "\\`/api/session/global/form/frm_1\\?message=opencode\\.el%20"
                            (cadar requests)))
    (should (string-search "Login" (alist-get 'message (car toasts))))))

(ert-deftest opencode-forms-when-test ()
  "Conditions hold by value, by multiselect membership, and never if unanswered."
  (cl-flet ((active (op value answers)
              (opencode-forms--active-p
               `((when . (((key . "a") (op . ,op) (value . ,value)))))
               answers)))
    (should (opencode-forms--active-p '((key . "x")) nil))
    (should (active "eq" "y" '(("a" . "y"))))
    (should-not (active "eq" "y" '(("a" . "n"))))
    (should (active "neq" "y" '(("a" . "n"))))
    (should (active "eq" "y" '(("a" . ["n" "y"]))))
    (should-not (active "neq" "y" '(("a" . ["n" "y"]))))
    (should-not (active "eq" "y" nil))
    (should-not (active "neq" "y" nil))
    (should (active "eq" 3 '(("a" . 3.0))))
    (should (active "eq" :false '(("a" . :json-false))))))

(ert-deftest opencode-forms-hidden-and-inactive-test ()
  "Hidden fields use their default, and inactive fields aren't answered."
  (opencode-forms-test--with-server
    (opencode--form-request
     '((id . "frm_1") (sessionID . "global") (title . "T")
       (fields . (((key . "flag") (type . "boolean") (hidden . t) (default . :false))
                  ((key . "tags") (type . "multiselect") (hidden . t)
                   (default . ("a")) (options . (((value . "a") (label . "A")))))
                  ((key . "none") (type . "string") (hidden . t))
                  ((key . "off") (type . "string")
                   (when . (((key . "flag") (op . "eq") (value . t))))
                   (required . t))))))
    (should-not menus)
    (should (equal '((post "/api/session/global/form/frm_1/reply" "/project/"
                      "{\"answer\":{\"flag\":false,\"tags\":[\"a\"]}}"))
                   requests))))

(ert-deftest opencode-forms-empty-answer-test ()
  "Skipping every field replies with an empty object, not null."
  (opencode-forms-test--with-server
    (opencode--form-request
     '((id . "frm_1") (sessionID . "global") (title . "T")
       (fields . (((key . "a") (type . "boolean"))))))
    (opencode-forms--answer nil)
    (should (equal "{\"answer\":{}}" (nth 3 (car requests))))))

(ert-deftest opencode-forms-minibuffer-test ()
  "Text and numbers are read and checked in the minibuffer."
  (opencode-forms-test--with-server
    (let ((inputs '("" "abc" "1.5" "200" "7" "")) prompts)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &optional _initial)
                   (push prompt prompts)
                   (pop inputs))))
        (opencode--form-request
         '((id . "frm_1") (sessionID . "global") (title . "T")
           (fields . (((key . "n") (type . "integer") (title . "Count")
                       (required . t) (maximum . 100))
                      ((key . "note") (type . "string"))))))
        (should (equal "{\"answer\":{\"n\":7}}" (nth 3 (car requests))))
        (should (string-search "(at most 100)" (nth 1 prompts)))
        (should (string-search "(enter a whole number)" (nth 2 prompts)))
        (should (string-search "(enter a number)" (nth 3 prompts)))
        (should (string-search "(required)" (nth 4 prompts)))))))

(ert-deftest opencode-forms-minibuffer-quit-test ()
  "Quitting the minibuffer cancels the form."
  (opencode-forms-test--with-server
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (signal 'quit nil))))
      (opencode--form-request
       '((id . "frm_1") (sessionID . "global") (title . "T")
         (fields . (((key . "note") (type . "string"))))))
      (should (equal '(delete "/api/session/global/form/frm_1")
                     (seq-take (car requests) 2)))
      (should-not opencode-forms--current))))

(ert-deftest opencode-forms-invalid-reply-test ()
  "An invalid answer asks again showing why, and a settled form is left alone."
  (opencode-forms-test--with-server
    (opencode--form-request opencode-forms-test--websearch)
    (opencode-forms--answer "allow")
    (funcall (car else-handlers) "error: FormAlreadySettledError: done")
    (should (length= menus 1))
    (funcall (car else-handlers) "error: FormInvalidAnswerError: bad")
    (should (length= menus 2))
    (should (string-search "bad" (opencode-forms--heading opencode-forms--current
                                                          (opencode-forms--field))))))

(ert-deftest opencode-forms-question-heading-test ()
  "Question forms don't show their generic title, and others show the title."
  (let ((field '((key . "k") (description . "Why?"))))
    (should (equal "Why?" (opencode-forms--heading
                           '((title . "Questions") (metadata . ((kind . "question"))))
                           field)))
    (should (equal "srv is requesting input\nSign in\nWhy?"
                   (opencode-forms--heading
                    '((title . "srv is requesting input")
                      (metadata . ((kind . "mcp-elicitation") (message . "Sign in"))))
                    field)))))

(ert-deftest opencode-json-falsy-test ()
  "All of JSON's false values are recognized, as `:false' is truthy in Lisp."
  (dolist (value '(nil :null :false 0))
    (should (opencode--json-falsy value)))
  (dolist (value '(t 1 "text"))
    (should-not (opencode--json-falsy value))))

(ert-deftest opencode-api-all-messages-test ()
  "Follow cursors through all pages, and return messages oldest first."
  (let (paths result)
    (cl-letf (((symbol-function 'opencode-api--request)
               (lambda (_method path &rest args)
                 (push path paths)
                 (funcall (plist-get args :then)
                          (if (string-search "cursor=" path)
                              '((data . (((id . "1"))))
                                (cursor . ((previous . "p") (next . :null))))
                            '((data . (((id . "3")) ((id . "2"))))
                              (cursor . ((next . "a=b")))))))))
      (opencode-api-all-messages ("session")
          messages
        (setq result messages)))
    (should (equal '("1" "2" "3") (mapcar (lambda (m) (alist-get 'id m)) result)))
    (should (equal "/api/session/session/message?limit=200&cursor=a%3Db" (car paths)))))

(defvar doom-switch-buffer-hook)

(ert-deftest opencode-switch-to-buffer-doom-hook-test ()
  "Doom's switch buffer hook runs with the new buffer current.
Also after a minibuffer read in an async callback leaves `this-command' set."
  (let ((buffer (generate-new-buffer " *opencode-switch-test*"))
        (opencode-open-session-same-window t)
        seen
        (doom-switch-buffer-hook nil))
    (add-hook 'doom-switch-buffer-hook (lambda () (push (current-buffer) seen)))
    (cl-letf (((symbol-function 'switch-to-buffer) #'ignore))
      (unwind-protect
          (progn
            (let ((this-command nil))
              (opencode--switch-to-buffer buffer))
            (should (equal (list buffer) seen))
            (setq seen nil)
            (let ((this-command 'vertico-exit))
              (opencode--switch-to-buffer buffer))
            (should (equal (list buffer) seen)))
        (kill-buffer buffer)))))

(defmacro opencode-session-test--with-claude-plan (tier &rest body)
  "Run BODY on Claude plan TIER, with a fixed catalog of API prices."
  (declare (indent 1))
  `(let ((opencode--claude-plan-tier ,tier)
         (opencode-claude-five-hour-budgets
          '(("pro" . 20) ("max_5x" . 125) ("max_20x" . 294)))
         (opencode--claude-catalog
          '((claude-opus-4-8 (release_date . "2026-05-28")
             (cost (input . 5) (output . 25) (cache_read . 0.5)
                   (cache_write . 6.25)))
            (claude-opus-5-5 (release_date . "2026-09-22")
                             (cost (input . 4) (output . 20) (cache_read . 0.2)
                                   (cache_write . 5))))))
     ,@body))

(defun opencode-session-test--claude-session (model &optional cost provider)
  "Return a session using MODEL of PROVIDER that charged COST.
Its tokens cost $2.00 at claude-opus-5-5's prices.  PROVIDER defaults to
claude-agent."
  `((cost . ,(or cost 0))
    (model (id . ,model) (providerID . ,(or provider "claude-agent")))
    (tokens (input . 100000) (output . 40000) (reasoning . 10000)
            (cache (read . 1000000) (write . 80000)))))

(ert-deftest opencode-session-control-cost-shows-budget-share ()
  "Claude subscription sessions show API dollars and the share of the budget."
  (dolist (case '(("pro" . "$2.00 10.0%")
                  ("max_5x" . "$2.00 1.6%")
                  ("max_20x" . "$2.00 0.7%")))
    (opencode-session-test--with-claude-plan (car case)
      (should (equal (cdr case)
                     (opencode--session-control-cost
                      (opencode-session-test--claude-session
                       "claude-opus-5-5")))))))

(ert-deftest opencode-session-control-cost-other-plans-omit-share ()
  "Plans without a budget, or an unknown plan, show dollars only."
  (dolist (tier '("other" "max_other" nil unfetched))
    (opencode-session-test--with-claude-plan tier
      (should (equal "$2.00"
                     (opencode--session-control-cost
                      (opencode-session-test--claude-session
                       "claude-opus-5-5")))))))

(ert-deftest opencode-session-control-cost-resolves-aliases ()
  "An alias uses the newest model of its family, and [1m] is ignored."
  (opencode-session-test--with-claude-plan "pro"
    (dolist (model '("opus" "opus[1m]" "claude-opus-5-5[1m]"))
      (should (equal "$2.00 10.0%"
                     (opencode--session-control-cost
                      (opencode-session-test--claude-session model)))))))

(ert-deftest opencode-session-control-cost-without-price ()
  "A model with no known price shows no cost."
  (opencode-session-test--with-claude-plan "pro"
    (should (equal "-"
                   (opencode--session-control-cost
                    (opencode-session-test--claude-session "claude-fable-5-2"))))
    (let ((opencode--claude-catalog nil))
      (should (equal "-"
                     (opencode--session-control-cost
                      (opencode-session-test--claude-session "opus")))))))

(ert-deftest opencode-session-control-cost-real-dollars-unchanged ()
  "Sessions the server charged for, and other providers, show the server's cost."
  (opencode-session-test--with-claude-plan "pro"
    (should (equal "$3.21"
                   (opencode--session-control-cost
                    (opencode-session-test--claude-session "opus" 3.21))))
    (should (equal "$0.00"
                   (opencode--session-control-cost
                    (opencode-session-test--claude-session
                     "gpt-5" 0 "openai"))))))

(defun opencode-session-test--rate-limit (now)
  "Return claude-agent rate limit info with windows resetting relative to NOW."
  `((status . "allowed")
    (unifiedWindows
     (five_hour (utilization . 0.09) (resetsAt . ,(+ now 3600)))
     (seven_day (utilization . 0.31) (resetsAt . ,(- now 60)))
     (seven_day_sonnet (utilization . 0.85) (resetsAt . ,(+ now 3600))))))

(ert-deftest opencode-claude-usage-fresh ()
  "Fresh usage shows exact percentages, and windows past their reset as 0%."
  (let* ((now (float-time))
         (usage (opencode--format-claude-usage
                 (opencode-session-test--rate-limit now))))
    (should (string-match-p
             (rx bos "Claude usage: 5h " (= 2 "━") (= 18 "─")
                 " 9% (resets " (+ (not ")")) ") · "
                 "week " (= 20 "─") " 0% (reset " (+ (not ")")) ") · "
                 "seven day sonnet " (= 17 "━") (= 3 "─")
                 " 85% (resets " (+ (not ")")) ")" eos)
             usage))
    (should (eq 'success (get-text-property (string-search "━" usage)
                                            'face usage)))
    (should (eq 'error (get-text-property
                        (string-search "━" usage (string-search "sonnet" usage))
                        'face usage)))
    (should-not (get-text-property (string-search "85%" usage) 'face usage))))

(ert-deftest opencode-claude-usage-wrap ()
  "Usage is split one window per line when it doesn't fit in WIDTH."
  (let* ((now (float-time))
         (rate-limit (opencode-session-test--rate-limit now))
         (line (opencode--format-claude-usage rate-limit))
         (narrow (opencode--format-claude-usage rate-limit 10)))
    (should (equal line (opencode--format-claude-usage rate-limit 100000)))
    (should-not (string-search " · " narrow))
    (should (string-match-p
             (rx bos "Claude usage:\n5h " (+ nonl) "\nweek " (+ nonl)
                 "\nseven day sonnet " (+ nonl) eos)
             narrow))))

(ert-deftest opencode-claude-usage-bar-face ()
  "Usage bars are green below 60%, warning below 85% and error above."
  (should (eq 'success (opencode--usage-bar-face 0.59)))
  (should (eq 'warning (opencode--usage-bar-face 0.6)))
  (should (eq 'warning (opencode--usage-bar-face 0.84)))
  (should (eq 'error (opencode--usage-bar-face 0.85))))

(ert-deftest opencode-claude-usage-overlay ()
  "Recorded usage is shown after the end of session control buffers."
  (let ((opencode--claude-rate-limit nil)
        (opencode--session-control-buffers nil))
    (with-temp-buffer
      (insert "table\n")
      (push (current-buffer) opencode--session-control-buffers)
      (opencode--session-control-update-usage)
      (should-not (overlay-get opencode--session-control-usage-overlay
                               'after-string))
      (opencode--update-claude-rate-limit
       (opencode-session-test--rate-limit (float-time)))
      (should (= (point-max) (overlay-start
                              opencode--session-control-usage-overlay)))
      (should (string-match-p
               (rx bos "\nClaude usage: 5h " (+ (any "━─")) " 9%")
               (overlay-get opencode--session-control-usage-overlay
                            'after-string)))
      ;; An older report doesn't replace a newer one
      (opencode--update-claude-rate-limit '((unifiedWindows)) 0)
      (should (string-search
               " 9% (resets" (overlay-get opencode--session-control-usage-overlay
                                          'after-string))))))

(defun opencode-session-test--tree-session (id parent created updated)
  "Return a session ID spawned by PARENT with CREATED and UPDATED times."
  `((id . ,id)
    ,@(when parent `((parentID . ,parent)))
    (title . ,id)
    (time (created . ,created) (updated . ,updated))))

(defconst opencode-session-test--tree-sessions
  (mapcar (lambda (args)
            (apply #'opencode-session-test--tree-session args))
          '(("second" "old-root" 20000 90000)
            ("new-root" nil 5000 100000)
            ("first" "old-root" 10000 95000)
            ("nested" "first" 12000 70000)
            ("old-root" nil 1000 50000)
            ("orphan" "missing" 3000 60000)))
  "Sessions in no particular order, forming a tree when arranged.")

(defconst opencode-session-test--tree-expected
  '("new-root" "orphan" "old-root" "├─ first" "│  └─ nested" "└─ second")
  "Titles and connectors of `opencode-session-test--tree-sessions' as a tree.")

(ert-deftest opencode-session-control-tree-order-and-prefixes ()
  "Subagents follow their parent oldest first, connected by tree characters."
  (should (equal opencode-session-test--tree-expected
                 (mapcar (lambda (session)
                           (concat (alist-get 'tree-prefix session)
                                   (alist-get 'id session)))
                         (opencode--session-control-tree
                          opencode-session-test--tree-sessions)))))

(defun opencode-session-test--table-titles (verbose &optional sort-by)
  "Return the title column of a session control table, with VERBOSE set.
If SORT-BY is non-nil, sort the table by it, a list of vtable sort keys
for the columns title and last-update."
  (with-temp-buffer
    (let ((opencode-session-control-verbose verbose))
      (opencode--session-control-insert-table
       opencode-session-test--tree-sessions '(title last-update))
      (goto-char (point-min))
      (when sort-by
        (setf (vtable-sort-by (vtable-current-table)) sort-by)
        (vtable-revert-command)
        (goto-char (point-min)))
      (let (titles)
        (while (not (eobp))
          (push (replace-regexp-in-string
                 " +[^ ]+\\'" ""
                 (buffer-substring-no-properties
                  (point) (line-end-position)))
                titles)
          (forward-line 1))
        (nreverse titles)))))

(ert-deftest opencode-session-control-verbose-table-keeps-tree-order ()
  "The verbose table isn't reordered by the last updated column."
  (should (equal opencode-session-test--tree-expected
                 (opencode-session-test--table-titles t))))

(ert-deftest opencode-session-control-verbose-table-sorts-only-parents ()
  "Sorting the verbose table reorders top level sessions, not subagents."
  (should (equal '("old-root" "├─ first" "│  └─ nested" "└─ second"
                   "orphan" "new-root")
                 (opencode-session-test--table-titles t '((1 . descend)))))
  (should (equal '("orphan" "old-root" "├─ first" "│  └─ nested"
                   "└─ second" "new-root")
                 (opencode-session-test--table-titles t '((0 . descend))))))

(ert-deftest opencode-session-control-usage-stays-at-end-when-sorting ()
  "The Claude usage overlay stays after the table when it's sorted."
  (with-temp-buffer
    (let ((opencode--claude-rate-limit
           '(0 (unifiedWindows (five_hour (utilization . 0.5))))))
      (opencode--session-control-insert-table
       opencode-session-test--tree-sessions '(title last-update))
      (opencode--session-control-update-usage)
      (goto-char (point-min))
      (setf (vtable-sort-by (vtable-current-table)) '((0 . descend)))
      (vtable-revert-command)
      (should (= (point-max)
                 (overlay-start opencode--session-control-usage-overlay))))))

(ert-deftest opencode-session-control-normal-table-sorted-by-update ()
  "Without verbose the table has no connectors and sorts by last update."
  (should (equal '("new-root" "first" "second" "nested" "orphan" "old-root")
                 (opencode-session-test--table-titles nil))))

(ert-deftest opencode-api-fetch-all-sessions-test ()
  "Sessions of every page are combined in the server's order."
  (let (paths result)
    (cl-letf (((symbol-function 'opencode-api--request)
               (cl-function
                (lambda (_method path &key then &allow-other-keys)
                  (push path paths)
                  (funcall then
                           (if (string-search "cursor=" path)
                               '((data ((id . "c")))
                                 (cursor (next . :null)))
                             '((data ((id . "a")) ((id . "b")))
                               (cursor (next . "a=b")))))))))
      (opencode-api--fetch-all-sessions
       "project" t (lambda (sessions) (setq result sessions)))
      (should (equal '("a" "b" "c")
                     (mapcar (lambda (s) (alist-get 'id s)) result)))
      (should (equal '("/api/session?project=project&archived=false&limit=200&cursor=a%3Db"
                       "/api/session?project=project&archived=false&limit=200")
                     paths))
      (setq paths nil)
      (opencode-api--fetch-all-sessions "project" nil #'ignore)
      (should (equal "/api/session?project=project&archived=false&limit=200&parentID=null"
                     (car (last paths)))))))

(defmacro opencode-session-test--with-finished-editing (&rest body)
  "Run BODY in a session whose finished editing hook records its FILES.
BODY can refer to `hook-calls', the base names of the FILES of each call
with the newest first, and `respond', which is nil until the session's
diff is requested, then a function to answer that request."
  (declare (indent 0) (debug t))
  `(with-opencode-session-test-buffer
     (let* ((hook-calls nil)
            (respond nil)
            (opencode-files-finished-editing-functions
             (list (lambda (files done)
                     (push (mapcar #'file-name-nondirectory files) hook-calls)
                     (funcall done)))))
       (cl-letf (((symbol-function 'opencode-api--request)
                  (lambda (_method path &rest args)
                    (should (equal path "/api/session/session/diff?context=0"))
                    (setq respond (plist-get args :then)))))
         ,@body))))

(defun opencode-session-test--diff-entry (file status &optional patch)
  "Return a session diff entry for FILE with STATUS and PATCH."
  `((file . ,file) (status . ,status) (patch . ,(or patch "@@ -1 +1 @@\n"))))

(ert-deftest opencode-files-finished-editing-uses-session-diff-test ()
  "Hooks get the existing text files in the turn's diff."
  (opencode-session-test--with-finished-editing
    (opencode--maybe-run-files-finished-editing-hook "session" nil)
    (should-not hook-calls)
    (funcall respond
             (list (opencode-session-test--diff-entry "opencode-api.el" "modified")
                   (opencode-session-test--diff-entry "opencode.el" "deleted")
                   (opencode-session-test--diff-entry "README.org" "modified" "")
                   (opencode-session-test--diff-entry "missing.el" "modified")))
    (should (equal hook-calls '(("opencode-api.el"))))))

(ert-deftest opencode-files-finished-editing-deletion-only-test ()
  "Hooks run with no files when the turn only deleted files."
  (opencode-session-test--with-finished-editing
    (opencode--maybe-run-files-finished-editing-hook "session" nil)
    (funcall respond (list (opencode-session-test--diff-entry "gone.el" "deleted")))
    (should (equal hook-calls '(nil)))))

(ert-deftest opencode-files-finished-editing-empty-diff-test ()
  "Hooks don't run for a turn that changed nothing, nor is it superseding."
  (opencode-session-test--with-finished-editing
    (setq opencode--files-finished-editing-run 'running)
    (opencode--maybe-run-files-finished-editing-hook "session" nil)
    (funcall respond nil)
    (should-not hook-calls)
    (should (eq opencode--files-finished-editing-run 'running))))

(ert-deftest opencode-files-finished-editing-no-hooks-test ()
  "The diff isn't requested when no hooks would use it."
  (opencode-session-test--with-finished-editing
    (let ((opencode-files-finished-editing-functions nil))
      (opencode--maybe-run-files-finished-editing-hook "session" nil))
    (should-not respond)))

(ert-deftest opencode-files-finished-editing-interrupted-test ()
  "Interrupting cancels running hooks and a pending diff, and requests none."
  (opencode-session-test--with-finished-editing
    (setq opencode--files-finished-editing-run 'running)
    (opencode--maybe-run-files-finished-editing-hook "session" t)
    (should-not respond)
    (should-not opencode--files-finished-editing-run)
    (opencode--maybe-run-files-finished-editing-hook "session" nil)
    (opencode--maybe-run-files-finished-editing-hook "session" t)
    (funcall respond (list (opencode-session-test--diff-entry "opencode.el" "modified")))
    (should-not hook-calls)))

(ert-deftest opencode-files-finished-editing-prompt-during-request-test ()
  "A diff answered after a new prompt was sent is of the wrong turn."
  (opencode-session-test--with-finished-editing
    (opencode--maybe-run-files-finished-editing-hook "session" nil)
    (setq opencode-session-status "busy")
    (funcall respond (list (opencode-session-test--diff-entry "opencode.el" "modified")))
    (should-not hook-calls)))

(ert-deftest opencode-files-finished-editing-superseded-request-test ()
  "Only the newest of overlapping diff requests runs the hooks."
  (opencode-session-test--with-finished-editing
    (opencode--maybe-run-files-finished-editing-hook "session" nil)
    (let ((stale respond))
      (opencode--maybe-run-files-finished-editing-hook "session" nil)
      (funcall stale (list (opencode-session-test--diff-entry "opencode.el" "modified")))
      (should-not hook-calls)
      (funcall respond (list (opencode-session-test--diff-entry "opencode-api.el" "modified")))
      (should (equal hook-calls '(("opencode-api.el")))))))

(defmacro opencode-session-test--with-btw (response &rest body)
  "Run BODY with generate requests answered with RESPONSE.
BODY can refer to the requests made as `requests', newest first, and to the
buffers displayed as `shown', newest first.  The displayed buffers are killed
afterwards."
  (declare (indent 1))
  `(let (requests shown)
     (cl-letf (((symbol-function 'opencode-api--request)
                (lambda (method path &rest args)
                  (push (list method path (plist-get args :body)) requests)
                  (funcall (plist-get args :then) ,response)))
               ((symbol-function 'opencode--switch-to-buffer)
                (lambda (buffer) (push buffer shown))))
       (unwind-protect
           (with-temp-buffer
             (setq opencode-session-id "session")
             ,@body)
         (mapc #'kill-buffer shown)))))

(ert-deftest opencode-btw-sends-transient-request-test ()
  (opencode-session-test--with-btw '((text . "answer"))
    (opencode-btw "why?")
    (pcase-let ((`((,method ,path ,body)) requests))
      (should (eq method 'post))
      (should (equal path "/api/session/session/generate"))
      (should (equal (alist-get 'prompt body)
                     (concat opencode--btw-instructions "\n\nwhy?"))))))

(ert-deftest opencode-btw-each-answer-gets-a-new-buffer-test ()
  (opencode-session-test--with-btw '((text . "  **answer**\n"))
    (opencode-btw "why?")
    (opencode-btw "why?")
    (should (= (length shown) 2))
    (should-not (eq (car shown) (cadr shown)))
    (with-current-buffer (car shown)
      (should (derived-mode-p 'markdown-ts-view-mode))
      (should buffer-read-only)
      (should (equal (buffer-string) "> why?\n\n**answer**\n")))))

(ert-deftest opencode-btw-blank-answer-test ()
  (opencode-session-test--with-btw '((text . " \n"))
    (opencode-btw "why?")
    (should-not shown)))

(ert-deftest opencode-btw-blank-question-test ()
  (opencode-session-test--with-btw '((text . "answer"))
    (should-error (opencode-btw "  ") :type 'user-error)
    (should-not requests)))

(provide 'opencode-sessions-test)
;;; opencode-sessions-test.el ends here
