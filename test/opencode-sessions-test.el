;;; opencode-sessions-test.el --- Session rendering tests  -*- lexical-binding: t; -*-
;;; Code:

(require 'ert)
(require 'etags)
(require 'opencode)
(require 'opencode-sessions)
(require 'opencode-test-helpers)

(defmacro opencode-session-test--with-stubbed-api (active &rest body)
  "Run BODY with api requests answered, and toasts and alerts discarded.
Requests for active sessions return ACTIVE, and others a session."
  (declare (indent 1))
  `(let ((opencode-toast-function #'ignore)
         (opencode-alerted-sessions nil)
         (opencode-file-edited-functions nil)
         (opencode-files-finished-editing-functions nil)
         (opencode-project-files-finished-editing-functions nil))
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

(ert-deftest opencode-tool-formatter-malformed-input-test ()
  "Show input a tool's formatter can't handle generically, not signal."
  (should (equal (opencode--format-tool-call
                  "question" '((questions . "[{\"question\":\"x\"}]")))
                 "question [{\"question\":\"x\"}]"))
  (should (equal (opencode--format-tool-call
                  "todowrite" '((todos . "[{\"content\":\"x\"}]") (merge . t)))
                 "todowrite (todos=[{\"content\":\"x\"}], merge=t)")))

(ert-deftest opencode-search-tool-code-span-test ()
  "Show search patterns and targets as code, not Markdown emphasis."
  (should (equal (opencode--format-tool-call
                  "grep" '((pattern . "a*b|c*d") (include . "*test*.el")))
                 "🔍 grep `a*b|c*d` in `*test*.el`"))
  (should (equal (opencode--format-tool-call "grep" '((pattern . "x`y")))
                 "🔍 grep ``x`y``"))
  (should (equal (opencode--format-tool-call "glob" '((pattern . "**/*.el")))
                 "🔍 glob `**/*.el`"))
  (with-opencode-session-test-buffer
    (font-lock-mode 1)
    (opencode--insert-tool-block
     "grep" '((pattern . "a*b|c*d") (include . "*test*.el")))
    (opencode--show-prompt)
    (opencode-session-test--refontify)
    (goto-char (point-min))
    (dolist (text '("a*b|c*d" "*test*.el"))
      (search-forward text)
      (let ((face (get-text-property (+ (match-beginning 0) 2) 'face)))
        (should-not (memq 'markdown-ts-emphasis (ensure-list face)))))))

(ert-deftest opencode-session-markdown-fontification-test ()
  "Stream Markdown without replacing text or losing session decorations."
  (with-opencode-session-test-buffer
    (font-lock-mode 1)
    (opencode-session-test--delta 'text "# Heading\n\nSee `opencode-sessions")
    (opencode-session-test--delta 'text ".el` and *emphasis*.\n\n")
    (opencode-session-test--ended 'text nil)
    (opencode--insert-tool-block "edit" opencode-session-test--edit)
    (opencode--output "**Testing button behavior**\n\n")
    (opencode--insert-block-with-margin
     (concat "emacs_eval_elisp:\n"
             (opencode--fenced-block "(+ 1 2)" "elisp"))
     'opencode-tool-margin-highlight)
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

(ert-deftest opencode-session-inline-image-test ()
  "Display references to images in assistant text as the image.
References in reasoning aren't buttons."
  (skip-unless (image-type-available-p 'svg))
  (let ((image (make-temp-file "opencode-test-" nil ".svg"
                               "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"8\" height=\"8\"/>")))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) #'always))
          (with-opencode-session-test-buffer
            (font-lock-mode 1)
            (opencode-session-test--delta
             'text (format "See `%s` and `opencode-sessions.el`.\n\nAgain `%s"
                           image (substring image 0 5)))
            (goto-char (point-min))
            (search-forward image)
            (let ((button (button-at (1- (point)))))
              (should button)
              (should (equal (car (button-get button 'button-data)) image))
              (should (eq (car-safe (get-text-property (1- (point)) 'display))
                          'image)))
            (search-forward "opencode-sessions.el")
            (should (button-at (1- (point))))
            (should-not (eq (car-safe (get-text-property (1- (point)) 'display))
                            'image))
            ;; The last line is only buttonized once the text part ends.
            (opencode-session-test--delta 'text (concat (substring image 5) "`"))
            (search-forward image)
            (should-not (button-at (1- (point))))
            (opencode-session-test--ended 'text nil)
            (should (button-at (1- (point))))
            (should (eq (car-safe (get-text-property (1- (point)) 'display))
                        'image))
            (opencode-session-test--delta 'reasoning (format "Seen `%s`\n\n" image) 1)
            (opencode-session-test--delta 'text "Done.\n\n" 2)
            (opencode-session-test--refontify)
            (search-forward image)
            (should-not (button-at (1- (point))))
            (should-not (get-text-property (1- (point)) 'display))))
      (delete-file image))))

(ert-deftest opencode-session-latex-test ()
  "Display math as images once rendered, but not the text between prices."
  (skip-unless (and (image-type-available-p 'svg)
                    (executable-find "latex")
                    (executable-find "dvisvgm")))
  (let ((opencode-render-latex t)
        (opencode--latex-cache (make-hash-table :test 'equal))
        (opencode--latex-waiting (make-hash-table :test 'equal))
        (opencode--latex-queue nil)
        (opencode--latex-busy nil))
    (cl-letf (((symbol-function 'display-graphic-p) #'always))
      (with-opencode-session-test-buffer
        (font-lock-mode 1)
        (opencode-session-test--delta
         'text "So $$E = mc^2$$ and $x^2$, for $5 and $10.\n\nBad $\\frac{$.\n\n")
        (font-lock-ensure)
        (let ((image-at (lambda (text)
                          (goto-char (point-min))
                          (search-forward text)
                          (let ((display (get-text-property (match-beginning 0) 'display)))
                            (and (eq (car-safe display) 'image) display)))))
          (opencode-test-wait-until
           (progn (font-lock-ensure)
                  (and (funcall image-at "E = mc^2") (funcall image-at "x^2")
                       (not opencode--latex-busy)))
           10)
          (should (consp (gethash '("E = mc^2" . t) opencode--latex-cache)))
          (should (consp (gethash '("x^2" . nil) opencode--latex-cache)))
          (should (eq 'error (gethash '("\\frac{" . nil) opencode--latex-cache)))
          (should-not (funcall image-at "\\frac{"))
          (should-not (funcall image-at "5 and "))
          (opencode-session-test--refontify)
          (should (funcall image-at "E = mc^2")))))))

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
      ;; replaying it showed another prompt before it
      (setq start (save-excursion
                    (goto-char (point-min))
                    (search-forward "> **bold**")
                    (match-beginning 0)))
      (should (get-char-property (+ start 2) 'invisible))
      (should (opencode-session-test--margin-p start)))))

(ert-deftest opencode-session-before-first-prompt-test ()
  "Fork and revert refuse, without a request, before the first prompt."
  (with-opencode-session-test-buffer
    (opencode--output "logo ❯\n\n")
    (opencode--replay-input "hi" "msg_1")
    (opencode--output "reply\n\n")
    (opencode--show-prompt)
    (cl-letf (((symbol-function 'opencode-api--request)
               (lambda (&rest _) (ert-fail "Unexpected api request"))))
      (let ((text (buffer-string)))
        (dolist (pos (list (point-min) 3))
          (goto-char pos)
          (should-error (opencode--user-message-id-at-point) :type 'user-error)
          (should-error (opencode-fork-session) :type 'user-error)
          (should-error (opencode-revert-message) :type 'user-error)
          (should-error (opencode-ediff-turn) :type 'user-error)
          (should (equal-including-properties (buffer-string) text)))))))

(ert-deftest opencode-session-user-message-id-at-point-test ()
  "The user message at point is the one its prompt's sent input is marked with.
Shell commands, sent input not yet given its id, and the unsent input
have none, even if copied from sent input, as after a revert."
  (with-opencode-session-test-buffer
    (opencode--replay-input "hi" "msg_1")
    (opencode--output "reply\n\n")
    (opencode--replay-input "!ls" 'shell)
    (opencode--output "file\n\n")
    (opencode--replay-input "pending")
    (opencode--show-prompt)
    (insert (propertize "unsent" 'opencode-user-message "msg_1"))
    (cl-flet ((id-at (text)
                (goto-char (point-min))
                (search-forward text)
                (condition-case err
                    (opencode--user-message-id-at-point)
                  (user-error (cadr err)))))
      (should (equal "msg_1" (id-at "hi")))
      (should (equal "msg_1" (id-at "reply")))
      (should (equal (format-message "Can't do that to a shell command") (id-at "!ls")))
      (should (equal (format-message "Can't do that to a shell command") (id-at "file")))
      (should (equal "No user message at point (not yet sent)" (id-at "pending")))
      (should (equal "No user message at point" (id-at "unsent"))))))

(ert-deftest opencode-session-send-empty-input-test ()
  "Don't send empty input, from the prompt or from an empty line above it."
  (with-opencode-session-test-buffer
    (let* ((sent nil)
           (comint-input-sender (lambda (_proc string) (push string sent))))
      (opencode--output "reply\n\n")
      (opencode--show-prompt)
      (let ((text (buffer-string)))
        (opencode-send-input)
        (goto-char (point-min))
        (forward-line 1)
        (opencode-send-input)
        (should (equal (buffer-string) text))
        (should-not sent))
      (goto-char (point-max))
      (insert "hi")
      (opencode-send-input)
      (should (equal sent '("hi"))))))

(ert-deftest opencode-session-keys-test ()
  "Queueing input and backgrounding jobs have their keys."
  (should (eq (keymap-lookup opencode-session-mode-map "C-<return>")
              'opencode-queue-input))
  (should (eq (keymap-lookup opencode-session-mode-map "C-c C-b")
              'opencode-background-all-jobs)))

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

(ert-deftest opencode-session-imenu-test ()
  "Imenu indexes sent user messages by their first line, not unsent input."
  (with-opencode-session-test-buffer
    (opencode--output "# Heading\n\n")
    (opencode--replay-input "first")
    (opencode--output "## Reply heading\n\n")
    (opencode--replay-input "\nsecond line one\nline two")
    (opencode--output "Reply\n\n")
    (opencode--show-prompt)
    (insert "unsent")
    (let ((imenu-use-markers nil)
          (index (funcall imenu-create-index-function)))
      (should (equal '("1: first" "2: second line one") (mapcar #'car index)))
      (goto-char (cdr (nth 1 index)))
      (should (looking-at-p "\nsecond line one")))))

(ert-deftest opencode-session-project-tags-test ()
  "Tags complete from the project's TAGS file, leaving global tables alone."
  (let* ((root (make-temp-file "opencode-tags" t))
         (tags (expand-file-name "TAGS" root))
         (global (expand-file-name "OTHER-TAGS" root))
         (etags-regen-mode nil)
         (tags-file-name global)
         (tags-table-list (list global))
         tags-table-computed-list tags-table-computed-list-for
         tags-table-list-pointer tags-table-list-started-at
         tags-table-set-list)
    (unwind-protect
        (progn
          (with-opencode-session-test-buffer
            (setq opencode--project-root root)
            (opencode--use-project-tags)
            (should-not (local-variable-p 'tags-file-name)))
          (with-temp-file tags
            (insert "\f\nfoo.el,60\n"
                    "(defun opencode-tags-test-symbol \x7f"
                    "opencode-tags-test-symbol\x011,0\n"))
          (with-opencode-session-test-buffer
            (setq opencode--project-root root)
            (opencode--use-project-tags)
            (should (equal tags tags-file-name))
            (should (equal (list tags) tags-table-list))
            (insert "opencode-tags-test-sy")
            (pcase-let ((`(,beg ,end ,table . ,_)
                         (tags-completion-at-point-function)))
              (should (equal '("opencode-tags-test-symbol")
                             (all-completions (buffer-substring beg end)
                                              table)))))
          (should (equal global tags-file-name))
          (should (equal (list global) tags-table-list)))
      (when-let* ((buffer (find-buffer-visiting tags)))
        (kill-buffer buffer))
      (delete-directory root t))))

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

(ert-deftest opencode-session-input-after-hidden-fence-test ()
  "Input typed after a hidden closing fence isn't hidden with it."
  (with-opencode-session-test-buffer
    (opencode--output "```bash\nsleep 10\n```\n")
    (font-lock-ensure)
    (should (get-char-property (1- (point-max)) 'invisible))
    (goto-char (point-max))
    (let ((last-command-event ?x))
      (self-insert-command 1))
    (should-not (get-char-property (1- (point-max)) 'invisible))))

(ert-deftest opencode-session-copy-hidden-fence-test ()
  "Copying from a hidden opening fence drops it, unless the whole block is copied."
  (with-opencode-session-test-buffer
    (opencode--output "```python\nx = 1\ny = 2\n```\n\nAfter\n")
    (font-lock-ensure)
    (let ((start (point-min)))
      (goto-char start)
      (search-forward "y = 2\n")
      (should (equal (substring-no-properties
                      (filter-buffer-substring start (point)))
                     "x = 1\ny = 2\n"))
      (search-forward "```\n")
      (should (equal (substring-no-properties
                      (filter-buffer-substring start (point)))
                     "```python\nx = 1\ny = 2\n```\n")))))

(ert-deftest opencode-session-yank-code-block-test ()
  "Copy only code from a fenced block, including embedded fences."
  (with-opencode-session-test-buffer
    (insert (opencode--fenced-block "```\ntext\n" "text") "\n")
    (goto-char (point-min))
    (search-forward "text" nil nil 2)
    (opencode-yank-code-block)
    (should (equal (current-kill 0 t) "```\ntext\n"))))

(defun opencode-session-test--table-displays (&optional properties)
  "Return the display strings of the rendered table rows, in buffer order.
Keep their text properties if PROPERTIES is non-nil."
  (mapcar (lambda (overlay)
            (let ((display (overlay-get overlay 'display)))
              (if properties display (substring-no-properties display))))
          (sort (seq-filter (lambda (overlay) (overlay-get overlay 'opencode-table))
                            (overlays-in (point-min) (point-max)))
                :key #'overlay-start)))

(ert-deftest opencode-session-completed-tables-test ()
  "Render completed streamed tables once without disturbing pending input."
  (with-opencode-session-test-buffer
    (let ((render (symbol-function 'opencode--render-table))
          (calls 0))
      (cl-letf (((symbol-function 'opencode--render-table)
                 (lambda (table)
                   (cl-incf calls)
                   (funcall render table))))
        (insert "pending input")
        (let ((table "| File | Value |\n| :--- | ---: |\n| `opencode-sessions.el` | 7 |\n"))
          (opencode-session-test--delta 'text table)
          (should (= calls 0))
          ;; An incomplete following line cannot yet establish the boundary.
          (opencode-session-test--delta 'text "# Following")
          (should (= calls 0))
          (opencode-session-test--delta 'text " prose.\n")
          (should (= calls 1))
          ;; The text stays the received Markdown, displayed as a box.
          (should (string-suffix-p (concat "\n" table "# Following prose.\n")
                                   (opencode-session-test--output)))
          (should (equal (opencode-session-test--table-displays)
                         '("┌──────────────────────┬───────┐\n│ File                 │ Value │"
                           "├──────────────────────┼───────┤"
                           "│ opencode-sessions.el │     7 │\n└──────────────────────┴───────┘"))))
        (should (equal (buffer-substring-no-properties
                        (process-mark process) (point-max))
                       "pending input"))
        (should (= (point) (point-max)))
        (save-excursion
          (goto-char (point-min))
          (search-forward "opencode-sessions.el")
          (should (button-at (1- (point)))))
        ;; A fenced table is code and must never be rendered.
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

(ert-deftest opencode-session-table-wrap-test ()
  "Tables wider than the window wrap their widest cells, and rewrap on resize."
  (with-opencode-session-test-buffer
    (let ((fill-column 80))
      (opencode-session-test--delta
       'text (concat "| Option | Description |\n| --- | :-: |\n"
                     "| a | The base URL of the server the client connects to |\n\n"))
      ;; It fits, so isn't wrapped.
      (should (equal (nth 2 (opencode-session-test--table-displays))
                     (concat "│ a      │ The base URL of the server the client connects to │\n"
                             "└────────┴───────────────────────────────────────────────────┘")))
      (setq fill-column 30)
      (opencode--rerender-tables)
      (let ((displays (opencode-session-test--table-displays)))
        (should (equal (nth 2 displays)
                       (concat "│ a      │  The base URL of  │\n"
                               "│        │  the server the   │\n"
                               "│        │  client connects  │\n"
                               "│        │        to         │\n"
                               "└────────┴───────────────────┘")))
        ;; Every line of the box is exactly as wide as is available.
        (should (equal (delete-dups
                        (mapcar #'string-width
                                (split-string (string-join displays "\n") "\n")))
                       '(30)))))))

(ert-deftest opencode-session-table-markdown-test ()
  "Table cells display their Markdown with its markup hidden."
  (with-opencode-session-test-buffer
    (opencode-session-test--delta
     'text "| A | B |\n| --- | --- |\n| `code` | **bold** [link](http://x.y) `a \\| b` |\n\n")
    (let ((row (car (split-string (nth 2 (opencode-session-test--table-displays t))
                                  "\n"))))
      (should (equal (substring-no-properties row) "│ code │ bold link a | b │"))
      (should (memq 'markdown-ts-code-span
                    (ensure-list (get-text-property (string-search "code" row) 'face row))))
      (should (memq 'markdown-ts-bold
                    (ensure-list (get-text-property (string-search "bold" row) 'face row)))))))

(ert-deftest opencode-session-table-button-test ()
  "File references in rendered tables can be clicked."
  (with-opencode-session-test-buffer
    (opencode-session-test--delta
     'text "| File | Line |\n| --- | --- |\n| `opencode-sessions.el:3` | 3 |\n\n")
    (let* ((display (nth 2 (opencode-session-test--table-displays t)))
           (index (string-search "opencode-sessions.el" display))
           visited)
      (should (eq (get-text-property index 'keymap display) button-map))
      (should (eq (get-text-property index 'mouse-face display) 'highlight))
      (cl-letf (((symbol-function 'opencode--visit-file-location)
                 (lambda (location) (setq visited location))))
        (save-window-excursion
          (switch-to-buffer (current-buffer))
          (push-button `(mouse-2 (,(selected-window) 1 (0 . 0) 0
                                  (,display . ,(1+ index)))))))
      (should (equal (cdr visited) 3))
      (should (string-suffix-p "/opencode-sessions.el" (car visited))))))

(ert-deftest opencode-session-table-cell-text-test ()
  "Cell widths ignore hidden markup, and long words are broken."
  (with-opencode-session-test-buffer
    (insert "**ab**")
    (put-text-property (point-min) (+ (point-min) 2) 'invisible t)
    (put-text-property (- (point-max) 2) (point-max) 'invisible t)
    (should (equal (opencode--table-cell-text (point-min) (point-max)) "ab")))
  (should (equal (opencode--table-wrap "abcdefghij kl" 4) '("abcd" "efgh" "ij" "kl")))
  (should (equal (opencode--table-wrap "" 4) '("")))
  (should (equal (opencode--table-column-widths '(3 50 20) 40) '(3 18 18))))

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

(ert-deftest opencode-session-output-test ()
  "Output goes before pending input, is read-only, and isn't a prompt."
  (with-opencode-session-test-buffer
    (let ((comint-prompt-read-only t))
      (opencode--output "hello world\nsec")
      (goto-char (point-max))
      (insert "typed")
      (goto-char (process-mark process))
      (opencode--output "ond")
      (should (equal (buffer-string) "hello world\nsecondtyped"))
      ;; Point at the process mark follows the output.
      (should (= (point) (process-mark process) 19))
      (should (eq (get-text-property 1 'field) 'output))
      (should-not (get-text-property 19 'field))
      ;; Unlike comint's, the last output line isn't highlighted as a prompt.
      (should-not (get-text-property 14 'font-lock-face))
      ;; Output can't be edited, even within it, but input can be typed.
      (dolist (pos '(1 4 13 14))
        (goto-char pos)
        (should-error (insert "X") :type 'text-read-only))
      (goto-char (process-mark process))
      (should-error (delete-char -1) :type 'text-read-only)
      (insert "X")
      (should (equal (buffer-string) "hello world\nsecondXtyped")))))

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
      (should (opencode-session-test--folded-p "grep `foo`"))
      (should (opencode-session-test--folded-p "\nls"))
      (should (opencode-session-test--folded-p "\nuname"))
      (should (opencode-session-test--folded-p "  nil)"))
      (should-not (opencode-session-test--folded-p "eval\n"))
      (should (string-suffix-p
               (concat "grep `foo`\n```text\na.el:1\nb.el:2\n```\n\n"
                       "```bash\nls\n```\n```text\nls-file\n```\n\n"
                       "```bash\nuname\n```\n```text\nagent-file\n```\n\n"
                       "eval\n(progn\n  nil)\n```text\nresult\n```\n")
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
                      ("grep `foo`" . "2 lines")
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
      (should-not (opencode-session-test--folded-p "grep `foo`"))
      (should (opencode-session-test--folded-p "glob `*.el`"))
      ;; Shown blocks can still be folded, and unfolded again.
      (dolist (header '("Thinking" "edit opencode-sessions.el:" "grep `foo`"))
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

(ert-deftest opencode-session-replay-undo-test ()
  "Replay isn't recorded for undo, even if output resets the undo list.
Resetting it made `combine-change-calls' warn that it was truncated."
  (with-opencode-session-test-buffer
    (buffer-enable-undo)
    (opencode--output "logo\n\n")
    (let ((undo-list buffer-undo-list)
          (reset (lambda (&rest _) (setq buffer-undo-list nil)))
          messages)
      (should (consp undo-list))
      (advice-add 'opencode--output :after reset)
      (unwind-protect
          (cl-letf (((symbol-function 'message)
                     (lambda (&rest args) (push (apply #'format-message args) messages))))
            (opencode--replay-session-messages
             '(((id . "msg_1") (type . "user") (text . "hello"))
               ((id . "msg_2") (type . "idle") (outcome . "failed")
                (error . ((type . "unknown") (message . "It broke")))))))
        (advice-remove 'opencode--output reset))
      (should (opencode-session-test--error-face-p "It broke"))
      (should-not messages)
      (should (eq buffer-undo-list undo-list)))))

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

(ert-deftest opencode-session-history-table-rendering-test ()
  "Render replayed assistant tables, but not user requests and code fences."
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
      ;; renderer without going through the table rendering path.
      (cl-letf (((symbol-function 'opencode--replay-user-request)
                 (lambda (message)
                   (opencode--output (alist-get 'text message))
                   (opencode--output "\n\n"))))
        (opencode--replay-session-messages messages))
      (let ((output (buffer-substring-no-properties (point-min) (point-max))))
        (should (equal output (concat table "\n\n" table "\n\n" fenced "\n"))))
      ;; Only the assistant's table, starting after the request's.
      (let ((overlays (seq-filter (lambda (overlay) (overlay-get overlay 'opencode-table))
                                  (overlays-in (point-min) (point-max)))))
        (should (= (length overlays) 3))
        (should (> (apply #'min (mapcar #'overlay-start overlays))
                   (length table))))
      (should (equal (car (last (opencode-session-test--table-displays)))
                     "│ x    │     y │\n└──────┴───────┘"))
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

(ert-deftest opencode-session-event-shutdown-test ()
  "A shutdown goes back to a prompt without an error, unless it'll resume.
It resumes after the restart if the session's still active."
  (with-opencode-session-test-buffer
    (opencode-session-test--with-stubbed-api nil
      (opencode--output "request\n\n")
      (setq opencode-session-status "busy")
      (opencode--handle-message
       '((type . "session.execution.interrupted")
         (data . ((sessionID . "session") (reason . "shutdown")))))
      (should (equal opencode-session-status "idle"))
      (should (opencode-session-test--at-prompt-p))
      (should-not (string-search "Interrupted" (buffer-string)))))
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

(ert-deftest opencode-session-event-server-clock-test ()
  "Turn times are on the server's clock, even when ours is off from it."
  (with-opencode-session-test-buffer
    (opencode-session-test--with-stubbed-api nil
      (setq opencode-buffer-connection opencode-default-connection)
      (let ((start (- (opencode--now-ms) 3600000)))
        (opencode--output "request\n\n")
        (opencode--handle-message
         `((type . "session.execution.started") (created . ,start)
           (data . ((sessionID . "session")))))
        (should (< (abs (- (opencode--server-now-ms) start)) 1000))
        (opencode--handle-message
         `((type . "session.execution.succeeded") (created . ,(+ start 5000))
           (data . ((sessionID . "session")))))
        (let ((turn (car opencode--turns)))
          (should (= start (opencode--turn-created turn)))
          (should (= (+ start 5000) (opencode--turn-ended turn))))))))

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

(ert-deftest opencode-session-inherited-agents-test ()
  "A new session gets the server's agents with the models chosen in this one."
  (with-opencode-session-test-buffer
    (setf (opencode-connection-agents opencode-default-connection)
          '(((id . "plan") (name . "Plan"))
            ((id . "build") (name . "Build") (model (id . "server")))
            ((id . "general") (mode . "subagent"))
            ((id . "new") (name . "New"))))
    (setq opencode-buffer-connection opencode-default-connection
          opencode-session-agents '(((id . "build") (name . "Build") (model (id . "stale")))
                                    ((id . "plan") (name . "Plan") (model (id . "p")))
                                    ((id . "gone") (name . "Gone")))
          opencode-session-agent '((id . "build") (name . "Build") (model (id . "b"))))
    (let* ((opencode-last-model '((id . "last")))
           (agents (opencode--inherited-agents)))
      (should (equal '("plan" "build" "general" "new")
                     (mapcar (lambda (a) (alist-get 'id a)) agents)))
      (should (equal '("p" "b" "last" "last")
                     (mapcar (lambda (a) (map-nested-elt a '(model id))) agents)))
      (should (equal "plan" (alist-get 'id (opencode--default-agent agents))))
      ;; models are copies, so changing a variant doesn't change the source's
      (setf (alist-get 'variant (alist-get 'model (car agents))) "high")
      (should-not (map-nested-elt (cadr opencode-session-agents) '(model variant))))
    ;; From a buffer that isn't a session on the server, an agent's own model
    ;; is used
    (setq opencode-buffer-connection nil)
    (let ((opencode-last-model '((id . "last"))))
      (should (equal "server" (map-nested-elt (nth 1 (opencode--inherited-agents))
                                              '(model id)))))))

(ert-deftest opencode-session-status-indicator-test ()
  "The mode line never shows nil for an agent.
The server reports no variant as \"default\", which is no selection."
  (with-opencode-session-test-buffer
    (cl-flet ((indicator (agent)
                (let ((opencode-session-agent agent))
                  (opencode--mode-line-process))))
      (should (string-search "🤖 Plan" (indicator '((id . "plan") (name . "Plan")))))
      (should (string-search "🤖 plan" (indicator '((id . "plan")))))
      (should (string-search "🤖 -" (indicator '((model . ((id . "m")))))))
      (should-not (string-search "nil" (indicator nil)))
      (cl-letf (((symbol-function 'opencode--current-model)
                 (lambda () '((name . "Model")
                              (variants . (((id . "low")) ((id . "medium"))
                                           ((id . "high")) ((id . "max"))))))))
        (let ((opencode-mode-line-process "[%m%V]"))
          (should (string-search
                   "Model]"
                   (indicator '((id . "a") (model . ((id . "m") (variant . "default")))))))
          (should (string-search
                   "Model ●●○]"
                   (indicator '((id . "a") (model . ((id . "m") (variant . "high"))))))))
        (let ((opencode-mode-line-process "%m%v"))
          (should-not (string-search
                       "default"
                       (indicator '((id . "a") (model . ((id . "m") (variant . "default")))))))
          (should (string-search
                   "Model high"
                   (indicator '((id . "a") (model . ((id . "m") (variant . "high"))))))))))))

(ert-deftest opencode-variant-dots-test ()
  "%V shows one dot fewer than the number of variants.
They're filled up to the selected one, and there are none when there's no
selection or only one variant."
  (with-opencode-session-test-buffer
    (cl-flet ((dots (variants variant)
                (cl-letf (((symbol-function 'opencode--current-model)
                           (lambda ()
                             `((name . "Model")
                               (variants . ,(mapcar (lambda (id) `((id . ,id)))
                                                    variants))))))
                  (let ((opencode-session-agent
                         `((id . "a") (model . ((id . "m") (variant . ,variant))))))
                    (substring-no-properties
                     (alist-get ?V (opencode--format-specs)))))))
      (let ((levels '("low" "medium" "high" "max")))
        (should (equal " ○○○" (dots levels "low")))
        (should (equal " ●○○" (dots levels "medium")))
        (should (equal " ●●●" (dots levels "max")))
        (should (equal "" (dots levels nil)))
        (should (equal "" (dots levels "default"))))
      (should (equal "" (dots '("high") "high"))))))

(ert-deftest opencode-mode-line-format-specs-test ()
  "Specs expand with their percent signs escaped for the mode line.
Specs that aren't ours, and a literal %%, are left for the mode line."
  (with-opencode-session-test-buffer
    (setq opencode-session-title "100% done"
          opencode-session-tokens 50000
          opencode-session-status "busy"
          opencode-session-agent '((id . "build") (name . "Build")))
    (cl-letf (((symbol-function 'opencode--current-model)
               (lambda () '((name . "Model") (limit . ((context . 200000)))))))
      ;; the results are mode line constructs, where %% is a percent sign
      (should (equal "100%% done 25%% ⏳ busy 50k Build %%"
                     (opencode--mode-line-expand "%t %c %I %i %T %a %%")))
      (should (equal "[100%%]" (opencode--mode-line-expand "[%>4t]")))
      (should (equal "%b" (opencode--mode-line-expand "%b"))))))

(ert-deftest opencode-mode-line-cost-test ()
  "%o is the session's cost in dollars."
  (with-opencode-session-test-buffer
    (cl-letf (((symbol-function 'opencode--current-model) #'ignore))
      ;; No cost shows nothing
      (should (equal "[]" (opencode--mode-line-expand "[%o]")))
      (setq opencode-session-cost 1.234)
      (should (equal "cost $1.23" (opencode--mode-line-expand "cost %o"))))))

(ert-deftest opencode-mode-line-context-bar-test ()
  "%C is a bar of the context window used.
It's blended with the line's face, and only made when used."
  (with-opencode-session-test-buffer
    (setq opencode-session-tokens 140000)
    (let (bar-args)
      (cl-letf (((symbol-function 'opencode--current-model)
                 (lambda () '((name . "Model") (limit . ((context . 200000))))))
                ((symbol-function 'opencode--usage-bar)
                 (lambda (&rest args) (setq bar-args args) "bar")))
        (opencode--mode-line-expand "%c" (opencode--format-specs 'header-line))
        (should-not bar-args)
        (should (equal "bar" (opencode--mode-line-expand
                              "%C" (opencode--format-specs 'header-line))))
        (should (equal '(0.7 header-line) bar-args))))
    (cl-letf (((symbol-function 'display-graphic-p) #'ignore))
      (let ((bar (opencode--usage-bar 0.7)))
        (should (equal (concat (make-string 14 ?━) (make-string 6 ?─))
                       (substring-no-properties bar)))
        (should (eq 'warning (get-text-property 0 'face bar)))
        (should (eq 'shadow (get-text-property 19 'face bar)))))))

(ert-deftest opencode-mode-line-render-format-test ()
  "Everything after `right-align' is right-aligned.
`overflow' shows `opencode-mode-line-process' only when it overflowed the
mode line."
  (with-opencode-session-test-buffer
    (setq opencode-session-title "Title")
    (let ((opencode-mode-line-process "[%t]")
          (opencode-overflow-format nil))
      (pcase-let ((`("" ,left ,space ,right)
                   (opencode--render-format '("%t" right-align "x") 'header-line)))
        (should (equal "Title" left))
        (should (equal "x" right))
        (should (eq 'space (car (get-text-property 0 'display space)))))
      (should (equal '("") (opencode--render-format '(overflow) 'header-line)))
      (should (equal "[Title]" (opencode--mode-line-process)))
      (cl-letf (((symbol-function 'window-parameter)
                 (lambda (_window parameter) (eq parameter 'opencode-overflow))))
        (should (equal '("" "[Title]")
                       (opencode--render-format '(overflow) 'header-line)))
        (should (equal "" (opencode--mode-line-process)))))))

(ert-deftest opencode-overflow-format-test ()
  "`opencode-overflow-format' replaces `overflow', and can right-align."
  (with-opencode-session-test-buffer
    (setq opencode-session-title "Title")
    (let ((opencode-mode-line-process "[%t]")
          (opencode-overflow-format '("%t" right-align "x")))
      (should (equal '("" "a") (opencode--render-format '("a" overflow) 'header-line)))
      (cl-letf (((symbol-function 'window-parameter)
                 (lambda (_window parameter) (eq parameter 'opencode-overflow))))
        (pcase-let ((`("" ,a ,left ,space ,right)
                     (opencode--render-format '("a" overflow) 'header-line)))
          (should (equal "a" a))
          (should (equal "Title" left))
          (should (equal "x" right))
          (should (eq 'space (car (get-text-property 0 'display space)))))
        ;; the user's list isn't modified by splicing
        (should (equal '("%t" right-align "x") opencode-overflow-format))))))

(ert-deftest opencode-mode-line-format-option-test ()
  "`opencode-mode-line-format' replaces the mode line and its process."
  (with-opencode-session-test-buffer
    (setq-local opencode-mode-line-format '("%i"))
    (opencode--setup-mode-line)
    (should (equal '(:eval (opencode--mode-line)) mode-line-format))
    (should (equal '("" "idle") (opencode--mode-line)))
    (should (equal "" (opencode--mode-line-process)))))

(ert-deftest opencode-mode-line-options-as-functions-test ()
  "The mode line and header line options can be functions returning them."
  (with-opencode-session-test-buffer
    (setq opencode-session-title "Title")
    (let* ((opencode-mode-line-process (lambda () "[%t]"))
           (opencode-overflow-format
            (lambda ()
              (if (opencode-claude-subscription-p) '("claude") '("other"))))
           (opencode-header-line-format (lambda () '("h" overflow))))
      (should (equal "[Title]" (opencode--mode-line-process)))
      (should (equal '("" "h") (opencode--header-line)))
      (cl-letf (((symbol-function 'window-parameter)
                 (lambda (_window parameter) (eq parameter 'opencode-overflow))))
        (setq opencode-session-agent
              '((model (providerID . "openai") (modelID . "gpt-5"))))
        (should (equal '("" "h" "other") (opencode--header-line)))
        (setq opencode-session-agent
              '((model (providerID . "claude-agent") (modelID . "opus"))))
        (should (equal '("" "h" "claude") (opencode--header-line)))
        ;; falls back to `opencode-mode-line-process' when it returns nil
        (let ((opencode-overflow-format #'ignore))
          (should (equal '("" "h" "[Title]") (opencode--header-line))))))
    ;; a mode line function returning nil uses the global mode line
    (let ((format '("%i")))
      (setq-local opencode-mode-line-format (lambda () format))
      (opencode--setup-mode-line)
      (should (equal '(:eval (opencode--mode-line)) mode-line-format))
      (should (equal '("" "idle") (opencode--mode-line)))
      (should (equal "" (opencode--mode-line-process)))
      (setq format nil)
      (should (equal (default-value 'mode-line-format) (opencode--mode-line)))
      (should (equal "[Title]"
                     (let ((opencode-mode-line-process "[%t]"))
                       (opencode--mode-line-process)))))))

(ert-deftest opencode-claude-subscription-p-test ()
  "Sessions and session buffers using claude-agent are subscriptions."
  (should (opencode-claude-subscription-p
           (opencode-session-test--claude-session "opus" 3.21)))
  (should-not (opencode-claude-subscription-p
               (opencode-session-test--claude-session "gpt-5" 0 "openai")))
  (with-temp-buffer
    (setq-local opencode-session-agent '((model (providerID . "claude-agent"))))
    (should (opencode-claude-subscription-p))
    (setq-local opencode-session-agent nil)
    (should-not (opencode-claude-subscription-p))))

(ert-deftest opencode-open-project-buffers-test ()
  "A control buffer is named after the main checkout of its project."
  (let* ((opencode-default-connection (opencode--make-connection :name 'local))
         (opencode-default-server 'local)
         buffers)
    (cl-letf* (((symbol-function 'opencode-sessions-redisplay) #'ignore)
               ((symbol-function 'switch-to-buffer)
                (lambda (buffer &rest _) (push buffer buffers)))
               ((symbol-function 'opencode-api--request)
                (lambda (_method _path &rest args)
                  (funcall
                   (plist-get args :then)
                   (pcase default-directory
                     ((or "/src/repo/" "/src/repo/sub/")
                      '((project . ((id . "repo") (directory . "/src/repo")
                                    (canonical . "/src/repo")))))
                     ("/src/worktree/"
                      '((project . ((id . "repo") (directory . "/src/worktree")
                                    (canonical . "/src/repo")))))
                     ("/other/repo/"
                      '((project . ((id . "other") (directory . "/other/repo")
                                    (canonical . "/other/repo")))))
                     ("/scratch/"
                      '((project . ((id . "global") (directory . "/scratch")
                                    (canonical . "/"))))))))))
      (unwind-protect
          (progn
            (opencode-open-project "/src/repo")
            (should (equal "*OpenCode Sessions: repo*" (buffer-name (car buffers))))
            ;; A worktree shares the buffer, and new sessions go in the worktree.
            (opencode-open-project "/src/worktree/")
            (should (eq (car buffers) (cadr buffers)))
            (should (equal "/src/worktree/"
                           (buffer-local-value 'default-directory (car buffers))))
            ;; From a subdirectory, new sessions go in the project root.
            (opencode-open-project "/src/repo/sub/")
            (should (eq (car buffers) (cadr buffers)))
            (should (equal "/src/repo/"
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
  "The agent and model are only switched if they differ from the server's.
Switching agent always records a message, and each switch is a round trip."
  (with-opencode-session-test-buffer
    (let (requests sent)
      (cl-letf (((symbol-function 'opencode-api--request)
                 (lambda (_method path &rest args)
                   (push path requests)
                   (funcall (plist-get args :then) nil))))
        (setq opencode-session-agent '((id . "build")
                                       (model . ((id . "m") (providerID . "p"))))
              opencode--server-agent "build"
              opencode--server-model nil)
        (opencode--sync-agent-and-model (lambda () (setq sent t)))
        (should sent)
        (should (equal requests '("/api/session/session/model")))
        ;; The server's model is now known, and no variant is its default
        (should (opencode--same-model-p
                 opencode--server-model '((id . "m") (providerID . "p") (variant . "default"))))
        (setq requests nil sent nil)
        (opencode--sync-agent-and-model (lambda () (setq sent t)))
        (should sent)
        (should-not requests)
        (setq opencode--server-agent "plan")
        (opencode--sync-agent-and-model #'ignore)
        (should (equal requests '("/api/session/session/agent")))
        (should (equal opencode--server-agent "build"))
        (setq requests nil)
        (setq opencode-session-agent
              '((id . "build") (model . ((id . "m") (providerID . "p") (variant . "high")))))
        (opencode--sync-agent-and-model #'ignore)
        (should (equal requests '("/api/session/session/model")))))))

(ert-deftest opencode-session-load-selection-test ()
  "Opening a session selects what it has, or what its messages used if migrated."
  (with-opencode-session-test-buffer
    (let ((opencode-session-agents '(((id . "build") (name . "Build"))
                                     ((id . "plan") (name . "Plan"))))
          (opencode-default-connection
           (opencode--make-connection
            :name 'test
            :agents '(((id . "build") (name . "Build") (mode . "primary"))))))
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
      (should (string-search "grep `foo`\n```text\nmatched" (buffer-string)))
      ;; The call replayed while streaming keeps its name once called.
      (opencode--handle-message
       '((type . "session.tool.called")
         (data . ((sessionID . "session") (assistantMessageID . "msg_5") (id . "later")
                  (input . ((pattern . "bar")))))))
      (should (string-search "grep `bar`" (buffer-string)))
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
                               opencode-forms--left opencode-forms--reading
                               opencode-forms--kept-selection
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
          (opencode-default-connection (opencode--make-connection :name 'test))
          (buffer (generate-new-buffer " *form-test*")))
      (unwind-protect
          (progn
            (puthash "ses_1" buffer
                     (opencode-connection-session-buffers opencode-default-connection))
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
  "Quitting the minibuffer leaves the form for later, and its cancel key cancels it."
  (opencode-forms-test--with-server
    (let ((quit t))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _)
                   (should opencode-forms--reading)
                   (if quit
                       (signal 'quit nil)
                     (opencode-forms-cancel-from-minibuffer)))))
        (opencode--form-request
         '((id . "frm_1") (sessionID . "global") (title . "T")
           (fields . (((key . "note") (type . "string"))))))
        (should-not requests)
        (should opencode-forms--current)
        (should-not opencode-forms--reading)
        (setq quit nil)
        (opencode-respond)
        (should (equal '(delete "/api/session/global/form/frm_1")
                       (seq-take (car requests) 2)))
        (should-not opencode-forms--current)))))

(ert-deftest opencode-forms-minibuffer-cancel-key-test ()
  "The cancel key is bound in the minibuffer while answering a form."
  (opencode-forms-test--with-server
    (let (binding)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _)
                   (run-hooks 'minibuffer-setup-hook)
                   (setq binding (keymap-lookup (current-local-map) "C-c C-k"))
                   "")))
        (with-temp-buffer
          (opencode--form-request
           '((id . "frm_1") (sessionID . "global") (title . "T")
             (fields . (((key . "note") (type . "string"))))))))
      (should (eq 'opencode-forms-cancel-from-minibuffer binding)))))

(ert-deftest opencode-forms-later-test ()
  "A form left for later isn't cancelled, holds back later forms, and is shown again."
  (opencode-forms-test--with-server
    (opencode--form-request opencode-forms-test--websearch)
    (should (length= menus 1))
    (opencode--form-request (cons '(id . "frm_2") (cdr opencode-forms-test--websearch)))
    (should (length= menus 1))
    (should (length= opencode-forms--queue 1))
    (should-not requests)
    (opencode-respond)
    (should (length= menus 2))
    (opencode-forms--answer "allow")
    (should (equal "/api/session/global/form/frm_1/reply" (nth 1 (car requests))))
    (should (equal "frm_2" (alist-get 'id opencode-forms--current)))
    (should (length= menus 3))))

(ert-deftest opencode-forms-later-return-test ()
  "A form left for later is shown again on returning to its session from elsewhere."
  (opencode-forms-test--with-server
    (let ((opencode-default-connection (opencode--make-connection :name 'test))
          (buffer (generate-new-buffer " *form-test*"))
          (other (generate-new-buffer " *other*")))
      (unwind-protect
          (progn
            (puthash "ses_1" buffer
                     (opencode-connection-session-buffers opencode-default-connection))
            (opencode-forms--set-current
             `((id . "frm_1") (sessionID . "ses_1")
               (opencode-connection . ,opencode-default-connection)
               ,@(cddr opencode-forms-test--websearch)))
            (opencode-forms--next)
            (should (length= menus 1))
            (opencode-forms--window-selected buffer)
            (with-current-buffer (get-buffer-create " *Minibuf-1*")
              (opencode-forms--window-selected (current-buffer)))
            (should (length= menus 1))
            (opencode-forms--window-selected other)
            (should opencode-forms--left)
            (opencode-forms--window-selected buffer)
            (should (length= menus 2))
            (should-not opencode-forms--left))
        (kill-buffer buffer)
        (kill-buffer other)))))

(ert-deftest opencode-forms-later-settled-test ()
  "A form left for later is forgotten once it's answered elsewhere."
  (opencode-forms-test--with-server
    (opencode--form-request opencode-forms-test--websearch)
    (opencode--form-settled "frm_1")
    (should-not opencode-forms--current)))

(defmacro opencode-forms-test--with-menu (&rest body)
  "Run BODY like `opencode-forms-test--with-server', but really showing menus."
  (declare (indent 0))
  `(let ((show (symbol-function 'opencode-forms--show-menu)))
     (opencode-forms-test--with-server
       (cl-letf (((symbol-function 'opencode-forms--show-menu) show))
         (unwind-protect
             (progn ,@body)
           (opencode-menu-close))))))

(ert-deftest opencode-forms-suspend-test ()
  "A suspended menu can be resumed to answer, or shown again by `opencode-respond'."
  (opencode-forms-test--with-menu
    (opencode--form-request opencode-forms-test--websearch)
    (should (opencode-menu-active-p "frm_1"))
    (execute-kbd-macro (kbd "C-z"))
    (should-not (opencode-menu-active-p))
    (should-not requests)
    (opencode-respond)
    (execute-kbd-macro (kbd "C-z"))
    (transient-resume)
    (should (opencode-menu-active-p "frm_1"))
    (execute-kbd-macro (kbd "1"))
    (should (equal '((post "/api/session/global/form/frm_1/reply" "/project/"
                      "{\"answer\":{\"choice\":\"allow\"}}"))
                   requests))
    (should-not opencode-forms--current)))

(ert-deftest opencode-forms-suspended-settled-test ()
  "Resuming the menu of a form answered elsewhere does nothing."
  (opencode-forms-test--with-menu
    (opencode--form-request opencode-forms-test--websearch)
    (execute-kbd-macro (kbd "C-z"))
    (opencode--form-settled "frm_1")
    (transient-resume)
    (execute-kbd-macro (kbd "1"))
    (should-not requests)))

(ert-deftest opencode-forms-later-keeps-selection-test ()
  "Leaving a multiselect for later with q keeps what's selected."
  (opencode-forms-test--with-menu
    (opencode--form-request
     '((id . "frm_1") (sessionID . "global") (title . "T")
       (fields . (((key . "tags") (type . "multiselect")
                   (options . (((value . "a") (label . "A"))
                               ((value . "b") (label . "B")))))))))
    (execute-kbd-macro (kbd "2 q"))
    (should-not (opencode-menu-active-p))
    (opencode-respond)
    (execute-kbd-macro (kbd "C-c C-c"))
    (should (equal "{\"answer\":{\"tags\":[\"b\"]}}" (nth 3 (car requests))))))

(ert-deftest opencode-permission-response-by-id-test ()
  "A permission menu answers the request it was shown for, if it's still pending."
  (with-temp-buffer
    (let (paths)
      (cl-letf (((symbol-function 'opencode-api--request)
                 (lambda (_method path &rest _) (push path paths)))
                ((symbol-function 'run-at-time) #'ignore))
        (setq-local opencode-session-pending-permission
                    '((:id "per_1" :session-id "ses_1") (:id "per_2" :session-id "ses_1")))
        (opencode--send-permission-response "once" "per_2")
        (should (equal '("/api/session/ses_1/permission/per_2/reply") paths))
        (should (equal '("per_1") (mapcar (lambda (perm) (plist-get perm :id))
                                          opencode-session-pending-permission)))
        (opencode--send-permission-response "once" "per_2")
        (should (length= paths 1))))))

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

(ert-deftest opencode-forms-menu-wraps-test ()
  "The menu wraps long questions even when lines are truncated by default."
  (let ((opencode-forms--current '((id . "frm_1") (sessionID . "global")
                                   (metadata . ((kind . "question")))))
        (opencode-forms--fields '(((key . "k") (type . "string") (description . "Why?")
                                   (options . (((value . "a") (label . "A")
                                                (description . "Because"))))))))
    (cl-letf (((default-value 'truncate-lines) t))
      (unwind-protect
          (progn
            (opencode-forms--show-menu nil)
            (with-current-buffer " *transient*"
              (should-not truncate-lines)
              (should word-wrap)
              (goto-char (point-min))
              (search-forward "Because")
              (should (equal "   " (get-text-property (match-beginning 0)
                                                      'wrap-prefix)))))
        (opencode-menu-close)))))

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
        (opencode-open-same-window t)
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

(ert-deftest opencode-in-command-context-test ()
  "Async callbacks of a command see its `this-command' and setup hooks.
That's what `vertico-repeat' needs to repeat and restore their reads."
  (let* (ran
         (callback (let ((this-command 'some-command))
                     (minibuffer-with-setup-hook (lambda () (setq ran this-command))
                       (opencode--in-command-context
                        (lambda (x)
                          (run-hooks 'minibuffer-setup-hook)
                          (list x this-command)))))))
    (let ((this-command nil))
      (should (equal '(1 some-command) (funcall callback 1)))
      (should (eq 'some-command ran))
      (should-not this-command)))
  ;; Outside a command the function is used as is
  (let ((this-command nil))
    (should (eq #'car (opencode--in-command-context #'car)))))

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
  (dolist (case '(("pro" . "~$2.00 10.0%")
                  ("max_5x" . "~$2.00 1.6%")
                  ("max_20x" . "~$2.00 0.7%")))
    (opencode-session-test--with-claude-plan (car case)
      (should (equal (cdr case)
                     (opencode--format-session-cost
                      (opencode-session-test--claude-session
                       "claude-opus-5-5")))))))

(ert-deftest opencode-session-control-cost-other-plans-omit-share ()
  "Plans without a budget, or an unknown plan, show dollars only."
  (dolist (tier '("other" "max_other" nil unfetched))
    (opencode-session-test--with-claude-plan tier
      (should (equal "~$2.00"
                     (opencode--format-session-cost
                      (opencode-session-test--claude-session
                       "claude-opus-5-5")))))))

(ert-deftest opencode-session-control-cost-resolves-aliases ()
  "An alias uses the newest model of its family, and [1m] is ignored."
  (opencode-session-test--with-claude-plan "pro"
    (dolist (model '("opus" "opus[1m]" "claude-opus-5-5[1m]"))
      (should (equal "~$2.00 10.0%"
                     (opencode--format-session-cost
                      (opencode-session-test--claude-session model)))))))

(ert-deftest opencode-session-control-cost-without-price ()
  "A model with no known price shows no cost."
  (opencode-session-test--with-claude-plan "pro"
    (should (equal "-"
                   (opencode--format-session-cost
                    (opencode-session-test--claude-session "claude-fable-5-2"))))
    (let ((opencode--claude-catalog nil))
      (should (equal "-"
                     (opencode--format-session-cost
                      (opencode-session-test--claude-session "opus")))))))

(ert-deftest opencode-session-control-cost-real-dollars ()
  "Sessions the server charged for show its cost before any estimate.
Other providers show only the server's cost."
  (opencode-session-test--with-claude-plan "pro"
    (should (equal "$3.21 + ~$2.00 10.0%"
                   (opencode--format-session-cost
                    (opencode-session-test--claude-session "opus" 3.21))))
    (should (equal "$3.21"
                   (opencode--format-session-cost
                    (opencode-session-test--claude-session
                     "claude-fable-5-2" 3.21))))
    (should (equal "$3.21"
                   (opencode--format-session-cost
                    (opencode-session-test--claude-session "gpt-5" 3.21 "openai"))))
    (should (equal "$0.00"
                   (opencode--format-session-cost
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
  "Fresh usage shows exact percentages.
Windows past their reset show as 0% without a reset time."
  (let* ((now (float-time))
         (usage (opencode--format-claude-usage
                 (opencode-session-test--rate-limit now))))
    (should (string-match-p
             (rx bos "5h " (= 2 "━") (= 18 "─")
                 " 9% resets " (+ (not "·")) " · "
                 "week " (= 20 "─") " 0% · "
                 "seven day sonnet " (= 17 "━") (= 3 "─")
                 " 85% resets " (+ nonl) eos)
             usage))
    (should (eq 'success (get-text-property (string-search "━" usage)
                                            'face usage)))
    (should (eq 'error (get-text-property
                        (string-search "━" usage (string-search "sonnet" usage))
                        'face usage)))
    (should-not (get-text-property (string-search "85%" usage) 'face usage))))

(ert-deftest opencode-claude-usage-levels ()
  "Usage can show only the 5h window, with or without its reset time."
  (let* ((now (float-time))
         (rate-limit (opencode-session-test--rate-limit now)))
    (should (equal (opencode--format-claude-usage rate-limit)
                   (opencode--format-claude-usage rate-limit 'all)))
    (should (string-match-p
             (rx bos "5h " (+ (any "━─")) " 9% resets " (+ nonl) eos)
             (opencode--format-claude-usage rate-limit 'five-hour)))
    (should (string-match-p
             (rx bos "5h " (+ (any "━─")) " 9%" eos)
             (opencode--format-claude-usage rate-limit 'five-hour-no-reset)))
    (should-not (opencode--format-claude-usage
                 '((unifiedWindows (seven_day (utilization . 0.1))))
                 'five-hour))))

(ert-deftest opencode-claude-window-reset-day ()
  "A reset time has its day unless it's today, and none once it's passed."
  (let* ((now (float-time))
         (tomorrow (+ now 86400))
         (midnight (float-time (encode-time
                                (append '(0 0 0) (seq-subseq (decode-time now) 3)))))
         (end-of-today (+ midnight 86400 -1)))
    (should (equal (format-time-string "%a %H:%M" tomorrow)
                   (opencode--claude-window-reset `((resetsAt . ,tomorrow)))))
    (when (> end-of-today now)
      (should (equal (format-time-string "%H:%M" end-of-today)
                     (opencode--claude-window-reset `((resetsAt . ,end-of-today))))))
    (should-not (opencode--claude-window-reset `((resetsAt . ,(- now 60)))))
    (should-not (opencode--claude-window-reset nil))))

(ert-deftest opencode-claude-usage-bar-face ()
  "Usage bars are green below 60%, warning below 85% and error above."
  (should (eq 'success (opencode--usage-bar-face 0.59)))
  (should (eq 'warning (opencode--usage-bar-face 0.6)))
  (should (eq 'warning (opencode--usage-bar-face 0.84)))
  (should (eq 'error (opencode--usage-bar-face 0.85))))

(ert-deftest opencode-claude-usage-mode-line-level ()
  "The most detailed level of usage that fits is chosen.
The latest report is used, not an older one reported after it."
  (let ((opencode-default-connection
         (opencode--make-connection :name 'test :claude-rate-limit nil)))
    (with-temp-buffer
      (should-not (opencode--session-control-usage-level 100000))
      (opencode--update-claude-rate-limit
       (opencode-session-test--rate-limit (float-time)))
      (opencode--update-claude-rate-limit '((unifiedWindows)) 0)
      (let ((widths (opencode--session-control-usage-widths)))
        (should (equal opencode--claude-usage-levels (mapcar #'car widths)))
        (should (apply #'> (mapcar #'cdr widths)))
        (pcase-dolist (`(,level . ,width) widths)
          (should (eq level (opencode--session-control-usage-level width)))
          (should (eq (cadr (memq level opencode--claude-usage-levels))
                      (opencode--session-control-usage-level (1- width)))))))))

(ert-deftest opencode-mode-line-content-width-test ()
  "Mode line width leaves out aligning spaces, and tells if there were any."
  (should (equal (cons (string-pixel-width "abcd") nil)
                 (opencode--mode-line-content-width "abcd")))
  (should (equal (cons (string-pixel-width "abcd") t)
                 (opencode--mode-line-content-width
                  (concat "ab" (propertize " " 'display '(space :align-to 50))
                          "cd")))))

(ert-deftest opencode-claude-usage-mode-line-item ()
  "Usage is first in a right-aligned part, or else right-aligned last."
  (let ((opencode-default-connection
         (opencode--make-connection :name 'test :claude-rate-limit nil))
        (window (selected-window)))
    (unwind-protect
        (with-temp-buffer
          (opencode--update-claude-rate-limit
           (opencode-session-test--rate-limit (float-time)))
          (set-window-parameter window 'opencode-usage-level nil)
          (should-not (opencode--session-control-usage-item nil))
          (should-not (opencode--session-control-usage-item t))
          (set-window-parameter window 'opencode-usage-level 'five-hour-no-reset)
          (set-window-parameter window 'opencode-usage-aligned t)
          (should (string-match-p (rx bos "5h " (+ (any "━─")) " 9%% " eos)
                                  (opencode--session-control-usage-item nil)))
          (should-not (opencode--session-control-usage-item t))
          (let ((opencode--session-control-usage-measuring t))
            (should-not (opencode--session-control-usage-item nil)))
          (set-window-parameter window 'opencode-usage-aligned nil)
          (should-not (opencode--session-control-usage-item nil))
          (pcase-let ((`(,space ,usage) (opencode--session-control-usage-item t)))
            (should (eq 'space (car (get-text-property 0 'display space))))
            (should (string-match-p (rx bos "5h " (+ (any "━─")) " 9%%" eos)
                                    usage))))
      (set-window-parameter window 'opencode-usage-level nil)
      (set-window-parameter window 'opencode-usage-aligned nil))))

(ert-deftest opencode-mode-line-claude-usage-test ()
  "%u, %U and %r show the Claude 5h usage, only in claude-agent sessions."
  (with-opencode-session-test-buffer
    (setq opencode-session-agent
          '((name . "build") (model (id . "opus") (providerID . "claude-agent"))))
    (let ((now (float-time))
          bar-args)
      (cl-letf (((symbol-function 'opencode--current-model) #'ignore)
                ((symbol-function 'opencode--usage-bar)
                 (lambda (&rest args) (setq bar-args args) "bar")))
        (should (eq 'unfetched (opencode-connection-claude-rate-limit (opencode--conn))))
        (should (equal "[]" (opencode--mode-line-expand "[%u]")))
        (should (equal "[]" (opencode--mode-line-expand "[%U]")))
        (should (equal "[]" (opencode--mode-line-expand "[%r]")))
        (should-not bar-args)
        (setf (opencode-connection-claude-rate-limit (opencode--conn))
              (cons now (opencode-session-test--rate-limit now)))
        ;; Escaped for the mode line
        (should (equal "5h 9%%" (opencode--mode-line-expand "5h %u")))
        (should (equal "bar" (opencode--mode-line-expand
                              "%U" (opencode--format-specs 'header-line))))
        (should (equal '(0.09 header-line) bar-args))
        (should (equal (format " resets %s"
                               (opencode--claude-window-reset `((resetsAt . ,(+ now 3600)))))
                       (opencode--mode-line-expand "%r")))
        ;; Past its reset nothing has been used, and there's no reset time
        (setf (opencode-connection-claude-rate-limit (opencode--conn))
              (cons now (opencode-session-test--rate-limit (- now 7200))))
        (should (equal "0%%" (opencode--mode-line-expand "%u")))
        (should (equal "[]" (opencode--mode-line-expand "[%r]")))
        (setf (opencode-connection-claude-rate-limit (opencode--conn))
              (cons now (opencode-session-test--rate-limit now)))
        ;; Another server's usage isn't shown
        (let ((opencode-buffer-connection (opencode--make-connection :name 'other)))
          (should (equal "[]" (opencode--mode-line-expand "[%u]"))))
        (setq opencode-session-agent
              '((name . "build") (model (id . "gpt") (providerID . "openai"))))
        (should (equal "[]" (opencode--mode-line-expand "[%u%U%r]")))))))

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
    (opencode-session-control-mode)
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
                     (car (last paths))))
      (setq paths nil)
      (opencode-api--fetch-all-sessions "project" nil #'ignore t)
      (should (equal "/api/session?project=project&limit=200&parentID=null"
                     (car (last paths)))))))

(ert-deftest opencode-session-control-archived-title-test ()
  "Archived sessions have an archive icon before their title."
  (let ((getter (cadr (alist-get 'title opencode--session-control-columns))))
    (cl-letf (((symbol-function 'opencode--session-status-icon) #'ignore))
      (should (equal "🗄 Old"
                     (funcall getter '((title . "Old")
                                       (time (archived . 1000))))))
      (should (equal "├─ 🗄 Old"
                     (funcall getter '((tree-prefix . "├─ ")
                                       (title . "Old")
                                       (time (archived . 1000))))))
      (should (equal "New" (funcall getter '((title . "New") (time)))))
      (should (equal "New" (funcall getter '((title . "New")
                                             (time (archived . :null)))))))))

(ert-deftest opencode-archive-session-test ()
  "Archiving and unarchiving patch the session's archived field.
They also fetch the session's children, to do the same to them."
  (let (requests)
    (cl-letf (((symbol-function 'opencode-api--request)
               (cl-function
                (lambda (method path &key body &allow-other-keys)
                  (push (list method path body) requests)))))
      (opencode-archive-session '((id . "s1")))
      (opencode-unarchive-session '((id . "s1")))
      (should (equal '((get "/api/session?parentID=s1" nil)
                       (patch "/api/session/s1" ((archived . :json-false)))
                       (get "/api/session?parentID=s1" nil)
                       (patch "/api/session/s1" ((archived . t))))
                     requests)))))

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

(ert-deftest opencode-turn-file-candidates-test ()
  "Turn files are annotated with aligned line counts, and unusual statuses."
  (let ((candidates
         (opencode--turn-file-candidates
          '(((file . "a.el") (additions . 12) (deletions . 3) (status . "modified"))
            ((file . "new.el") (additions . 4) (deletions . 0) (status . "added"))
            ((file . "old.el") (additions . 0) (deletions . 100) (status . "deleted"))))))
    (should (equal '("a.el" "new.el" "old.el") (mapcar #'car candidates)))
    (should (equal "new.el" (alist-get 'file (nth 1 (nth 1 candidates)))))
    (should (equal '("+12  -3" "+4   -0    added" "+0   -100  deleted")
                   (mapcar #'caddr candidates)))
    (let ((annotation (caddr (car candidates))))
      (should (eq 'diff-indicator-added (get-text-property 0 'face annotation)))
      (should (eq 'diff-indicator-removed (get-text-property 5 'face annotation))))
    (should (eq 'shadow (get-text-property 11 'face (caddr (nth 1 candidates)))))))

(ert-deftest opencode-session-preview-open-buffer-test ()
  "Previewing an open session shows its buffer, and ending restores the window."
  (skip-unless (require 'consult nil t))
  (with-opencode-session-test-buffer
    (let* ((session-buffer (current-buffer))
           (window (selected-window))
           (original (window-buffer window))
           (state (opencode--session-preview-state)))
      (funcall state 'setup nil)
      (funcall state 'preview '((id . "session") (title . "Test")))
      (should (eq session-buffer (window-buffer window)))
      (funcall state 'preview nil)
      (funcall state 'exit nil)
      (funcall state 'return nil)
      (should (eq original (window-buffer window))))))

(ert-deftest opencode-message-text-test ()
  "Only the non-blank text parts of a message are kept."
  (should (equal "one\n\ntwo"
                 (opencode--message-text
                  '((content ((type . "reasoning") (text . "hmm"))
                     ((type . "text") (text . "one\n"))
                     ((type . "tool") (name . "read"))
                     ((type . "text") (text . "  "))
                     ((type . "text") (text . "two")))))))
  (should-not (opencode--message-text '((content ((type . "tool")))))))

(provide 'opencode-sessions-test)
;;; opencode-sessions-test.el ends here
