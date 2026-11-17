;;; opencode-session-output.el --- Rendering of opencode session buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Display streamed and replayed session messages, tool calls and
;; their output in session buffers.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'comint)
(require 'hideshow)
(require 'markdown-ts-mode)
(require 'opencode-common)
(require 'opencode-format-tool-calls)

(defcustom opencode-shown-outputs '(shell-non-zero error edit patch question)
  "Outputs in session buffers to show unfolded when first displayed.
A list of symbols: `reasoning', a tool name like `shell' or `grep',
`shell-non-zero' to show shell tool output only if the command failed, or
`error' to show the error of any tool call that failed.
Shown outputs can still be folded manually with `hs-toggle-hiding'."
  :type '(repeat (choice (const reasoning)
                         (const shell-non-zero)
                         (const error)
                         (symbol :tag "Tool name")))
  :group 'opencode)

(defface opencode-request-margin-highlight
  '((t :inherit outline-1 :height reset))
  "OpenCode margin face to apply to user requests."
  :group 'opencode-faces)

(defface opencode-reasoning-margin-highlight
  '((t :inherit outline-2 :height reset))
  "OpenCode margin face to apply to reasoning blocks."
  :group 'opencode-faces)

(defface opencode-tool-margin-highlight
  '((t :inherit outline-5 :height reset))
  "OpenCode margin face to apply to tool call blocks."
  :group 'opencode-faces)

(defvar-local opencode-session-tokens 0
  "Tokens consumed by the current session.")

(defvar-local opencode-session-cost 0
  "Cost of the current session in dollars, as reported by the server.")

(defvar-local opencode--table-scan-marker nil
  "Marker at the unprocessed Markdown in the current assistant text part.")

(defvar-local opencode--reasoning-start nil
  "Start of the streaming reasoning block, to fold once the next block starts.")

(defvar opencode--tool-calls
  (make-hash-table :test 'equal)
  "A mapping of `opencode--tool-key's to plists of in-progress tool calls.
They have :name and :input, as finishing events don't carry them, and
:marker at the end of the call if it's displayed, where its output goes.
Global so that file edit hooks also run for sessions with no buffer.")

(defvar-local opencode--current-block nil
  "(TYPE . KEY) of the block last output in this step, or nil.
TYPE is text, reasoning, tool or compaction.  KEY identifies the block.")

(defvar-local opencode--blocks-with-output nil
  "KEYs of the text and reasoning blocks of this step that have output.")

(defvar-local opencode--replayed-unfinished-messages nil
  "Ids of the messages still being generated when they were replayed.
Their blocks had output in the replay, without this client seeing any delta.")

(defvar-local opencode--prompt nil
  "(START . END) markers of the last prompt `opencode--show-prompt' showed.")

(defvar-local opencode--pending-input-overlay nil
  "Overlay separating input typed while busy from the output before it.")

(defvar-local opencode-last-response-start nil
  "Position of the start of the latest assistant text block.")

(defvar opencode-code-block-languages-without-background '(diff)
  "Fenced code block languages whose background overlay face is removed.
Use for blocks whose own faces draw backgrounds, like diff line faces.")

(defun opencode--fontify-code-block (mode beg end)
  "Collect MODE faces for BEG..END without sharing session text properties."
  (let ((text (buffer-substring-no-properties beg end))
        faces)
    (with-temp-buffer
      (insert text)
      (delay-mode-hooks (funcall mode))
      (font-lock-ensure)
      (let ((pos (point-min)))
        (while (< pos (point-max))
          (let ((next (next-single-property-change pos 'face nil (point-max)))
                (face (get-text-property pos 'face)))
            (when face
              (push (list (+ beg (1- pos)) (+ beg (1- next)) face) faces))
            (setq pos next)))))
    (nreverse faces)))

(defun opencode--fontify-markdown (start end &optional loudly)
  "Fontify Markdown between START and END, optionally LOUDLY.
Isolate conventional code-block fontification from session properties."
  (let ((collect (symbol-function 'markdown-ts--fontify-non-ts-collect-faces)))
    (cl-letf (((symbol-function 'markdown-ts--fontify-non-ts-collect-faces)
               (lambda (mode beg finish)
                 (if (memq mode markdown-ts-code-block-force-conventional-modes)
                     (funcall collect mode beg finish)
                   (opencode--fontify-code-block mode beg finish)))))
      (treesit-font-lock-fontify-region start end loudly)))
  ;; Show all markup of the input being written.
  (when-let* ((process (get-buffer-process (current-buffer)))
              (input-start (max start (process-mark process)))
              ((< input-start end)))
    (with-silent-modifications
      (remove-list-of-text-properties input-start end '(invisible))))
  ;; An overlay's background would hide the backgrounds a block draws itself,
  ;; like diff line faces.  Fontifying just a fence line resets its block's
  ;; overlay, so look past it.
  (dolist (overlay (overlays-in (save-excursion (goto-char start) (pos-bol 0))
                                (save-excursion (goto-char end) (pos-bol 3))))
    (when (memq (overlay-get overlay 'markdown-ts-code-block-language)
                opencode-code-block-languages-without-background)
      (overlay-put overlay 'face nil))))

(defun opencode--fold-beginning ()
  "Move to the start of the foldable block containing point, if any.
Folds don't nest, so this is the nearest block start before point."
  (when-let* ((start (previous-single-property-change (1+ (point)) 'opencode-fold))
              (start (if (get-text-property start 'opencode-fold)
                         start
                       (previous-single-property-change start 'opencode-fold)))
              ((>= (get-text-property start 'opencode-fold) (point))))
    (goto-char start)))

(defun opencode--fold-line-count (beg end)
  "Return how many visible lines follow the first line of a fold.
The fold spans BEG, at the end of its first line, to END.  END already
excludes a final closing fence, so skip only the fences at the start: a
tool input's closing fence, if any, and the output's opening fence."
  (save-excursion
    (goto-char beg)
    (let ((start (if (looking-at "\n\\(?:[`~]\\{3,\\}[[:blank:]]*\n\\)?[`~]\\{3,\\}[^`\n]*\n?")
                     (match-end 0)
                   (1+ beg))))
      (count-lines (min start end) end))))

(defun opencode--fold-label (overlay)
  "Return the label for the fold OVERLAY, from its `display' ellipsis."
  (let ((ellipsis (overlay-get overlay 'display))
        (lines (opencode--fold-line-count (overlay-start overlay)
                                          (overlay-end overlay))))
    (concat (apply #'propertize
                   (format "%d %s" lines (if (= lines 1) "line" "lines"))
                   (text-properties-at 0 ellipsis))
            ellipsis)))

(defun opencode--fold (beg &optional end shown)
  "Make the output from BEG to END foldable, except its first line.
END defaults to the process marker.  Fold it now unless SHOWN is non-nil."
  (with-silent-modifications
    (put-text-property beg (1+ beg) 'opencode-fold
                       (copy-marker (opencode--output-content-end end))))
  (unless shown
    (save-excursion
      (goto-char beg)
      (hs-hide-block-at-point))))

(defun opencode--highlight-input (&optional _proc _string)
  "Highlight last prompt input."
  ;; Hide the sent input's markup, as in the rest of the output.
  (font-lock-flush comint-last-input-start comint-last-input-end)
  (opencode--add-margin comint-last-input-start
                        comint-last-input-end
                        'opencode-request-margin-highlight))

(defun opencode--block-spacing (pos)
  "Return the newlines needed before POS for a blank line before a block."
  (cond ((not (eq (char-before pos) ?\n)) "\n\n")
        ((not (eq (char-before (1- pos)) ?\n)) "\n")
        (t "")))

(defun opencode--maybe-insert-block-spacing ()
  "Ensure \n\n before block."
  (opencode--output (opencode--block-spacing (opencode--session-process-position))))

(defun opencode--output (string)
  "Output STRING as comint output."
  (let ((start (save-excursion
                 (goto-char (opencode--session-process-position))
                 (let ((inhibit-field-text-motion t))
                   (line-beginning-position)))))
    (comint-output-filter (get-buffer-process (current-buffer)) string)
    (opencode--buttonize-file-references
     start (opencode--session-process-position))
    (opencode--update-pending-input)))

(defun opencode--align-output-table (beg end)
  "Align the completed Markdown table between BEG and END.
Markdown's table command applies only its text changes, so session
properties and markers survive on unchanged text."
  (let ((final-newline (eq (char-before end) ?\n))
        (end (copy-marker end t))
        (inhibit-read-only t))
    (unwind-protect
        (save-excursion
          (goto-char beg)
          ;; The command only aligns tables with an in-table overlay.
          (markdown-ts-in-table-mode 1)
          (unwind-protect
              (markdown-ts-table-align-table t)
            (markdown-ts-in-table-mode -1))
          ;; The command always adds a newline.  Do not introduce one if the
          ;; final text part ended at the last cell instead.
          (when (and (not final-newline) (eq (char-before end) ?\n))
            (delete-region (1- end) end))
          ;; Diff insertion preserves existing cells, but newly inserted
          ;; padding also needs comint's output properties.
          (opencode--mark-as-output beg end))
      (set-marker end nil))))

(defun opencode--mark-as-output (beg end)
  "Mark text from BEG to END, inserted outside `comint-output-filter', as output."
  (comint--mark-as-output beg end)
  (when comint-prompt-read-only
    (with-silent-modifications
      (put-text-property beg end 'read-only t))))

(defvar opencode--table-query nil
  "Compiled query for Markdown tables, compiled on first use.")

(defun opencode--next-table (beg end)
  "Return the first Markdown table ending after BEG and starting before END."
  ;; Querying the tree reparses it, which is costly in large sessions, and
  ;; there can't be a table without a pipe.
  (when (save-excursion
          (goto-char beg)
          (search-forward "|" end t))
    (seq-find (lambda (node) (> (treesit-node-end node) beg))
              ;; An uncompiled query is recompiled on every call.
              (treesit-query-capture
               'markdown
               (or opencode--table-query
                   (setq opencode--table-query
                         (treesit-query-compile
                          'markdown '((pipe_table) @table) t)))
               beg end t))))

(defun opencode--align-finished-tables (&optional finished)
  "Align tables whose end is confirmed by subsequent complete output lines.
If FINISHED is non-nil, also align a table at the end of the text part.
Retain one line of context for headers whose delimiter has yet to arrive."
  (when opencode--table-scan-marker
    (save-excursion
      (save-restriction
        ;; Pending comint input is not part of the assistant's Markdown.
        (narrow-to-region (point-min) (opencode--session-process-position))
        (let* ((inhibit-field-text-motion t)
               (limit (copy-marker (point-max)))
               (complete-end
                (copy-marker
                 (if finished
                     limit
                   (goto-char limit)
                   (line-beginning-position))))
               table waiting)
          (unwind-protect
              (progn
                (while (and (not waiting)
                            (setq table (opencode--next-table
                                         opencode--table-scan-marker
                                         complete-end)))
                  (let ((beg (treesit-node-start table))
                        (end (treesit-node-end table)))
                    (if (or finished
                            (< (save-excursion
                                 (goto-char end)
                                 (if (bolp) (point)
                                   (line-beginning-position 2)))
                               complete-end))
                        (let ((next (copy-marker end t)))
                          (unwind-protect
                              (progn
                                (opencode--align-output-table beg end)
                                (set-marker opencode--table-scan-marker next))
                            (set-marker next nil)))
                      (set-marker opencode--table-scan-marker beg)
                      (setq waiting t))))
                (if finished
                    (progn
                      (set-marker opencode--table-scan-marker nil)
                      (setq opencode--table-scan-marker nil))
                  (unless waiting
                    (goto-char complete-end)
                    (forward-line -1)
                    (when (> (point) opencode--table-scan-marker)
                      (set-marker opencode--table-scan-marker (point))))))
            (set-marker limit nil)
            (set-marker complete-end nil)))))))

(defun opencode--logo-render (line face)
  "Render logo LINE using FACE, expanding the TUI's shadow markers.
`_' is a shadow-background space, `^' is `▀' on a shadow background,
`~' is a shadow `▀' and `,' is a shadow `▄'.  The shadow color is
FACE's foreground blended 25% into the default background.  It's
unspecified when the colors aren't known, like in batch mode."
  (let* ((fg (or (face-foreground face nil t)
                 (face-foreground 'default nil t)))
         (bg (face-background 'default nil t))
         (fg-rgb (and fg (color-name-to-rgb fg)))
         (bg-rgb (and bg (color-name-to-rgb bg)))
         (shadow (and fg-rgb bg-rgb
                      (apply #'color-rgb-to-hex
                             (append (color-blend fg-rgb bg-rgb 0.25) '(2)))))
         (weight (and (eq face 'bold) '(:weight bold))))
    (mapconcat
     (lambda (ch)
       (pcase ch
         (?_ (propertize " " 'font-lock-face `(:background ,shadow)))
         (?^ (propertize "▀" 'font-lock-face `(:foreground ,fg :background ,shadow ,@weight)))
         (?~ (propertize "▀" 'font-lock-face `(:foreground ,shadow)))
         (?, (propertize "▄" 'font-lock-face `(:foreground ,shadow)))
         (_ (propertize (string ch) 'font-lock-face face))))
     line "")))

(defun opencode-insert-logo ()
  "Insert the opencode logo."
  (let ((logo-left '("                   " "█▀▀█ █▀▀█ █▀▀█ █▀▀▄"
                     "█__█ █__█ █^^^ █__█" "▀▀▀▀ █▀▀▀ ▀▀▀▀ ▀~~▀"))
        (logo-right '("             ▄     " "█▀▀▀ █▀▀█ █▀▀█ █▀▀█"
                      "█___ █__█ █__█ █^^^" "▀▀▀▀ ▀▀▀▀ ▀▀▀▀ ▀▀▀▀")))
    (cl-loop for left in logo-left
             for right in logo-right
             do (opencode--output (opencode--logo-render left 'shadow))
             (opencode--output " ")
             (opencode--output (opencode--logo-render right 'bold))
             (opencode--output "\n"))
    (opencode--output "\n")))

(defun opencode--show-prompt ()
  "Show an invisible prompt with a margin after displaying output.
The prompt isn't Markdown markup.  It gives the margin's `line-prefix'
a character on an empty prompt line, and a field boundary before the
input, which commands acting on the input line, like evil's `dd', expect."
  (let ((start (opencode--session-process-position)))
    (opencode--output "❯ ")
    (overlay-put (make-overlay start (opencode--session-process-position))
                 'invisible t)
    (setq opencode--prompt (cons (copy-marker start)
                                 (copy-marker (opencode--session-process-position)))))
  (opencode--add-margin (car comint-last-prompt)
                        (cdr comint-last-prompt)
                        'opencode-request-margin-highlight))

(defun opencode--prompt-showing-p ()
  "Return non-nil if the last prompt is showing, with no input sent since."
  (and opencode--prompt
       (marker-position (cdr opencode--prompt))
       (= (cdr opencode--prompt) (opencode--session-process-position))))

(defun opencode--retract-prompt ()
  "Delete the showing prompt, so output goes where it was.
Pending input after it stays, and a later prompt goes before it."
  (when (opencode--prompt-showing-p)
    (pcase-let ((`(,start . ,end) opencode--prompt)
                (inhibit-read-only t))
      (dolist (ov (overlays-in start end))
        (when (and (>= (overlay-start ov) start)
                   (<= (overlay-end ov) end))
          (delete-overlay ov)))
      (delete-region start end)
      (set-marker (process-mark (get-buffer-process (current-buffer))) start)
      (set-marker start nil)
      (set-marker end nil)
      (setq opencode--prompt nil
            comint-last-prompt nil)
      (opencode--update-pending-input))))

(defun opencode--update-pending-input (&rest _)
  "Separate input typed while busy from the output before it.
Display a blank line and the request margin before the pending input."
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (and proc (marker-position (process-mark proc)))))
    (if (and pmark
             (equal opencode-session-status "busy")
             (< pmark (point-max)))
        (progn
          (if opencode--pending-input-overlay
              (move-overlay opencode--pending-input-overlay pmark (point-max))
            (setq opencode--pending-input-overlay
                  (make-overlay pmark (point-max) nil t t)))
          (overlay-put opencode--pending-input-overlay 'before-string
                       (concat (opencode--block-spacing pmark)
                               (opencode--margin-string
                                'opencode-request-margin-highlight))))
      (when opencode--pending-input-overlay
        (delete-overlay opencode--pending-input-overlay)
        (setq opencode--pending-input-overlay nil)))))

(defun opencode-session--start-block (type key)
  "Start the block of TYPE identified by KEY, finishing the last one."
  (let ((last-type (car opencode--current-block)))
    (opencode--align-finished-tables t)
    (when (and (eq type 'text)
               (not (eq last-type 'text)))
      (setf opencode-last-response-start
            (opencode--session-process-position)))
    (when last-type
      (opencode--maybe-insert-block-spacing))
    ;; fold reasoning once the next block starts
    (when opencode--reasoning-start
      (opencode--fold opencode--reasoning-start nil
                      (memq 'reasoning opencode-shown-outputs))
      (setq opencode--reasoning-start nil))
    (when (eq type 'reasoning)
      (setq opencode--reasoning-start
            (opencode--session-process-position)))
    (when (eq type 'text)
      (setq opencode--table-scan-marker
            (copy-marker (opencode--session-process-position))))
    (setq opencode--current-block (cons type key))))

(defun opencode-session--block-delta (session-id key delta)
  "Output DELTA of the text or reasoning block KEY of SESSION-ID.
KEY is (TYPE MESSAGE-ID ORDINAL)."
  (opencode--with-session-buffer session-id
    (let* ((type (car key))
           (new-block-p (not (equal key (cdr opencode--current-block))))
           ;; don't display margins on extra whitespace
           (delta (if new-block-p (string-trim-left delta) delta)))
      ;; blocks start once they have visible content
      (unless (string-empty-p delta)
        (when new-block-p
          (opencode-session--start-block type key))
        (cl-pushnew key opencode--blocks-with-output :test #'equal)
        (if (eq type 'reasoning)
            (opencode--insert-reasoning-block delta)
          (opencode--output delta))
        (when (and (eq type 'text) (string-search "\n" delta))
          (opencode--align-finished-tables nil))))))

(defun opencode-session--block-ended (session-id key text)
  "Finish the text or reasoning block KEY of SESSION-ID with final TEXT.
TEXT is only output if the block had no delta, as some providers only
send the whole block once it's done."
  (opencode--with-session-buffer session-id
    (when (and text
               (not (member key opencode--blocks-with-output))
               (not (member (cadr key) opencode--replayed-unfinished-messages)))
      (opencode-session--block-delta session-id key text))
    (when (and (eq (car key) 'text)
               (equal key (cdr opencode--current-block)))
      (opencode--align-finished-tables t))))

(defun opencode--tokens-total (tokens)
  "Return the sum of the fields of TOKENS, a TokenUsage.Info."
  (let-alist tokens
    (+ .input .output .reasoning .cache.read .cache.write)))

(defun opencode--format-tokens (n)
  "Return token count N in compact form, like 3.2k."
  (file-size-human-readable (or n 0) 'si))

(defun opencode--tool-key (message-id id)
  "Return the key of tool call ID, as they are only unique within MESSAGE-ID."
  (concat message-id "/" id))

(defun opencode-session--display-tool-call (key)
  "Start a block for the tool call KEY, unless it has one."
  (let* ((call (gethash key opencode--tool-calls))
         (name (or (plist-get call :name) "tool")))
    (unless (plist-get call :marker)
      (opencode-session--start-block 'tool key)
      (puthash key
               (plist-put call :marker
                          (opencode--insert-tool-block name (plist-get call :input)))
               opencode--tool-calls))))

(defun opencode-session--tool-finished (session-id key state)
  "Display the result of the tool call KEY of SESSION-ID, finished with STATE.
Return the plist of the call, from `opencode--tool-calls'."
  (let* ((call (gethash key opencode--tool-calls))
         (name (or (plist-get call :name) "tool")))
    (opencode--with-session-buffer session-id
      ;; A call with malformed input fails without ever being called.
      (opencode-session--display-tool-call key)
      (opencode--maybe-insert-tool-output
       name state (plist-get (gethash key opencode--tool-calls) :marker)))
    (remhash key opencode--tool-calls)
    call))

(defun opencode--insert-shell-output (output failed)
  "Insert OUTPUT of a user's shell command, styled as an error if FAILED."
  (unless (string-empty-p output)
    (let ((start (opencode--session-process-position)))
      (opencode--insert-block-with-margin
       (opencode--fenced-block output "text")
       (if failed 'error 'opencode-tool-margin-highlight))
      (when failed
        (opencode--add-error-face (save-excursion
                                    (goto-char start)
                                    (forward-line 1)
                                    (point))
                                  (opencode--output-content-end)))
      (opencode--maybe-insert-block-spacing))))

(defun opencode--insert-compaction-started ()
  "Insert the status line for a compaction starting."
  (opencode-session--start-block 'compaction (float-time))
  (opencode--maybe-insert-block-spacing)
  (opencode--output (propertize "🗜 Compacting session...\n\n" 'font-lock-face 'shadow)))

(defun opencode--insert-compaction-summary (summary)
  "Insert the SUMMARY of a finished compaction, folded."
  (let ((start (opencode--session-process-position)))
    (opencode--output (concat "🗜 Compaction summary\n" (string-trim summary)))
    (opencode--output "\n\n")
    (opencode--fold start))
  (setq opencode-session-tokens 0)
  (force-mode-line-update))

(defun opencode--insert-skills (skills)
  "Insert the SKILLS loaded with a prompt, each with its text folded."
  (dolist (skill skills)
    (let-alist skill
      (let ((start (opencode--session-process-position)))
        (opencode--output (format "→ Skill \"%s\"\n%s" .name (string-trim .text)))
        (opencode--output "\n\n")
        (opencode--fold start)))))

(defun opencode--margin-string (face)
  "Return a string displaying the margin bar with FACE in the left margin."
  (propertize " " 'display `((margin left-margin) ,(propertize "▎" 'face face))))

(defun opencode--add-margin (start end face)
  "Display margin from START (inclusive) to END (exclusive) with FACE."
  ;; a reversed range would highlight the lines before START
  (when (> end start)
    (let ((ov (make-overlay start (1- end)))
          (margin (opencode--margin-string face)))
      (overlay-put ov 'line-prefix margin)
      (overlay-put ov 'wrap-prefix margin))))

(defun opencode--replay-input (text)
  "Replay a prompt with input TEXT."
  (opencode--show-prompt)
  (insert text)
  (let ((comint-input-sender #'opencode--highlight-input))
    (comint-send-input))
  (opencode--output "\n"))

(defun opencode--replay-user-request (message)
  "Replay a user request MESSAGE."
  (opencode--replay-input (alist-get 'text message))
  (opencode--insert-skills (alist-get 'skills message)))

(defun opencode--insert-block-with-margin (text face)
  "Insert TEXT with FACE margin highlight."
  (unless (string-empty-p text)
    (let ((beginning (opencode--session-process-position)))
      (opencode--output text)
      (opencode--add-margin beginning (1+ (opencode--output-content-end)) face))))

(defun opencode--output-content-end (&optional pos)
  "Return the end of the last visible line of output before POS.
POS defaults to the process marker.  Markdown hides closing fence lines
along with their newline, so they are excluded, or they would show as an
extra margin line, and the line after a fold would join its first line."
  (save-excursion
    (goto-char (or pos (opencode--session-process-position)))
    (skip-chars-backward "\r\n[:blank:]")
    (let ((end (point))
          (inhibit-field-text-motion t))
      (beginning-of-line)
      (if (looking-at-p "[`~]\\{3,\\}[[:blank:]]*$")
          (1- (point))
        end))))

(defun opencode--insert-reasoning-block (text)
  "Insert TEXT as reasoning block."
  (opencode--insert-block-with-margin text 'opencode-reasoning-margin-highlight))

(defun opencode--session-process-position ()
  "Return position of process marker."
  (marker-position
   (process-mark
    (get-buffer-process
     (current-buffer)))))

(defun opencode--refine-diff-hunks (start)
  "Refine and hide diff hunk headers from START to the process marker."
  (let ((inhibit-read-only t)
        (inhibit-field-text-motion t))
    (save-excursion
      (save-restriction
        (narrow-to-region start (opencode--session-process-position))
        (goto-char (point-min))
        (while (re-search-forward diff-hunk-header-re nil t)
          (let* ((beg (match-beginning 0))
                 (header-end (line-beginning-position 2))
                 (end (progn
                        (goto-char beg)
                        ;; patch tool headers have synthetic line counts.
                        (diff-end-of-hunk nil t)
                        (point))))
            (when diff-refine
              (diff--refine-hunk beg end t))
            ;; Font-lock can clear invisible text properties on refontification.
            (let ((overlay (make-overlay beg header-end)))
              (overlay-put overlay 'invisible t)
              (overlay-put overlay 'evaporate t))
            (goto-char (max header-end end))))))))

(defun opencode--add-shell-prompt (start)
  "Display a `$ ' prompt before the shell command in the block at START."
  (save-excursion
    (goto-char start)
    ;; Skip the hidden opening fence line
    (forward-line 1)
    (let ((overlay (make-overlay (point) (1+ (point)))))
      (overlay-put overlay 'before-string
                   (propertize "$ " 'face 'comint-highlight-prompt))
      (overlay-put overlay 'evaporate t))))

(defun opencode--insert-tool-block (tool input)
  "Insert TOOL call with INPUT as margin-highlighted block.
Return a marker at the end of the call, where its output goes."
  (let ((start (opencode--session-process-position)))
    (opencode--insert-block-with-margin
     (opencode--format-tool-call tool input)
     'opencode-tool-margin-highlight)
    (prog1 (save-excursion
             (goto-char (opencode--session-process-position))
             (skip-chars-backward "\n")
             (point-marker))
      (opencode--maybe-insert-block-spacing)
      (when (and (equal tool "shell") (alist-get 'command input))
        (opencode--add-shell-prompt start))
      (when (member tool '("edit" "patch"))
        (opencode--refine-diff-hunks start)
        (opencode--fold start nil (memq (intern tool) opencode-shown-outputs))))))

(defun opencode--shell-failed-p (tool info)
  "Return non-nil if INFO is of a failed shell TOOL.
INFO has an optional status and exit, like the metadata of a tool call."
  (let-alist info
    (and (string= tool "shell")
         (or (member .status '("timeout" "killed"))
             (and (numberp .exit) (/= .exit 0))))))

(defun opencode--tool-output-shown-p (tool state)
  "Return non-nil if the output of the TOOL call with STATE starts unfolded."
  (let-alist state
    (or (memq (intern tool) opencode-shown-outputs)
        (and (string= .status "error")
             (memq 'error opencode-shown-outputs))
        (and (memq 'shell-non-zero opencode-shown-outputs)
             (opencode--shell-failed-p tool .metadata)))))

(defconst opencode--ignore-output-for-tools '("edit" "write" "patch" "todowrite")
  "Don't show the output for these tools.")

(defun opencode--format-question-answers (answers)
  "Return ANSWERS of a question tool call, one line per question."
  (mapconcat (lambda (answer)
               (concat "→ " (if (seq-empty-p answer)
                                "Unanswered"
                              (string-join answer ", "))))
             answers "\n"))

(defun opencode--tool-content-text (content)
  "Return the text items of tool output CONTENT joined, or nil if there are none."
  (when-let* ((texts (cl-loop for item in content
                              when (equal "text" (alist-get 'type item))
                              collect (alist-get 'text item))))
    (string-join texts "\n")))

(defun opencode--add-error-face (beg end)
  "Style BEG to END with the `error' face."
  ;; Font-lock can clear text properties on refontification.
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'face 'error)
    (overlay-put overlay 'evaporate t)))

(defun opencode--maybe-insert-tool-output (tool state call-end)
  "Insert the output or error of the finished TOOL call with STATE at CALL-END.
Parallel tool calls can complete after later output, so it's inserted
after its call, foldable onto the call's last line, and folded unless
`opencode--tool-output-shown-p'.  Edits show only their diff, unless
they failed.  Errors and the output of shell tool calls that exited with a
non-zero status are highlighted with the `error' face."
  (let-alist state
    (when (and (member .status '("completed" "error"))
               call-end
               (marker-position call-end))
      (let* ((failed (string= .status "error"))
             (output (cond
                      (failed .error.message)
                      ((and (string= tool "question") .metadata.answers)
                       (opencode--format-question-answers .metadata.answers))
                      ((not (member tool opencode--ignore-output-for-tools))
                       (opencode--tool-content-text .content))))
             (errorp (or failed (opencode--shell-failed-p tool .metadata))))
        (when output
          (let ((inhibit-read-only t)
                (end (copy-marker call-end t)))
            (save-excursion
              (goto-char call-end)
              (insert "\n" (if failed
                               (opencode--fenced-block output "text")
                             (opencode--fenced-output output))))
            (opencode--mark-as-output call-end end)
            (opencode--buttonize-file-references call-end end)
            ;; The first visible output line starts at the call's hidden
            ;; closing fence, so that's where its margin is read from.
            (opencode--add-margin (1+ (opencode--output-content-end call-end))
                                  (1+ (opencode--output-content-end end))
                                  (if errorp 'error 'opencode-tool-margin-highlight))
            (when errorp
              (opencode--add-error-face (save-excursion
                                          (goto-char call-end)
                                          (forward-line 2)
                                          (point))
                                        (opencode--output-content-end end)))
            (opencode--fold (save-excursion
                              (goto-char (opencode--output-content-end call-end))
                              (let ((inhibit-field-text-motion t))
                                (line-beginning-position)))
                            end
                            (opencode--tool-output-shown-p tool state))
            (set-marker end nil))))
      (set-marker call-end nil))))

(defconst opencode--migrated-tool-names
  '(("bash" . "shell") ("apply_patch" . "patch") ("task" . "subagent"))
  "Old names of tools, that sessions migrated from v1 still have.")

(defconst opencode--migrated-tool-inputs
  '((filePath . path) (subagent_type . agent))
  "Old names of tool inputs, that sessions migrated from v1 still have.")

(defun opencode--migrate-tool-input (input)
  "Return tool INPUT with the current names of its arguments."
  (if (listp input)
      (mapcar (lambda (pair)
                (cons (alist-get (car pair) opencode--migrated-tool-inputs (car pair))
                      (cdr pair)))
              input)
    input))

(defun opencode--replay-tool (message-id tool)
  "Display the TOOL content item of the assistant message MESSAGE-ID.
Calls not yet finished are recorded so that later events finish them."
  (let-alist tool
    (let ((key (opencode--tool-key message-id .id))
          (name (or (cdr (assoc .name opencode--migrated-tool-names)) .name))
          (input (opencode--migrate-tool-input .state.input)))
      (pcase .state.status
        ;; its input is still incomplete, so it's displayed once called
        ("streaming" (puthash key (list :name name) opencode--tool-calls))
        ("running"
         (puthash key (list :name name :input input) opencode--tool-calls)
         (opencode-session--display-tool-call key))
        (_ (opencode--maybe-insert-tool-output
            name .state (opencode--insert-tool-block name input)))))))

(defun opencode--replay-assistant-message (message)
  "Display the assistant MESSAGE."
  (let-alist message
    (unless .time.completed
      (push .id opencode--replayed-unfinished-messages))
    (let (last-type)
      (dolist (item .content)
        (let ((type (alist-get 'type item))
              (text (string-trim (or (alist-get 'text item) ""))))
          (if (string= type "tool")
              (opencode--replay-tool .id item)
            (unless (string-empty-p text)
              (let ((start (opencode--session-process-position)))
                (pcase type
                  ("text"
                   (unless (equal last-type "text")
                     (setf opencode-last-response-start start))
                   (setq opencode--table-scan-marker (copy-marker start))
                   (opencode--output text)
                   (opencode--align-finished-tables t))
                  ("reasoning"
                   (opencode--insert-reasoning-block text)))
                (opencode--output "\n\n")
                (when (equal type "reasoning")
                  (opencode--fold
                   start nil
                   (memq 'reasoning opencode-shown-outputs))))
              (setq last-type type))))))
    (when .tokens
      (setq opencode-session-tokens (opencode--tokens-total .tokens)))))

(defun opencode--replay-message (message)
  "Display the past session MESSAGE."
  (let-alist message
    (pcase .type
      ("user" (opencode--replay-user-request message))
      ("assistant" (opencode--replay-assistant-message message))
      ("shell"
       (opencode--replay-input (concat "!" .command))
       (opencode--insert-shell-output (or .output.output "")
                                      (opencode--shell-failed-p "shell" message)))
      ("compaction"
       (opencode--insert-compaction-started)
       (pcase .status
         ("completed" (opencode--insert-compaction-summary .summary))
         ("failed" (opencode--insert-error .error.message))))
      ("idle"
       (pcase .outcome
         ("failed" (opencode--insert-error (or .error.message "Session failed")))
         ("interrupted" (opencode--insert-error "Interrupted")))))))

(defun opencode--replay-session-messages (messages &optional revert-message-id)
  "Display past session MESSAGES.
Those from REVERT-MESSAGE-ID on have a staged revert, so are hidden."
  (let ((inhibit-redisplay t)
        ;; collect once afterwards, rather than many times during the replay
        (gc-cons-threshold most-positive-fixnum)
        (comint-output-filter-functions
         (remove #'comint-postoutput-scroll-to-bottom
                 comint-output-filter-functions)))
    ;; Run the change hooks, like jit-lock's, once for the whole replay
    ;; rather than for every insertion.
    (combine-change-calls (point-min) (point-max)
      ;; Replayed history isn't undoable.  Recording it would be wasted
      ;; work, and anything resetting the undo list meanwhile, like Doom's
      ;; `comint-output-filter' advice, makes `combine-change-calls' warn
      ;; that it was truncated.
      (let ((buffer-undo-list t))
        (dolist (message messages)
          (unless (and revert-message-id
                       (not (string< (alist-get 'id message) revert-message-id)))
            (opencode--replay-message message)))))
    (setq opencode--current-block nil)))

(defun opencode--insert-error (message)
  "Insert error MESSAGE in the current session buffer."
  (let ((start (opencode--session-process-position)))
    (opencode--output message)
    (opencode--add-error-face start (opencode--session-process-position)))
  (opencode--output "\n\n"))

(defun opencode-session--display-error (session-id message)
  "Display error MESSAGE in SESSION-ID."
  (opencode--with-session-buffer session-id
    (opencode--insert-error message)))

(provide 'opencode-session-output)
;;; opencode-session-output.el ends here
