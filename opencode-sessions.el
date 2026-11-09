;;; opencode-sessions.el --- Code for managing opencode sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Code for managing opencode sessions

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'hideshow)
(require 'markdown-ts-mode)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-convert)
(require 'opencode-edit-hooks)
(require 'opencode-session-input)
(require 'opencode-session-output)
(require 'yank-media)

(defun opencode--tab-dispatch ()
  "Cycle session agent, or move to next button if point is on one."
  (interactive)
  (if (button-at (point))
      (forward-button 1)
    (call-interactively #'opencode-cycle-session-agent)))

(defvar opencode-session-mode-map
  (define-keymap
    "C-c C-y" 'opencode-yank-code-block
    "C-c Y" 'opencode-copy-conversation
    "C-c C-c" 'opencode-abort-session
    "C-c C-p" 'opencode-respond-permission
    "C-c x" 'opencode-kill-session
    "<backtab>" 'backward-button
    "TAB" 'opencode--tab-dispatch
    "C-c r" 'opencode-rename-session
    "C-c n" 'opencode-new-session
    "C-c l" 'opencode-select-session
    "C-c c" 'opencode-select-child-session
    "C-c C" 'opencode-compact-session
    "C-c t" 'opencode-scroll-to-last-response-start
    "C-c p" 'opencode-open-parent
    "C-c f" 'opencode-add-file
    "C-c b" 'opencode-add-buffer-dwim
    "C-c m" 'opencode-select-model
    "C-c v" 'opencode-select-variant
    "C-c M" 'opencode-toggle-mcp
    "C-c F" 'opencode-fork-session
    "C-c R" 'opencode-revert-message
    "/" 'opencode-insert-slash-command
    "@" 'opencode-add-mention))

(with-eval-after-load 'evil
  (declare-function evil-define-key* "evil-core")
  (evil-define-key* 'insert opencode-session-mode-map
    "@" 'opencode-add-mention
    "/" 'opencode-insert-slash-command)
  (evil-define-key* 'normal opencode-session-mode-map
    (kbd "RET") 'comint-send-input))

(defvar-local opencode-session-pending-questions nil
  "Pending questions for this session buffer, awaiting user response.
When non-nil, a plist of :session-id, :form-id and :questions, a vector.")

(defvar-local opencode-session-pending-permission nil
  "Pending permission requests for this session buffer, as a list of plists.
Each plist has keys :id, :session-id, :type, :title, and :marker.
Requests are processed FIFO.")

(defun opencode--session-buffer-p (buf)
  "Return non-nil if BUF is a opencode session buffer.
Accepts (\"name\" . buffer) to work as a read-buffer predicate."
  (with-current-buffer (cl-etypecase buf
                         ((or string buffer) buf)
                         (list (cdr buf)))
    opencode-session-id))

(defun opencode-select-open-session ()
  "Select among open session buffers."
  (interactive)
  (switch-to-buffer
   (read-buffer "Switch to: " nil t 'opencode--session-buffer-p)))

(defun opencode-consult-sessions ()
  "Run `consult-line-multi' across all open opencode sessions."
  (interactive)
  (if (require 'consult nil t)
      (progn
        (declare-function consult-line-multi "consult")
        (consult-line-multi '(:predicate opencode--session-buffer-p)))
    (user-error "This requires consult")))

(defun opencode-visit-last-idle ()
  "Open the most recent session to notify as idle."
  (interactive)
  (if opencode-alerted-sessions
      (opencode-open-session (pop opencode-alerted-sessions))
    (message "No idle sessions")))

(defun opencode--select-sessions (prompt sessions no-sessions-message)
  "Select a session from SESSIONS to open.
Use PROMPT and display NO-SESSIONS-MESSAGE if SESSIONS is empty."
  (if sessions
      (opencode-open-session
       (opencode--annotated-completion
        prompt
        (cl-loop for session in sessions
                 collect (let-alist session
                           (list (opencode--session-title session)
                                 session
                                 (opencode--format-time-ago
                                  (opencode--time-ago
                                   session 'updated))
                                 (opencode--time-ago
                                  session 'updated))))))
    (message no-sessions-message)))

(defun opencode-select-idle ()
  "Select a session that hasn't been visited since it went idle."
  (interactive)
  (opencode--select-sessions "Session: " opencode-alerted-sessions "No idle sessions"))

(defun opencode-select-child-session ()
  "Open a child (subagent) session of the current session."
  (interactive)
  (opencode-api-session-children (opencode-session-id)
      children
    (opencode--select-sessions "Subagent: " children "No children")))

(defun opencode-open-parent ()
  "Open the parent of the current session."
  (interactive)
  (opencode-api-session (opencode-session-id)
      session
    (opencode-api-session ((alist-get 'parentID session))
        parent
      (opencode-open-session parent))))

(defun opencode--session-status-indicator ()
  "Return mode line indicator for session status."
  (let-alist opencode-session-agent
    (let* ((agent (pcase (or .name .id "-")
                    ("Planner-Sisyphus" "Planner")
                    (name name)))
           (model (opencode--current-model))
           (status (pcase opencode-session-status
                     ("busy" "⏳")
                     ("idle" "🚀")
                     (_ "")))
           (limit (map-nested-elt model '(limit context)))
           (context-used (if (and (numberp limit) (> limit 0))
                             (* 100 (/ (float opencode-session-tokens) limit))
                           0)))
      (if (< (window-width) 115)
          (format "[🤖 %s] %.0f%%%% %s  " agent context-used status)
        (format "[🤖 %s - %s] %.0f%%%% %s  "
                agent
                (concat (alist-get 'name model)
                        (when (and .model.variant
                                   (not (equal .model.variant "default")))
                          (propertize (format " %s" .model.variant)
                                      'face '(bold opencode-request-margin-highlight))))
                context-used status)))))

(defun opencode--session-title (session)
  "Return the title of SESSION, which a new session may not have yet."
  (or (alist-get 'title session) "Untitled"))

(defun opencode--session-status-icon (session)
  "Return an icon for SESSION's execution state, or nil if nothing to show.
A session is busy when its last execution started after the last one
ended, like the OpenCode app's inbox."
  (let-alist session
    (cond ((and .time.busy (or (null .time.idle) (> .time.busy .time.idle)))
           "⏳")
          ((equal .outcome "failed") "❌"))))

(defun opencode-session--set-status (session-id status)
  "Set STATUS for the session with SESSION-ID and update modeline."
  (opencode--with-session-buffer session-id
    (setq opencode-session-status status)
    (force-mode-line-update)))

(defvar evil-buffer-regexps)

(define-derived-mode opencode-session-mode comint-mode "OpenCode"
  "Major mode for interacting with an opencode session."
  (setq-local comint-use-prompt-regexp nil
              comint-input-sender 'opencode--send-input
              comint-highlight-input nil
              left-margin-width (1+ left-margin-width))
  ;; Output comes from the server, never a password prompt or terminal
  ;; escapes, and these filters are costly when replaying large sessions.
  ;; They're global, so `remove-hook' can't remove them locally.
  (setq-local comint-output-filter-functions
              (seq-difference comint-output-filter-functions
                              '(comint-watch-for-password-prompt
                                ansi-color-process-output)))
  (setq-local markdown-ts-hide-markup t
              markdown-ts-enable-code-block-context-mode nil
              markdown-ts-enable-table-mode nil)
  ;; Set up Markdown's parser and font-lock without replacing comint as the
  ;; major mode (and losing its process, prompt, and input handling).
  (unless (and (treesit-language-available-p 'markdown)
               (treesit-language-available-p 'markdown-inline))
    (error "OpenCode sessions require the markdown and markdown-inline Tree-sitter grammars"))
  (markdown-ts-mode--initialize)
  ;; Markdown's conventional-mode helper uses an indirect buffer sharing all
  ;; session properties.  Mode initialization/cleanup there can interfere with
  ;; already-fontified Markdown.  Collect code faces in an isolated buffer.
  (setq-local font-lock-defaults
              '(nil nil nil nil
                (font-lock-fontify-syntactically-function
                 . opencode--fontify-markdown)))
  (setq-local font-lock-fontify-syntactically-function #'opencode--fontify-markdown)
  ;; File-reference buttons are created by comint output, not font-lock,
  ;; and prompt labels are displayed with `display'.
  (setq-local font-lock-extra-managed-props
              (cl-set-difference font-lock-extra-managed-props
                                 '(button category action help-echo display)))
  (visual-line-mode)
  (yank-media-handler "\\`image/" #'opencode--yank-image)
  ;; A foldable block starts with a line whose first char has an
  ;; `opencode-fold' property: a marker at the end of the block.
  (setq-local hs-block-start-regexp "^."
              hs-block-end-regexp nil
              hs-looking-at-block-start-predicate
              (lambda () (and (get-text-property (point) 'opencode-fold)
                              (looking-at ".*")))
              hs-forward-sexp-function
              (lambda (_) (goto-char (get-text-property (point) 'opencode-fold)))
              hs-find-block-beginning-function #'opencode--fold-beginning
              hs-inside-comment-predicate #'ignore
              ;; comint re-marks output read-only, which would unfold it
              hs-set-up-overlay
              (lambda (ov)
                (overlay-put ov 'modification-hooks nil)
                ;; redisplay skips a `display' string after invisible
                ;; text, like markup hidden at the end of the first line
                (overlay-put ov 'before-string
                             (concat " " (overlay-get ov 'display)))
                (overlay-put ov 'display nil))
              hs-display-lines-hidden t)
  (hs-minor-mode)
  ;; Evil picks a buffer's initial state from its minor modes before its
  ;; major mode, so a minor mode like `hs-minor-mode' (normal state in
  ;; evil-collection) would override comint's insert state.  Buffer name
  ;; rules are checked first, so always start in insert state.
  (setq-local evil-buffer-regexps '(("" . insert))))

;; Markdown hides whole fence lines, including their newline, only in view mode.
;; this also allows us to call markdown-ts-table-align-table in the buffer
(derived-mode-add-parents 'opencode-session-mode '(markdown-ts-view-mode))

(defun opencode-yank-code-block ()
  "Yank the code inside the markdown code block under point."
  (interactive)
  (let* ((node (treesit-parent-until (treesit-node-at (point) 'markdown)
                                     "fenced_code_block" t))
         (content (and node (treesit-search-subtree node "code_fence_content"))))
    (unless node
      (user-error "Not in a fenced code block"))
    (if content
        (copy-region-as-kill (treesit-node-start content) (treesit-node-end content))
      (kill-new ""))))

(defun opencode-copy-conversation ()
  "Copy the current conversation as a Markdown transcript.
The transcript is built from raw session messages, so it includes tool
output that isn't displayed in the session buffer, such as edit results.

Useful if you have separate usage limits for opencode vs webchat, so you can let
it gather context and start a plan in opencode, and then iterate on the plan
in webchat."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an OpenCode session"))
  (message "Fetching conversation...")
  (opencode-api-all-messages (opencode-session-id)
      messages
    (let ((transcript (opencode--conversation-to-markdown messages)))
      (kill-new transcript)
      (message "Copied conversation (%d messages, %d characters)"
               (length messages)
               (length transcript)))))

(defun opencode-kill-session (&optional session)
  "Kill SESSION."
  (interactive)
  (opencode-api-delete-session ((or (alist-get 'id session)
                                    opencode-session-id))
      _result
    ;; if called from session control buffer, don't kill buffer
    ;; but if called from a session buffer then kill this buffer
    (unless session
      (kill-buffer (current-buffer)))))

(defun opencode--session-cleanup ()
  "Clean up resources for the current session buffer."
  (when-let* ((process (get-buffer-process (current-buffer))))
    (delete-process process))
  (opencode--delete-temp-files opencode--temp-files)
  (setq opencode--temp-files nil))

(defvar opencode-session-loaded-functions nil
  "Abnormal hook run in a session buffer once its session is displayed.
Each function receives the SESSION-ID.  Events are lost when not
connected, so this is where to fetch what is pending for the session.")

(defun opencode--load-session (session &optional callback)
  "Display SESSION in the current buffer and go to a prompt, unless it's busy.
If CALLBACK is given, it will be called with SESSION afterwards."
  (let-alist session
    (opencode-api-all-messages (.id)
        messages
      (setq opencode--replayed-unfinished-messages nil
            opencode--server-agent
            (or .agent
                ;; sessions migrated from v1 only have it on their messages
                (alist-get 'agent
                           (seq-find (lambda (message)
                                       (equal "assistant" (alist-get 'type message)))
                                     (reverse messages)))
                ;; a session with no agent yet uses the default, listed first
                (alist-get 'id (seq-find #'opencode--selectable-agent-p
                                         opencode-agents))))
      (opencode--replay-session-messages messages .revert.messageID)
      ;; A new session keeps the agent and model of where it was opened from.
      (when messages
        (opencode--adopt-selection opencode--server-agent .model))
      (opencode-api-sessions-active
        active
        (if (assq (intern .id) active)
            (setq opencode-session-status "busy")
          (setq opencode-session-status "idle")
          (opencode--show-prompt))
        (force-mode-line-update)
        (run-hook-with-args 'opencode-session-loaded-functions .id)
        (when callback
          (funcall callback session))))))

(cl-defun opencode-open-session (session &key callback)
  "Open comint based shell for SESSION.
Returns the buffer.
If CALLBACK is given, it will be called with the session after it is initialized."
  (let-alist session
    (let ((old-buffer (gethash .id opencode-session-buffers)))
      (if (buffer-live-p old-buffer)
          (opencode--switch-to-buffer old-buffer)
        (let ((buffer (generate-new-buffer
                       (format "*OpenCode: %s*" (opencode--session-title session))))
              (agent (copy-tree opencode-session-agent))
              (agents (copy-tree opencode-session-agents)))
          (with-current-buffer buffer
            (opencode-session-mode)
            (setq opencode-session-id .id
                  opencode-last-session-buffer buffer
                  default-directory (file-name-as-directory .location.directory)
                  opencode-session-agents (mapcar (lambda (agent)
                                                    (unless (alist-get 'model agent)
                                                      (setf (alist-get 'model agent)
                                                            opencode-last-model))
                                                    agent)
                                                  (or agents
                                                      (copy-tree opencode-agents)))
                  opencode-session-agent (or agent
                                             (seq-find #'opencode--selectable-agent-p
                                                       opencode-session-agents))
                  mode-line-process '(:eval (opencode--session-status-indicator)))
            (hack-dir-local-variables-non-file-buffer)
            (opencode--download-slash-commands default-directory)
            (opencode--download-skills default-directory)
            (puthash .id buffer opencode-session-buffers)
            (let ((proc (start-process "dummy" buffer nil)))
              (set-process-query-on-exit-flag proc nil)
              (add-hook 'kill-buffer-hook #'opencode--session-cleanup nil t)
              (opencode-insert-logo)
              (opencode--load-session session callback))
            (opencode--switch-to-buffer buffer)))))))

(defun opencode--reload-session ()
  "Redisplay the current session from the server, keeping any typed input.
It keeps its properties, which hold the parts of its labels."
  (let ((input (buffer-substring (opencode--session-process-position)
                                 (point-max))))
    (let ((inhibit-read-only t))
      (remove-overlays)
      (erase-buffer))
    (setq opencode--current-block nil)
    (opencode-insert-logo)
    (opencode-api-session (opencode-session-id)
        session
      (opencode--load-session
       session
       (lambda (_session)
         (when (equal opencode-session-status "idle")
           (goto-char (point-max))
           (insert input)))))))

(defun opencode--goto-prompt-input ()
  "Move point to the input start of the prompt at or before point."
  (if (get-char-property (point) 'field)
      (comint-previous-prompt 1)
    (goto-char (field-beginning))))

(defun opencode--prompt-start ()
  "Return the start of the line of the prompt before point."
  (save-excursion
    (opencode--goto-prompt-input)
    (let ((inhibit-field-text-motion t))
      (line-beginning-position))))

(defun opencode--truncate-to-prompt (start text)
  "Delete from START to the end of the buffer, then prompt with TEXT as input."
  (let ((inhibit-read-only t))
    (remove-overlays start (point-max))
    (delete-region start (point-max)))
  (set-marker (process-mark (get-buffer-process (current-buffer))) start)
  (setq opencode--current-block nil)
  (opencode--show-prompt)
  (goto-char (point-max))
  (insert text))

(defun opencode--current-message-number ()
  "Return the 0-indexed message number at point.
Counts prompts from the beginning of the buffer to the current position.
Returns nil if point is before the first prompt."
  (save-excursion
    (opencode--goto-prompt-input)
    (let ((target-point (point)))
      (goto-char (point-min))
      (cl-loop do (comint-next-prompt 1)
               while (< (point) target-point)
               count t))))

(defmacro opencode--current-user-message (result &rest body)
  "Run BODY with RESULT as the user message at point, nil before the first.
Prompts are user messages and shell commands, in order."
  (declare (indent defun))
  (let ((message-number (gensym "message-number"))
        (messages (gensym "messages"))
        (continue (gensym "continue")))
    `(let ((,message-number (opencode--current-message-number))
           (,continue (lambda (,result) ,@body)))
       (if (not ,message-number)
           (funcall ,continue nil)
         (opencode-api-all-messages (opencode-session-id)
             ,messages
           (let ((message (nth ,message-number
                               (seq-filter (lambda (message)
                                             (member (alist-get 'type message)
                                                     '("user" "shell")))
                                           ,messages))))
             (when (equal "shell" (alist-get 'type message))
               (user-error "Can't do that to a shell command"))
             (funcall ,continue message)))))))

(defun opencode-rename-session (&optional session)
  "Rename SESSION. If in a session buffer, rename that session."
  (interactive)
  (let* ((old-title (or (alist-get 'title session)
                        (string-remove-prefix
                         "*OpenCode: "
                         (string-trim-right (buffer-name)
                                            "\\*\\(<[0-9]+>\\)?"))))
         (title (read-string "Title: " old-title)))
    (opencode-api-rename-session ((or (alist-get 'id session)
                                      opencode-session-id))
        `((title . ,title))
        _res)))

(defun opencode-abort-session ()
  "Abort a busy session and go back to prompt."
  (interactive)
  (opencode-api-interrupt-session (opencode-session-id)
      result
    (when (opencode--json-falsy (alist-get 'interrupted result))
      (message "Failed to abort session."))))

(defun opencode-compact-session ()
  "Compact the current session."
  (interactive)
  (if opencode-session-id
      (opencode-api-compact-session (opencode-session-id)
          (make-hash-table)
          _result)
    (user-error "Not in an OpenCode session")))

(defun opencode-scroll-to-last-response-start ()
  "Scroll to the top of the last text part of assistant response."
  (interactive)
  (unless opencode-last-response-start
    (user-error "No last response"))
  (goto-char opencode-last-response-start)
  (forward-word 1)
  (backward-word 1)
  (recenter-top-bottom 0))

(provide 'opencode-sessions)
;;; opencode-sessions.el ends here

