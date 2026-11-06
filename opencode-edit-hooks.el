;;; opencode-edit-hooks.el --- Hooks run on files opencode edits  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Run `opencode-file-edited-functions' after each edit, and
;; `opencode-files-finished-editing-functions' once a session goes idle,
;; sending the diagnostics they report back to the session.

;;; Code:

(require 'cl-lib)
(require 'opencode-common)
(require 'opencode-diff-parser)
(require 'opencode-session-input)

(defcustom opencode-file-edited-functions
  '(opencode--fix-parens
    opencode--reload-elisp-file)
  "Hook run after opencode edits a file.
This is an abnormal hook.  Each function receives two arguments, FILE and
RANGES, after a completed write, edit, or patch tool call.  FILE is the
edited file path.  RANGES is a list of (START-LINE . END-LINE) cons cells in
the post-edit file, or nil when the whole file was written.  For deletion-only
edits, END-LINE is nil."
  :type 'hook
  :group 'opencode)

(defcustom opencode-files-finished-editing-functions
  '(opencode--indent-files
    opencode--flycheck-edited-files)
  "Run hooks after opencode finishes editing files.
This is an abnormal hook.  Each entry receives one argument, FILES,
the list of file paths from completed write, edit, or patch tool calls.
Hooks run when the session becomes idle.  Hooks that finish
synchronously can just return.  Hooks that finish asynchronously must
return `:opencode-async' and call `opencode-hook-finished' when done.
While a hook is active, it may call `opencode-report-diagnostic' zero
or more times to queue diagnostic messages; all queued diagnostics are
sent to the session together after all hooks finish."
  :type 'hook
  :group 'opencode)

(defcustom opencode-project-files-finished-editing-functions
  nil
  "Like `opencode-files-finished-editing-functions', but per-project.
Designed to be set in `.dir-locals.el'"
  :local t
  :type 'hook
  :group 'opencode)

(defvar-local opencode--files-edited-this-turn nil
  "Files edited by opencode during the current turn.")

(defvar-local opencode--skip-files-finished-editing-hook-on-next-idle nil
  "Non-nil means the next session idle should not run finished-editing hooks.")

(defun opencode--defer-files-finished-editing-hook (session-id)
  "Return and clear SESSION-ID's next-idle finished-editing hook skip flag."
  (opencode--with-session-buffer session-id
    (prog1 opencode--skip-files-finished-editing-hook-on-next-idle
      (setq opencode--skip-files-finished-editing-hook-on-next-idle nil))))

(defun opencode--maybe-run-file-edited-hook (session-id tool input metadata)
  "Record files edited by a completed TOOL call and run per-file hooks.
INPUT is the input of the call in SESSION-ID, and METADATA its result metadata."
  (dolist (file-ranges
           (pcase tool
             ("write"
              (when-let* ((path (alist-get 'path input)))
                (list (cons path nil))))
             ((or "edit" "patch")
              ;; The patch tool's diff headers have absolute paths, so trust .file
              (mapcar (lambda (file)
                        (cons (alist-get 'file file)
                              (mapcan (lambda (entry) (copy-sequence (cdr entry)))
                                      (opencode-diff->source-line-ranges
                                       (alist-get 'patch file)))))
                      (alist-get 'files metadata)))))
    (when-let* ((file (opencode--resolve-file-reference (car file-ranges))))
      (opencode--with-session-buffer session-id
        (cl-pushnew file opencode--files-edited-this-turn :test #'equal))
      (run-hook-with-args 'opencode-file-edited-functions
                          file (cdr file-ranges)))))

(defvar opencode--files-finished-editing-current nil
  "Current `opencode-files-finished-editing-functions' run context.")

(defvar opencode--files-finished-editing-queue nil
  "Queued `opencode-files-finished-editing-functions' run contexts.")

(defun opencode-report-diagnostic (message)
  "Queue diagnostic MESSAGE for the active finished-editing hook."
  (unless opencode--files-finished-editing-current
    (error "`opencode-report-diagnostic' called outside a finished-editing hook"))
  (when (and message (not (string= message "")))
    (push message
          (plist-get opencode--files-finished-editing-current :diagnostics))))

(defun opencode-hook-finished ()
  "Mark the active finished-editing hook as complete."
  (let ((context opencode--files-finished-editing-current))
    (unless context
      (error "`opencode-hook-finished' called outside a finished-editing hook"))
    (unless (plist-get context :active-hook)
      (error "`opencode-hook-finished' called without an active hook"))
    (plist-put context :active-hook nil)
    (opencode--run-next-files-finished-editing-hook)))

(defun opencode--files-finished-editing-diagnostic-message (diagnostics)
  "Return a synthetic input message for DIAGNOSTICS."
  (concat
   "Diagnostics were reported after editing files. "
   "Please fix any problems caused by your changes.\n\n"
   (mapconcat #'identity diagnostics "\n\n")))

(defun opencode--maybe-start-files-finished-editing-hook ()
  "Start the next queued finished-editing hook run if none is active."
  (unless opencode--files-finished-editing-current
    (when opencode--files-finished-editing-queue
      (let ((context (pop opencode--files-finished-editing-queue)))
        (setq opencode--files-finished-editing-current context)
        (opencode--run-next-files-finished-editing-hook)))))

(defun opencode--finish-files-finished-editing-hook-run (context)
  "Finish finished-editing hook run CONTEXT and send queued diagnostics."
  (let ((diagnostics (nreverse (plist-get context :diagnostics)))
        (buffer (plist-get context :session-buffer)))
    (setq opencode--files-finished-editing-current nil)
    (when (and diagnostics (buffer-live-p buffer))
      (with-current-buffer buffer
        (opencode-session--send-synthetic-input
         (opencode--files-finished-editing-diagnostic-message diagnostics)))))
  (opencode--maybe-start-files-finished-editing-hook))

(defun opencode--run-next-files-finished-editing-hook ()
  "Run the next hook in the active finished-editing context."
  (let ((context opencode--files-finished-editing-current))
    (unless context
      (error "No active finished-editing hook context"))
    (when-let* ((buffer (plist-get context :session-buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (if-let* ((hook (car (plist-get context :hooks))))
              (let (result errored)
                (plist-put context :hooks (cdr (plist-get context :hooks)))
                (plist-put context :active-hook hook)
                (condition-case err
                    (setq result (funcall hook (plist-get context :files)))
                  (error
                   (setq errored t)
                   (opencode--log-event
                    "WARNING FINISHED EDITING HOOK"
                    (format "%S failed: %s" hook (error-message-string err)))
                   (when (and (eq opencode--files-finished-editing-current context)
                              (eq (plist-get context :active-hook) hook))
                     (plist-put context :active-hook nil)
                     (opencode--run-next-files-finished-editing-hook))))
                (when (and (not errored)
                           (not (eq result :opencode-async))
                           (eq opencode--files-finished-editing-current context)
                           (eq (plist-get context :active-hook) hook))
                  (plist-put context :active-hook nil)
                  (opencode--run-next-files-finished-editing-hook)))
            (opencode--finish-files-finished-editing-hook-run context)))))))

(defun opencode--maybe-run-files-finished-editing-hook (session-id)
  "Run `opencode-files-finished-editing-functions' for SESSION-ID's pending files."
  (opencode--with-session-buffer session-id
    (when opencode--files-edited-this-turn
      (let ((files opencode--files-edited-this-turn))
        (setq opencode--files-edited-this-turn nil)
        (when-let* ((hooks (append opencode-files-finished-editing-functions
                                   opencode-project-files-finished-editing-functions)))
          (setq opencode--files-finished-editing-queue
                (append opencode--files-finished-editing-queue
                        (list (list :session-buffer (current-buffer)
                                    :files files
                                    :hooks hooks
                                    :diagnostics nil
                                    :active-hook nil))))
          (opencode--maybe-start-files-finished-editing-hook))))))

(defun opencode--indent-files (files)
  "Indent FILES with Emacs."
  (dolist (file files)
    (with-current-buffer (find-file-noselect file)
      (unless (opencode--indent-sensitive-buffer-p)
        (let ((inhibit-message t))
          (revert-buffer t t t)
          (indent-region (point-min) (point-max)))
        (when (buffer-modified-p)
          (message "opencode indented %s" file)
          (save-buffer))))))

(defun opencode-run-command-diagnostic (command)
  "Run COMMAND asynchronously and report diagnostics.
For use within `opencode-files-finished-editing-functions' or
`opencode-project-files-finished-editing-functions'."
  (let* ((command-name (car command))
         (buffer (generate-new-buffer (format "*%s*" command-name)))
         (command-string (string-join command " ")))
    (condition-case err
        (progn
          (make-process
           :name command-name
           :buffer buffer
           :command command
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (process _event)
             (when (memq (process-status process) '(exit signal))
               (unwind-protect
                   (unless (zerop (process-exit-status process))
                     (let ((output (with-current-buffer (process-buffer process)
                                     (string-trim (buffer-string)))))
                       (opencode-report-diagnostic
                        (if (equal output "")
                            (format "`%s` failed" command-string)
                          (format "`%s` failed:\n\n%s"
                                  command-string
                                  output)))))
                 (kill-buffer buffer)
                 (opencode-hook-finished)))))
          :opencode-async)
      (error
       (kill-buffer buffer)
       (opencode-report-diagnostic
        (format "Failed to start `%s`: %s"
                command-string
                (error-message-string err)))
       nil))))

(defun opencode--emacs-ert-command ()
  "Return the batch Emacs command used to test with ERT."
  (append
   (list (concat invocation-directory invocation-name) "--batch")
   (cl-loop for path in (delete-dups (append (list default-directory) load-path nil))
            when (and (stringp path) (file-directory-p path))
            append (list "-L" (expand-file-name path)))
   (cl-loop for file in (directory-files-recursively default-directory
                                                     "\\(?:-test\\|-tests\\)\\.el\\'")
            append (list "-l" file))
   (list "-f" "ert-run-tests-batch-and-exit")))

(defun opencode-uv-pytest (_files)
  "Run pytest with uv and report diagnostics."
  (opencode-run-command-diagnostic
   '("uv" "run" "python3" "-m" "pytest")))

(defun opencode-emacs-ert (_files)
  "Run ERT and report diagnostics."
  (opencode-run-command-diagnostic
   (opencode--emacs-ert-command)))

(defun opencode--reload-elisp-file (file &optional _ranges)
  "Reload elisp FILE after LLM edits."
  (when (string-suffix-p ".el" file)
    (condition-case err
        (load-file file)
      (error
       (message "Error reloading %s after LLM edit: %s" file err)))))

(defvar flycheck-checkers)

(defun opencode--fix-parens (file &optional _ranges)
  "Use parinfer to fix parens in FILE after LLM edits."
  (when (and (or (string-suffix-p ".el" file)
                 (string-suffix-p ".lisp" file))
             (require 'parinfer-rust-mode nil t))
    (declare-function parinfer-rust--set-default-state "parinfer-rust-mode")
    (declare-function parinfer-rust--execute "parinfer-rust-mode")
    (defvar parinfer-rust--mode)
    (with-current-buffer (find-file-noselect file)
      (revert-buffer t t t)
      (condition-case nil
          (check-parens)
        (error
         (let ((flycheck-checkers (delq 'parinfer-rust
                                        (and (boundp 'flycheck-checkers)
                                             flycheck-checkers))))
           (parinfer-rust--set-default-state)
           (let ((parinfer-rust--mode "indent"))
             (parinfer-rust--execute)))
         (save-buffer))))))

(defun opencode--indent-sensitive-buffer-p ()
  "Return non-nil if the current buffer is indentation-sensitive."
  (derived-mode-p '(python-base-mode
                    haskell-mode
                    haskell-ts-mode
                    fsharp-mode
                    fsharp-ts-mode
                    yaml-mode
                    yaml-ts-mode
                    nim-mode
                    nim-ts-mode)))

(provide 'opencode-edit-hooks)
;;; opencode-edit-hooks.el ends here
