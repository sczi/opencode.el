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

(defcustom opencode-files-finished-editing-functions nil
  "Run hooks after opencode finishes editing files.
This is an abnormal hook.  Each entry receives two arguments, FILES and
DONE.  FILES is the list of file paths from completed write, edit, or
patch tool calls.  DONE is a function the hook must call exactly once
when it finishes, either synchronously or asynchronously, optionally
with a diagnostic message string.

Hooks run in order when the session becomes idle; each starts after the
previous one calls DONE.  All diagnostics are sent to the session
together after the last hook finishes.  A new run in the same session
supersedes a running one, whose remaining hooks and diagnostics are
dropped."
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

(defvar-local opencode--files-finished-editing-run nil
  "Token identifying the session's live finished-editing hook run.")

(defun opencode--files-finished-editing-diagnostic-message (diagnostics)
  "Return a synthetic input message for DIAGNOSTICS."
  (concat
   "Diagnostics were reported after editing files. "
   "Please fix any problems caused by your changes.\n\n"
   (mapconcat #'identity diagnostics "\n\n")))

(defun opencode--run-files-finished-editing-hooks (files hooks)
  "Run HOOKS on FILES in sequence, then send their diagnostics.
Must be called in a session buffer.  Starting a new run in the same
session supersedes this one."
  (let ((buffer (current-buffer))
        (run (gensym "run"))
        diagnostics)
    (setq opencode--files-finished-editing-run run)
    (cl-labels
        ((next ()
           (when (and (buffer-live-p buffer)
                      (eq run (buffer-local-value
                               'opencode--files-finished-editing-run buffer)))
             (with-current-buffer buffer
               (if-let* ((hook (pop hooks)))
                   (let* ((called nil)
                          (done (lambda (&optional diagnostic)
                                  (unless called
                                    (setq called t)
                                    (when (and diagnostic
                                               (not (string-empty-p diagnostic)))
                                      (push diagnostic diagnostics))
                                    (next)))))
                     (condition-case err
                         (funcall hook files done)
                       (error
                        (opencode--log-event
                         "WARNING FINISHED EDITING HOOK"
                         (format "%S failed: %s" hook (error-message-string err)))
                        (funcall done))))
                 (setq opencode--files-finished-editing-run nil)
                 (when diagnostics
                   (opencode-session--send-synthetic-input
                    (opencode--files-finished-editing-diagnostic-message
                     (nreverse diagnostics)))))))))
      (next))))

(defun opencode--maybe-run-files-finished-editing-hook (session-id)
  "Run `opencode-files-finished-editing-functions' for SESSION-ID's pending files."
  (opencode--with-session-buffer session-id
    (when opencode--files-edited-this-turn
      (let ((files opencode--files-edited-this-turn))
        (setq opencode--files-edited-this-turn nil)
        (when-let* ((hooks (append opencode-files-finished-editing-functions
                                   opencode-project-files-finished-editing-functions)))
          (opencode--run-files-finished-editing-hooks files hooks))))))

(defun opencode-indent-files (files done)
  "Indent FILES with Emacs, then call DONE."
  (dolist (file files)
    (with-current-buffer (find-file-noselect file)
      (unless (opencode--indent-sensitive-buffer-p)
        (let ((inhibit-message t))
          (revert-buffer t t t)
          (indent-region (point-min) (point-max)))
        (when (buffer-modified-p)
          (message "opencode indented %s" file)
          (save-buffer)))))
  (funcall done))

(defcustom opencode-command-diagnostic-timeout 60
  "Seconds before `opencode-run-command-diagnostic' kills its command.
Nil means no timeout."
  :type '(choice (const :tag "No timeout" nil) number)
  :group 'opencode)

(defun opencode-run-command-diagnostic (command done)
  "Run COMMAND asynchronously, then call DONE with a diagnostic on failure.
For use within `opencode-files-finished-editing-functions' or
`opencode-project-files-finished-editing-functions'.  COMMAND is killed
after `opencode-command-diagnostic-timeout' seconds."
  (let* ((command-name (car command))
         (buffer (generate-new-buffer (format "*%s*" command-name)))
         (command-string (string-join command " "))
         (timed-out nil)
         timer)
    (condition-case err
        (let ((process
               (make-process
                :name command-name
                :buffer buffer
                :command command
                :connection-type 'pipe
                :noquery t
                :sentinel
                (lambda (process _event)
                  (when (memq (process-status process) '(exit signal))
                    (when timer (cancel-timer timer))
                    (let ((output (with-current-buffer buffer
                                    (string-trim (buffer-string)))))
                      (kill-buffer buffer)
                      (funcall
                       done
                       (cond
                        (timed-out
                         (format "`%s` timed out after %s seconds:\n\n%s"
                                 command-string
                                 opencode-command-diagnostic-timeout
                                 output))
                        ((zerop (process-exit-status process)) nil)
                        ((equal output "")
                         (format "`%s` failed" command-string))
                        (t
                         (format "`%s` failed:\n\n%s" command-string output))))))))))
          ;; Commands that prompt for input fail instead of hanging
          (process-send-eof process)
          (when opencode-command-diagnostic-timeout
            (setq timer (run-at-time opencode-command-diagnostic-timeout nil
                                     (lambda ()
                                       (when (process-live-p process)
                                         (setq timed-out t)
                                         (delete-process process)))))))
      (error
       (kill-buffer buffer)
       (funcall done (format "Failed to start `%s`: %s"
                             command-string
                             (error-message-string err)))))))

(defun opencode--emacs-ert-command ()
  "Return the batch Emacs command used to test with ERT."
  (append
   (list (concat invocation-directory invocation-name) "--batch"
         ;; Use this Emacs's tree-sitter grammars instead of prompting to install
         "--eval"
         (prin1-to-string
          `(setq treesit-auto-install-grammar 'never
                 treesit-extra-load-path
                 ',(cons (expand-file-name "tree-sitter" user-emacs-directory)
                         (bound-and-true-p treesit-extra-load-path)))))
   (cl-loop for path in (delete-dups (append (list default-directory) load-path nil))
            when (and (stringp path) (file-directory-p path))
            append (list "-L" (expand-file-name path)))
   (cl-loop for file in (directory-files-recursively default-directory
                                                     "\\(?:-test\\|-tests\\)\\.el\\'")
            ;; Skip Flycheck's temporary copies, which may vanish before loading
            unless (string-prefix-p "flycheck_" (file-name-nondirectory file))
            append (list "-l" file))
   ;; Set after loading, since libraries like dbus call `read-event' on load
   (list "--eval" "(setq inhibit-interaction t)"
         "-f" "ert-run-tests-batch-and-exit")))

(defun opencode-uv-pytest (_files done)
  "Run pytest with uv, then call DONE with any diagnostics."
  (opencode-run-command-diagnostic
   '("uv" "run" "python3" "-m" "pytest")
   done))

(defun opencode-emacs-ert (_files done)
  "Run ERT, then call DONE with any diagnostics."
  (opencode-run-command-diagnostic
   (opencode--emacs-ert-command)
   done))

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
