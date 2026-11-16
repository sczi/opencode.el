;;; opencode-flycheck.el --- Integration with flycheck  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Integration with flycheck - report warnings and errors to opencode

;;; Code:

(require 'flycheck)
(require 'opencode)

(defun opencode--flycheck-report-diagnostics (file base-directory errors)
  "Return formatted Flycheck ERRORS for FILE relative to BASE-DIRECTORY."
  (when errors
    (mapconcat (lambda (err)
                 (concat "- "
                         (file-relative-name
                          (or (flycheck-error-filename err) file)
                          base-directory)
                         ":"
                         (number-to-string (or (flycheck-error-line err) 1))
                         (when-let* ((column (flycheck-error-column err)))
                           (format ":%d" column))
                         ": "
                         (symbol-name (flycheck-error-level err))
                         ": "
                         (flycheck-error-format err nil)))
               errors
               "\n")))

(defun opencode--flycheck-fix-prompt (base-directory &optional errors)
  "Return a prompt to fix the current buffer's Flycheck errors, or nil.
If ERRORS is non-nil, use it instead of all of the buffer's errors.
File names are shown relative to BASE-DIRECTORY."
  (when-let* ((report (opencode--flycheck-report-diagnostics
                       (buffer-file-name) base-directory
                       (or errors flycheck-current-errors))))
    (concat "Fix the following:\n\n" report)))

(defun opencode--flycheck-report-status (file base-directory status)
  "Return a formatted Flycheck STATUS report for FILE.
BASE-DIRECTORY is used to shorten FILE in the report."
  (when (memq status '(errored interrupted suspicious))
    (format "- %s: warning: Flycheck ended with status `%s'"
            (file-relative-name file base-directory)
            status)))

;;;###autoload
(defun opencode-fix-flycheck (&optional at-point)
  "Start a new opencode session to fix the current buffer's Flycheck errors.
With a prefix argument AT-POINT, only fix the errors at point."
  (interactive "P")
  (unless flycheck-mode
    (user-error "Flycheck is not enabled in this buffer"))
  (let ((prompt
         (if at-point
             (opencode--flycheck-fix-prompt
              default-directory
              (or (flycheck-overlay-errors-at (point))
                  (user-error "No Flycheck error at point")))
           (or (opencode--flycheck-fix-prompt default-directory)
               (user-error "No Flycheck errors in this buffer")))))
    (opencode-new-session :initial-prompt prompt)))

;;;###autoload
(defun opencode-flycheck-edited-files (files done)
  "Run Flycheck on FILES, then call DONE with the diagnostics."
  (if (not files)
      (funcall done)
    (let ((base-directory default-directory)
          (remaining (length files))
          reports)
      (cl-labels ((finish (report)
                    (when report
                      (push report reports))
                    (cl-decf remaining)
                    (when (zerop remaining)
                      (funcall done (string-join (nreverse reports) "\n")))))
        (dolist (file files)
          (with-current-buffer (find-file-noselect file)
            (revert-buffer t t t)
            (let* ((flycheck-was-enabled flycheck-mode)
                   (completed nil)
                   after-hook
                   status-hook)
              (setq after-hook
                    (lambda ()
                      (unless completed
                        (setq completed t)
                        (remove-hook 'flycheck-after-syntax-check-hook after-hook t)
                        (remove-hook 'flycheck-status-changed-functions status-hook t)
                        (unless flycheck-was-enabled
                          (flycheck-mode -1))
                        (finish
                         (opencode--flycheck-report-diagnostics
                          file base-directory flycheck-current-errors)))))
              (setq status-hook
                    (lambda (status)
                      (when (and (not completed)
                                 (memq status '(no-checker errored interrupted suspicious)))
                        (setq completed t)
                        (remove-hook 'flycheck-after-syntax-check-hook after-hook t)
                        (remove-hook 'flycheck-status-changed-functions status-hook t)
                        (unless flycheck-was-enabled
                          (flycheck-mode -1))
                        (finish
                         (unless (eq status 'no-checker)
                           (opencode--flycheck-report-status
                            file base-directory status))))))
              (unless flycheck-mode
                (flycheck-mode 1))
              (when (flycheck-running-p)
                (flycheck-stop))
              (add-hook 'flycheck-after-syntax-check-hook after-hook nil t)
              (add-hook 'flycheck-status-changed-functions status-hook nil t)
              (flycheck-buffer))))))))

(provide 'opencode-flycheck)
;;; opencode-flycheck.el ends here
