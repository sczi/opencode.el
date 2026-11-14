;;; opencode-ert.el --- Integration with ERT  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Integration with ERT - report test failures to opencode

;;; Code:

(require 'ert)
(require 'find-func)
(require 'opencode)

(defvar ert--results-stats)

(defun opencode--ert-failed-tests (stats)
  "Return (TEST . RESULT) for each unexpected result in ERT STATS."
  (cl-loop for test across (ert--stats-tests stats)
           for result across (ert--stats-test-results stats)
           unless (ert-test-result-expected-p test result)
           collect (cons test result)))

(defun opencode--ert-test-location (name)
  "Return (FILE LINE SOURCE) of the definition of the ERT test NAME, or nil."
  (when-let* ((loc (ignore-errors (find-definition-noselect name 'ert--test)))
              ((cdr loc)))
    (with-current-buffer (car loc)
      (save-excursion
        (goto-char (cdr loc))
        (list (buffer-file-name)
              (line-number-at-pos)
              (buffer-substring-no-properties
               (point) (progn (forward-sexp) (point))))))))

(defun opencode--ert-report-failure (test result base-directory)
  "Return a report of the unexpected RESULT of ERT TEST.
File names are shown relative to BASE-DIRECTORY."
  (let* ((name (ert-test-name test))
         (location (opencode--ert-test-location name)))
    (pcase-let ((`(,file ,line ,source) location))
      (concat "## " (symbol-name name)
              (when file
                (format " (%s:%d)" (file-relative-name file base-directory) line))
              "\n\n"
              (when source
                (concat "Source:\n```elisp\n" source "\n```\n\n"))
              (if (ert-test-result-with-condition-p result)
                  (let ((print-escape-newlines t)
                        (print-level 8)
                        (print-length 10))
                    (concat "Failure:\n```\n"
                            (string-trim-right
                             (pp-to-string
                              (ert-test-result-with-condition-condition result)))
                            "\n```"))
                (format "Result: %s" (ert-string-for-test-result
                                      result
                                      (ert-test-result-expected-p test result))))))))

(defun opencode--ert-fix-prompt (failures base-directory)
  "Return a prompt to diagnose and fix FAILURES, a list of (TEST . RESULT).
File names are shown relative to BASE-DIRECTORY."
  (concat "Diagnose and fix the following ERT test failures:\n\n"
          (mapconcat (pcase-lambda (`(,test . ,result))
                       (opencode--ert-report-failure test result base-directory))
                     failures
                     "\n\n")))

;;;###autoload
(defun opencode-fix-ert (&optional at-point)
  "Start a new opencode session to diagnose and fix ERT test failures.
Uses all unexpected results in the current ERT results buffer.
With a prefix argument AT-POINT, only use the test at point."
  (interactive "P")
  (unless (derived-mode-p 'ert-results-mode)
    (user-error "Not in an ERT results buffer"))
  (let* ((stats ert--results-stats)
         (failures
          (if at-point
              (let* ((test (or (ert--results-test-at-point-no-redefinition)
                               (user-error "No test at point")))
                     (result (aref (ert--stats-test-results stats)
                                   (ert--stats-test-pos stats test))))
                (when (ert-test-result-expected-p test result)
                  (user-error "Test at point passed as expected"))
                (list (cons test result)))
            (or (opencode--ert-failed-tests stats)
                (user-error "No unexpected ERT failures")))))
    (opencode-new-session
     :initial-prompt (opencode--ert-fix-prompt failures default-directory))))

;;;###autoload
(with-eval-after-load 'ert
  (keymap-set ert-results-mode-map "o" #'opencode-fix-ert)
  (with-eval-after-load 'evil
    (declare-function evil-define-key* "evil-core")
    (evil-define-key* 'normal ert-results-mode-map "o" #'opencode-fix-ert)))

(provide 'opencode-ert)
;;; opencode-ert.el ends here
