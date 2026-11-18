;;; opencode-test-helpers.el --- Helpers shared by the opencode tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Helpers shared by the opencode tests

;;; Code:

(require 'ert)
(require 'opencode)

(defvar opencode-test-timeout 10
  "Seconds to wait for the test server before failing.")

(defun opencode-test--wait (predicate description &optional timeout)
  "Wait until PREDICATE returns non-nil, and return its value.
Fail with DESCRIPTION after TIMEOUT seconds, `opencode-test-timeout' by default."
  (let ((end (+ (float-time) (or timeout opencode-test-timeout)))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) end))
      (accept-process-output nil 0.01))
    (or value
        (ert-fail (list "Timed out waiting for" description)))))

(defmacro opencode-test-wait-until (form &optional timeout)
  "Wait until FORM is non-nil, and return its value.
Fail after TIMEOUT seconds, `opencode-test-timeout' by default."
  `(opencode-test--wait (lambda () ,form) ',form ,timeout))

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
     (let* ((process (start-process "opencode-test" (current-buffer) nil))
            (opencode-default-connection (opencode--make-connection :name 'test))
            (opencode-connections (list opencode-default-connection))
            (opencode--tool-calls (make-hash-table :test 'equal)))
       (setq opencode-session-id "session")
       (puthash "session" (current-buffer)
                (opencode-connection-session-buffers opencode-default-connection))
       (unwind-protect
           (progn ,@body)
         (delete-process process)))))


(provide 'opencode-test-helpers)
;;; opencode-test-helpers.el ends here
