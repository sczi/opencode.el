;;; opencode-test-helpers.el --- Helpers shared by the opencode tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Helpers shared by the opencode tests

;;; Code:

(require 'opencode)

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
