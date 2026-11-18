;;; opencode-edit-hooks-test.el --- Tests of hooks run on edited files  -*- lexical-binding: t; -*-
;;; Code:

(require 'ert)
(require 'opencode-edit-hooks)

(ert-deftest opencode-reload-elisp-file-requires-siblings-test ()
  "Reloading a file finds the libraries it requires from its directory."
  (let* ((dir (make-temp-file "opencode-reload" t))
         (helper (expand-file-name "opencode-reload-test-helper.el" dir))
         (user (expand-file-name "opencode-reload-test-user.el" dir))
         (load-path-before load-path))
    (unwind-protect
        (progn
          (with-temp-file helper
            (insert "(defmacro opencode-reload-test-macro (arg &rest body)\n"
                    "  (declare (indent 1))\n"
                    "  `(list ,arg ,@body))\n"
                    "(provide 'opencode-reload-test-helper)\n"))
          (with-temp-file user
            (insert "(require 'opencode-reload-test-helper)\n"))
          (opencode--reload-elisp-file user)
          (should (featurep 'opencode-reload-test-helper))
          (should (eql (get 'opencode-reload-test-macro 'lisp-indent-function) 1))
          (should (equal load-path load-path-before)))
      (setq features (delq 'opencode-reload-test-helper features))
      (fmakunbound 'opencode-reload-test-macro)
      (put 'opencode-reload-test-macro 'lisp-indent-function nil)
      (delete-directory dir t))))

(provide 'opencode-edit-hooks-test)
;;; opencode-edit-hooks-test.el ends here
