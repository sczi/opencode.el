;;; opencode-worktree-test.el --- Worktree tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests of moving sessions and changes between git worktrees, in local
;; repositories, without a server.

;;; Code:

(require 'ert)
(require 'opencode)
(require 'opencode-sessions)
(require 'opencode-test-helpers)

(defun opencode-worktree-test--git (&rest args)
  "Run git with ARGS in `default-directory', and return its output lines.
Signal an error if it fails."
  (with-temp-buffer
    (unless (zerop (apply #'call-process "git" nil t nil args))
      (error "Git %s failed in %s: %s" (car args) default-directory (buffer-string)))
    (split-string (buffer-string) "\n" t)))

(defmacro opencode-worktree-test--with-repo (&rest body)
  "Run BODY with `default-directory' a new git repository's main worktree.
It has a committed a.txt, and a linked worktree, whose directory BODY can
refer to as `worktree'.  Our own git settings are ignored."
  (declare (indent 0) (debug t))
  `(let* ((parent (file-name-as-directory
                   (file-truename (make-temp-file "opencode-worktree-test-" t))))
          (main (file-name-as-directory (expand-file-name "main" parent)))
          (worktree (file-name-as-directory (expand-file-name "linked" parent)))
          (process-environment
           (append '("GIT_AUTHOR_NAME=opencode-test" "GIT_AUTHOR_EMAIL=test@example.com"
                     "GIT_COMMITTER_NAME=opencode-test" "GIT_COMMITTER_EMAIL=test@example.com"
                     "GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1")
                   (seq-remove (lambda (variable) (string-prefix-p "GIT_" variable))
                               process-environment)))
          (safe-local-variable-directories nil))
     (unwind-protect
         (let ((default-directory main))
           (make-directory "src" t)
           (with-temp-file "a.txt" (insert "alpha\n"))
           (with-temp-file "src/b.txt" (insert "beta\n"))
           (opencode-worktree-test--git "init" "-q" "-b" "main")
           (opencode-worktree-test--git "add" "-A")
           (opencode-worktree-test--git "commit" "-q" "-m" "Initial")
           (opencode-worktree-test--git "worktree" "add" "-q" "--detach"
                                        (directory-file-name worktree))
           ,@body)
       (delete-directory parent t))))

(ert-deftest opencode-trust-worktree-test ()
  "A linked worktree is trusted, once, if and only if its main one is."
  (opencode-worktree-test--with-repo
    ;; Not when the main worktree isn't trusted
    (opencode--trust-worktree worktree)
    (should-not safe-local-variable-directories)
    (setq safe-local-variable-directories (list main))
    ;; Nor the main worktree itself
    (opencode--trust-worktree main)
    (should (equal (list main) safe-local-variable-directories))
    ;; From a subdirectory, its worktree's root is trusted
    (opencode--trust-worktree (expand-file-name "src/" worktree))
    (should (equal (list worktree main) safe-local-variable-directories))
    (opencode--trust-worktree worktree)
    (should (equal (list worktree main) safe-local-variable-directories))))

(ert-deftest opencode-move-changes-to-worktree-test ()
  "Uncommitted changes, untracked files too, are moved to the worktree.
They're gone from the original tree, and other stashes aren't touched."
  (opencode-worktree-test--with-repo
    (with-temp-file "other.txt" (insert "other\n"))
    (opencode-worktree-test--git "stash" "push" "-q" "-u" "-m" "other")
    (with-temp-file "src/b.txt" (insert "changed\n"))
    (with-temp-file "c.txt" (insert "new\n"))
    (opencode--move-changes-to-worktree worktree)
    (let ((default-directory worktree))
      (should (equal "changed\n" (with-temp-buffer
                                   (insert-file-contents "src/b.txt")
                                   (buffer-string))))
      (should (file-exists-p "c.txt"))
      (should-not (file-exists-p "other.txt")))
    (should-not (opencode-worktree-test--git "status" "--porcelain"))
    (should (equal '("On main: other")
                   (opencode-worktree-test--git "stash" "list" "--format=%gs")))
    (should-not (opencode-worktree-test--git "for-each-ref" "refs/opencode/"))
    ;; Without changes, there's nothing to move
    (opencode--move-changes-to-worktree worktree)
    (should-not (opencode-worktree-test--git "for-each-ref" "refs/opencode/"))))

(ert-deftest opencode-move-changes-to-worktree-failed-test ()
  "Changes that fail to apply in the worktree are kept in a ref."
  (opencode-worktree-test--with-repo
    (with-temp-file "c.txt" (insert "new\n"))
    (let ((default-directory worktree))
      (with-temp-file "c.txt" (insert "in the way\n")))
    (let ((inhibit-message t))
      (opencode--move-changes-to-worktree worktree))
    (should-not (opencode-worktree-test--git "status" "--porcelain"))
    (should (length= (opencode-worktree-test--git "for-each-ref" "refs/opencode/") 1))))

(ert-deftest opencode-new-worktree-busy-session-test ()
  "A busy session isn't moved to a new worktree, nor one made."
  (with-opencode-session-test-buffer
    (let (connected)
      (cl-letf (((symbol-function 'opencode-autoconnect)
                 (lambda (&rest _) (setq connected t))))
        (setq opencode-session-status "busy")
        (should-error (opencode-new-worktree "feature") :type 'user-error)
        (should-not connected)))))

(ert-deftest opencode-relocation-target-test ()
  "A session whose directory is gone goes to that subdirectory of its project.
Or to the project's top-level directory if that doesn't exist, or nowhere
if neither does, or the project is unknown."
  (let* ((root (file-name-as-directory
                (file-truename (make-temp-file "opencode-relocation-test-" t))))
         (projects `(((id . "other") (canonical . "/elsewhere"))
                     ((id . "project") (canonical . ,(directory-file-name root)))))
         (session (lambda (subpath &optional project)
                    `((projectID . ,(or project "project")) (subpath . ,subpath)
                      (location . ((directory . "/gone/worktree")))))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "src/" root))
          (should (equal (expand-file-name "src/" root)
                         (opencode--relocation-target (funcall session "src") projects)))
          (should (equal root (opencode--relocation-target
                               (funcall session "only-in-worktree") projects)))
          (should (equal root (opencode--relocation-target (funcall session "") projects)))
          (should (equal root (opencode--relocation-target (funcall session "../up") projects)))
          (should-not (opencode--relocation-target (funcall session "src" "unknown") projects))
          (delete-directory root t)
          (should-not (opencode--relocation-target (funcall session "src") projects)))
      (delete-directory root t))))

(provide 'opencode-worktree-test)
;;; opencode-worktree-test.el ends here
