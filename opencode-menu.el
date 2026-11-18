;;; opencode-menu.el --- Transient menus for answering the server  -*- lexical-binding: t; -*-

;; Keywords: internal

;;; Commentary:

;; A transient menu with a heading and suffixes given when it's shown, used
;; for permission requests and forms.  Choosing a suffix made with
;; `opencode-menu-action' does it once the menu has exited, so it can show
;; another menu or prompt.  Dismissing the menu does nothing, and it can be
;; suspended and resumed as usual.  Long lines are wrapped, as headings and
;; choices come from the server and can be any length.

;;; Code:

(require 'transient)

(defvar opencode-menu--heading nil
  "The heading of the menu being shown.")

(defvar opencode-menu--suffixes nil
  "The suffix specs of the menu being shown.")

(defvar opencode-menu--buffer nil
  "The buffer the menu was shown from, where what's chosen is done.")

(defun opencode-menu-action (function &rest args)
  "Return a menu command calling FUNCTION with ARGS once the menu has exited."
  (lambda ()
    (interactive)
    (opencode-menu-after (lambda () (apply function args)))))

(defun opencode-menu-after (function)
  "Call FUNCTION once the menu has exited, in the buffer it was shown from.
For use in a menu command, which should exit the menu."
  (let ((buffer opencode-menu--buffer))
    (run-at-time 0 nil (lambda ()
                         (if (buffer-live-p buffer)
                             (with-current-buffer buffer (funcall function))
                           (funcall function))))))

(defun opencode-menu--children (_children)
  "Return the suffixes of the menu being shown.
_CHILDREN is the statically-defined children, of which there are none."
  (transient-parse-suffixes 'opencode-menu opencode-menu--suffixes))

(transient-define-prefix opencode-menu ()
  "Answer a request from the opencode server."
  [:class transient-column
   :description (lambda () opencode-menu--heading)
   :setup-children opencode-menu--children])

(defun opencode-menu--wrap-lines ()
  "Wrap long lines in the menu."
  (when (eq (oref transient--prefix command) 'opencode-menu)
    (setq truncate-lines nil
          word-wrap t)))

(add-hook 'transient-setup-buffer-hook #'opencode-menu--wrap-lines)

(defun opencode-menu-show (heading suffixes &optional value scope)
  "Show a menu with HEADING and SUFFIXES, a list of transient suffix specs.
VALUE is the list of arguments to enable initially.  SCOPE identifies what
the menu is for, see `opencode-menu-active-p'.  Actions are done in the
current buffer.  This replaces a suspended menu."
  (setq opencode-menu--heading heading
        opencode-menu--suffixes suffixes
        opencode-menu--buffer (current-buffer))
  (transient-setup 'opencode-menu nil nil :value value :scope scope))

(defun opencode-menu-active-p (&optional scope)
  "Return non-nil if the menu is being shown.
If SCOPE is non-nil, only if it was shown with that scope."
  (when-let* ((prefix (transient-active-prefix 'opencode-menu)))
    (or (null scope) (equal scope (oref prefix scope)))))

(defun opencode-menu-close (&optional scope)
  "Close the menu without doing anything, as what it asks is no longer needed.
If SCOPE is non-nil, only if it was shown with that scope."
  (when (opencode-menu-active-p scope)
    (transient--emergency-exit)))

(provide 'opencode-menu)
;;; opencode-menu.el ends here