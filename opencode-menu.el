;;; opencode-menu.el --- Transient menus for answering the server  -*- lexical-binding: t; -*-

;; Keywords: internal

;;; Commentary:

;; A transient menu with a heading and suffixes given when it's shown, used
;; for permission requests and forms.  Choosing a suffix made with
;; `opencode-menu-action' does it once the menu has exited, so it can show
;; another menu or prompt.  Dismissing the menu without choosing one calls the
;; function given for that, if any.  Long lines are wrapped, as headings and
;; choices come from the server and can be any length.

;;; Code:

(require 'transient)

(defvar opencode-menu--heading nil
  "The heading of the menu being shown.")

(defvar opencode-menu--suffixes nil
  "The suffix specs of the menu being shown.")

(defvar opencode-menu--buffer nil
  "The buffer the menu was shown from, where what's chosen is done.")

(defvar opencode-menu--after nil
  "Function to call once the menu has exited, or nil if nothing was chosen.")

(defvar opencode-menu--dismissed nil
  "Function to call if the menu is exited without choosing anything, or nil.")

(defun opencode-menu-action (function &rest args)
  "Return a menu command calling FUNCTION with ARGS once the menu has exited."
  (lambda ()
    (interactive)
    (opencode-menu-after (lambda () (apply function args)))))

(defun opencode-menu-after (function)
  "Call FUNCTION once the menu has exited, instead of the dismissal function.
For use in a menu command, which should exit the menu."
  (setq opencode-menu--after function))

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

(defun opencode-menu--exited ()
  "Do what was chosen in the menu, or what's done when it's dismissed."
  (remove-hook 'transient-exit-hook #'opencode-menu--exited)
  (when-let* ((function (or opencode-menu--after opencode-menu--dismissed)))
    (let ((buffer opencode-menu--buffer))
      (run-at-time 0 nil (lambda ()
                           (if (buffer-live-p buffer)
                               (with-current-buffer buffer (funcall function))
                             (funcall function)))))))

(defun opencode-menu-show (heading suffixes &optional dismissed value)
  "Show a menu with HEADING and SUFFIXES, a list of transient suffix specs.
DISMISSED is called if the menu is exited without choosing a suffix made with
`opencode-menu-action'.  VALUE is the list of arguments to enable initially.
The functions called on exit are called in the current buffer."
  (setq opencode-menu--heading heading
        opencode-menu--suffixes suffixes
        opencode-menu--buffer (current-buffer)
        opencode-menu--after nil
        opencode-menu--dismissed dismissed)
  (add-hook 'transient-exit-hook #'opencode-menu--exited)
  (transient-setup 'opencode-menu nil nil :value value))

(defun opencode-menu-active-p ()
  "Return non-nil if the menu is being shown."
  (transient-active-prefix 'opencode-menu))

(defun opencode-menu-close ()
  "Close the menu without doing anything, as what it asks is no longer needed."
  (when (opencode-menu-active-p)
    (setq opencode-menu--after nil
          opencode-menu--dismissed nil)
    (transient--emergency-exit)))

(provide 'opencode-menu)
;;; opencode-menu.el ends here
