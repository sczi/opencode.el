;;; opencode.el --- Emacs interface to opencode -*- lexical-binding: t; byte-compile-warnings: (not docstrings-wide); -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: tools, llm, opencode
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "31.1") (magit "4.0") (plz "0.9") (plz-media-type "0.2.4") (plz-event-source "0.1.3"))
;; URL: https://codeberg.org/sczi/opencode.el/

;;; Commentary:

;; Emacs interface to opencode.
;; Provides a comint-based mode for interacting with an opencode server.

;;; Code:

(require 'json)
(require 'magit)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-edit-hooks)
(require 'opencode-events)
(require 'opencode-flycheck)
(require 'opencode-permission)
(require 'opencode-forms)
(require 'opencode-servers)
(require 'opencode-session-control)
(require 'opencode-sessions)
(require 'plz-event-source)
(require 'project)

;; pending fix upstream at https://github.com/r0man/plz-event-source/pull/15
;; see also: https://codeberg.org/sczi/opencode.el/issues/12
(setq plz-event-source-parser--line-regexp
      (rx (*? not-newline) (or "\r\n" "\n" "\r")))

(defgroup opencode nil
  "Emacs interface to opencode."
  :group 'applications)

(defgroup opencode-faces nil
  "Faces for opencode interface."
  :group 'opencode)

;;;###autoload
(defun opencode (&optional server)
  "Open opencode sessions control buffer for the current project directory.
It's on the current connection's server, or with a prefix argument the
SERVER in `opencode-servers' read from the minibuffer.  Connects to the
server if needed, starting it if it isn't running and its :auto-start
allows it."
  (interactive (list (when current-prefix-arg (opencode--read-server))))
  (let ((directory (expand-file-name default-directory)))
    (opencode-autoconnect (lambda () (opencode-open-project directory))
                          (when server (opencode--server-connection server)))))

(defun opencode-reload-locations ()
  "Make the server resolve the project of each directory again.
Run this after a directory's project changes, such as on `git init', its
first commit or adding a remote."
  (interactive)
  (opencode-api-reload-locations _
    :else (lambda (error-msg)
            ;; It still reloads the others when a directory's been deleted
            (setf (opencode-connection-locations (opencode--conn)) nil)
            (message "%s" error-msg))
    (setf (opencode-connection-locations (opencode--conn)) nil)
    (message "Reloaded opencode locations")))

(defvar opencode-worktree-directory (expand-file-name "~/opencode_worktrees/")
  "Directory to store worktrees created for opencode.")

(defun opencode-new-worktree ()
  "Create a new git branch, and worktree prompting for a name.
Then open an opencode session in it."
  (interactive)
  (let* ((name (read-string "Worktree and branch name: "))
         (directory (file-name-concat opencode-worktree-directory name)))
    (when (magit-worktree-branch directory name "HEAD")
      (let ((default-directory directory))
        (opencode-new-session)))))

(defun opencode-select-project (&optional connection)
  "Completing read to prompt which project to select.
It's on CONNECTION's server, defaulting to the current connection."
  (interactive)
  (opencode-autoconnect #'opencode--read-project connection))

(defun opencode-select-session ()
  "Select and open a session from the current project using completion."
  (interactive)
  (opencode-autoconnect
   (lambda ()
     (opencode-api--cached-location
      (lambda (location)
        (opencode-api-root-sessions-for-project ((map-nested-elt location '(project id)))
            sessions
          (opencode--select-sessions "Session: "
                                     sessions
                                     (format "No sessions in %s" default-directory))))))))

(defun opencode--selection-change-hook (&optional frame)
  "Hook to remove session for FRAME from the alerted sessions list when it's visited.
Also prompts for pending questions or permissions if any.
When switching frames this hook is also run for the frame that was
deselected, so only act on the currently selected frame."
  (when (or (null frame) (eq frame (selected-frame)))
    (with-current-buffer (window-buffer (selected-window))
      (when opencode-session-id
        (setf opencode-alerted-sessions
              (cl-delete-if (lambda (session)
                              (string= opencode-session-id
                                       (alist-get 'id session)))
                            opencode-alerted-sessions)
              opencode-last-session-buffer (current-buffer))
        ;; Show pending forms when buffer becomes active
        (opencode-forms--prompt-next)
        ;; Handle pending permissions when buffer becomes active
        (when opencode-session-pending-permission
          (run-at-time 0 nil #'opencode-respond-permission))))))

(add-hook 'window-selection-change-functions 'opencode--selection-change-hook)

;; Handles the case where the session buffer was already selected when the
;; request arrived but Emacs did not have OS focus at the time.
(add-function :after after-focus-change-function #'opencode--selection-change-hook)

(cl-defun opencode-new-session (&key title callback initial-prompt)
  "Create a new session. With a prefix argument it will ask for TITLE.
Without it will use a default title and then automatically generate one.
If CALLBACK is given, it will be called with the session after it is created.
If INITIAL-PROMPT is given, it is sent after CALLBACK is called."
  (interactive
   (when current-prefix-arg
     (list :title (read-string "Title: "))))
  (opencode-autoconnect
   (lambda ()
     ;; Create it with the agent and model `opencode-open-session' will select,
     ;; as a session without an agent gets the reminders some agents add only
     ;; during its first turn, which then causes an extra turn.
     (let* ((agent (car (opencode--inherited-agents)))
            (model (alist-get 'model agent)))
       (opencode-api-create-session
           `((location . ((directory . ,(directory-file-name
                                         (expand-file-name default-directory)))))
             ,@(when title
                 `((title . ,title)))
             ,@(when-let* ((id (alist-get 'id agent)))
                 `((agent . ,id)))
             ,@(when model
                 `((model . ,model))))
           session
         (opencode-open-session
          session
          :callback (lambda (session)
                      (when callback
                        (funcall callback session))
                      (when initial-prompt
                        (goto-char (point-max))
                        (insert initial-prompt)
                        (comint-send-input)))))))))

(defun opencode-fix-flycheck ()
  "Start a new opencode session to fix the current buffer's Flycheck errors."
  (interactive)
  (unless (bound-and-true-p flycheck-mode)
    (user-error "Flycheck is not enabled in this buffer"))
  (let ((prompt (or (opencode--flycheck-fix-prompt default-directory)
                    (user-error "No Flycheck errors in this buffer"))))
    (opencode-new-session :initial-prompt prompt)))

(defun opencode-toggle-mcp ()
  "Completing read to select an MCP to toggle."
  (interactive)
  (opencode-api-mcps mcps
    (let* ((name (opencode--annotated-completion
                  "MCP: "
                  (cl-loop for mcp in mcps
                           for name = (alist-get 'name mcp)
                           collect (list name
                                         name
                                         (pcase (map-nested-elt mcp '(status status))
                                           ("connected"  "🟢 connected")
                                           ("disabled"   "⚪ disabled")
                                           ("failed"     "🔴 failed")
                                           ("needs_auth" "🟡 needs auth")
                                           ("pending"    "⏳ pending"))))))
           (mcp (seq-find (lambda (mcp) (equal name (alist-get 'name mcp))) mcps)))
      (when mcp
        (if (equal "connected" (map-nested-elt mcp '(status status)))
            (opencode-api-disable-mcp (name)
                _res
              (message "Disabled %s" name))
          (opencode-api-enable-mcp (name)
              _res
            (message "Enabled %s" name)))))))

(defun opencode-fork-session ()
  "Fork the current session from the message at point.
Creates a new session starting from the current user message.
If point is before the first prompt, creates a new session instead."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an opencode session buffer"))
  (opencode--current-user-message message
    (if message
        (opencode-api-fork-session (opencode-session-id)
            `((before . ,(alist-get 'id message)))
            session
          (opencode-open-session session))
      ;; if before the first prompt just open a new session
      (opencode-new-session))))

(defun opencode-revert-message ()
  "Revert the user message at point and everything after it.
Its text and labels are put back in the prompt to edit and resend."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an opencode session buffer"))
  (let ((start (copy-marker (opencode--prompt-start)))
        (input (opencode--prompt-input)))
    (opencode--current-user-message message
      (unless message
        (user-error "No user message at point"))
      (opencode-api-revert-message (opencode-session-id)
          `((messageID . ,(alist-get 'id message)))
          _result
        (opencode--truncate-to-prompt start input)
        (message "Reverted edits after message")))))

(defun opencode-unrevert-all ()
  "Unrevert all reverted messages in the current session."
  (interactive)
  (opencode-api-unrevert-all (opencode-session-id)
      _result
    (opencode--reload-session)
    (message "Restored all edits in session")))

(provide 'opencode)
;;; opencode.el ends here
