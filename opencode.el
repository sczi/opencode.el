;;; opencode.el --- Emacs interface to opencode -*- lexical-binding: t; byte-compile-warnings: (not docstrings-wide); -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: tools, llm, opencode
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "31.1") (magit "4.0") (plz "0.9") (plz-media-type "0.2.4") (plz-event-source "0.1.5-pre"))
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
allows it.  If the directory isn't on the server's host, a project of the
server is read from the minibuffer instead."
  (interactive (list (when current-prefix-arg (opencode--read-server))))
  (let ((directory (expand-file-name default-directory)))
    (opencode-autoconnect (lambda ()
                            (if (opencode--to-server-path directory)
                                (opencode-open-project directory)
                              (opencode--read-project)))
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

;;;###autoload
(defun opencode-new-worktree ()
  "Create a new git branch, and worktree prompting for a name.
Then open an opencode session in it."
  (interactive)
  (let* ((name (read-string "Worktree and branch name: "))
         (directory (file-name-concat opencode-worktree-directory name)))
    (when (magit-worktree-branch directory name "HEAD")
      (let ((default-directory directory))
        (opencode-new-session)))))

;;;###autoload
(defun opencode-select-project (&optional connection)
  "Completing read to prompt which project to select.
It's on CONNECTION's server, defaulting to the current connection."
  (interactive)
  (opencode-autoconnect #'opencode--read-project connection))

;;;###autoload
(defun opencode-select-session ()
  "Select and open a session from the current project, sorted by last updated time.
With consult, \\`C-SPC' previews the session under point."
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
      ;; Show a form put aside on returning to its session
      (opencode-forms--window-selected (current-buffer))
      (when opencode-session-id
        (setq opencode-last-session-buffer (current-buffer))
        (let ((key (lambda (session) (alist-get 'id session))))
          (when (cl-member opencode-session-id opencode-alerted-sessions
                           :key key :test #'string=)
            (setq opencode-alerted-sessions
                  (cl-delete opencode-session-id opencode-alerted-sessions
                             :key key :test #'string=))
            ;; Redraw from the cached sessions to clear its ✦ marker
            (opencode--redraw-session-control-buffers)))
        ;; Show pending forms when buffer becomes active
        (opencode-forms--prompt-next)
        ;; Handle pending permissions when buffer becomes active
        (when opencode-session-pending-permission
          (run-at-time 0 nil #'opencode--respond-permission))))))

(add-hook 'window-selection-change-functions 'opencode--selection-change-hook)
;; Opening a session in the selected window changes only its buffer
(add-hook 'window-buffer-change-functions 'opencode--selection-change-hook)

;; Handles the case where the session buffer was already selected when the
;; request arrived but Emacs did not have OS focus at the time.
(add-function :after after-focus-change-function #'opencode--selection-change-hook)

;;;###autoload
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
     (let* ((agent (opencode--default-agent (opencode--inherited-agents)))
            (model (alist-get 'model agent))
            (directory (or (opencode--to-server-path default-directory)
                           (user-error "%s isn't on the host of opencode server %s"
                                       default-directory
                                       (opencode-connection-name (opencode--conn))))))
       (opencode-api-create-session
           `((location . ((directory . ,(directory-file-name directory))))
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
Creates a new session starting from the current user message."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an opencode session buffer"))
  (opencode-api-fork-session (opencode-session-id)
      `((before . ,(opencode--user-message-id-at-point)))
      session
    (opencode-open-session session)))

(defun opencode-revert-message ()
  "Revert the user message at point and everything after it.
Its text and labels are put back in the prompt to edit and resend."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an opencode session buffer"))
  (let ((id (opencode--user-message-id-at-point))
        (start (copy-marker (opencode--prompt-start)))
        (input (opencode--prompt-input)))
    (opencode-api-revert-message (opencode-session-id)
        `((messageID . ,id))
        _result
      (opencode--truncate-to-prompt start input)
      (message "Reverted edits after message"))))

(defun opencode--turn-file-diffs (message-id then)
  "Call THEN with the diffs of the text files changed in MESSAGE-ID's turn.
They're the server's file diffs, with patches covering the whole files."
  (opencode-api-turn-diff (opencode-session-id message-id)
      diffs
    (funcall then (seq-remove (lambda (diff) (equal "" (alist-get 'patch diff)))
                              diffs))))

(defun opencode--turn-file-candidates (diffs)
  "Return `opencode--annotated-completion' candidates for file DIFFS.
They're annotated with their added and removed line counts, and status
unless it's modified."
  (let ((width (lambda (key)
                 (seq-max (mapcar (lambda (diff)
                                    (length (number-to-string (alist-get key diff))))
                                  diffs)))))
    (let ((added-width (funcall width 'additions))
          (removed-width (funcall width 'deletions)))
      (mapcar
       (lambda (diff)
         (let-alist diff
           (list .file
                 diff
                 (string-trim-right
                  (concat
                   (propertize (string-pad (format "+%d" .additions) (1+ added-width))
                               'face 'diff-indicator-added)
                   "  "
                   (propertize (string-pad (format "-%d" .deletions) (1+ removed-width))
                               'face 'diff-indicator-removed)
                   (unless (equal .status "modified")
                     (concat "  " (propertize .status 'face 'shadow))))))))
       diffs))))

(defun opencode--ediff-file-diff (diff)
  "Ediff the before and after texts of file DIFF.
Its buffers are killed when ediff quits."
  (let* ((file (alist-get 'file diff))
         (sides (opencode--patch-sides (alist-get 'patch diff)))
         (make-buffer
          (lambda (label text)
            (let ((buffer (generate-new-buffer
                           (format "*opencode-turn %s (%s)*" file label))))
              (with-current-buffer buffer
                (insert text)
                (goto-char (point-min))
                (let ((buffer-file-name (expand-file-name file (opencode--project-root))))
                  (set-auto-mode))
                (set-buffer-modified-p nil)
                (setq buffer-read-only t))
              buffer)))
         (before (funcall make-buffer "before" (car sides)))
         (after (funcall make-buffer "after" (cdr sides))))
    (ediff-buffers
     before after
     (list (lambda ()
             (add-hook 'ediff-quit-hook
                       (lambda ()
                         (kill-buffer before)
                         (kill-buffer after))
                       nil t))))))

(defun opencode-ediff-turn ()
  "Ediff a file changed during the turn of the user message at point.
The files are compared from before the turn to its end, so later changes
aren't shown.  It reads the file when the turn changed several."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an opencode session buffer"))
  (opencode--turn-file-diffs
   (opencode--user-message-id-at-point)
   (lambda (diffs)
     (pcase diffs
       ('nil (user-error "The turn changed no text files"))
       (`(,diff) (opencode--ediff-file-diff diff))
       (_ (when-let* ((diff (opencode--annotated-completion
                             "Ediff file: "
                             (opencode--turn-file-candidates diffs))))
            (opencode--ediff-file-diff diff)))))))

(defun opencode-unrevert-all ()
  "Unrevert all reverted messages in the current session."
  (interactive)
  (opencode-api-unrevert-all (opencode-session-id)
      _result
    (opencode--reload-session)
    (message "Restored all edits in session")))

(provide 'opencode)
;;; opencode.el ends here
