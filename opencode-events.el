;;; opencode-events.el --- Handling of opencode server events  -*- lexical-binding: t; -*-

;; Keywords: internal

;;; Commentary:

;; Dispatch of the events from an opencode server's event stream.

;;; Code:

(require 'map)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-edit-hooks)
(require 'opencode-forms)
(require 'opencode-permission)
(require 'opencode-session-control)
(require 'opencode-sessions)
(require 'plz-event-source)

(defcustom opencode-session-deleted-functions nil
  "Hook run when an opencode session is deleted.
This is an abnormal hook. Each function receives one argument, SESSION-ID."
  :type 'hook
  :group 'opencode)

(defconst opencode--event-types
  '("session.created" "session.deleted" "session.renamed" "session.archived"
    "session.unarchived" "session.moved" "session.forked"
    "session.agent.selected" "session.model.selected"
    "session.execution.started" "session.execution.succeeded"
    "session.execution.failed" "session.execution.interrupted"
    "session.step.started" "session.step.ended" "session.step.failed"
    "session.text.delta" "session.text.ended"
    "session.reasoning.delta" "session.reasoning.ended"
    "session.tool.input.started" "session.tool.called"
    "session.tool.success" "session.tool.failed"
    "session.retry.scheduled" "session.shell.ended"
    "session.compaction.started" "session.compaction.ended"
    "session.compaction.failed"
    "permission.asked" "permission.replied"
    "form.created" "form.replied" "form.cancelled" "tui.toast.show"
    "command.updated" "skill.updated")
  "Types of the events to subscribe to.")

(defun opencode--session-finished (session-id &optional interrupted)
  "Handle the end of SESSION-ID's execution.
INTERRUPTED means it was interrupted rather than finishing on its own."
  (opencode-session--set-status session-id "idle")
  ;; show it with the status change, so pending input is never left
  ;; without its separator or a prompt
  (opencode--with-session-buffer session-id
    (opencode--show-prompt))
  (opencode--redisplay-session-control-buffers)
  (opencode--maybe-run-files-finished-editing-hook session-id interrupted)
  (opencode-api-session (session-id)
      session
    (opencode--with-session-buffer session-id
      (unless (or
               (opencode--buffer-active-p (current-buffer))
               ;; don't show alert for subagent sessions
               (alist-get 'parentID session))
        ;; A background subagent or shell still running will wake the
        ;; session when it finishes, so alert once that run finishes.
        (opencode-api-session-running-jobs (session-id)
            jobs
          (unless (or jobs (opencode--buffer-active-p (current-buffer)))
            (opencode--toast-show `((title . "OpenCode Finished")
                                    (message . ,(opencode--session-title session))
                                    (variant . "success")))
            (opencode--alert-session session)))))))

(defun opencode--handle-message (event)
  "Handle decoded EVENT from opencode server."
  (let ((type (intern (alist-get 'type event)))
        (data (alist-get 'data event)))
    (opencode--log-event "MESSAGE" event)
    (let-alist data
      (let ((block-key (list (if (memq type '(session.text.delta session.text.ended))
                                 'text
                               'reasoning)
                             .assistantMessageID .ordinal))
            (tool-key (opencode--tool-key .assistantMessageID .id)))
        (cl-case type
          (server.connected nil)
          (tui.toast.show (opencode--toast-show data))
          (session.execution.started
           (opencode-session--set-status .sessionID "busy")
           ;; a session can be woken after showing its prompt, like by
           ;; a background subagent finishing, so output goes before it
           (opencode--with-session-buffer .sessionID
             (opencode--retract-prompt))
           (opencode--redisplay-session-control-buffers))
          (session.execution.succeeded (opencode--session-finished .sessionID))
          (session.execution.interrupted
           ;; On a real shutdown the drain resumes after the restart, but the
           ;; server also reports any interrupt without a reason this way, like
           ;; rejecting a permission, and then nothing follows.  Otherwise the
           ;; user interrupted it.
           (if (equal .reason "shutdown")
               (opencode--unless-session-active
                .sessionID (lambda () (opencode--session-finished .sessionID t)))
             (opencode-session--display-error .sessionID "Interrupted")
             (opencode--session-finished .sessionID t)))
          (session.execution.failed
           (opencode-session--display-error .sessionID .error.message)
           (opencode--session-finished .sessionID))
          (session.step.started
           (opencode--with-session-buffer .sessionID
             ;; Keys are only unique within a step, so forget the last
             ;; block's key, but keep its type to space the next block.
             (setq opencode--current-block (and opencode--current-block
                                                (list (car opencode--current-block)))
                   opencode--blocks-with-output nil)))
          (session.step.ended
           (opencode--update-claude-rate-limit .native.rateLimit)
           (opencode--with-session-buffer .sessionID
             (opencode--finish-text-lines t)
             (setq opencode-session-tokens (opencode--tokens-total .tokens))
             (setq opencode-session-cost (+ opencode-session-cost (or .cost 0)))
             (force-mode-line-update)
             (when (equal "stop" .finish)
               (opencode--maybe-insert-block-spacing))))
          (session.step.failed
           (opencode--update-claude-rate-limit .native.rateLimit)
           (opencode--with-session-buffer .sessionID
             (opencode--finish-text-lines t)))
          (session.retry.scheduled
           (opencode-api-session (.sessionID)
               session
             (opencode--toast-show
              `((title . ,(concat "OpenCode: " (opencode--session-title session)))
                (message . ,(format "%s\n\nRetry #%d" .error.message .attempt))
                (variant . "warning")))))
          (session.renamed
           (opencode--with-session-buffer .sessionID
             (setq opencode-session-title .title)
             (rename-buffer (format "*OpenCode: %s*" .title) t))
           (opencode--redisplay-session-control-buffers))
          (session.deleted
           (run-hook-with-args 'opencode-session-deleted-functions .sessionID)
           (opencode--redisplay-session-control-buffers)
           (opencode--with-session-buffer .sessionID
             (delete-process)))
          ((session.created session.archived session.unarchived
                            session.moved session.forked)
           (opencode--redisplay-session-control-buffers))
          (session.agent.selected
           (opencode--with-session-buffer .sessionID
             (unless (equal .agent opencode--server-agent)
               (setq opencode--server-agent .agent)
               (opencode--adopt-selection
                .agent (alist-get 'model opencode-session-agent)))))
          (session.model.selected
           (opencode--with-session-buffer .sessionID
             (opencode--adopt-selection nil .model)))
          ((session.text.delta session.reasoning.delta)
           (opencode-session--block-delta .sessionID block-key .delta))
          ((session.text.ended session.reasoning.ended)
           (opencode-session--block-ended .sessionID block-key .text))
          (session.tool.input.started
           (puthash tool-key (list :name .name) opencode--tool-calls))
          (session.tool.called
           (puthash tool-key
                    (plist-put (gethash tool-key opencode--tool-calls) :input .input)
                    opencode--tool-calls)
           (opencode--with-session-buffer .sessionID
             (opencode-session--display-tool-call tool-key)))
          ((session.tool.success session.tool.failed)
           (let ((call (opencode-session--tool-finished
                        .sessionID tool-key
                        `((status . ,(if (eq type 'session.tool.success) "completed" "error"))
                          (error . ,.error) (content . ,.content) (metadata . ,.metadata)))))
             (when (eq type 'session.tool.success)
               (opencode--maybe-run-file-edited-hook
                (plist-get call :name) (plist-get call :input) .metadata))))
          (session.shell.ended
           (opencode--with-session-buffer .sessionID
             (opencode--insert-shell-output (or .output.output "")
                                            (opencode--shell-failed-p "shell" .shell))
             ;; A shell command isn't an execution, so no events follow to do so.
             (when (equal opencode-session-status "idle")
               (opencode--show-prompt))))
          (session.compaction.started
           (opencode--with-session-buffer .sessionID
             (opencode--insert-compaction-started)))
          (session.compaction.ended
           (opencode--with-session-buffer .sessionID
             (opencode--insert-compaction-summary .text)))
          (session.compaction.failed
           (opencode-session--display-error .sessionID .error.message))
          (permission.asked
           (opencode--permission-request
            .id .sessionID .action .metadata .resources .save .message))
          (permission.replied
           (opencode--permission-replied .sessionID .requestID .reply))
          ((command.updated skill.updated)
           (opencode--directory-cache-updated
            (if (eq type 'command.updated) 'slash-commands 'skills)
            (opencode--from-server-path (map-nested-elt event '(location directory)))))
          (form.created (opencode--form-request .form))
          ((form.replied form.cancelled)
           (opencode--form-settled .id))
          (otherwise (opencode--log-event "WARNING" "unhandled message type")))))))

(defun opencode--handle-event (connection event)
  "Handle EVENT from the event stream of CONNECTION's server."
  (let* ((opencode--dispatching-connection connection)
         (data (json-parse-string (plz-event-source-event-data event)
                                  :array-type 'list
                                  :object-type 'alist))
         (directory (map-nested-elt data '(location directory)))
         (directory-for-event (if (stringp directory)
                                  (opencode--normalize-directory
                                   (opencode--from-server-path directory))
                                default-directory)))
    ;; Responses to requests made here run in the buffer that made them, so
    ;; use one that outlives the stream.  An error here shouldn't take the
    ;; stream's process filter down with it.
    (with-current-buffer (get-buffer-create " *opencode-events*")
      (let ((default-directory directory-for-event))
        (condition-case err
            (opencode--handle-message data)
          (error
           (message "opencode: error handling %s event: %s"
                    (alist-get 'type data) (error-message-string err))
           (opencode--log-event "WARNING EVENT HANDLER"
                                (format "%S failed: %s" data (error-message-string err)))))))))

(defun opencode--directory-cache-updated (slot directory)
  "Refetch SLOT of `opencode--directory-fetchers' for DIRECTORY if it was downloaded."
  (when (opencode--directory-cache-entry slot directory)
    (opencode--download-for-directory slot directory t)))

(provide 'opencode-events)
;;; opencode-events.el ends here
