;;; opencode-session-control.el --- Session control buffers for opencode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; A table of the sessions of a project, to open and manage them.

;;; Code:

(require 'magit)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-session-output)
(require 'opencode-sessions)
(require 'profiler)
(require 'vtable)
(require 'xdg)

(defvar doom-real-buffer-p)

(defvar opencode--session-control-buffers nil
  "List of session control buffers.")

(defun opencode--session-control-command (fun)
  "Return a command that calls FUN with the session at point."
  (lambda ()
    (:documentation (format "Call `%s' on the session at point." fun))
    (interactive)
    (funcall fun (or (vtable-current-object)
                     (user-error "No session at point")))))

(defvar opencode-session-control-mode-map
  (define-keymap
    ;; needed here instead of vtable :actions, as emojify-mode keymap was taking
    ;; precedence over vtable's keymap
    "RET" (opencode--session-control-command #'opencode-open-session)
    "o" (opencode--session-control-command #'opencode-open-session)
    "R" (opencode--session-control-command #'opencode-rename-session)
    "x" (opencode--session-control-command #'opencode-kill-session)
    "r" 'opencode-sessions-redisplay
    "g" nil
    "SPC" nil
    "n" 'opencode-new-session
    "M" 'opencode-toggle-mcp
    "v" 'opencode-session-control-toggle-verbose))

(with-eval-after-load 'evil
  (declare-function evil-define-key* "evil-core")
  (evil-define-key* 'normal opencode-session-control-mode-map
    (kbd "RET") (opencode--session-control-command #'opencode-open-session)
    "o" (opencode--session-control-command #'opencode-open-session)
    "R" (opencode--session-control-command #'opencode-rename-session)
    "x" (opencode--session-control-command #'opencode-kill-session)
    "r" 'opencode-sessions-redisplay
    "n" 'opencode-new-session
    "M" 'opencode-toggle-mcp
    "v" 'opencode-session-control-toggle-verbose
    "gv" 'opencode-session-control-toggle-verbose))

(defvar-local opencode-session-control-verbose nil
  "Toggle whether to display subagents in session control buffer.")

(defvar-local opencode-session-control-project-id nil
  "Id of the project whose sessions the session control buffer displays.")

(defvar-local opencode--session-control-sessions nil
  "Sessions last displayed in the session control buffer.")

(defvar-local opencode--session-control-width nil
  "Window body width in pixels the session control table was rendered for.")

(define-derived-mode opencode-session-control-mode special-mode "Sessions"
  "Opencode session control panel mode."
  ;; Doom ignores buffers it doesn't consider real when choosing which buffer
  ;; to show after killing one, or for `evil-switch-to-windows-last-buffer'.
  (setq-local doom-real-buffer-p t)
  (add-hook 'window-size-change-functions
            #'opencode--session-control-window-size-change nil t))

(defun opencode-session-control-toggle-verbose ()
  "Toggle verbose mode in session control buffer."
  (interactive)
  (setq opencode-session-control-verbose
        (not opencode-session-control-verbose))
  (opencode-sessions-redisplay))

(defun opencode--profile-session-loading (session)
  "Profile loading SESSION, the session at point, with the CPU profiler.
Kill its buffer if it has one, reopen it, and show the profiler report
once it is loaded, along with how many redisplays happened before and
after its messages were replayed."
  (interactive (list (or (vtable-current-object)
                         (user-error "No session at point")))
               opencode-session-control-mode)
  (let* ((buffer (gethash (alist-get 'id session) opencode-session-buffers))
         (start (float-time))
         (redisplays 0)
         before-replay
         ;; `pre-redisplay-functions' runs once per window, but this runs
         ;; once per redisplay.
         (count-redisplay (lambda (&rest _) (setq redisplays (1+ redisplays))))
         (mark-replayed (lambda (&rest _)
                          (setq before-replay (or before-replay redisplays)))))
    (when (buffer-live-p buffer)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))
    (advice-add 'redisplay--pre-redisplay-functions :before count-redisplay)
    (advice-add 'opencode--replay-session-messages :after mark-replayed)
    (profiler-reset)
    (profiler-start 'cpu)
    (opencode-open-session
     session
     :callback (lambda (_session)
                 (profiler-stop)
                 (advice-remove 'redisplay--pre-redisplay-functions count-redisplay)
                 (advice-remove 'opencode--replay-session-messages mark-replayed)
                 (message "Loaded %s in %.3fs with %d redisplays before replay and %d after"
                          (opencode--session-title session)
                          (- (float-time) start)
                          (or before-replay redisplays)
                          (- redisplays (or before-replay redisplays)))
                 (profiler-report)))))

(defvar opencode--session-control-branch-cache (make-hash-table :test #'equal)
  "Cache of git branch per directory for the session control table.
Cleared each time the table is displayed.")

(defvar opencode--claude-catalog 'unloaded
  "Alist of Anthropic models from OpenCode's models.dev cache.
Loaded once, or the symbol `unloaded' before that.")

(defvar opencode--claude-plan-tier 'unfetched
  "The planTier reported by `claude auth status', nil if it isn't known.
The symbol `unfetched' before the command has been started.")

(defun opencode--claude-catalog ()
  "Return the alist of Anthropic models, reading it the first time."
  (when (eq opencode--claude-catalog 'unloaded)
    (setq opencode--claude-catalog
          (condition-case err
              (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "opencode/models.json" (xdg-cache-home)))
                (map-nested-elt (json-parse-buffer :object-type 'alist)
                                '(anthropic models)))
            (error
             (opencode--log-event "WARNING"
                                  (format "Couldn't read Claude prices: %S" err))
             nil))))
  opencode--claude-catalog)

(defun opencode--claude-model-price (id)
  "Return the API price of claude-agent model ID, or nil if it isn't known.
An alias of a model family, such as opus, uses the family's newest model."
  (let* ((catalog (opencode--claude-catalog))
         (id (string-remove-suffix "[1m]" id))
         (prefix (format "claude-%s-" id)))
    (alist-get
     'cost
     (or (alist-get (intern id) catalog)
         (cdar (sort (seq-filter (lambda (model)
                                   (string-prefix-p prefix
                                                    (symbol-name (car model))))
                                 catalog)
                     (lambda (a b)
                       (string> (alist-get 'release_date (cdr a))
                                (alist-get 'release_date (cdr b))))))))))

(defun opencode--claude-api-cost (session)
  "Return what SESSION's tokens would cost at API prices, nil if there's no price."
  (when-let* ((price (opencode--claude-model-price
                      (map-nested-elt session '(model id)))))
    (let-alist session
      (/ (+ (* (or .tokens.input 0)
               (or (alist-get 'input price) 0))
            (* (+ (or .tokens.output 0) (or .tokens.reasoning 0))
               (or (alist-get 'output price) 0))
            (* (or .tokens.cache.read 0)
               (or (alist-get 'cache_read price) 0))
            (* (or .tokens.cache.write 0)
               (or (alist-get 'cache_write price) 0)))
         1e6))))

(defun opencode--claude-subscription-session-p (session)
  "Return non-nil if SESSION uses claude-agent and the server charged nothing."
  (and (equal (map-nested-elt session '(model providerID)) "claude-agent")
       (zerop (or (alist-get 'cost session) 0))))

(defun opencode--fetch-claude-plan-tier ()
  "Find the Claude plan, once, then refresh the session control buffers."
  (when (eq opencode--claude-plan-tier 'unfetched)
    (setq opencode--claude-plan-tier nil)
    (when (executable-find "claude")
      (let ((buffer (generate-new-buffer " *opencode-claude-auth*")))
        (make-process
         :name "opencode-claude-auth"
         :buffer buffer
         :noquery t
         :connection-type 'pipe
         :command (list shell-file-name shell-command-switch
                        "claude auth status 2>/dev/null")
         :sentinel
         (lambda (process _event)
           (unless (process-live-p process)
             (setq opencode--claude-plan-tier
                   (condition-case err
                       (with-current-buffer buffer
                         (goto-char (point-min))
                         (alist-get 'planTier
                                    (json-parse-buffer :object-type 'alist)))
                     (error
                      (opencode--log-event
                       "WARNING" (format "Couldn't read Claude plan: %S" err))
                      nil)))
             (kill-buffer buffer)
             (when opencode--claude-plan-tier
               (opencode--redisplay-session-control-buffers)))))))))

(defun opencode--session-control-cost (session)
  "Return the text of the cost column for SESSION."
  (if (opencode--claude-subscription-session-p session)
      (if-let* ((cost (opencode--claude-api-cost session)))
          (if-let* ((budget (cdr (assoc opencode--claude-plan-tier
                                        opencode-claude-five-hour-budgets))))
              (format "$%.2f %.1f%%" cost (/ (* 100 cost) budget))
            (format "$%.2f" cost))
        "-")
    (format "$%.2f" (or (alist-get 'cost session) 0))))

(defconst opencode--session-control-columns
  `((title
     (:name "Title" :max-width "50%")
     ,(lambda (object)
        (let ((icon (opencode--session-status-icon object)))
          (concat (when icon (concat icon " "))
                  (opencode--session-title object)))))
    (branch
     (:name "Branch" :min-width 6)
     ,(lambda (object)
        (let-alist object
          (if (and .location.directory
                   (file-exists-p .location.directory))
              (let ((default-directory .location.directory))
                (with-memoization (gethash .location.directory
                                           opencode--session-control-branch-cache)
                  (or (magit-get-current-branch) "-")))
            "-"))))
    (last-update
     (:name "Last Updated" :width 12
      :formatter opencode--format-time-ago
      :primary ascend)
     ,(lambda (object) (opencode--time-ago object 'updated)))
    (created-at
     (:name "Created at" :width 10
      :formatter opencode--format-time-ago)
     ,(lambda (object) (opencode--time-ago object 'created)))
    (cost
     (:name "Cost" :min-width 8 :align right)
     ,#'opencode--session-control-cost)
    (tokens
     (:name "Tokens")
     ,(lambda (object)
        (let-alist object
          (format "↑%s ↓%s 🧠%s"
                  (opencode--format-tokens
                   (+ (or .tokens.input 0)
                      (or .tokens.cache.read 0)
                      (or .tokens.cache.write 0)))
                  (opencode--format-tokens .tokens.output)
                  (opencode--format-tokens .tokens.reasoning)))))
    (cache
     (:name "Cache" :width 6 :align right)
     ,(lambda (object)
        (let-alist object
          (let ((total (+ (or .tokens.input 0)
                          (or .tokens.cache.read 0)
                          (or .tokens.cache.write 0))))
            (if (zerop total)
                "-"
              (format "%d%%"
                      (round (* 100.0 (or .tokens.cache.read 0))
                             total))))))))
  "Alist of (KEY VTABLE-COLUMN-SPEC GETTER) for the session control table.
GETTER is called with the session object.")

(defun opencode--session-control-insert-table (sessions columns)
  "Insert a vtable of SESSIONS with COLUMNS, a list of column keys."
  (let* ((columns (mapcar (lambda (key)
                            (or (alist-get key opencode--session-control-columns)
                                (error "Unknown session control column: %s" key)))
                          columns))
         (getters (vconcat (mapcar #'cadr columns))))
    (make-vtable
     :columns (mapcar #'car columns)
     :objects sessions
     :getter (lambda (object column _vtable)
               (funcall (aref getters column) object))
     :separator-width 3
     :keymap opencode-session-control-mode-map)))

(defun opencode--session-control-display (sessions)
  "Display SESSIONS in the current session control buffer.
Optional columns from `opencode-session-control-table-columns' are
only shown if the table fits in the window."
  (setq opencode--session-control-sessions sessions)
  (clrhash opencode--session-control-branch-cache)
  (when (seq-some #'opencode--claude-subscription-session-p sessions)
    (opencode--fetch-claude-plan-tier))
  (let ((inhibit-read-only t)
        (point (point))
        (window (get-buffer-window (current-buffer) t))
        (required (plist-get opencode-session-control-table-columns :required))
        (optional (plist-get opencode-session-control-table-columns :optional)))
    (erase-buffer)
    (if (not sessions)
        (insert "No sessions in " (or default-directory "unknown directory"))
      (if (not window)
          (opencode--session-control-insert-table sessions required)
        ;; vtable computes percentage widths from the selected window
        (with-selected-window window
          (opencode--session-control-insert-table
           sessions (append required optional))
          (when (and optional
                     (> (car (window-text-pixel-size
                              window (point-min) (point-max) t))
                        (window-body-width window t)))
            (erase-buffer)
            (opencode--session-control-insert-table sessions required)))))
    (setq opencode--session-control-width
          (and window (window-body-width window t)))
    (goto-char point)))

(defun opencode--session-control-window-size-change (window)
  "Redisplay the session control table in WINDOW if its width changed."
  (with-current-buffer (window-buffer window)
    (when (and opencode--session-control-sessions
               (not (eql opencode--session-control-width
                         (window-body-width window t))))
      (opencode--session-control-display opencode--session-control-sessions))))

(defun opencode-sessions-redisplay ()
  "Refresh the session display table for DIRECTORY."
  (interactive)
  (when opencode-session-control-project-id
    (if opencode-session-control-verbose
        (opencode-api-sessions-for-project (opencode-session-control-project-id)
            sessions
          (opencode--session-control-display sessions))
      (opencode-api-root-sessions-for-project (opencode-session-control-project-id)
          sessions
        (opencode--session-control-display sessions)))))

(defun opencode--redisplay-session-control-buffers ()
  "Refresh all live session control buffers."
  (setq opencode--session-control-buffers
        (seq-filter #'buffer-live-p opencode--session-control-buffers))
  (dolist (buffer opencode--session-control-buffers)
    (with-current-buffer buffer
      (opencode-sessions-redisplay))))

(defun opencode-open-project (directory)
  "Open the session control buffer for the project of DIRECTORY.
Worktrees of a project share one, and new sessions started from it go in the
directory it was last opened for."
  (let* ((directory (file-name-as-directory directory))
         (default-directory directory))
    (opencode-api--cached-location
     (lambda (location)
       (let-alist location
         (with-current-buffer
             (get-buffer-create
              (format "*OpenCode Sessions: %s*"
                      (if (equal .project.id "global")
                          "global"
                        (file-name-nondirectory
                         (directory-file-name .project.canonical)))))
           (unless (derived-mode-p 'opencode-session-control-mode)
             (opencode-session-control-mode)
             (push (current-buffer) opencode--session-control-buffers))
           (setq default-directory directory
                 opencode-session-control-project-id .project.id)
           (opencode-sessions-redisplay)
           (opencode--switch-to-buffer (current-buffer))))))))

(provide 'opencode-session-control)
;;; opencode-session-control.el ends here
