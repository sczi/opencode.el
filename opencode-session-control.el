;;; opencode-session-control.el --- Session control buffers for opencode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; A table of the sessions of a project, to open and manage them.

;;; Code:

(require 'color)
(require 'magit)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-session-output)
(require 'opencode-sessions)
(require 'profiler)
(require 'svg)
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

(defvar opencode--claude-rate-limit 'unfetched
  "The latest Claude subscription rate limit info, nil if there is none.
A cons of the time in seconds it was reported and the rateLimit alist that
claude-agent puts in a step's native metadata.  The symbol `unfetched' before
the latest stored one has been requested.")

(defvar-local opencode--session-control-usage-overlay nil
  "Overlay at the end of the session control buffer showing Claude usage.")

(defconst opencode--claude-usage-window-names
  '((five_hour . "5h") (seven_day . "week"))
  "Alist of names to show for Claude rate limit windows.
Windows not in it are shown with their name's underscores replaced by spaces.")

(defun opencode--update-claude-rate-limit (rate-limit &optional time)
  "Record RATE-LIMIT, reported at TIME in seconds, or now if nil.
It is ignored if older than the one already recorded."
  (let ((time (or time (float-time))))
    (when (and rate-limit
               (or (not (consp opencode--claude-rate-limit))
                   (>= time (car opencode--claude-rate-limit))))
      (setq opencode--claude-rate-limit (cons time rate-limit))
      (opencode--map-session-control-buffers
       #'opencode--session-control-update-usage))))

(defun opencode--fetch-claude-rate-limit (sessions)
  "Record the latest rate limit info stored in the newest of SESSIONS, once.
Only claude-agent sessions are considered."
  (when-let* (((eq opencode--claude-rate-limit 'unfetched))
              (session (car (seq-sort-by
                             (lambda (session)
                               (or (map-nested-elt session '(time updated)) 0))
                             #'>
                             (seq-filter
                              (lambda (session)
                                (equal (map-nested-elt session '(model providerID))
                                       "claude-agent"))
                              sessions)))))
    (setq opencode--claude-rate-limit nil)
    ;; Only steps that got a rate_limit_event have it
    (opencode--request-messages-with-rate-limit (alist-get 'id session))))

(defun opencode--request-messages-with-rate-limit (session-id)
  "Record the rate limit info of SESSION-ID's newest message that has it."
  (opencode-api-recent-assistant-messages (session-id)
      messages
    :else (lambda (error-msg)
            (opencode--log-event
             "WARNING" (format "Couldn't fetch Claude rate limit: %s" error-msg)))
    (when-let* ((message (seq-find (lambda (message)
                                     (map-nested-elt message '(native rateLimit)))
                                   messages)))
      (let-alist message
        (opencode--update-claude-rate-limit
         .native.rateLimit
         (/ (or .time.completed .time.created) 1000.0))))))

(defun opencode--format-claude-reset (resets-at)
  "Return RESETS-AT, in epoch seconds, as a short time."
  (format-time-string (if (< (- resets-at (float-time)) (* 20 3600))
                          "%H:%M"
                        "%a %H:%M")
                      resets-at))

(defconst opencode--usage-bar-width 20
  "Width in characters of Claude usage bars.")

(defun opencode--usage-bar-face (fraction)
  "Return the face for a usage bar FRACTION full."
  (cond ((>= fraction 0.85) 'error)
        ((>= fraction 0.6) 'warning)
        (t 'success)))

(defun opencode--usage-bar-svg (fraction face)
  "Return an SVG image of a rounded bar FRACTION full, filled in FACE's color."
  (let* ((width (* opencode--usage-bar-width (frame-char-width)))
         (height (max 4 (round (* 0.5 (frame-char-height)))))
         (radius (/ height 2.0))
         (background (color-name-to-rgb
                      (or (face-background 'default nil t) "white")))
         (fill-rgb (or (color-name-to-rgb (face-foreground face nil t))
                       background))
         (blend (lambda (alpha)
                  (apply #'color-rgb-to-hex
                         (append (color-blend fill-rgb background alpha)
                                 '(2)))))
         (filled (* (min 1 (max 0 fraction)) width))
         (svg (svg-create width height)))
    (svg-rectangle svg 0.5 0.5 (- width 1) (- height 1)
                   :rx (- radius 0.5) :fill (funcall blend 0.12)
                   :stroke (funcall blend 0.25) :stroke-width 1)
    (when (> filled 0)
      (svg-rectangle svg 0 0 (max height filled) height
                     :rx radius :fill (funcall blend 1)))
    (svg-image svg :ascent 'center)))

(defun opencode--usage-bar (fraction)
  "Return a bar showing FRACTION, colored by how full it is.
It is drawn with box drawing characters, displayed as an SVG on graphical
frames."
  (let* ((face (opencode--usage-bar-face fraction))
         (filled (round (* opencode--usage-bar-width (min 1 (max 0 fraction)))))
         (text (concat (propertize (make-string filled ?━) 'face face)
                       (propertize (make-string (- opencode--usage-bar-width
                                                   filled)
                                                ?─)
                                   'face 'shadow))))
    (if (and (display-graphic-p) (image-type-available-p 'svg))
        (propertize text 'display (opencode--usage-bar-svg fraction face))
      text)))

(defun opencode--format-claude-usage (rate-limit &optional width)
  "Return a line describing Claude usage from RATE-LIMIT.
If WIDTH, in pixels, is too narrow to fit it on one line, put each
usage window on its own line instead."
  (let* ((now (float-time))
         (windows
          (mapcar
           (pcase-lambda (`(,name . ,window))
             (let-alist window
               (let* ((reset (and .resetsAt (<= .resetsAt now)))
                      (utilization (if reset 0 (or .utilization 0))))
                 (concat
                  (or (alist-get name opencode--claude-usage-window-names)
                      (string-replace "_" " " (symbol-name name)))
                  " "
                  (opencode--usage-bar utilization)
                  " "
                  (format "%d%%" (round (* 100 utilization)))
                  (when .resetsAt
                    (format " (%s %s)"
                            (if reset "reset" "resets")
                            (opencode--format-claude-reset .resetsAt)))))))
           (alist-get 'unifiedWindows rate-limit))))
    (when windows
      (let ((line (concat "Claude usage: " (string-join windows " · "))))
        (if (and width (> (string-pixel-width line) width))
            (concat "Claude usage:\n" (string-join windows "\n"))
          line)))))

(defun opencode--session-control-update-usage ()
  "Show Claude usage at the end of the current session control buffer."
  (let* ((window (get-buffer-window (current-buffer)))
         (usage (and (consp opencode--claude-rate-limit)
                     (opencode--format-claude-usage
                      (cdr opencode--claude-rate-limit)
                      (and window (window-body-width window t))))))
    (unless (overlayp opencode--session-control-usage-overlay)
      (setq opencode--session-control-usage-overlay
            (make-overlay (point-max) (point-max))))
    (move-overlay opencode--session-control-usage-overlay
                  (point-max) (point-max))
    (overlay-put opencode--session-control-usage-overlay 'after-string
                 (when usage (concat "\n" usage "\n")))))

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
          (concat (alist-get 'tree-prefix object)
                  (when icon (concat icon " "))
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

(defun opencode--session-control-tree (sessions)
  "Return SESSIONS with subagents directly under the session that spawned them.
Top level sessions are ordered by last update, newest first, and the
subagents of a session by creation, oldest first.  Sessions below the
top level get a `tree-prefix' entry with the characters connecting them
to their parent.  A session whose parent isn't in SESSIONS is top level."
  (let ((ids (make-hash-table :test #'equal))
        (children (make-hash-table :test #'equal))
        roots tree)
    (dolist (session sessions)
      (puthash (alist-get 'id session) t ids))
    (dolist (session sessions)
      (let ((parent (alist-get 'parentID session)))
        (if (gethash parent ids)
            (push session (gethash parent children))
          (push session roots))))
    (cl-labels ((time-getter (type)
                  (lambda (session)
                    (or (map-nested-elt session `(time ,type)) 0)))
                (walk (session prefix connector)
                  (push (if connector
                            (cons (cons 'tree-prefix (concat prefix connector))
                                  session)
                          session)
                        tree)
                  (let* ((subagents (seq-sort-by
                                     (time-getter 'created) #'<
                                     (gethash (alist-get 'id session) children)))
                         (last-subagent (car (last subagents)))
                         (indent (concat prefix
                                         (pcase connector
                                           ("├─ " "│  ")
                                           ("└─ " "   ")))))
                    (dolist (subagent subagents)
                      (walk subagent indent
                            (if (eq subagent last-subagent) "└─ " "├─ "))))))
      (dolist (root (seq-sort-by (time-getter 'updated) #'> roots))
        (walk root "" nil)))
    (nreverse tree)))

(defun opencode--session-control-insert-table (sessions columns)
  "Insert a vtable of SESSIONS with COLUMNS, a list of column keys.
If `opencode-session-control-verbose' is non-nil show them as a tree."
  (let* ((columns (mapcar (lambda (key)
                            (or (alist-get key opencode--session-control-columns)
                                (error "Unknown session control column: %s" key)))
                          columns))
         (getters (vconcat (mapcar #'cadr columns)))
         (table (make-vtable
                 :columns (mapcar #'car columns)
                 :objects (if opencode-session-control-verbose
                              (opencode--session-control-tree sessions)
                            sessions)
                 :getter (lambda (object column _vtable)
                           (funcall (aref getters column) object))
                 :separator-width 3
                 :keymap opencode-session-control-mode-map
                 :insert nil)))
    ;; The primary column's sort would undo the tree order
    (when opencode-session-control-verbose
      (setf (vtable-sort-by table) nil))
    (vtable-insert table)))

(defun opencode--session-control-display (sessions)
  "Display SESSIONS in the current session control buffer.
Optional columns from `opencode-session-control-table-columns' are
only shown if the table fits in the window."
  (setq opencode--session-control-sessions sessions)
  (clrhash opencode--session-control-branch-cache)
  (when (seq-some #'opencode--claude-subscription-session-p sessions)
    (opencode--fetch-claude-plan-tier))
  (opencode--fetch-claude-rate-limit sessions)
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
    (opencode--session-control-update-usage)
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
    (opencode-api--fetch-all-sessions opencode-session-control-project-id
                                      opencode-session-control-verbose
                                      #'opencode--session-control-display)))

(defun opencode--map-session-control-buffers (fn)
  "Call FN with no arguments in each live session control buffer."
  (setq opencode--session-control-buffers
        (seq-filter #'buffer-live-p opencode--session-control-buffers))
  (dolist (buffer opencode--session-control-buffers)
    (with-current-buffer buffer
      (funcall fn))))

(defun opencode--redisplay-session-control-buffers ()
  "Refresh all live session control buffers."
  (opencode--map-session-control-buffers #'opencode-sessions-redisplay))

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
