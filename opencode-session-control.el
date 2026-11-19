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

(defvar-local opencode--session-control-sessions nil
  "Sessions last displayed in the session control buffer.")

(defun opencode--session-control-selected-sessions ()
  "Return the sessions on the lines of the active region, or the one at point.
A session is left out if one of its ancestors is also selected, as acting
on a session acts on its subagents too."
  (let ((sessions
         (if (use-region-p)
             (save-excursion
               (let ((end (region-end))
                     sessions)
                 (goto-char (region-beginning))
                 (while (progn
                          (when-let* ((object (vtable-current-object)))
                            (push object sessions))
                          (and (zerop (forward-line 1))
                               (< (point) end))))
                 (delete-dups (nreverse sessions))))
           (when-let* ((object (vtable-current-object)))
             (list object)))))
    (unless sessions
      (user-error "No session at point"))
    (let ((selected (make-hash-table :test #'equal))
          (parents (make-hash-table :test #'equal)))
      (dolist (session (append opencode--session-control-sessions sessions))
        (puthash (alist-get 'id session) (alist-get 'parentID session) parents))
      (dolist (session sessions)
        (puthash (alist-get 'id session) t selected))
      (seq-remove (lambda (session)
                    (let ((parent (alist-get 'parentID session))
                          (seen nil)
                          found)
                      (while (and parent (not found) (not (member parent seen)))
                        (push parent seen)
                        (setq found (gethash parent selected)
                              parent (gethash parent parents)))
                      found))
                  sessions))))

(defun opencode--session-control-command (fun &optional verb confirm)
  "Return a command that calls FUN with the session at point.
If VERB, a past tense verb such as \"Archived\", FUN is instead called
with each session in the region when it's active, and VERB is used to
report how many there were.  If CONFIRM, a present tense verb, ask
before acting on more than one session."
  (lambda ()
    (:documentation
     (format (if verb
                 "Call `%s' on the sessions in the region, or the one at point."
               "Call `%s' on the session at point.")
             fun))
    (interactive)
    (if (not verb)
        (funcall fun (or (vtable-current-object)
                         (user-error "No session at point")))
      (let* ((sessions (opencode--session-control-selected-sessions))
             (count (length sessions)))
        (when (and confirm (> count 1)
                   (not (y-or-n-p (format "%s %d sessions? " confirm count))))
          (user-error "Cancelled"))
        (deactivate-mark)
        (mapc fun sessions)
        (when (> count 1)
          (message "%s %d sessions" verb count))))))

(defvar opencode-session-control-mode-map
  (define-keymap
    ;; needed here instead of vtable :actions, as emojify-mode keymap was taking
    ;; precedence over vtable's keymap
    "RET" (opencode--session-control-command #'opencode-open-session)
    "o" (opencode--session-control-command #'opencode-open-session)
    "R" (opencode--session-control-command #'opencode-rename-session)
    "x" (opencode--session-control-command #'opencode-kill-session
                                           "Deleted" "Delete")
    "a" (opencode--session-control-command #'opencode-archive-session
                                           "Archived")
    "u" (opencode--session-control-command #'opencode-unarchive-session
                                           "Unarchived")
    "A" 'opencode-session-control-toggle-archived
    "r" 'opencode-sessions-redisplay
    "g" nil
    "SPC" nil
    "n" 'opencode-new-session
    "M" 'opencode-toggle-mcp
    "v" 'opencode-session-control-toggle-verbose
    "C-c C-x n" 'opencode-next-connection
    "C-c C-x p" 'opencode-prev-connection
    "C-c C-x c" 'opencode-list-connections))

(with-eval-after-load 'evil
  (declare-function evil-define-key* "evil-core")
  (declare-function evil-make-overriding-map "evil-core")
  (evil-make-overriding-map opencode-session-control-mode-map 'normal)
  (evil-define-key* 'normal opencode-session-control-mode-map
    "gv" 'opencode-session-control-toggle-verbose)
  (evil-define-key* 'visual opencode-session-control-mode-map
    "x" (opencode--session-control-command #'opencode-kill-session
                                           "Deleted" "Delete")
    "a" (opencode--session-control-command #'opencode-archive-session
                                           "Archived")
    "u" (opencode--session-control-command #'opencode-unarchive-session
                                           "Unarchived")))

(defvar-local opencode-session-control-verbose nil
  "Toggle whether to display subagents in session control buffer.")

(defvar-local opencode-session-control-show-archived nil
  "Toggle whether to display archived sessions in session control buffer.")

(defvar-local opencode-session-control-project-id nil
  "Id of the project whose sessions the session control buffer displays.")

(defvar-local opencode--session-control-width nil
  "Window body width in pixels the session control table was rendered for.")

(define-derived-mode opencode-session-control-mode special-mode "Sessions"
  "Opencode session control panel mode."
  ;; Doom ignores buffers it doesn't consider real when choosing which buffer
  ;; to show after killing one, or for `evil-switch-to-windows-last-buffer'.
  (setq-local doom-real-buffer-p t)
  (setq-local mode-line-misc-info
              '((:eval (opencode--session-control-usage-item nil))
                (:eval (default-value 'mode-line-misc-info))
                (:eval (opencode--session-control-usage-item t))))
  (add-hook 'pre-redisplay-functions
            #'opencode--session-control-update-usage nil t)
  (add-hook 'window-size-change-functions
            #'opencode--session-control-window-size-change nil t))

(defun opencode-session-control-toggle-verbose ()
  "Toggle verbose mode in session control buffer."
  (interactive)
  (setq opencode-session-control-verbose
        (not opencode-session-control-verbose))
  (opencode-sessions-redisplay))

(defun opencode-session-control-toggle-archived ()
  "Toggle showing archived sessions in session control buffer."
  (interactive)
  (setq opencode-session-control-show-archived
        (not opencode-session-control-show-archived))
  (message "%s archived sessions"
           (if opencode-session-control-show-archived "Showing" "Hiding"))
  (opencode-sessions-redisplay))

(defun opencode--profile-session-loading (session)
  "Profile loading SESSION, the session at point, with the CPU profiler.
Kill its buffer if it has one, reopen it, and show the profiler report
once it is loaded, along with how many redisplays happened before and
after its messages were replayed."
  (interactive (list (or (vtable-current-object)
                         (user-error "No session at point")))
               opencode-session-control-mode)
  (let* ((buffer (opencode--session-buffer (alist-get 'id session)))
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
               (dolist (connection opencode-connections)
                 (let ((opencode--dispatching-connection connection))
                   (opencode--redraw-session-control-buffers)))))))))))

(defconst opencode--claude-usage-window-names
  '((five_hour . "5h") (seven_day . "week"))
  "Alist of names to show for Claude rate limit windows.
Windows not in it are shown with their name's underscores replaced by spaces.")

(defun opencode--update-claude-rate-limit (rate-limit &optional time)
  "Record RATE-LIMIT of the current connection, reported at TIME in seconds.
TIME is now if nil.  It is ignored if older than the one already recorded."
  (let ((time (or time (float-time)))
        (current (opencode--claude-rate-limit)))
    (when (and rate-limit
               (or (not current) (>= time (car current))))
      (setf (opencode-connection-claude-rate-limit (opencode--conn))
            (cons time rate-limit))
      ;; For %u and %U in the mode line of the server's sessions, and
      ;; the usage in the mode line of its session control buffers
      (force-mode-line-update t))))

(defun opencode--fetch-claude-rate-limit (sessions)
  "Record the latest rate limit info stored in the newest of SESSIONS, once.
It's done once per connection.  Only claude-agent sessions are considered."
  (when-let* ((connection (opencode-current-connection))
              ((eq (opencode-connection-claude-rate-limit connection) 'unfetched))
              (session (car (seq-sort-by
                             (lambda (session)
                               (or (map-nested-elt session '(time updated)) 0))
                             #'>
                             (seq-filter
                              (lambda (session)
                                (equal (map-nested-elt session '(model providerID))
                                       "claude-agent"))
                              sessions)))))
    (setf (opencode-connection-claude-rate-limit connection) nil)
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

(defconst opencode--claude-usage-levels '(all five-hour five-hour-no-reset)
  "Levels of detail of Claude usage in the mode line, most detailed first.
See `opencode--format-claude-usage'.")

(defun opencode--format-claude-usage (rate-limit &optional level face)
  "Return a line describing Claude usage from RATE-LIMIT, or nil if unknown.
LEVEL is `all' or nil to show every usage window, `five-hour' to show
only the 5h window and `five-hour-no-reset' to show it without when it
resets.  FACE is the face the line is shown in, for the background of
the usage bars."
  (let* ((five-hour (memq level '(five-hour five-hour-no-reset)))
         (windows
          (mapcan
           (pcase-lambda (`(,name . ,window))
             (when (or (not five-hour) (eq name 'five_hour))
               (let ((utilization (opencode--claude-window-utilization window))
                     (reset (and (not (eq level 'five-hour-no-reset))
                                 (opencode--claude-window-reset window))))
                 (list
                  (concat
                   (or (alist-get name opencode--claude-usage-window-names)
                       (string-replace "_" " " (symbol-name name)))
                   " "
                   (opencode--usage-bar utilization face)
                   " "
                   (format "%d%%" (round (* 100 utilization)))
                   (when reset
                     (format " (resets %s)" reset)))))))
           (alist-get 'unifiedWindows rate-limit))))
    (when windows
      (string-join windows " · "))))

(defvar opencode--session-control-usage-measuring nil
  "Non-nil while measuring the mode line, so it leaves out Claude usage.")

(defvar-local opencode--session-control-usage-widths nil
  "Cache of the pixel widths of each level of Claude usage.
A cons of what they depend on and an alist of the widths by level.")

(defun opencode--session-control-usage-widths ()
  "Return an alist of the pixel width of each level of Claude usage.
Levels without any usage aren't included."
  (when-let* ((rate-limit (opencode--claude-rate-limit)))
    ;; Reset times and utilization change at most once a minute
    (let ((key (list rate-limit (format-time-string "%F %R")
                     (frame-char-width))))
      (unless (equal key (car opencode--session-control-usage-widths))
        (setq opencode--session-control-usage-widths
              (cons key
                    (mapcan
                     (lambda (level)
                       (when-let* ((usage (opencode--format-claude-usage
                                           (cdr rate-limit) level)))
                         (list (cons level (string-pixel-width
                                            (concat usage " "))))))
                     opencode--claude-usage-levels))))
      (cdr opencode--session-control-usage-widths))))

(defun opencode--session-control-usage-level (available)
  "Return the most detailed level of Claude usage AVAILABLE pixels fit.
It's nil if none fit.  See `opencode--claude-usage-levels'."
  (car (seq-find (lambda (level) (<= (cdr level) available))
                 (opencode--session-control-usage-widths))))

(defun opencode--mode-line-content-width (line)
  "Return the pixel width of LINE, a formatted mode line, and if it aligns.
It's a cons of the width without the spaces that align parts of it, as
`mode-line-format-right-align' and doom-modeline use, and whether it had
any of them."
  (let ((start 0)
        (parts nil)
        (aligned nil))
    (while (< start (length line))
      (let ((end (next-single-property-change start 'display line (length line)))
            (display (get-text-property start 'display line)))
        (if (and (eq (car-safe display) 'space)
                 (plist-member (cdr display) :align-to))
            (setq aligned t)
          (push (substring line start end) parts))
        (setq start end)))
    (cons (string-pixel-width (apply #'concat (nreverse parts))) aligned)))

(defun opencode--session-control-update-usage (window)
  "Choose how much Claude usage fits in the mode line of WINDOW.
It's the most detailed of `opencode--claude-usage-levels' that fits
with the rest of the mode line, or none.  For `pre-redisplay-functions'."
  (when (and (windowp window) (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (pcase-let* ((`(,width . ,aligned)
                  (opencode--mode-line-content-width
                   (let ((opencode--session-control-usage-measuring t))
                     (format-mode-line mode-line-format 'mode-line window))))
                 (level (opencode--session-control-usage-level
                         (- (window-pixel-width window)
                            (window-right-divider-width window)
                            width
                            ;; between the left and right parts
                            (* 2 (frame-char-width (window-frame window)))))))
      (unless (and (eq level (window-parameter window 'opencode-usage-level))
                   (eq aligned (window-parameter window 'opencode-usage-aligned)))
        (set-window-parameter window 'opencode-usage-level level)
        (set-window-parameter window 'opencode-usage-aligned aligned)
        (force-mode-line-update)))))

(defun opencode--session-control-usage-item (alone)
  "Return the Claude usage in the mode line of the session control buffer.
It's both first and last in its `mode-line-misc-info'.  When the mode
line has a right-aligned part, the first one is shown, so it starts the
right-aligned part.  Otherwise the last one, with ALONE non-nil, is
shown, right-aligned by itself."
  (when-let* (((not opencode--session-control-usage-measuring))
              (level (window-parameter nil 'opencode-usage-level))
              ((eq alone (not (window-parameter nil 'opencode-usage-aligned))))
              (rate-limit (opencode--claude-rate-limit))
              (usage (opencode--format-claude-usage
                      (cdr rate-limit) level
                      (if (mode-line-window-selected-p)
                          'mode-line-active
                        'mode-line-inactive))))
    (let ((usage (string-replace "%" "%%" usage)))
      (if alone
          (list (opencode--right-align-space (list usage) 'mode-line) usage)
        (concat usage " ")))))

(defun opencode--session-control-cost (session)
  "Return the text of the cost column for SESSION.
Claude subscription sessions show what their tokens would cost at API
prices, as ~$N, and the share of the 5h budget, after the server's cost
if it charged for other models."
  (let* ((cost (or (alist-get 'cost session) 0))
         (real (and (not (zerop cost)) (format "$%.2f" cost)))
         (estimate
          (when-let* (((opencode-claude-subscription-p session))
                      (api (opencode--claude-api-cost session)))
            (if-let* ((budget (cdr (assoc opencode--claude-plan-tier
                                          opencode-claude-five-hour-budgets))))
                (format "~$%.2f %.1f%%" api (/ (* 100 api) budget))
              (format "~$%.2f" api)))))
    (cond ((and real estimate) (concat real " + " estimate))
          ((or real estimate))
          ((opencode-claude-subscription-p session) "-")
          (t "$0.00"))))

(defconst opencode--session-control-columns
  `((title
     (:name "Title" :max-width "50%")
     ,(lambda (object)
        (let ((icon (opencode--session-status-icon object)))
          (concat (alist-get 'tree-prefix object)
                  (unless (opencode--json-falsy
                           (map-nested-elt object '(time archived)))
                    "🗄 ")
                  (when icon (concat icon " "))
                  (opencode--session-title object)))))
    (branch
     (:name "Branch" :min-width 6)
     ,(lambda (object)
        (let-alist object
          ;; It's not shown for remote servers, as it's run for each session
          ;; each time the table is drawn, which can be from a process filter
          (if (and .location.directory
                   (not (opencode--tramp-prefix))
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
     ,#'opencode--session-control-cost
     ;; Only what the server charged, not subscription estimates
     ,(lambda (object) (or (alist-get 'cost object) 0)))
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
                  (opencode--format-tokens .tokens.reasoning))))
     ,(lambda (object)
        (let-alist object
          (+ (or .tokens.input 0)
             (or .tokens.output 0)
             (or .tokens.reasoning 0)
             (or .tokens.cache.read 0)
             (or .tokens.cache.write 0)))))
    (cache
     (:name "Cache" :width 6 :align right
      :formatter ,(lambda (ratio)
                    (if (< ratio 0)
                        "-"
                      (format "%d%%" (round (* 100 ratio))))))
     ,(lambda (object)
        (let-alist object
          (let ((total (+ (or .tokens.input 0)
                          (or .tokens.cache.read 0)
                          (or .tokens.cache.write 0))))
            ;; A number rather than "-" keeps the column numerical
            (if (zerop total)
                -1
              (/ (float (or .tokens.cache.read 0)) total)))))))
  "Alist of (KEY VTABLE-COLUMN-SPEC GETTER [SORT-KEY]) for the session control table.
GETTER is called with the session object.  If SORT-KEY is given, the
column is sorted by the number it returns when called with the session
object, instead of by the value GETTER returns.")

(defvar-local opencode--session-control-sort-keys nil
  "Vector of the SORT-KEY of each column of the session control table, or nil.")

(defun opencode--session-control-tree (sessions)
  "Return SESSIONS with subagents directly under the session that spawned them.
Top level sessions are ordered by last update, newest first, and the
subagents of a session by creation, oldest first.  Sessions below the
top level get a `tree-prefix' entry with the characters connecting them
to their parent.  Every session gets a `tree-root' entry with the id of
its top level session.  A session whose parent isn't in SESSIONS is top
level."
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
                (walk (session root-id prefix connector)
                  (push (append (when connector
                                  (list (cons 'tree-prefix
                                              (concat prefix connector))))
                                (list (cons 'tree-root root-id))
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
                      (walk subagent root-id indent
                            (if (eq subagent last-subagent) "└─ " "├─ "))))))
      (dolist (root (seq-sort-by (time-getter 'updated) #'> roots))
        (walk root (alist-get 'id root) "" nil)))
    (nreverse tree)))

(defun opencode--session-control-sort-tree (sort table cache)
  "Keep subagents under their parent after SORT sorts TABLE's CACHE lines.
Sessions without a parent keep the order SORT gave them, and each one's
subagents follow it in the order of `opencode--session-control-tree'.
Columns with a SORT-KEY in `opencode--session-control-columns' are
sorted by it.  Tables in other modes are left to SORT, as this is advice
on every table, and `vtable--sort' runs in the table's buffer."
  (if (not (derived-mode-p 'opencode-session-control-mode))
      (funcall sort table cache)
    ;; Like `vtable--sort', stably sort by each column in turn, so the last
    ;; one in `vtable-sort-by' takes precedence
    (let ((sort-by (vtable-sort-by table)))
      (unwind-protect
          (pcase-dolist ((and entry `(,index . ,direction)) sort-by)
            (if-let* ((key (and opencode--session-control-sort-keys
                                (aref opencode--session-control-sort-keys index))))
                (setcar cache (sort (car cache)
                                    :key (lambda (line) (funcall key (car line)))
                                    :reverse (eq direction 'descend)))
              (setf (vtable-sort-by table) (list entry))
              (funcall sort table cache)))
        (setf (vtable-sort-by table) sort-by)))
    (when (alist-get 'tree-root (car (vtable-objects table)))
      (let ((tree-order (make-hash-table :test #'eq))
            (root-order (make-hash-table :test #'equal)))
        (seq-do-indexed (lambda (object index)
                          (puthash object index tree-order))
                        (vtable-objects table))
        (seq-do-indexed (lambda (line index)
                          (let ((object (car line)))
                            (when (equal (alist-get 'id object)
                                         (alist-get 'tree-root object))
                              (puthash (alist-get 'id object) index root-order))))
                        (car cache))
        (setcar cache
                (sort (car cache)
                      :key (lambda (line)
                             (list (gethash (alist-get 'tree-root (car line))
                                            root-order)
                                   (gethash (car line) tree-order)))))))))

(advice-add 'vtable--sort :around #'opencode--session-control-sort-tree)

(defun opencode--session-control-insert-table (sessions columns)
  "Insert a vtable of SESSIONS with COLUMNS, a list of column keys.
If `opencode-session-control-verbose' is non-nil show them as a tree."
  (let* ((columns (mapcar (lambda (key)
                            (or (alist-get key opencode--session-control-columns)
                                (error "Unknown session control column: %s" key)))
                          columns))
         (getters (vconcat (mapcar #'cadr columns))))
    (setq opencode--session-control-sort-keys (vconcat (mapcar #'caddr columns)))
    (make-vtable
     :columns (mapcar #'car columns)
     :objects (if opencode-session-control-verbose
                  (opencode--session-control-tree sessions)
                sessions)
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
  (when (seq-some #'opencode-claude-subscription-p sessions)
    (opencode--fetch-claude-plan-tier))
  (opencode--fetch-claude-rate-limit sessions)
  (let ((inhibit-read-only t)
        (window (get-buffer-window (current-buffer) t))
        (required (plist-get opencode-session-control-table-columns :required))
        (optional (plist-get opencode-session-control-table-columns :optional)))
    (opencode--with-preserved-lines
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
            (and window (window-body-width window t))))))

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
                                      #'opencode--session-control-display
                                      opencode-session-control-show-archived)))

(defun opencode--map-session-control-buffers (fn)
  "Call FN with no arguments in the current connection's session control buffers.
Only live ones, with none if there's no current connection."
  (when-let* ((connection (opencode-current-connection)))
    (dolist (buffer (setf (opencode-connection-session-control-buffers connection)
                          (seq-filter
                           #'buffer-live-p
                           (opencode-connection-session-control-buffers
                            connection))))
      (with-current-buffer buffer
        (funcall fn)))))

(defun opencode--redisplay-session-control-buffers ()
  "Refresh the live session control buffers of the current connection's server."
  (opencode--map-session-control-buffers #'opencode-sessions-redisplay))

(defun opencode--redraw-session-control-buffers ()
  "Redraw the current connection's session control buffers from their cached sessions.
Unlike `opencode--redisplay-session-control-buffers' it doesn't refetch
them, for when only local state shown in the table changed."
  (opencode--map-session-control-buffers
   (lambda ()
     (when opencode--session-control-sessions
       (opencode--session-control-display opencode--session-control-sessions)))))

(defun opencode--alert-session (session)
  "Add SESSION of the current connection's server to `opencode-alerted-sessions'.
Redraw the session control buffers to show its ✦ marker."
  (push (cons (cons 'opencode-connection (opencode-current-connection)) session)
        opencode-alerted-sessions)
  (opencode--redraw-session-control-buffers))

(defun opencode-open-project (directory)
  "Open the session control buffer for the project of DIRECTORY.
The server decides the project: the git repository containing DIRECTORY, or
DIRECTORY itself outside git.  Worktrees of a project share one buffer, and
new sessions started from it go in the root of the worktree it was last
opened for.  It's on the current connection's server."
  (let* ((directory (file-name-as-directory directory))
         (default-directory directory)
         (connection (opencode--conn)))
    (opencode-api--cached-location
     (lambda (location)
       (let-alist location
         (with-current-buffer
             (get-buffer-create
              (format "*OpenCode Sessions%s: %s*"
                      (opencode--server-tag connection)
                      (if (equal .project.id "global")
                          "global"
                        (file-name-nondirectory
                         (directory-file-name .project.canonical)))))
           (unless (derived-mode-p 'opencode-session-control-mode)
             (opencode-session-control-mode)
             (setq opencode-buffer-connection connection)
             (push (current-buffer)
                   (opencode-connection-session-control-buffers connection)))
           (setq default-directory (file-name-as-directory
                                    (opencode--from-server-path .project.directory))
                 opencode--project-root default-directory
                 opencode-session-control-project-id .project.id)
           (opencode-sessions-redisplay)
           (opencode--switch-to-buffer (current-buffer))))))))

(defun opencode--read-project ()
  "Read a project of the current connection's server and open it."
  (opencode-api-projects projects
    (opencode-open-project
     (opencode--from-server-path
      (opencode--annotated-completion
       "Project: "
       (cl-loop for project in projects
                for canonical = (alist-get 'canonical project)
                when canonical
                collect (list (string-remove-prefix (expand-file-name "~/")
                                                    canonical)
                              canonical
                              (opencode--format-time-ago
                               (opencode--time-ago project 'active))
                              (opencode--time-ago project 'active))))))))

(provide 'opencode-session-control)
;;; opencode-session-control.el ends here
