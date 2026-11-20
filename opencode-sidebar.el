;;; opencode-sidebar.el --- Sidebar of session information  -*- lexical-binding: t; -*-

;; Keywords: tools

;;; Commentary:

;; A side window showing information about a session, like the TUI's
;; sidebar: the agent, model and timing of the step at point, its context
;; usage and cost, todo list, the files it changed, the MCP servers and
;; its directory and branch.  There's one per frame, which follows the
;; session buffer last selected in it.
;;
;; partially inspired by https://github.com/michaelmjhhhh/pi-atelier

;;; Code:

(require 'cl-lib)
(require 'magit-git)
(require 'magit-section)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-session-control)
(require 'opencode-sessions)

(defgroup opencode-sidebar nil
  "Sidebar of opencode session information."
  :group 'opencode)

(defcustom opencode-sidebar-width 42
  "Width of the sidebar window in columns."
  :type 'natnum
  :group 'opencode-sidebar)

(defcustom opencode-sidebar-side 'right
  "Side of the frame the sidebar is shown on."
  :type '(choice (const left) (const right))
  :group 'opencode-sidebar)

(defcustom opencode-sidebar-auto nil
  "Whether to show the sidebar on selecting a session buffer.
Nil means only `opencode-sidebar-toggle' shows it, t means always, and a
number means when the frame is at least that many columns wide.  It's not
shown in a frame where it was hidden with `opencode-sidebar-toggle'."
  :type '(choice (const :tag "Only with `opencode-sidebar-toggle'" nil)
          (const :tag "Always" t)
          (natnum :tag "When the frame is at least this wide"))
  :group 'opencode-sidebar)

(defcustom opencode-sidebar-speed-start 'created
  "When a step's tokens per second count from.
`created' counts from the request, so the rate includes the time to first token.
`first-token' counts from the first streamed token, as the TUI does, but
providers may buffer output, like the input of a tool call, or anthropic
buffering reasoning before outputting its reasoning summary, so most of it can
arrive in a burst just after the first token, and the rate is then far too
high."
  :type '(choice (const :tag "The request" created)
          (const :tag "The first token" first-token))
  :group 'opencode-sidebar)

(defface opencode-sidebar-title
  '((t :inherit bold))
  "Face of the session title in the sidebar."
  :group 'opencode-sidebar)

(defface opencode-sidebar-heading
  '((t :inherit magit-section-heading))
  "Face of section headings in the sidebar."
  :group 'opencode-sidebar)

(defvar-local opencode-sidebar--session nil
  "The session buffer the sidebar shows.")

(defvar-local opencode-sidebar--visibility nil
  "Alist of the sidebar's section types and whether they're hidden.
It's kept across redraws, and for every session the sidebar shows.")

(defvar-local opencode-sidebar--data nil
  "Plist of what a session buffer's sidebar shows, fetched from the server.
The keys are :diff, :mcps and :branch.  A missing key hasn't been fetched.")

(defvar opencode-sidebar--timer nil
  "Timer of the pending redraw of the sidebars.")

(defvar opencode-sidebar--tick-timer nil
  "Timer redrawing the sidebars showing a running turn, as its time counts up.")

(defvar-local opencode-sidebar--running nil
  "Non-nil if the sidebar shows a running turn.")

(defvar-local opencode-sidebar--step nil
  "The (TURN . MESSAGE-ID) at point in a session buffer, last time it was looked up.")

;;; Buffers and windows

(defun opencode-sidebar--buffer (&optional frame create)
  "Return the sidebar buffer of FRAME, or nil if it has none.
FRAME defaults to the selected frame.  If CREATE, create it if needed."
  (let ((buffer (frame-parameter frame 'opencode-sidebar)))
    (cond ((buffer-live-p buffer) buffer)
          (create
           (let ((buffer (generate-new-buffer "*opencode-sidebar*")))
             (with-current-buffer buffer
               (opencode-sidebar-mode))
             (set-frame-parameter frame 'opencode-sidebar buffer)
             buffer)))))

(defun opencode-sidebar--window (&optional frame)
  "Return the window showing the sidebar of FRAME, or nil."
  (when-let* ((buffer (opencode-sidebar--buffer frame)))
    (get-buffer-window buffer frame)))

(defun opencode-sidebar--sidebars ()
  "Return the sidebar buffers shown in a window."
  (cl-loop for frame in (frame-list)
           for buffer = (opencode-sidebar--buffer frame)
           when (and buffer (get-buffer-window buffer frame))
           collect buffer))

(defun opencode-sidebar--display (buffer)
  "Display sidebar BUFFER in a side window of the selected frame.
`display-buffer-alist' can override where it goes."
  (display-buffer
   buffer
   `(display-buffer-in-side-window
     (side . ,opencode-sidebar-side)
     (slot . 0)
     (window-width . ,opencode-sidebar-width)
     (preserve-size . (t . nil))
     (dedicated . t)
     (window-parameters (no-delete-other-windows . t)
                        (no-other-window . t)))))

(defun opencode-sidebar--session-p (buffer)
  "Return non-nil if BUFFER is a session buffer."
  (with-current-buffer buffer
    (derived-mode-p 'opencode-session-mode)))

(defun opencode-sidebar--visible-session (&optional frame)
  "Return the session buffer FRAME's sidebar should show, or nil if none.
That's the session of FRAME's selected window, else the one its sidebar
shows if it's still in a window of FRAME, else any session in a window
of FRAME.  FRAME defaults to the selected frame."
  (let* ((frame (or frame (selected-frame)))
         (selected (window-buffer (frame-selected-window frame)))
         (sidebar (opencode-sidebar--buffer frame))
         (shown (and sidebar (buffer-local-value 'opencode-sidebar--session sidebar))))
    (cond ((opencode-sidebar--session-p selected) selected)
          ((and (buffer-live-p shown) (get-buffer-window shown frame)) shown)
          (t (seq-some (lambda (window)
                         (let ((buffer (window-buffer window)))
                           (and (opencode-sidebar--session-p buffer) buffer)))
                       (window-list frame 'never))))))

(defun opencode-sidebar--show (session &optional frame)
  "Show FRAME's sidebar, for SESSION buffer.
FRAME defaults to the selected frame."
  (with-selected-frame (or frame (selected-frame))
    (let ((sidebar (opencode-sidebar--buffer nil t)))
      (opencode-sidebar--display sidebar)
      (opencode-sidebar--set-session sidebar session t))))

;;;###autoload
(defun opencode-sidebar-toggle ()
  "Show or hide the sidebar of the selected frame.
It's only shown while a session buffer is visible in the frame, and
shows the selected one, or else one in another window."
  (interactive)
  (if-let* ((window (opencode-sidebar--window)))
      (progn
        (set-frame-parameter nil 'opencode-sidebar-dismissed t)
        (set-frame-parameter nil 'opencode-sidebar-suspended nil)
        (delete-window window))
    (if-let* ((session (opencode-sidebar--visible-session)))
        (progn
          (set-frame-parameter nil 'opencode-sidebar-dismissed nil)
          (opencode-sidebar--show session))
      (message "No session visible in this frame"))))

(defun opencode-sidebar--auto-p ()
  "Return non-nil if the selected frame's sidebar should show on its own."
  (and opencode-sidebar-auto
       (not (frame-parameter nil 'opencode-sidebar-dismissed))
       (or (not (numberp opencode-sidebar-auto))
           (>= (frame-width) opencode-sidebar-auto))))

(defun opencode-sidebar--follow (&optional frame)
  "Make FRAME's sidebar show the session buffer visible in it.
See `opencode-sidebar--visible-session'.  With none visible, the sidebar
is hidden until one is.  For `window-selection-change-functions' and
`window-buffer-change-functions', which also run for frames that aren't
selected."
  (let* ((frame (if (framep frame) frame (selected-frame)))
         (session (opencode-sidebar--visible-session frame))
         (window (opencode-sidebar--window frame)))
    (cond ((not session)
           (when window
             (set-frame-parameter frame 'opencode-sidebar-suspended t)
             (delete-window window)))
          (window
           (opencode-sidebar--set-session (window-buffer window) session))
          ((or (frame-parameter frame 'opencode-sidebar-suspended)
               (and (eq frame (selected-frame))
                    (opencode-sidebar--auto-p)))
           (set-frame-parameter frame 'opencode-sidebar-suspended nil)
           (opencode-sidebar--show session frame)))))

(add-hook 'window-selection-change-functions #'opencode-sidebar--follow)
(add-hook 'window-buffer-change-functions #'opencode-sidebar--follow)

(defun opencode-sidebar--set-session (sidebar session &optional force)
  "Make SIDEBAR show SESSION buffer, redrawing it if it changed or FORCE."
  (with-current-buffer sidebar
    (when (or force (not (eq session opencode-sidebar--session)))
      (setq opencode-sidebar--session session)
      (when (buffer-live-p session)
        (setq default-directory (buffer-local-value 'default-directory session))
        (opencode-sidebar--fetch session))
      (opencode-sidebar--redraw))))

;;; Data

(defun opencode-sidebar--shown-p (session)
  "Return non-nil if a visible sidebar shows SESSION buffer."
  (seq-some (lambda (sidebar)
              (eq (buffer-local-value 'opencode-sidebar--session sidebar) session))
            (opencode-sidebar--sidebars)))

(defun opencode-sidebar--store (key value)
  "Set KEY of the current session buffer's sidebar data to VALUE, and redraw."
  (setq opencode-sidebar--data (plist-put opencode-sidebar--data key value))
  (opencode-sidebar--schedule-redraw))

(defun opencode-sidebar--diffstat (diff)
  "Return (FILES ADDITIONS DELETIONS) of DIFF, a list of FileDiff.Info."
  (list (length diff)
        (cl-loop for file in diff sum (or (alist-get 'additions file) 0))
        (cl-loop for file in diff sum (or (alist-get 'deletions file) 0))))

(defun opencode-sidebar--fetch-diff ()
  "Fetch the files the current buffer's session changed, over all its turns."
  (let ((id opencode-session-id)
        (fail (lambda (message)
                ;; like when its turns span a change of location
                (opencode--log-event "WARNING SIDEBAR" (format "Session diff failed: %s" message))
                (opencode-sidebar--store :diff nil))))
    (opencode-api-first-user-message (id)
        first
      :else fail
      (opencode-api-last-user-message (id)
          last
        :else fail
        (if (not (and first last))
            (opencode-sidebar--store :diff nil)
          (opencode-api-session-range-diff (id (alist-get 'id (car first))
                                               (alist-get 'id (car last)))
              diff
            :else fail
            (opencode-sidebar--store :diff (opencode-sidebar--diffstat diff))))))))

(defun opencode-sidebar--fetch-session ()
  "Fetch the current buffer's session, for its total tokens and cost."
  (opencode-api-session (opencode-session-id)
      session
    :else (lambda (message)
            (opencode--log-event "WARNING SIDEBAR" (format "Session fetch failed: %s" message))
            (opencode-sidebar--store :session nil))
    (opencode-sidebar--store :session session)))

(defun opencode-sidebar--fetch-mcps ()
  "Fetch the MCP servers of the current session buffer's directory."
  (opencode-api-mcps mcps
    :else (lambda (message)
            (opencode--log-event "WARNING SIDEBAR" (format "MCP list failed: %s" message))
            (opencode-sidebar--store :mcps nil))
    (opencode-sidebar--store :mcps mcps)))

(defun opencode-sidebar--fetch-branch ()
  "Find the git branch of the current session buffer's directory.
Not over Tramp, as it's slow and can be run from a process filter."
  (opencode-sidebar--store
   :branch (unless (file-remote-p default-directory)
             (ignore-errors (magit-get-current-branch)))))

(defconst opencode-sidebar--fetchers
  '((:session . opencode-sidebar--fetch-session)
    (:diff . opencode-sidebar--fetch-diff)
    (:mcps . opencode-sidebar--fetch-mcps)
    (:branch . opencode-sidebar--fetch-branch))
  "Alist of the keys of the sidebar's data and the functions fetching them.
They're called in the session buffer, and store what they fetch with
`opencode-sidebar--store'.")

(defun opencode-sidebar--fetch (session &optional refetch)
  "Fetch what the sidebar shows for SESSION buffer that it doesn't have yet.
Keys in REFETCH are fetched even if it has them, keeping their old values
until the new ones arrive."
  (with-current-buffer session
    (when opencode-session-id
      ;; for the share of the 5h budget in the cost
      (when (opencode-claude-subscription-p)
        (opencode--fetch-claude-plan-tier))
      (pcase-dolist (`(,key . ,fetch) opencode-sidebar--fetchers)
        (when (or (memq key refetch)
                  (not (plist-member opencode-sidebar--data key)))
          ;; so it's fetched once while the request is pending
          (unless (plist-member opencode-sidebar--data key)
            (setq opencode-sidebar--data (plist-put opencode-sidebar--data key nil)))
          (funcall fetch))))))

(defun opencode-sidebar--invalidate (session &rest keys)
  "Refetch KEYS of SESSION buffer's sidebar data if it's shown, else forget them.
With no KEYS, all of it.  While it's refetched the old values are shown,
so sections don't disappear until the new ones arrive."
  (let ((keys (or keys (mapcar #'car opencode-sidebar--fetchers))))
    (if (opencode-sidebar--shown-p session)
        ;; storing what's fetched redraws it
        (opencode-sidebar--fetch session keys)
      (with-current-buffer session
        (setq opencode-sidebar--data
              (map-into (map-remove (lambda (key _) (memq key keys))
                                    opencode-sidebar--data)
                        'plist))))))

(defun opencode-sidebar-session-changed (&rest keys)
  "Update the sidebar for a change to the current session buffer.
KEYS of its data that the change made stale are refetched."
  (cond (keys (apply #'opencode-sidebar--invalidate (current-buffer) keys))
        ((opencode-sidebar--shown-p (current-buffer))
         (opencode-sidebar--schedule-redraw))))

(defun opencode-sidebar-mcps-changed ()
  "Refetch the MCP servers of every session buffer's sidebar."
  (dolist (buffer (buffer-list))
    (when (and (buffer-local-value 'opencode-session-id buffer)
               (plist-member (buffer-local-value 'opencode-sidebar--data buffer) :mcps))
      (opencode-sidebar--invalidate buffer :mcps))))

(defun opencode-sidebar--session-loaded (_session-id)
  "Refetch the current session buffer's sidebar data, as it's been (re)loaded.
For `opencode-session-loaded-functions'.  It's forgotten first, as it
may be another session's."
  (setq opencode-sidebar--data nil)
  (opencode-sidebar--invalidate (current-buffer)))

(add-hook 'opencode-session-loaded-functions #'opencode-sidebar--session-loaded)

(defun opencode-sidebar--point-moved ()
  "Redraw the sidebar if point moved onto another step of the session.
For a session buffer's `post-command-hook'."
  (let ((step (opencode--message-at (point))))
    (unless (and (eq (car step) (car opencode-sidebar--step))
                 (equal (cdr step) (cdr opencode-sidebar--step)))
      (setq opencode-sidebar--step step)
      (opencode-sidebar-session-changed))))

(defun opencode-sidebar--track-point ()
  "Make the sidebar of the current session buffer follow its point."
  (add-hook 'post-command-hook #'opencode-sidebar--point-moved nil t))

(add-hook 'opencode-session-mode-hook #'opencode-sidebar--track-point)

;; for the share of the 5h budget in the cost
(add-hook 'opencode-claude-plan-tier-hook #'opencode-sidebar--schedule-redraw)

;;; Drawing

(defun opencode-sidebar--schedule-redraw ()
  "Redraw the visible sidebars soon, once for a burst of changes."
  (unless (timerp opencode-sidebar--timer)
    (setq opencode-sidebar--timer
          (run-with-timer 0.2 nil #'opencode-sidebar--redraw-all))))

(defun opencode-sidebar--redraw-all ()
  "Redraw the visible sidebars."
  (setq opencode-sidebar--timer nil)
  (dolist (sidebar (opencode-sidebar--sidebars))
    (with-current-buffer sidebar
      (opencode-sidebar--redraw))))

(defun opencode-sidebar--remember-visibility ()
  "Record whether each of the sidebar's sections is hidden."
  (when magit-root-section
    (dolist (section (oref magit-root-section children))
      (setf (alist-get (oref section type) opencode-sidebar--visibility)
            (if (oref section hidden) 'hide 'show)))))

(defun opencode-sidebar--section-visibility (section)
  "Return the recorded visibility of SECTION.
For `magit-section-set-visibility-hook'."
  (alist-get (oref section type) opencode-sidebar--visibility))

(defun opencode-sidebar--heading (heading &optional summary)
  "Insert HEADING of a section, followed by a SUMMARY in the `shadow' face."
  (magit-insert-heading
    (concat (propertize heading 'font-lock-face 'opencode-sidebar-heading)
            (when summary
              (concat " " (propertize summary 'font-lock-face 'shadow))))))

(defun opencode-sidebar--item (marker text &optional face)
  "Insert an item line of MARKER and TEXT, with TEXT in FACE.
Lines it wraps onto are indented to the TEXT."
  (let ((prefix (make-string (+ 3 (string-width marker)) ?\s)))
    (insert (propertize (concat " " marker " " (propertize text 'font-lock-face face) "\n")
                        'wrap-prefix prefix))))

(defun opencode-sidebar--step ()
  "Return a plist of the step at point in the current session buffer.
It has the :turn and the plist about its assistant :message, and the
:agent, Model.Ref :model and its Model.Info :model-info, which are those
selected in the session when it has no step.  :status is the session's."
  (pcase-let* ((`(,turn . ,id) (opencode--message-at (point)))
               (message (and id (opencode--message-info id)))
               (agent (plist-get message :agent))
               (model (if agent
                          (plist-get message :model)
                        (alist-get 'model opencode-session-agent))))
    (list :turn turn
          :message message
          :agent (if agent
                     (or (seq-find (lambda (candidate)
                                     (equal agent (alist-get 'id candidate)))
                                   opencode-session-agents)
                         `((id . ,agent)))
                   opencode-session-agent)
          :model model
          :model-info (and model (opencode--find-model model))
          :status opencode-session-status)))

(defun opencode-sidebar--insert-agent (step)
  "Insert the agent section of STEP, from `opencode-sidebar--step'."
  (when-let* ((agent (plist-get step :agent)))
    (magit-insert-section (opencode-sidebar-agent)
      (opencode-sidebar--heading "Agent")
      (let ((status (plist-get step :status)))
        (insert " " (opencode--agent-label agent)
                (if status
                    (concat " · " (opencode--status-icon status) " " status)
                  "")
                "\n"))
      (when-let* ((model (plist-get step :model)))
        (let-alist model
          (when .providerID
            (insert " " .providerID "\n"))
          (insert " " (or (alist-get 'name (plist-get step :model-info)) .id "-")
                  (if (and .variant (not (equal .variant "default")))
                      (concat " " .variant)
                    "")
                  "\n")))
      (insert "\n"))))

(defun opencode-sidebar--duration (ms &optional whole)
  "Return the duration MS in ms, like 2.5s, 42s or 3m 5s.
With WHOLE, round to the nearest second, like 3s."
  (let ((seconds (/ (max 0 ms) 1000.0)))
    (when whole
      (setq seconds (round seconds)))
    (cond ((and (not whole) (< seconds 10)) (format "%.1fs" seconds))
          ((< seconds 60) (format "%ds" seconds))
          ((< seconds 3600) (format "%dm %ds" (/ seconds 60) (mod seconds 60)))
          (t (format "%dh %dm" (/ seconds 3600) (mod (/ seconds 60) 60))))))

(defun opencode-sidebar--speed (message)
  "Return the time to first token and tokens per second of MESSAGE, or nil.
MESSAGE is a plist about an assistant message, from `opencode--message-info'.
Tokens per second are of the output and reasoning tokens, until the
response was streamed, from when `opencode-sidebar-speed-start' says."
  (cl-destructuring-bind (&key created first-token streamed completed tokens
                               &allow-other-keys)
      message
    (let* ((start (if (eq opencode-sidebar-speed-start 'first-token)
                      (or first-token created)
                    created))
           (end (or streamed completed))
           (generated (and tokens (+ (or (alist-get 'output tokens) 0)
                                     (or (alist-get 'reasoning tokens) 0))))
           (seconds (and start end (/ (- end start) 1000.0)))
           (rate (and generated seconds (plusp generated) (plusp seconds)
                      (/ generated seconds)))
           (parts (delq nil (list (and created first-token
                                       (concat "TTFT " (opencode-sidebar--duration
                                                        (- first-token created))))
                                  (and rate (format "%.0f tok/s" rate))))))
      (when parts
        (string-join parts " · ")))))

(defun opencode-sidebar--insert-activity (step)
  "Insert the activity section of STEP, from `opencode-sidebar--step'.
A running turn's time counts up, by redrawing the sidebar every second."
  (when-let* ((turn (plist-get step :turn)))
    (magit-insert-section (opencode-sidebar-activity)
      (opencode-sidebar--heading "Activity")
      (let ((ended (opencode--turn-ended turn)))
        (setq opencode-sidebar--running (not ended))
        (insert (format " turn %d · %s\n"
                        (opencode--turn-number turn)
                        (opencode-sidebar--duration
                         (- (or ended (opencode--now-ms)) (opencode--turn-created turn))
                         (not ended)))))
      (when-let* ((speed (opencode-sidebar--speed (plist-get step :message))))
        (insert " " speed "\n"))
      (insert "\n"))))

(defun opencode-sidebar--insert-context (tokens model)
  "Insert the context section for TOKENS, with MODEL's limit."
  (when (plusp tokens)
    (magit-insert-section (opencode-sidebar-context)
      (opencode-sidebar--heading "Context")
      (insert (format " %s tokens\n" (opencode--format-tokens tokens)))
      (let ((limit (map-nested-elt model '(limit context))))
        (when (and (numberp limit) (plusp limit))
          (insert (format " %.0f%% used\n" (* 100 (/ (float tokens) limit))))))
      (insert "\n"))))

(defun opencode-sidebar--insert-usage (session rate-limit)
  "Insert the usage section for SESSION, a Session.Info.
RATE-LIMIT is the Claude rate limit alist, shown for claude-agent sessions."
  (when session
    (magit-insert-section (opencode-sidebar-usage)
      (opencode-sidebar--heading "Usage")
      (insert " " (opencode--format-session-tokens session) "\n")
      (when-let* ((rate (opencode--cache-rate session)))
        (insert " " (opencode--format-cache-rate rate) " cache\n"))
      (insert " " (opencode--format-session-cost session) "\n")
      (when (opencode-claude-subscription-p session)
        (insert "\n")
        (let* ((windows (alist-get 'unifiedWindows rate-limit))
               (width (seq-max (cons 0 (mapcar (lambda (window)
                                                 (string-width (opencode--claude-window-name
                                                                (car window))))
                                               windows)))))
          (pcase-dolist (`(,name . ,window) windows)
            (insert " " (opencode--format-claude-window name window nil t width) "\n")
            ;; on its own line under the bar, as it doesn't fit beside it
            (when-let* ((reset (opencode--claude-window-reset-text window)))
              (insert (opencode-sidebar--centered
                       reset (+ 2 width (/ opencode--usage-bar-width 2.0)))
                      "\n")))))
      (insert "\n"))))

(defun opencode-sidebar--centered (text column)
  "Return TEXT preceded by space that centers it on COLUMN."
  (concat (propertize " " 'display
                      `(space :align-to (- ,column (,(/ (string-pixel-width text) 2)))))
          text))

(defun opencode-sidebar--todo-face (status)
  "Return the face of a todo with STATUS."
  (pcase status
    ("in_progress" 'warning)
    ((or "completed" "cancelled") 'shadow)))

(defun opencode-sidebar--insert-todos (todos)
  "Insert the todo section for TODOS."
  (when todos
    (magit-insert-section (opencode-sidebar-todos)
      (opencode-sidebar--heading
       "Todo" (format "(%d remaining)"
                      (seq-count (lambda (todo)
                                   (member (alist-get 'status todo)
                                           '("pending" "in_progress")))
                                 todos)))
      (dolist (todo todos)
        (let-alist todo
          (magit-insert-section (opencode-sidebar-todo todo)
            (opencode-sidebar--item (pcase .status
                                      ("completed" "[✓]")
                                      ("in_progress" "[•]")
                                      ("cancelled" "[-]")
                                      (_ "[ ]"))
                                    .content
                                    (opencode-sidebar--todo-face .status)))))
      (insert "\n"))))

(defun opencode-sidebar--insert-changes (diff)
  "Insert the changes section for DIFF, as (FILES ADDITIONS DELETIONS)."
  (pcase diff
    (`(,files ,additions ,deletions)
     (when (plusp files)
       (magit-insert-section (opencode-sidebar-changes diff)
         (insert (propertize "Changed" 'font-lock-face 'opencode-sidebar-heading)
                 (format " %d file%s " files (if (= files 1) "" "s"))
                 (propertize (format "+%d" additions) 'font-lock-face 'magit-diffstat-added)
                 " "
                 (propertize (format "-%d" deletions) 'font-lock-face 'magit-diffstat-removed)
                 "\n\n"))))))

(defun opencode-sidebar--mcp-status (mcp)
  "Return the status of MCP, a Mcp.Server."
  (map-nested-elt mcp '(status status)))

(defun opencode-sidebar--mcp-face (status)
  "Return the face of an MCP server with STATUS."
  (pcase status
    ("connected" 'success)
    ("failed" 'error)
    ("needs_auth" 'warning)
    (_ 'shadow)))

(defvar-keymap opencode-sidebar-mcp-map
  :doc "Keymap of an MCP server in the sidebar."
  "<remap> <magit-visit-thing>" #'opencode-sidebar-toggle-mcp
  "RET" #'opencode-sidebar-toggle-mcp)

(defun opencode-sidebar--insert-mcps (mcps)
  "Insert the MCP section for MCPS."
  (when mcps
    (magit-insert-section (opencode-sidebar-mcps)
      (opencode-sidebar--heading
       "MCP" (let ((errors (seq-count (lambda (mcp)
                                        (equal (opencode-sidebar--mcp-status mcp) "failed"))
                                      mcps)))
               (format "(%d active, %d %s)"
                       (seq-count (lambda (mcp)
                                    (equal (opencode-sidebar--mcp-status mcp) "connected"))
                                  mcps)
                       errors
                       (ngettext "error" "errors" errors))))
      (dolist (mcp mcps)
        (let ((status (opencode-sidebar--mcp-status mcp)))
          (magit-insert-section section (opencode-sidebar-mcp mcp)
                                (oset section keymap 'opencode-sidebar-mcp-map)
                                (opencode-sidebar--item
                                 (propertize "●" 'font-lock-face (opencode-sidebar--mcp-face status))
                                 (concat (alist-get 'name mcp)
                                         (propertize (concat " " (string-replace "_" " " status))
                                                     'font-lock-face 'shadow)))
                                (when-let* ((error (map-nested-elt mcp '(status error))))
                                  (put-text-property (oref section start) (point) 'help-echo error)))))
      (insert "\n"))))

(defun opencode-sidebar--insert-location (directory branch)
  "Insert the DIRECTORY and git BRANCH of the session."
  (magit-insert-section (opencode-sidebar-location)
    (insert (propertize (concat (abbreviate-file-name (directory-file-name directory))
                                (when branch (concat ":" branch)))
                        'font-lock-face 'shadow)
            "\n")))

(defun opencode-sidebar--insert (session)
  "Insert the sidebar of SESSION buffer."
  (pcase-let ((`(,title ,step ,tokens ,model ,todos ,data ,directory ,rate-limit)
               (with-current-buffer session
                 (list opencode-session-title (opencode-sidebar--step)
                       opencode-session-tokens
                       (opencode--current-model)
                       opencode-session-todos opencode-sidebar--data
                       default-directory (cdr (opencode--claude-rate-limit))))))
    (insert (propertize (string-fill (or title "")
                                     (if-let* ((window (get-buffer-window)))
                                         (window-body-width window)
                                       opencode-sidebar-width))
                        'font-lock-face 'opencode-sidebar-title)
            "\n\n")
    (opencode-sidebar--insert-agent step)
    (opencode-sidebar--insert-activity step)
    (opencode-sidebar--insert-changes (plist-get data :diff))
    (opencode-sidebar--insert-context tokens model)
    (opencode-sidebar--insert-usage (plist-get data :session) rate-limit)
    (opencode-sidebar--insert-todos todos)
    (opencode-sidebar--insert-mcps (plist-get data :mcps))
    (opencode-sidebar--insert-location directory (plist-get data :branch))))

(defun opencode-sidebar--redraw ()
  "Redraw the current sidebar buffer."
  (opencode--call-preserving-lines
   (lambda ()
     (let ((inhibit-read-only t)
           (session opencode-sidebar--session))
       (opencode-sidebar--remember-visibility)
       (erase-buffer)
       (setq magit-root-section nil
             opencode-sidebar--running nil)
       (magit-insert-section (opencode-sidebar-root)
         (if (buffer-live-p session)
             (opencode-sidebar--insert session)
           (insert (propertize "No session" 'font-lock-face 'shadow) "\n"))))))
  (opencode-sidebar--update-tick))

(defun opencode-sidebar--update-tick ()
  "Redraw the visible sidebars showing a running turn every second, if any."
  (let ((running (seq-some (lambda (sidebar)
                             (buffer-local-value 'opencode-sidebar--running sidebar))
                           (opencode-sidebar--sidebars))))
    (cond ((and running (not (timerp opencode-sidebar--tick-timer)))
           (setq opencode-sidebar--tick-timer
                 (run-with-timer 1 1 #'opencode-sidebar--tick)))
          ((and (not running) (timerp opencode-sidebar--tick-timer))
           (cancel-timer opencode-sidebar--tick-timer)
           (setq opencode-sidebar--tick-timer nil)))))

(defun opencode-sidebar--tick ()
  "Redraw the visible sidebars showing a running turn."
  (dolist (sidebar (opencode-sidebar--sidebars))
    (when (buffer-local-value 'opencode-sidebar--running sidebar)
      (with-current-buffer sidebar
        (opencode-sidebar--redraw))))
  (opencode-sidebar--update-tick))

;;; Mode

(defun opencode-sidebar-toggle-mcp ()
  "Connect or disconnect the MCP server at point."
  (interactive)
  (let* ((mcp (oref (magit-current-section) value))
         (name (alist-get 'name mcp))
         (session opencode-sidebar--session))
    (unless (buffer-live-p session)
      (user-error "The session's buffer was killed"))
    (with-current-buffer session
      (if (equal (opencode-sidebar--mcp-status mcp) "connected")
          (opencode-api-disable-mcp (name)
              _
            (message "Disconnected MCP %s" name)
            (opencode-sidebar--invalidate session :mcps))
        (opencode-api-enable-mcp (name)
            _
          (message "Connected MCP %s" name)
          (opencode-sidebar--invalidate session :mcps))))))

(defun opencode-sidebar-refresh ()
  "Refetch everything the sidebar shows."
  (interactive)
  (when (buffer-live-p opencode-sidebar--session)
    (opencode-sidebar--invalidate opencode-sidebar--session))
  (opencode-sidebar--redraw))

(defvar-keymap opencode-sidebar-mode-map
  :doc "Keymap of `opencode-sidebar-mode'."
  :parent magit-section-mode-map
  "g" #'opencode-sidebar-refresh
  "q" #'opencode-sidebar-toggle)

(define-derived-mode opencode-sidebar-mode magit-section-mode "OpenCode Sidebar"
  "Major mode of the opencode session sidebar."
  :interactive nil
  (setq-local truncate-lines nil
              word-wrap t
              mode-line-format nil
              cursor-in-non-selected-windows nil)
  (add-hook 'magit-section-set-visibility-hook
            #'opencode-sidebar--section-visibility nil t))

(provide 'opencode-sidebar)
;;; opencode-sidebar.el ends here
