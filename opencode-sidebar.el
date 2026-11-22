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

(require 'ansi-color)
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
shown in a frame where it was closed, with `opencode-sidebar-toggle' or
otherwise, until it's shown there again with `opencode-sidebar-toggle'."
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

(defcustom opencode-sidebar-updated-after 20
  "Minutes since a session was last updated before the sidebar shows when it was.
It's shown under the title, while the session is idle.  Nil means never."
  :type '(choice (const :tag "Never" nil)
          (natnum :tag "Minutes"))
  :group 'opencode-sidebar)

(defcustom opencode-sidebar-test-file-regexp
  (rx (or (seq (or (seq (or bos (not alpha)) (or "test" "TEST"))
                   ;; a camelCase word
                   "Test")
               (? (or "s" "ing"))
               (or eos (not lower)))
          (seq (or bos "/") (or "testdata" "e2e") "/")
          (seq (or bos "/") "conftest.py" eos)
          ;; Jest, Vitest, Jasmine
          (seq ".spec." (or "js" "jsx" "ts" "tsx" "mjs" "cjs" "mts" "cts") eos)
          ;; RSpec, Hspec
          (seq (or bos "/") "spec/" (* nonl) "." (or "rb" "hs") eos)))
  "Regexp matching the paths of test files.
The sidebar counts changes to them separately from other files.  It's
matched case-sensitively.  The default matches the word test, plural or
with -ing, lowercase, capitalized, as in camelCase, or uppercase; files in
testdata and e2e directories; pytest's conftest.py; and spec files of
test frameworks, but not other specs, like design docs."
  :type 'regexp
  :group 'opencode-sidebar)

(defcustom opencode-sidebar-doc-extensions '("md" "markdown" "rst" "org" "adoc" "txt")
  "Extensions of doc files, which are test files only if in a test directory.
So docs about tests, like TESTING.md, aren't counted as tests."
  :type '(repeat string)
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

;;; Sections

(defvar opencode-sidebar--sections nil
  "Alist of the names of the sidebar's sections and their definitions.
A definition is a plist of :function, :fetch and :join, as given to
`opencode-sidebar-define-section'.")

(defcustom opencode-sidebar
  '(title last-updated agent activity jobs changes context usage todos mcp location)
  "Sections of the sidebar, in order.
Each is the name of a section defined with `opencode-sidebar-define-section'.
The built-in ones are:

  title         the session's title
  last-updated  when an idle session was last updated, see
                `opencode-sidebar-updated-after'
  agent         the agent and model of the step at point
  activity      the turn of the step at point, its time and speed
  jobs          the running subagents and shell commands of the session
                and its subagents, RET on one opens it and x cancels it
  changes       how many files the session changed
  context       the context's tokens, and the share of the model's limit
  usage         the session's tokens, cache rate and cost
  todos         the session's todo list
  mcp           the MCP servers, RET on one connects or disconnects it
  location      the session's directory and git branch

Only the data the sections shown use is fetched from the server."
  :type '(repeat (choice (const title) (const last-updated) (const agent)
                         (const activity) (const jobs) (const changes) (const context)
                         (const usage) (const todos) (const mcp) (const location)
                         (symbol :tag "Other section")))
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (opencode-sidebar--sections-changed))
  :group 'opencode-sidebar)

(defmacro opencode-sidebar-define-section (name docstring &rest body)
  "Define a sidebar section NAME, for `opencode-sidebar'.
BODY inserts it into the sidebar buffer, where it's run.  It's inserted
within a magit section of type opencode-sidebar-NAME, so BODY can begin
with `magit-insert-heading' to make it collapsible.  A blank line is
inserted after it, and if BODY inserts nothing the section isn't shown.
It can get what it shows with `opencode-sidebar-session',
`opencode-sidebar-with-session', `opencode-sidebar-step' and
`opencode-sidebar-data'.  It's also defined as a function,
opencode-sidebar-insert-NAME, documented by DOCSTRING.

BODY can begin with keyword arguments:

:fetch ALIST  An alist of keys of data fetched for the section, and the
              functions fetching them, which are called in the session
              buffer and store what they fetch with
              `opencode-sidebar-store'.  It's not evaluated.  Data is
              fetched when the sidebar first shows a session, and
              refetched by `opencode-sidebar-refresh' or
              `opencode-sidebar-session-changed'.
:join BOOL    Whether the section continues the one before it, with no
              blank line between them."
  (declare (indent defun) (doc-string 2)
           (debug (&define name stringp [&rest keywordp sexp] def-body)))
  (let (fetch join)
    (while (keywordp (car body))
      (pcase (pop body)
        (:fetch (setq fetch (pop body)))
        (:join (setq join (pop body)))
        (keyword (error "Unknown keyword %s" keyword))))
    (let ((function (intern (format "opencode-sidebar-insert-%s" name))))
      `(progn
         (defun ,function ()
           ,docstring
           ,@body)
         (setf (alist-get ',name opencode-sidebar--sections)
               (list :function #',function :fetch ',fetch :join ,join))
         ',name))))

(defun opencode-sidebar--section (name &optional noerror)
  "Return the definition of the section NAME.
Signal an error if there's none, unless NOERROR."
  (or (alist-get name opencode-sidebar--sections)
      (unless noerror
        (error "No sidebar section %s is defined, see `opencode-sidebar'" name))))

(defvar opencode-sidebar--drawn nil
  "The session buffer whose sidebar is being drawn.")

(defvar opencode-sidebar--step-cache nil
  "While drawing a sidebar, a cons of whether its step is known, and the step.")

(defun opencode-sidebar-session ()
  "Return the session buffer whose sidebar is being drawn, or shown."
  (or opencode-sidebar--drawn opencode-sidebar--session))

(defmacro opencode-sidebar-with-session (&rest body)
  "Run BODY in the session buffer whose sidebar is being drawn."
  (declare (indent 0) (debug t))
  `(with-current-buffer (opencode-sidebar-session)
     ,@body))

(defun opencode-sidebar-data (key)
  "Return the data of KEY fetched for the sidebar of the session.
KEY is one of those of a section's :fetch, see
`opencode-sidebar-define-section'.  The built-in ones are :session, its
Session.Info, :diff, its diffstat from `opencode-sidebar--diffstat',
:mcps, its MCP servers, :jobs, its running subagent and shell Job.Info,
and :branch, its git branch."
  (plist-get (buffer-local-value 'opencode-sidebar--data (opencode-sidebar-session))
             key))

(defun opencode-sidebar-step ()
  "Return a plist of the step at point in the session, see `opencode-sidebar--step'.
It's found once per redraw."
  (let ((cache opencode-sidebar--step-cache))
    (cond ((not cache) (opencode-sidebar-with-session (opencode-sidebar--step)))
          ((car cache) (cdr cache))
          (t (setcar cache t)
             (setcdr cache (opencode-sidebar-with-session (opencode-sidebar--step)))))))

(defvar-local opencode-sidebar--data nil
  "Plist of what a session buffer's sidebar shows, fetched from the server.
The keys are :session, :diff, :mcps, :jobs and :branch, see
`opencode-sidebar-data'.  A missing key hasn't been fetched.")

(defvar opencode-sidebar--timer nil
  "Timer of the pending redraw of the sidebars.")

(defvar opencode-sidebar--tick-timer nil
  "Timer ticking every second while a sidebar is visible, see `opencode-sidebar--tick'.")

(defvar opencode-sidebar--ticks 0
  "Seconds `opencode-sidebar--tick-timer' has ticked, modulo 60.")

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
        (set-frame-parameter nil 'opencode-sidebar-suspended nil)
        (delete-window window))
    (if-let* ((session (opencode-sidebar--visible-session)))
        (opencode-sidebar--show session)
      (message "No session visible in this frame"))))

(defun opencode-sidebar--auto-p ()
  "Return non-nil if the selected frame's sidebar should show on its own.
Only in a frame that hasn't had a sidebar yet: once it has, its buffer
not being in a window means it was closed."
  (and opencode-sidebar-auto
       (not (opencode-sidebar--buffer))
       (or (not (numberp opencode-sidebar-auto))
           (>= (frame-width) opencode-sidebar-auto))))

(defun opencode-sidebar--follow (&optional frame)
  "Make FRAME's sidebar show the session buffer visible in it.
See `opencode-sidebar--visible-session'.  With none visible, the sidebar
is suspended, hidden until one is.  If it was closed, with
`opencode-sidebar-toggle' or otherwise, like with `quit-window', it stays
closed.  For `window-selection-change-functions' and
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

(defun opencode-sidebar-store (key value)
  "Set KEY of the current session buffer's sidebar data to VALUE, and redraw.
For the fetchers of sections, see `opencode-sidebar-define-section'."
  (setq opencode-sidebar--data (plist-put opencode-sidebar--data key value))
  (opencode-sidebar--schedule-redraw))

(defun opencode-sidebar--test-file-p (path)
  "Return non-nil if PATH is a test file, per `opencode-sidebar-test-file-regexp'.
A doc file, per `opencode-sidebar-doc-extensions', is one only if its
directory matches, so docs about tests, like TESTING.md, aren't."
  (let ((case-fold-search nil))
    (string-match-p opencode-sidebar-test-file-regexp
                    (if (member (file-name-extension path) opencode-sidebar-doc-extensions)
                        (or (file-name-directory path) "")
                      path))))

(defun opencode-sidebar--diffstat (diff)
  "Return the diffstat of DIFF, a list of FileDiff.Info.
It's (FILES ADDITIONS DELETIONS TESTS TEST-ADDITIONS TEST-DELETIONS), with
test files, per `opencode-sidebar--test-file-p', counted only in the last
three."
  (cl-loop for file in diff
           for add = (or (alist-get 'additions file) 0)
           for del = (or (alist-get 'deletions file) 0)
           if (opencode-sidebar--test-file-p (or (alist-get 'file file) ""))
           count t into tests and sum add into test-additions
           and sum del into test-deletions
           else count t into files and sum add into additions
           and sum del into deletions
           finally return (list files additions deletions
                                tests test-additions test-deletions)))

(defun opencode-sidebar--fetch-diff ()
  "Fetch the files the current buffer's session changed, over all its turns."
  (let ((id opencode-session-id)
        (fail (lambda (message)
                ;; like when its turns span a change of location
                (opencode--log-event "WARNING SIDEBAR" (format "Session diff failed: %s" message))
                (opencode-sidebar-store :diff nil))))
    (opencode-api-first-user-message (id)
        first
      :else fail
      (opencode-api-last-user-message (id)
          last
        :else fail
        (if (not (and first last))
            (opencode-sidebar-store :diff nil)
          (opencode-api-session-range-diff (id (alist-get 'id (car first))
                                               (alist-get 'id (car last)))
              diff
            :else fail
            (opencode-sidebar-store :diff (opencode-sidebar--diffstat diff))))))))

(defun opencode-sidebar--fetch-session ()
  "Fetch the current buffer's session, for its total tokens and cost."
  (opencode-api-session (opencode-session-id)
      session
    :else (lambda (message)
            (opencode--log-event "WARNING SIDEBAR" (format "Session fetch failed: %s" message))
            (opencode-sidebar-store :session nil))
    (opencode-sidebar-store :session session)))

(defun opencode-sidebar--fetch-mcps ()
  "Fetch the MCP servers of the current session buffer's directory."
  (opencode-api-mcps mcps
    :else (lambda (message)
            (opencode--log-event "WARNING SIDEBAR" (format "MCP list failed: %s" message))
            (opencode-sidebar-store :mcps nil))
    (opencode-sidebar-store :mcps mcps)))

(defun opencode-sidebar--fetch-jobs ()
  "Fetch the running subagents and shell commands of the current buffer's session.
Including those of its subagents, oldest first."
  (opencode-api-session-running-jobs (opencode-session-id)
      jobs
    :else (lambda (message)
            (opencode--log-event "WARNING SIDEBAR" (format "Job list failed: %s" message))
            (opencode-sidebar-store :jobs nil))
    (opencode-sidebar-store
     :jobs (seq-sort-by (lambda (job) (map-nested-elt job '(time started)))
                        #'<
                        (seq-filter (lambda (job)
                                      (member (alist-get 'kind job) '("subagent" "shell")))
                                    jobs)))))

(defun opencode-sidebar--fetch-branch ()
  "Find the git branch of the current session buffer's directory.
Not over Tramp, as it's slow and can be run from a process filter."
  (opencode-sidebar-store
   :branch (unless (file-remote-p default-directory)
             (ignore-errors (magit-get-current-branch)))))

(defun opencode-sidebar--fetchers ()
  "Return an alist of the keys of the sidebar's data and the functions fetching them.
Of the sections in `opencode-sidebar', from their :fetch, see
`opencode-sidebar-define-section'.  Undefined sections are left to
`opencode-sidebar--redraw' to complain about."
  (let (fetchers)
    (dolist (name opencode-sidebar)
      (pcase-dolist (`(,key . ,fetch) (plist-get (opencode-sidebar--section name t) :fetch))
        (unless (assq key fetchers)
          (push (cons key fetch) fetchers))))
    (nreverse fetchers)))

(defun opencode-sidebar--fetch (session &optional refetch)
  "Fetch what the sidebar shows for SESSION buffer that it doesn't have yet.
Keys in REFETCH are fetched even if it has them, keeping their old values
until the new ones arrive."
  (with-current-buffer session
    (when opencode-session-id
      ;; for the share of the 5h budget in the cost
      (when (opencode-claude-subscription-p)
        (opencode--fetch-claude-plan-tier))
      (pcase-dolist (`(,key . ,fetch) (opencode-sidebar--fetchers))
        (when (or (memq key refetch)
                  (not (plist-member opencode-sidebar--data key)))
          ;; so it's fetched once while the request is pending
          (unless (plist-member opencode-sidebar--data key)
            (setq opencode-sidebar--data (plist-put opencode-sidebar--data key nil)))
          (funcall fetch))))))

(defun opencode-sidebar--invalidate (session &rest keys)
  "Refetch KEYS of SESSION buffer's sidebar data if it's shown, else forget them.
With no KEYS, all of it.  While it's refetched the old values are shown,
so sections don't disappear until the new ones arrive.  Only keys of
sections in `opencode-sidebar' are refetched, others are forgotten."
  (let* ((enabled (mapcar #'car (opencode-sidebar--fetchers)))
         (keys (or keys enabled))
         (shown (opencode-sidebar--shown-p session))
         (forget (if shown (seq-difference keys enabled) keys)))
    (with-current-buffer session
      (setq opencode-sidebar--data
            (map-into (map-remove (lambda (key _) (memq key forget))
                                  opencode-sidebar--data)
                      'plist)))
    (when shown
      ;; storing what's fetched redraws it
      (opencode-sidebar--fetch session keys))))

(defun opencode-sidebar--sections-changed ()
  "Fetch what's missing for the sections of the visible sidebars, and redraw them.
For when `opencode-sidebar' is changed."
  (dolist (sidebar (opencode-sidebar--sidebars))
    (with-current-buffer sidebar
      (when (buffer-live-p opencode-sidebar--session)
        (opencode-sidebar--fetch opencode-sidebar--session))
      (opencode-sidebar--redraw))))

(defun opencode-sidebar-session-changed (&rest keys)
  "Update the sidebar for a change to the current session buffer.
KEYS of its data that the change made stale are refetched."
  (cond (keys (apply #'opencode-sidebar--invalidate (current-buffer) keys))
        ((opencode-sidebar--shown-p (current-buffer))
         (opencode-sidebar--schedule-redraw))))

(defun opencode-sidebar-session-moved ()
  "Update the sidebars for the current session buffer moving to another directory.
The `default-directory' of those showing it, for commands run in them,
follows the session's, and what depends on its location is refetched."
  (let ((session (current-buffer)))
    (dolist (sidebar (opencode-sidebar--sidebars))
      (with-current-buffer sidebar
        (when (eq opencode-sidebar--session session)
          (setq default-directory (buffer-local-value 'default-directory session)))))
    (opencode-sidebar-session-changed :session :diff :mcps :branch)))

(defun opencode-sidebar--invalidate-all (key)
  "Refetch KEY of the sidebar data of every session buffer that has it."
  (dolist (buffer (buffer-list))
    (when (and (buffer-local-value 'opencode-session-id buffer)
               (plist-member (buffer-local-value 'opencode-sidebar--data buffer) key))
      (opencode-sidebar--invalidate buffer key))))

(defun opencode-sidebar-mcps-changed ()
  "Refetch the MCP servers of every session buffer's sidebar."
  (opencode-sidebar--invalidate-all :mcps))

(defun opencode-sidebar-jobs-changed ()
  "Refetch the jobs of every session buffer's sidebar.
A job may be of a subagent of a session, at any depth, so they're all
refetched rather than finding which sessions it's of."
  (opencode-sidebar--invalidate-all :jobs))

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

(opencode-sidebar-define-section title
  "Insert the session's title, wrapped to the sidebar's width."
  (insert (propertize (string-fill (or (opencode-sidebar-with-session opencode-session-title)
                                       "")
                                   (if-let* ((window (get-buffer-window)))
                                       (window-body-width window)
                                     opencode-sidebar-width))
                      'font-lock-face 'opencode-sidebar-title)
          "\n"))

(opencode-sidebar-define-section last-updated
  "Insert when the session was last updated, see `opencode-sidebar--updated'."
  :join t
  :fetch ((:session . opencode-sidebar--fetch-session))
  (when-let* ((updated (opencode-sidebar--updated (opencode-sidebar-data :session)
                                                  (plist-get (opencode-sidebar-step) :status))))
    (insert (propertize updated 'font-lock-face 'shadow) "\n")))

(opencode-sidebar-define-section agent
  "Insert the agent and model of the step at point."
  (let ((step (opencode-sidebar-step)))
    (when-let* ((agent (plist-get step :agent)))
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
                  "\n"))))))

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

(opencode-sidebar-define-section activity
  "Insert the turn of the step at point, its time and speed.
A running turn's time counts up, by redrawing the sidebar every second."
  (let ((step (opencode-sidebar-step)))
    (when-let* ((turn (plist-get step :turn)))
      (opencode-sidebar--heading "Activity")
      (let ((ended (opencode--turn-ended turn)))
        (insert (format " %s%s\n"
                        ;; turns are numbered from the first displayed
                        (if (opencode-sidebar-with-session opencode--older-messages-hidden)
                            ""
                          (format "turn %d · " (opencode--turn-number turn)))
                        (opencode-sidebar--duration
                         (- (or ended (opencode-sidebar-with-session (opencode--server-now-ms)))
                            (opencode--turn-created turn))
                         (not ended)))))
      (when-let* ((speed (opencode-sidebar--speed (plist-get step :message))))
        (insert " " speed "\n")))))

(defvar-keymap opencode-sidebar-job-map
  :doc "Keymap of a job in the sidebar."
  "<remap> <magit-visit-thing>" #'opencode-sidebar-visit-job
  "RET" #'opencode-sidebar-visit-job
  "x" #'opencode-sidebar-cancel-job)

(defun opencode-sidebar--job-text (job)
  "Return the text of JOB's item, a subagent or shell Job.Info."
  (let-alist job
    (if (equal .kind "shell")
        (let ((lines (string-lines (string-trim .command))))
          (concat (car lines) (when (cdr lines) " …")))
      (concat "@" .agent " " .description))))

(opencode-sidebar-define-section jobs
  "Insert the running subagents and shell commands of the session and its subagents.
RET on one opens it, and x cancels it.  Their times count up, by
redrawing the sidebar every second."
  :fetch ((:jobs . opencode-sidebar--fetch-jobs))
  (when-let* ((jobs (opencode-sidebar-data :jobs)))
    (opencode-sidebar--heading "Jobs" (format "(%d running)" (length jobs)))
    (let ((now (opencode-sidebar-with-session (opencode--server-now-ms))))
      (dolist (job jobs)
        (magit-insert-section section (opencode-sidebar-job job)
                              (oset section keymap 'opencode-sidebar-job-map)
                              (opencode-sidebar--item
                               (if (equal (alist-get 'kind job) "shell") "$" "●")
                               (concat (opencode-sidebar--job-text job)
                                       (propertize (concat " " (opencode-sidebar--duration
                                                                (- now (map-nested-elt job '(time started)))
                                                                t))
                                                   'font-lock-face 'shadow))))))))

(opencode-sidebar-define-section context
  "Insert the context's tokens, and the share of the model's limit."
  (pcase-let ((`(,tokens . ,model) (opencode-sidebar-with-session
                                     (cons opencode-session-tokens
                                           (opencode--current-model)))))
    (when (plusp tokens)
      (opencode-sidebar--heading "Context")
      (insert (format " %s tokens\n" (opencode--format-tokens tokens)))
      (let ((limit (map-nested-elt model '(limit context))))
        (when (and (numberp limit) (plusp limit))
          (insert (format " %.0f%% used\n" (* 100 (/ (float tokens) limit)))))))))

(opencode-sidebar-define-section usage
  "Insert the session's tokens, cache rate and cost.
And for claude-agent sessions, the Claude rate limit windows."
  :fetch ((:session . opencode-sidebar--fetch-session))
  (when-let* ((session (opencode-sidebar-data :session)))
    (let ((rate-limit (opencode-sidebar-with-session (cdr (opencode--claude-rate-limit)))))
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
                      "\n"))))))))

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

(opencode-sidebar-define-section todos
  "Insert the session's todo list."
  (when-let* ((todos (opencode-sidebar-with-session opencode-session-todos)))
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
                                  (opencode-sidebar--todo-face .status)))))))

(defun opencode-sidebar--diffstat-line (count noun additions deletions)
  "Return a line of COUNT NOUNs changed, with ADDITIONS and DELETIONS."
  (concat (format "%d %s%s " count noun (if (= count 1) "" "s"))
          (propertize (format "+%d" additions) 'font-lock-face 'magit-diffstat-added)
          " "
          (propertize (format "-%d" deletions) 'font-lock-face 'magit-diffstat-removed)
          "\n"))

(opencode-sidebar-define-section changes
  "Insert how many files the session changed, from `opencode-sidebar--diffstat'.
Test files are counted apart from other files.  If only one kind changed,
it's counted on the heading's line, else each on its own line under it,
so the section's collapsible."
  :fetch ((:diff . opencode-sidebar--fetch-diff))
  (pcase (opencode-sidebar-data :diff)
    (`(,files ,additions ,deletions ,tests ,test-additions ,test-deletions)
     (when (plusp (+ files tests))
       (let ((lines (append (when (plusp files)
                              (list (opencode-sidebar--diffstat-line
                                     files "file" additions deletions)))
                            (when (plusp tests)
                              (list (opencode-sidebar--diffstat-line
                                     tests "test" test-additions test-deletions))))))
         (if (cdr lines)
             (progn
               (opencode-sidebar--heading "Changed")
               (insert (mapconcat (lambda (line) (concat " " line)) lines)))
           (insert (propertize "Changed" 'font-lock-face 'opencode-sidebar-heading)
                   " " (car lines))))))))

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

(opencode-sidebar-define-section mcp
  "Insert the MCP servers, which RET on connects or disconnects."
  :fetch ((:mcps . opencode-sidebar--fetch-mcps))
  (when-let* ((mcps (opencode-sidebar-data :mcps)))
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
        (magit-insert-section section
          (opencode-sidebar-mcp-server mcp)
          (oset section keymap 'opencode-sidebar-mcp-map)
          (opencode-sidebar--item
           (propertize "●" 'font-lock-face (opencode-sidebar--mcp-face status))
           (concat (alist-get 'name mcp)
                   (propertize (concat " " (string-replace "_" " " status))
                               'font-lock-face 'shadow)))
          (when-let* ((error (map-nested-elt mcp '(status error))))
            (put-text-property (oref section start) (point) 'help-echo error)))))))

(opencode-sidebar-define-section location
  "Insert the directory and git branch of the session."
  :fetch ((:branch . opencode-sidebar--fetch-branch))
  (let ((directory (opencode-sidebar-with-session default-directory))
        (branch (opencode-sidebar-data :branch)))
    (insert (propertize (concat (abbreviate-file-name (directory-file-name directory))
                                (when branch (concat ":" branch)))
                        'font-lock-face 'shadow)
            "\n")))

(defun opencode-sidebar--updated (session status)
  "Return when SESSION, a Session.Info, was last updated, or nil to not show it.
It's shown once it's `opencode-sidebar-updated-after' minutes ago, unless
STATUS is busy, as SESSION is only refetched once a step ends."
  (when-let* ((opencode-sidebar-updated-after)
              ((not (equal status "busy")))
              (seconds (opencode--time-ago session 'updated)))
    (when (>= seconds (* 60 opencode-sidebar-updated-after))
      (concat "updated " (opencode--format-time-ago seconds) " ago"))))

(defun opencode-sidebar--insert-section (name)
  "Insert the section NAME, returning whether it inserted anything.
An error in it is logged, and it's left out."
  (let ((start (point)))
    (condition-case err
        (funcall (plist-get (opencode-sidebar--section name) :function))
      (error
       (opencode--log-event "WARNING SIDEBAR"
                            (format "Section %s failed: %s" name (error-message-string err)))
       (delete-region start (point))
       (oset magit-insert-section--current children nil)
       (oset magit-insert-section--current content nil)))
    (> (point) start)))

(defun opencode-sidebar--insert (session)
  "Insert the sidebar of SESSION buffer, with the sections of `opencode-sidebar'.
Each is followed by a blank line, within it so it's hidden when it's
collapsed, except before a :join section, which it's moved after.  If
that's empty, it's inserted in its place."
  (let ((opencode-sidebar--drawn session)
        (opencode-sidebar--step-cache (cons nil nil))
        (names opencode-sidebar)
        owed)
    ;; signal undefined sections before inserting any
    (mapc #'opencode-sidebar--section names)
    (while names
      (let* ((name (pop names))
             (next-joins (and names
                              (plist-get (opencode-sidebar--section (car names)) :join))))
        (magit-insert-section ((eval (intern (format "opencode-sidebar-%s" name))))
          (if (not (or (opencode-sidebar--insert-section name) owed))
              (magit-cancel-section)
            (setq owed next-joins)
            (unless owed
              (insert "\n"))))))
    ;; the last section's blank line
    (when (and (> (point) (1+ (point-min)))
               (equal (buffer-substring (- (point) 2) (point)) "\n\n"))
      (delete-char -1))))

(defun opencode-sidebar--redraw ()
  "Redraw the current sidebar buffer."
  (opencode--call-preserving-lines
   (lambda ()
     (let ((inhibit-read-only t)
           (session opencode-sidebar--session))
       (erase-buffer)
       ;; the old `magit-root-section' isn't cleared, and magit-section's
       ;; visibility cache keeps collapsed sections collapsed
       (magit-insert-section (opencode-sidebar-root)
         (if (buffer-live-p session)
             (opencode-sidebar--insert session)
           (insert (propertize "No session" 'font-lock-face 'shadow) "\n")))
       ;; hide the sections marked hidden, like `magit-refresh-buffer' does by
       ;; showing the root, but so only they get a visibility indicator
       (magit-map-sections
        (lambda (section)
          (when (and (oref section hidden) (not (eq section magit-root-section)))
            (magit-section-hide section)))))))
  (when (and (get-buffer-window (current-buffer) t)
             (not (timerp opencode-sidebar--tick-timer)))
    (setq opencode-sidebar--ticks 0
          opencode-sidebar--tick-timer (run-with-timer 1 1 #'opencode-sidebar--tick))))

(defun opencode-sidebar--running-p ()
  "Return non-nil if the current sidebar shows something running.
That's a running turn, the step at point's, or jobs."
  (and (buffer-live-p opencode-sidebar--session)
       (with-current-buffer opencode-sidebar--session
         (or (and (memq 'jobs opencode-sidebar)
                  (plist-get opencode-sidebar--data :jobs))
             (when-let* ((turn (car (opencode--message-at (point)))))
               (not (opencode--turn-ended turn)))))))

(defun opencode-sidebar--tick ()
  "Redraw the visible sidebars showing something running, as its time counts up.
And every minute all of them, as when idle sessions were last updated ages.
Run every second while a sidebar is visible."
  (if-let* ((sidebars (opencode-sidebar--sidebars)))
      (progn
        (setq opencode-sidebar--ticks (% (1+ opencode-sidebar--ticks) 60))
        (dolist (sidebar sidebars)
          (with-current-buffer sidebar
            (when (or (zerop opencode-sidebar--ticks) (opencode-sidebar--running-p))
              (opencode-sidebar--redraw)))))
    (cancel-timer opencode-sidebar--tick-timer)
    (setq opencode-sidebar--tick-timer nil)))

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

(defun opencode-sidebar--job-at-point ()
  "Return the job at point and the session buffer of the sidebar, as a cons."
  (let ((section (magit-current-section))
        (session opencode-sidebar--session))
    (unless (and section (eq (oref section type) 'opencode-sidebar-job))
      (user-error "No job at point"))
    (unless (buffer-live-p session)
      (user-error "The session's buffer was killed"))
    (cons (oref section value) session)))

(defun opencode-sidebar-visit-job ()
  "Open the job at point: a subagent's session, or a shell command's output.
It's opened in another window than the session's, which stays visible."
  (interactive)
  (pcase-let ((`(,job . ,session) (opencode-sidebar--job-at-point)))
    ;; the sidebar's window is dedicated, so pop it up from the session's
    (when-let* ((window (get-buffer-window session)))
      (select-window window))
    (with-current-buffer session
      (let-alist job
        (if (equal .kind "subagent")
            (opencode-api-session (.childSessionID)
                child
              (let ((opencode-open-same-window nil))
                (opencode-open-session child)))
          (opencode-shell-output job))))))

(defun opencode-sidebar-cancel-job ()
  "Cancel the job at point, a subagent or shell command."
  (interactive)
  (pcase-let ((`(,job . ,session) (opencode-sidebar--job-at-point)))
    (with-current-buffer session
      (opencode--cancel-job job))))

;;; Shell output

(defconst opencode-shell-output--page (* 64 1024)
  "Bytes of a shell command's output fetched at once.
At most this much of what it's output already is shown when it's opened.")

(defcustom opencode-shell-output-interval 1
  "Seconds between fetches of a running shell command's new output."
  :type 'number
  :group 'opencode-sidebar)

(defconst opencode-shell-output--past-end 9007199254740991
  "A cursor past the end of any output, the largest the server accepts.")

(defvar-local opencode-shell-output--id nil
  "The id of the shell command whose output the buffer shows.")

(defvar-local opencode-shell-output--job nil
  "The Job.Shell of the shell command, to cancel it.")

(defvar-local opencode-shell-output--cursor nil
  "Byte offset of the output fetched so far, nil before it's first fetched.")

(defvar-local opencode-shell-output--timer nil
  "Timer fetching the new output, while the command runs.")

(defvar-local opencode-shell-output--fetching nil
  "Non-nil while a fetch is in progress.")

(defvar-local opencode-shell-output--killed nil
  "Non-nil once the shell command was killed from Emacs.
Its output's removed with it, so it's then no longer found.")

(defun opencode--cancel-job (job)
  "Cancel JOB, a subagent or shell Job.Info, and refetch the jobs the sidebars show.
A subagent's session is interrupted, and a shell command is killed and
removed, with its output."
  (let* ((text (opencode-sidebar--job-text job))
         (done (lambda (_)
                 (message "Cancelled %s" text)
                 (opencode-sidebar-jobs-changed))))
    (let-alist job
      (if (equal .kind "subagent")
          (opencode-api-interrupt-session (.childSessionID)
              result
            (funcall done result))
        (dolist (buffer (buffer-list))
          (when (equal .shellID (buffer-local-value 'opencode-shell-output--id buffer))
            (with-current-buffer buffer
              (setq opencode-shell-output--killed t))))
        (opencode-api-remove-shell (.shellID)
            result
          (funcall done result))))))

(defun opencode-shell-output (job)
  "Show the output of the shell command of JOB, a Job.Shell.
It's followed while it runs, and shown in another window.  Run in its
session's buffer, for its server and directory."
  (let* ((id (alist-get 'shellID job))
         (command (alist-get 'command job))
         (buffer (or (seq-find (lambda (buffer)
                                 (equal id (buffer-local-value 'opencode-shell-output--id
                                                               buffer)))
                               (buffer-list))
                     (let ((connection (opencode--conn))
                           (directory default-directory)
                           (buffer (generate-new-buffer
                                    (format "*OpenCode shell: %s*"
                                            (truncate-string-to-width
                                             (car (string-lines (string-trim command)))
                                             40 nil nil "…")))))
                       (with-current-buffer buffer
                         (opencode-shell-output-mode)
                         (setq opencode-buffer-connection connection
                               default-directory directory
                               opencode-shell-output--id id
                               opencode-shell-output--job job)
                         (let ((inhibit-read-only t))
                           (insert (propertize (concat "$ " command "\n")
                                               'font-lock-face 'opencode-sidebar-heading)))
                         (opencode-shell-output--poll))
                       buffer))))
    (let ((opencode-open-same-window nil))
      (opencode--switch-to-buffer buffer))))

(defun opencode-shell-output--insert (text)
  "Insert TEXT at the end of the output, following it where point is at the end."
  (let ((inhibit-read-only t))
    ;; windows at the end follow by `window-point-insertion-type'
    (if (= (point) (point-max))
        (insert text)
      (save-excursion
        (goto-char (point-max))
        (insert text)))))

(defun opencode-shell-output--insert-status (text face)
  "Insert TEXT in FACE after the output, on a line of its own after a blank one."
  (opencode-shell-output--insert
   (propertize (concat (unless (memq (char-before (point-max)) '(?\n nil)) "\n")
                       "\n" text "\n")
               'font-lock-face face)))

(defun opencode-shell-output--read (then else)
  "Fetch the output since the last fetch, then call THEN.
At first only the last `opencode-shell-output--page' bytes of it.  Call
ELSE with the error message if a request fails."
  (let ((id opencode-shell-output--id))
    (cl-labels ((page ()
                  (opencode-api-shell-output (id opencode-shell-output--cursor
                                                 opencode-shell-output--page)
                      result
                    :else else
                    (let-alist result
                      (let ((advanced (> .cursor opencode-shell-output--cursor)))
                        (opencode-shell-output--insert (ansi-color-apply .output))
                        (setq opencode-shell-output--cursor .cursor)
                        (if (and advanced (< .cursor .size))
                            (page)
                          (funcall then)))))))
      (if opencode-shell-output--cursor
          (page)
        ;; its size, from past its end
        (opencode-api-shell-output (id opencode-shell-output--past-end 1)
            head
          :else else
          (setq opencode-shell-output--cursor
                (max 0 (- (alist-get 'size head) opencode-shell-output--page)))
          (when (plusp opencode-shell-output--cursor)
            (opencode-shell-output--insert
             (propertize "[earlier output omitted]\n" 'font-lock-face 'shadow)))
          (page))))))

(defun opencode-shell-output--status (shell)
  "Return the line saying how SHELL, a Shell.Info that's not running, ended."
  (let-alist shell
    (pcase .status
      ("exited" (format "[exited %s]" (if (numberp .exit) (truncate .exit) "?")))
      ("timeout" "[timed out]")
      ("killed" (if .signal (format "[killed by %s]" .signal) "[killed]"))
      (status (format "[%s]" status)))))

(defun opencode-shell-output--poll ()
  "Fetch the shell command's new output, and whether it's still running.
Its status is fetched first, so once it's ended all its output is read,
and then it's no longer followed."
  (unless opencode-shell-output--fetching
    (setq opencode-shell-output--fetching t)
    (let ((fail (lambda (message)
                  (setq opencode-shell-output--fetching nil)
                  (opencode-shell-output--stop)
                  (cond ((not (string-search "ShellNotFoundError" message))
                         (opencode-shell-output--insert-status message 'error))
                        (opencode-shell-output--killed
                         (opencode-shell-output--insert-status "[killed]" 'shadow))
                        (t (opencode-shell-output--insert-status
                            "Shell output is no longer available." 'error))))))
      (opencode-api-shell (opencode-shell-output--id)
          shell
        :else fail
        (opencode-shell-output--read
         (lambda ()
           (setq opencode-shell-output--fetching nil)
           (if (equal (alist-get 'status shell) "running")
               (unless (timerp opencode-shell-output--timer)
                 (let ((buffer (current-buffer)))
                   (setq opencode-shell-output--timer
                         (run-with-timer opencode-shell-output-interval
                                         opencode-shell-output-interval
                                         (lambda ()
                                           (when (buffer-live-p buffer)
                                             (with-current-buffer buffer
                                               (opencode-shell-output--poll))))))))
             (opencode-shell-output--stop)
             (opencode-shell-output--insert-status (opencode-shell-output--status shell)
                                                   'shadow)))
         fail)))))

(defun opencode-shell-output--stop ()
  "Stop following the shell command's output."
  (when (timerp opencode-shell-output--timer)
    (cancel-timer opencode-shell-output--timer))
  (setq opencode-shell-output--timer nil))

(defun opencode-shell-output-refresh ()
  "Fetch the shell command's new output, following it again if it's running."
  (interactive)
  (opencode-shell-output--poll))

(defun opencode-shell-output-kill ()
  "Kill the shell command, by cancelling its job."
  (interactive)
  (opencode--cancel-job opencode-shell-output--job))

(defvar-keymap opencode-shell-output-mode-map
  :doc "Keymap of `opencode-shell-output-mode'."
  "g" #'opencode-shell-output-refresh
  "x" #'opencode-shell-output-kill)

(define-derived-mode opencode-shell-output-mode special-mode "OpenCode Shell"
  "Major mode showing the output of an opencode shell command.
It's followed while the command runs."
  :interactive nil
  (setq-local window-point-insertion-type t)
  (add-hook 'kill-buffer-hook #'opencode-shell-output--stop nil t))

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

(with-eval-after-load 'evil
  (declare-function evil-define-key* "evil-core")
  (evil-define-key* 'normal opencode-sidebar-mode-map
    "gr" #'opencode-sidebar-refresh
    "q" #'opencode-sidebar-toggle))

(define-derived-mode opencode-sidebar-mode magit-section-mode "OpenCode Sidebar"
  "Major mode of the opencode session sidebar."
  :interactive nil
  (setq-local truncate-lines nil
              word-wrap t
              mode-line-format nil
              cursor-in-non-selected-windows nil))

(provide 'opencode-sidebar)
;;; opencode-sidebar.el ends here
