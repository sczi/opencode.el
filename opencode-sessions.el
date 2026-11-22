;;; opencode-sessions.el --- Code for managing opencode sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Code for managing opencode sessions

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'hideshow)
(require 'imenu)
(require 'magit-git)
(require 'markdown-ts-mode)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-convert)
(require 'opencode-edit-hooks)
(require 'opencode-session-input)
(require 'opencode-session-output)
(require 'yank-media)

(defun opencode--tab-dispatch ()
  "Cycle session agent, or move to next button if point is on one."
  (interactive)
  (if (button-at (point))
      (forward-button 1)
    (call-interactively #'opencode-cycle-session-agent)))

(defvar opencode-session-mode-map
  (define-keymap
    "C-c C-y" 'opencode-yank-code-block
    "C-c y" 'yank-media
    "C-c Y" 'opencode-copy-conversation
    "C-c C-c" 'opencode-abort-session
    "C-c C-b" 'opencode-background-all-jobs
    "C-c C-p" 'opencode-respond
    "C-c x" 'opencode-kill-session
    "<backtab>" 'backward-button
    "TAB" 'opencode--tab-dispatch
    "RET" 'opencode-send-input
    "C-<return>" 'opencode-queue-input
    "C-c R" 'opencode-rename-session
    "C-c n" 'opencode-new-session
    "C-c l" 'opencode-select-session
    "C-c c" 'opencode-select-child-session
    "C-c C" 'opencode-compact-session
    "C-c t" 'opencode-scroll-to-last-response-start
    "C-c p" 'opencode-open-parent
    "C-c f" 'opencode-add-file
    "C-c b" 'opencode-btw
    "C-c B" 'opencode-add-buffer-dwim
    "C-c m" 'opencode-select-model
    "C-c g" 'opencode-build-go
    "M-<tab>" 'opencode-cycle-variant
    ;; only bind in terminals that can't receive M-<tab>
    "C-M-i" '(menu-item "" opencode-cycle-variant
              :filter (lambda (cmd) (unless (display-graphic-p) cmd)))
    "M-<backtab>" 'opencode-cycle-variant-backwards
    "M-S-<iso-lefttab>" 'opencode-cycle-variant-backwards
    "M-S-<tab>" 'opencode-cycle-variant-backwards
    "ESC <backtab>" 'opencode-cycle-variant-backwards
    "C-c M" 'opencode-toggle-mcp
    "C-c s" 'opencode-sidebar-toggle
    "C-c F" 'opencode-fork-session
    "C-c r" 'opencode-revert-message
    "C-c e" 'opencode-ediff-turn
    "C-c C-x n" 'opencode-next-connection
    "C-c C-x p" 'opencode-prev-connection
    "C-c C-x c" 'opencode-list-connections
    "/" 'opencode-insert-slash-command
    "@" 'opencode-add-mention))

(with-eval-after-load 'evil
  (declare-function evil-define-key* "evil-core")
  (evil-define-key* 'insert opencode-session-mode-map
    "@" 'opencode-add-mention
    "/" 'opencode-insert-slash-command)
  (evil-define-key* 'normal opencode-session-mode-map
    (kbd "RET") 'opencode-send-input
    (kbd "C-<return>") 'opencode-queue-input))

(defvar-local opencode-session-pending-permission nil
  "Pending permission requests for this session buffer, as a list of plists.
Each plist has keys :id, :session-id, :type, :title, and :marker.
Requests are processed FIFO.")

(defun opencode--session-buffer-p (buf)
  "Return non-nil if BUF is a opencode session buffer.
Accepts (\"name\" . buffer) to work as a read-buffer predicate."
  (with-current-buffer (cl-etypecase buf
                         ((or string buffer) buf)
                         (list (cdr buf)))
    opencode-session-id))

(defun opencode-select-open-session ()
  "Select among open session buffers."
  (interactive)
  (switch-to-buffer
   (read-buffer "Switch to: " nil t 'opencode--session-buffer-p)))

(defun opencode-consult-sessions (all)
  "Run `consult-line-multi' across open opencode sessions of the current project.
With prefix arg ALL, search all open opencode sessions."
  (interactive "P")
  (if (fboundp 'consult-line-multi)
      (let ((root (unless all
                    (file-name-as-directory
                     (expand-file-name
                      (or opencode--project-root
                          (when-let* ((project (project-current)))
                            (project-root project))
                          default-directory))))))
        (consult-line-multi
         (list :predicate
               (lambda (buf)
                 (and (opencode--session-buffer-p buf)
                      (or all
                          (with-current-buffer buf
                            (equal (opencode--project-root) root))))))))
    (user-error "This requires consult")))

(defun opencode-visit-last-idle ()
  "Open the most recent session to notify as idle."
  (interactive)
  (if opencode-alerted-sessions
      (opencode-open-session (car opencode-alerted-sessions))
    (message "No idle sessions")))

(defconst opencode--session-preview-buffer-name "*opencode-session-preview*"
  "Name of the buffer previewing a session that isn't open.
It doesn't start with a space, since `font-lock-mode' won't turn on in
such buffers.")

(defun opencode--message-text (message)
  "Return the text parts of MESSAGE joined by blank lines, or nil if none."
  (when-let* ((texts (cl-loop for item in (alist-get 'content message)
                              for text = (alist-get 'text item)
                              when (and (equal (alist-get 'type item) "text")
                                        (stringp text)
                                        (not (string-blank-p text)))
                              collect (string-trim text))))
    (string-join texts "\n\n")))

(declare-function consult--buffer-preview "ext:consult")

(defun opencode--session-preview-state ()
  "Return a consult preview state function for selecting sessions.
An open session is previewed by showing its buffer.  Otherwise the text of
its last assistant message is fetched and shown in a temporary buffer."
  (let ((buffer-state (consult--buffer-preview))
        (texts (make-hash-table :test 'equal))
        (previewed nil))
    (cl-labels
        ((preview-buffer ()
           (or (get-buffer opencode--session-preview-buffer-name)
               (with-current-buffer
                   (get-buffer-create opencode--session-preview-buffer-name)
                 (markdown-ts-view-mode)
                 (visual-line-mode)
                 (current-buffer))))
         (fill (session text)
           (when-let* ((buffer (get-buffer opencode--session-preview-buffer-name)))
             (with-current-buffer buffer
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert "# " (opencode--session-title session) "\n\n" text "\n")
                 (goto-char (point-min))))))
         (show (session)
           (let ((id (alist-get 'id session)))
             (or (opencode--session-buffer id)
                 (let ((buffer (preview-buffer)))
                   (setq previewed id)
                   (if-let* ((text (gethash id texts)))
                       (fill session text)
                     (fill session "_Loading…_")
                     (opencode-api-last-assistant-message (id)
                         messages
                       :else (lambda (error-msg)
                               (when (equal previewed id)
                                 (fill session (format "_Couldn't fetch: %s_" error-msg))))
                       (let ((text (or (opencode--message-text (car messages))
                                       "_No reply yet_")))
                         (puthash id text texts)
                         (when (equal previewed id)
                           (fill session text)))))
                   buffer)))))
      (lambda (action session)
        (prog1 (funcall buffer-state action
                        (and (eq action 'preview) session (show session)))
          (when (eq action 'exit)
            (setq previewed nil)
            (when-let* ((buffer (get-buffer opencode--session-preview-buffer-name)))
              (kill-buffer buffer))))))))

(defun opencode--select-sessions (prompt sessions no-sessions-message)
  "Select a session from SESSIONS to open.
Use PROMPT and display NO-SESSIONS-MESSAGE if SESSIONS is empty.
With consult, \\`C-SPC' previews the session under point."
  (if sessions
      (opencode-open-session
       (opencode--annotated-completion
        prompt
        (cl-loop for session in sessions
                 collect (let-alist session
                           (list (opencode--session-title session)
                                 session
                                 (opencode--format-time-ago
                                  (opencode--time-ago
                                   session 'updated))
                                 (opencode--time-ago
                                  session 'updated))))
        :state (when (require 'consult nil t)
                 (opencode--session-preview-state))
        :preview-key "C-SPC"))
    (message no-sessions-message)))

(defun opencode-select-idle ()
  "Select a session that hasn't been visited since it went idle."
  (interactive)
  (opencode--select-sessions "Session: " opencode-alerted-sessions "No idle sessions"))

(defun opencode-select-child-session ()
  "Open a child (subagent) session of the current session."
  (interactive)
  (opencode-api-session-children (opencode-session-id)
      children
    (opencode--select-sessions "Subagent: " children "No children")))

(defun opencode-open-parent ()
  "Open the parent of the current session."
  (interactive)
  (opencode-api-session (opencode-session-id)
      session
    (if-let* ((parent-id (alist-get 'parentID session)))
        (opencode-api-session (parent-id)
            parent-session
          (opencode-open-session parent-session))
      (message "This session has no parent"))))

;;; Mode line and header line

(defun opencode--set-mode-line-option (symbol value)
  "Set SYMBOL to VALUE and update the mode line of session buffers."
  (set-default-toplevel-value symbol value)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'opencode-session-mode)
        (opencode--setup-mode-line))))
  (force-mode-line-update t))

(defcustom opencode-mode-line-process "%s🤖 %a · %m%V %c %I  "
  "Format of the session information shown in `mode-line-process'.
Expanded with `format-spec', so specs take modifiers like %>30t to
truncate the title to 30 characters.  The specs are:

  %I  busy/idle icon        %i  busy/idle
  %a  agent                 %m  model
  %v  variant               %V  variant level as dots, like ●●○
  %c  percent of the context window used
  %C  percent of the context window used, as a bar
  %u  percent of the Claude subscription's 5h usage limit used
  %U  percent of the Claude subscription's 5h usage limit used, as a bar
  %r  when the Claude subscription's 5h usage limit resets
  %T  tokens used           %t  session title
  %o  session cost          %d  session directory
  %s  server name, only when connected to more than one
  %%  a literal percent sign

%u, %U and %r are empty unless the session uses the claude-agent provider
and it has reported its usage.  %r is also empty once the limit has reset.

Other specs, like %b, are left for the mode line to expand.  Ours take
precedence over the mode line's %c, %C, %i, %I, %o, %s, %u and %U.

When the mode line is too wide for the window, this is hidden from the
mode line and shown where `overflow' is in `opencode-header-line-format',
unless `opencode-overflow-format' is non-nil.  Ignored when `opencode-mode-line-format' is non-nil.

This and `opencode-header-line-format', `opencode-overflow-format' and
`opencode-mode-line-format' can also be a function, called with no
arguments in the session buffer on each mode line update, that returns
the value to use.  For example, it could return a different value when
`opencode-claude-subscription-p'.  It should be fast."
  :type '(choice string function)
  :group 'opencode)

(defcustom opencode-header-line-format '(overflow)
  "Format of the header line of session buffers.
A list whose elements are strings expanded like
`opencode-mode-line-process', other mode line constructs, and the
symbols:

  `right-align'  right-align everything after it
  `overflow'     `opencode-overflow-format', or `opencode-mode-line-process'
                 if that is nil, when `opencode-mode-line-process' didn't
                 fit in the mode line

The header line is hidden when it would be blank, so the default only
shows one when the mode line overflows.  Can also be a function
returning the list, see `opencode-mode-line-process'."
  :type '(choice (repeat (choice string
                                 (const right-align)
                                 (const overflow)
                                 sexp))
          function)
  :group 'opencode)

(defcustom opencode-overflow-format '("%d %o%U%r" right-align  "%s🤖 %a · %m%V %T %c %I ")
  "What `overflow' in `opencode-header-line-format' shows, or nil.
It's shown when `opencode-mode-line-process' is hidden from the mode
line because the mode line is too wide.  When nil,
`opencode-mode-line-process' is shown.  Otherwise a list like
`opencode-header-line-format', except without `overflow'.  A
`right-align' in it right-aligns the rest of the header line.  Can also
be a function returning one of these, see `opencode-mode-line-process'."
  :type '(choice (const :tag "Show opencode-mode-line-process" nil)
          (repeat (choice string
                          (const right-align)
                          sexp))
          function)
  :group 'opencode)

(defcustom opencode-mode-line-format nil
  "Format of the whole mode line of session buffers, or nil.
When nil, the global `mode-line-format' is used, with session information
in `opencode-mode-line-process'.  Otherwise a list like
`opencode-header-line-format', except without `overflow', which replaces
the mode line and makes `opencode-mode-line-process' unused.  Can also be
a function returning one of these, see `opencode-mode-line-process'."
  :type '(choice (const :tag "Use mode-line-format" nil)
          (repeat (choice string
                          (const right-align)
                          sexp))
          function)
  :initialize #'custom-initialize-default
  :set #'opencode--set-mode-line-option
  :group 'opencode)

(defvar opencode--mode-line-measuring nil
  "Non-nil while measuring the mode line, so it includes everything.")

(defun opencode-claude-subscription-p (&optional session)
  "Return non-nil if SESSION uses the claude-agent provider.
SESSION defaults to the current session buffer's session.  For use in
functions that are the value of `opencode-mode-line-process' and the
other mode line and header line options."
  (equal (map-nested-elt (or session opencode-session-agent)
                         '(model providerID))
         "claude-agent"))

(defun opencode--option-value (value)
  "Return VALUE, or the result of calling it if it's a function."
  (if (functionp value) (funcall value) value))

(defun opencode--status-icon (status)
  "Return the icon of a session with STATUS, busy or idle."
  (pcase status
    ("busy" "⏳")
    ("idle" "🚀")))

(defun opencode--agent-label (agent)
  "Return the name shown for AGENT, an Agent.Info."
  (let-alist agent
    (pcase (or .name .id "-")
      ("Planner-Sisyphus" "Planner")
      (name name))))

(defun opencode--format-specs (&optional face)
  "Return the `format-spec' specs for the current session's mode line.
FACE is the face of the line they're shown in, for the background of the
context bar.  Values are marked with the `opencode--spec' property, so
their percent signs can be escaped after `format-spec' truncates them.
Values that are expensive to compute are functions, called only when used."
  (let-alist opencode-session-agent
    (let* ((model (opencode--current-model))
           (limit (map-nested-elt model '(limit context)))
           (context (if (and (numberp limit) (> limit 0))
                        (* 100 (/ (float opencode-session-tokens) limit))
                      0))
           ;; Only claude-agent reports it, it'd be misleading for others
           (claude (opencode-claude-subscription-p))
           (usage (and claude (opencode--claude-five-hour-utilization)))
           (reset (and claude (opencode--claude-window-reset-text
                               (opencode--claude-five-hour-window))))
           (mark (lambda (value)
                   (let ((value (copy-sequence (or value ""))))
                     (put-text-property 0 (length value) 'opencode--spec t value)
                     value))))
      (mapcar
       (pcase-lambda (`(,char . ,value))
         (cons char (if (functionp value)
                        (lambda () (funcall mark (funcall value)))
                      (funcall mark value))))
       `((?I . ,(opencode--status-icon opencode-session-status))
         (?i . ,opencode-session-status)
         (?a . ,(opencode--agent-label opencode-session-agent))
         (?m . ,(alist-get 'name model))
         (?v . ,(when (and .model.variant
                           (not (equal .model.variant "default")))
                  (concat " " .model.variant)))
         (?V . ,(when-let* ((ids (mapcar (lambda (variant) (alist-get 'id variant))
                                         (alist-get 'variants model)))
                            ((length> ids 1))
                            (pos (cl-position .model.variant ids :test #'equal)))
                  (concat " " (make-string pos ?●)
                          (make-string (- (length ids) 1 pos) ?○))))
         (?c . ,(format "%.0f%%" context))
         (?C . ,(lambda () (opencode--usage-bar (/ context 100) face)))
         (?u . ,(when usage (format "%d%%" (round (* 100 usage)))))
         (?U . ,(lambda () (when usage (opencode--usage-bar usage face))))
         (?r . ,(when reset (concat " " reset)))
         (?T . ,(opencode--format-tokens opencode-session-tokens))
         (?o . ,(when (plusp opencode-session-cost)
                  (format "$%.2f" opencode-session-cost)))
         (?t . ,opencode-session-title)
         (?d . ,(abbreviate-file-name default-directory))
         (?s . ,(when-let* ((label (opencode--server-label)))
                  (concat label " "))))))))

(defun opencode--mode-line-expand (string &optional specs)
  "Expand STRING with SPECS, by default `opencode--format-specs'.
The result is a mode line construct, so percent signs from specs and a
literal %% are escaped, and other specs are left for the mode line."
  (let ((result (format-spec (string-replace "%%" "%%%%" string)
                             (or specs (opencode--format-specs))
                             'ignore))
        (start 0))
    (while (setq start (string-search "%" result start))
      (if (get-text-property start 'opencode--spec result)
          (setq result (concat (substring result 0 start) "%" (substring result start))
                start (+ start 2))
        ;; skip the mode line's own construct, or an escaped %%
        (setq start (+ start 2))))
    result))

(defun opencode--mode-line-overflow-p ()
  "Return non-nil if `opencode-mode-line-process' goes in the header line."
  (and (not opencode--mode-line-measuring)
       (not (opencode--option-value opencode-mode-line-format))
       (window-parameter nil 'opencode-overflow)))

(defun opencode--line-face (line)
  "Return the face of LINE, `mode-line' or `header-line', in this window.
It's the active or inactive variant, depending on whether the window is
selected."
  (let ((active (mode-line-window-selected-p)))
    (pcase line
      ('mode-line (if active 'mode-line-active 'mode-line-inactive))
      ('header-line (if active 'header-line-active 'header-line-inactive))
      (_ line))))

(defun opencode--mode-line-process ()
  "Return `opencode-mode-line-process' expanded, if shown in the mode line."
  (if (or (opencode--option-value opencode-mode-line-format)
          (opencode--mode-line-overflow-p))
      ""
    (opencode--mode-line-expand
     (opencode--option-value opencode-mode-line-process)
     (opencode--format-specs (opencode--line-face 'mode-line)))))

(defun opencode--render-format (format face)
  "Return FORMAT, a list of mode line elements, as a mode line construct.
Strings are expanded with `opencode--format-specs', and `right-align' and
`overflow' are handled as in `opencode-header-line-format'.  FACE is the
face it's shown in, for measuring."
  (let* ((specs (opencode--format-specs (opencode--line-face face)))
         ;; Splice in the overflow first, so a `right-align' in
         ;; `opencode-overflow-format' aligns the rest of the line
         (format (mapcan
                  (lambda (element)
                    (cond ((not (eq element 'overflow)) (list element))
                          ((not (opencode--mode-line-overflow-p)) nil)
                          ((when-let* ((overflow (opencode--option-value
                                                  opencode-overflow-format)))
                             (copy-sequence (remq 'overflow overflow))))
                          (t (list (opencode--option-value
                                    opencode-mode-line-process)))))
                  format))
         (convert
          (lambda (element)
            (if (stringp element)
                (opencode--mode-line-expand element specs)
              element)))
         (tail (memq 'right-align format))
         (left (mapcar convert (take (- (length format) (length tail)) format)))
         (right (mapcar convert (remq 'right-align (cdr tail)))))
    (if tail
        `("" ,@left ,(opencode--right-align-space right face) ,@right)
      `("" ,@left))))

(defun opencode--mode-line ()
  "Return the mode line for `opencode-mode-line-format'.
It's the global `mode-line-format' if that's a function returning nil."
  (if-let* ((format (opencode--option-value opencode-mode-line-format)))
      (opencode--render-format format 'mode-line)
    (default-value 'mode-line-format)))

(defun opencode--header-line ()
  "Return the header line for `opencode-header-line-format'."
  (opencode--render-format (opencode--option-value opencode-header-line-format)
                           'header-line))

(defun opencode--update-header-line (window)
  "Show or hide the header line of WINDOW, which shows a session buffer.
It overflows the mode line when `opencode-mode-line-process' doesn't fit,
and is hidden when blank.  For `pre-redisplay-functions'."
  (when (and (windowp window) (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (let* ((overflow
            (and (not (opencode--option-value opencode-mode-line-format))
                 mode-line-format
                 (> (let ((opencode--mode-line-measuring t))
                      (string-pixel-width
                       (format-mode-line mode-line-format 'mode-line window)))
                    (- (window-pixel-width window)
                       (window-right-divider-width window)))))
           (_ (set-window-parameter window 'opencode-overflow overflow))
           (header (and (opencode--option-value opencode-header-line-format)
                        (not (string-blank-p
                              (format-mode-line '(:eval (opencode--header-line))
                                                'header-line window)))
                        '(:eval (opencode--header-line)))))
      (unless (equal header (window-parameter window 'header-line-format))
        (set-window-parameter window 'header-line-format header)
        (set-window-parameter window 'opencode-header (and header t))
        (force-mode-line-update)))))

(defun opencode--clear-stale-header-lines (frame)
  "Remove session header lines from windows of FRAME showing other buffers.
For `window-buffer-change-functions'."
  (dolist (window (window-list frame 'never))
    (when (and (window-parameter window 'opencode-header)
               (not (with-current-buffer (window-buffer window)
                      (derived-mode-p 'opencode-session-mode))))
      (set-window-parameter window 'header-line-format nil)
      (set-window-parameter window 'opencode-header nil)
      (set-window-parameter window 'opencode-overflow nil))))

(add-hook 'window-buffer-change-functions #'opencode--clear-stale-header-lines)

(defun opencode--setup-mode-line ()
  "Set up the mode line and header line of a session buffer."
  (setq-local mode-line-process '(:eval (opencode--mode-line-process)))
  (if opencode-mode-line-format
      (setq-local mode-line-format '(:eval (opencode--mode-line)))
    (kill-local-variable 'mode-line-format))
  (add-hook 'pre-redisplay-functions #'opencode--update-header-line nil t))

(defun opencode--session-title (session)
  "Return the title of SESSION, which a new session may not have yet."
  (or (alist-get 'title session) "Untitled"))

(defun opencode--session-status-icon (session)
  "Return an icon for SESSION's execution state, or nil if nothing to show.
A session is busy when its last execution started after the last one
ended, like the OpenCode app's inbox.  Sessions in
`opencode-alerted-sessions', which finished but haven't been visited,
are marked with ✦."
  (let-alist session
    (cond ((and .time.busy (or (null .time.idle) (> .time.busy .time.idle)))
           "⏳")
          ((seq-find (lambda (alerted) (equal .id (alist-get 'id alerted)))
                     opencode-alerted-sessions)
           (if (equal .outcome "failed") "❌" "✦")))))

(defun opencode-session--set-status (session-id status)
  "Set STATUS for the session with SESSION-ID and update modeline."
  (opencode--with-session-buffer session-id
    (setq opencode-session-status status)
    (opencode--update-pending-input)
    (force-mode-line-update)))

(defvar evil-buffer-regexps)

(defun opencode--dabbrev-project-buffers ()
  "Return the buffers dabbrev searches: the current project's, but not this one."
  (when-let* ((project (project-current)))
    (remq (current-buffer) (project-buffers project))))

(defvar eglot--servers-by-project)
(declare-function eglot--current-project "eglot")
(declare-function eglot--capabilities "eglot")
(declare-function eglot--request "eglot")

(defun opencode--eglot-workspace-symbols (prefix)
  "Return names of symbols matching PREFIX from the project's Eglot servers.
Return the symbol `cancelled' if input arrives before every server replies."
  (catch 'cancelled
    (seq-uniq
     (seq-mapcat
      (lambda (server)
        (when (plist-get (eglot--capabilities server) :workspaceSymbolProvider)
          ;; A server that errors, times out or has died contributes
          ;; nothing, rather than aborting the other servers and capfs.
          (let ((symbols (condition-case err
                             (eglot--request server :workspace/symbol
                                             `(:query ,prefix)
                                             :immediate t
                                             :cancel-on-input t
                                             :cancel-on-input-retval 'cancelled)
                           (error
                            (opencode--log-event
                             "WARNING eglot"
                             (format "workspace/symbol failed: %S" err))
                            nil))))
            (when (eq symbols 'cancelled)
              (throw 'cancelled 'cancelled))
            (mapcar (lambda (symbol) (plist-get symbol :name)) symbols))))
      (gethash (eglot--current-project) eglot--servers-by-project)))))

(defun opencode--eglot-capf ()
  "Complete the symbol at point from the project's Eglot servers.
Unlike Eglot's own completion, which needs a position in a file, this
asks every server for the project for workspace symbols, so it works
from a session buffer whatever languages the project uses."
  (when-let* (((bound-and-true-p eglot--servers-by-project))
              ((gethash (eglot--current-project) eglot--servers-by-project))
              (bounds (bounds-of-thing-at-point 'symbol)))
    ;; Query with the symbol at point, not the string the completion
    ;; style asks for: orderless asks for "", which makes servers like
    ;; jedi scan everything they can reach.  Servers match the query
    ;; loosely, so the result is a superset for the style to filter.
    (let ((query (buffer-substring-no-properties (car bounds) (cdr bounds)))
          (names 'missing))
      (list (car bounds) (cdr bounds)
            (completion-table-dynamic
             (lambda (_)
               (when (eq names 'missing)
                 (let ((result (opencode--eglot-workspace-symbols query)))
                   ;; Don't memoize a cancelled request.
                   (unless (eq result 'cancelled)
                     (setq names result))))
               (unless (eq names 'missing) names)))
            :exclusive 'no))))

(declare-function slime--completion-at-point "slime")

(defun opencode--slime-capf ()
  "Complete the symbol at point from the connected SLIME Lisp."
  (when (and (fboundp 'slime-connected-p) (slime-connected-p))
    ;; The Lisp may abort the request (swanky-python does outside Python
    ;; buffers), which shouldn't stop the remaining capfs.
    (when-let* ((result (ignore-errors
                          (slime--completion-at-point))))
      (if (functionp result)
          result
        (append result '(:exclusive no))))))

(declare-function capf-autosuggest-mode "capf-autosuggest")

(defvar-local opencode--project-has-elisp 'unknown
  "Whether the session's project has Emacs Lisp files, or `unknown'.")

(defun opencode--elisp-capf ()
  "Complete Emacs Lisp symbols at point, if the project has Emacs Lisp files."
  (when (eq opencode--project-has-elisp 'unknown)
    (setq opencode--project-has-elisp
          (when-let* ((project (project-current)))
            (seq-some (lambda (file) (string-suffix-p ".el" file))
                      (project-files project)))))
  ;; Not `elisp-completion-at-point', which uses ( etc as context for what
  ;; symbols to complete
  (when-let* ((opencode--project-has-elisp)
              (bounds (with-syntax-table emacs-lisp-mode-syntax-table
                        (bounds-of-thing-at-point 'symbol))))
    (list (car bounds) (cdr bounds) obarray
          :predicate (lambda (symbol)
                       (or (fboundp symbol) (boundp symbol) (facep symbol)))
          :exclusive 'no)))

(define-derived-mode opencode-session-mode comint-mode "OpenCode"
  "Major mode for interacting with an opencode session."
  (setq-local comint-use-prompt-regexp nil
              comint-input-sender 'opencode--send-input
              comint-highlight-input nil
              left-margin-width (1+ left-margin-width))
  (setq-local markdown-ts-hide-markup t
              markdown-ts-enable-code-block-context-mode nil
              markdown-ts-enable-table-mode nil)
  ;; Set up Markdown's parser and font-lock without replacing comint as the
  ;; major mode (and losing its process, prompt, and input handling).
  (markdown-ts-mode--initialize)
  ;; Markdown only parses inline markup in paragraphs, but tables render
  ;; their cells' markup too.
  (setq-local treesit-range-settings
              (append treesit-range-settings
                      (treesit-range-rules
                       :embed 'markdown-inline
                       :host 'markdown
                       :local t
                       '((pipe_table_cell) @markdown-inline))))
  ;; Index user messages instead of Markdown headings.  The index is a
  ;; cheap text property walk, so rescan it as the session grows,
  ;; whatever its size.
  ;; Complete words from the code being discussed, not just other sessions.
  (setq-local dabbrev-select-buffers-function #'opencode--dabbrev-project-buffers)
  ;; Before the global capfs, which include tags.
  (add-hook 'completion-at-point-functions #'opencode--elisp-capf nil t)
  (add-hook 'completion-at-point-functions #'opencode--eglot-capf nil t)
  (add-hook 'completion-at-point-functions #'opencode--slime-capf nil t)
  ;; `comint-completion-at-point' treats almost any word as a partial
  ;; filename and returns exclusive data, so the global capfs (tags) were
  ;; never reached.  Drop it, and run filename completion after the
  ;; global capfs instead; tags is non-exclusive, so this still fires
  ;; when nothing matches a tag.
  (remove-hook 'completion-at-point-functions #'comint-completion-at-point t)
  (add-hook 'completion-at-point-functions #'comint-filename-completion 90 t)
  ;; History autosuggestions from users' comint setups don't suit session
  ;; buffers.  Depth 100 runs this after the global `comint-mode-hook'
  ;; functions, but before `opencode-session-mode-hook', so users can still
  ;; opt back in there.
  (add-hook 'comint-mode-hook
            (lambda ()
              (when (bound-and-true-p capf-autosuggest-mode)
                (capf-autosuggest-mode -1)))
            100 t)
  (setq-local imenu-create-index-function #'opencode--imenu-create-index
              imenu-auto-rescan t
              imenu-auto-rescan-maxout most-positive-fixnum)
  ;; Markdown's conventional-mode helper uses an indirect buffer sharing all
  ;; session properties.  Mode initialization/cleanup there can interfere with
  ;; already-fontified Markdown.  Collect code faces in an isolated buffer.
  (setq-local font-lock-defaults
              '(nil nil nil nil
                (font-lock-fontify-syntactically-function
                 . opencode--fontify-markdown)))
  (setq-local font-lock-fontify-syntactically-function #'opencode--fontify-markdown)
  ;; File-reference buttons are created by comint output, not font-lock,
  ;; and prompt labels are displayed with `display'.
  (setq-local font-lock-extra-managed-props
              (cl-set-difference font-lock-extra-managed-props
                                 '(button category action help-echo display)))
  ;; Input typed after hidden markup, like a closing fence, would inherit
  ;; its `invisible', and redisplay skipping it never fontifies it visible.
  (setq-local text-property-default-nonsticky
              (cons '(invisible . t) text-property-default-nonsticky))
  (add-function :filter-return (local 'filter-buffer-substring-function)
                #'opencode--drop-stray-hidden-fence)
  (opencode--setup-mode-line)
  ;; typing while busy starts or ends pending input
  (add-hook 'after-change-functions #'opencode--update-pending-input nil t)
  ;; Rendered tables wrap their cells to fit the window.
  (add-hook 'window-size-change-functions #'opencode--tables-window-size-change nil t)
  (visual-line-mode)
  (yank-media-handler "\\`image/" #'opencode--yank-image)
  ;; A foldable block starts with a line whose first char has an
  ;; `opencode-fold' property: a marker at the end of the block.
  (setq-local hs-block-start-regexp "^."
              hs-block-end-regexp nil
              hs-looking-at-block-start-predicate
              (lambda () (and (get-text-property (point) 'opencode-fold)
                              (looking-at ".*")))
              hs-forward-sexp-function
              (lambda (_) (goto-char (get-text-property (point) 'opencode-fold)))
              hs-find-block-beginning-function #'opencode--fold-beginning
              hs-inside-comment-predicate #'ignore
              ;; comint re-marks output read-only, which would unfold it
              hs-set-up-overlay
              (lambda (ov)
                (overlay-put ov 'modification-hooks nil)
                ;; redisplay skips a `display' string after invisible
                ;; text, like markup hidden at the end of the first line
                (overlay-put ov 'before-string
                             (concat " " (opencode--fold-label ov)))
                (overlay-put ov 'display nil))
              ;; `opencode--fold-label' counts lines without hidden fences
              hs-display-lines-hidden nil)
  (hs-minor-mode)
  ;; Evil picks a buffer's initial state from its minor modes before its
  ;; major mode, so a minor mode like `hs-minor-mode' (normal state in
  ;; evil-collection) would override comint's insert state.  Buffer name
  ;; rules are checked first, so always start in insert state.
  (setq-local evil-buffer-regexps '(("" . insert))))

;; Markdown hides whole fence lines, including their newline, only in view mode.
(derived-mode-add-parents 'opencode-session-mode '(markdown-ts-view-mode))

(defun opencode--drop-stray-hidden-fence (text)
  "Return TEXT without a hidden opening fence line it starts with.
Point drawn at the start of a code block can be before its hidden
opening fence, so a copy starting there would include the fence by
accident.  Keep it if TEXT also ends with a hidden fence: the whole
block was copied on purpose."
  (let ((fence "[`~]\\{3,\\}"))
    (if (and (string-match-p (concat "\\`" fence ".*\n") text)
             (get-text-property 0 'invisible text)
             (not (and (string-match (concat "^" fence "[ \t]*\n?\\'") text)
                       (> (match-beginning 0) 0)
                       (get-text-property (match-beginning 0) 'invisible text))))
        (substring text (1+ (string-search "\n" text)))
      text)))

(defun opencode-yank-code-block ()
  "Yank the code inside the markdown code block under point."
  (interactive)
  (let* ((node (treesit-parent-until (treesit-node-at (point) 'markdown)
                                     "\\`fenced_code_block\\'" t))
         (content (and node (treesit-search-subtree
                             node "\\`code_fence_content\\'"))))
    (unless node
      (user-error "Not in a fenced code block"))
    (if content
        (copy-region-as-kill (treesit-node-start content) (treesit-node-end content))
      (kill-new ""))))

(defun opencode-copy-conversation ()
  "Copy the current conversation as a Markdown transcript.
The transcript is built from raw session messages, so it includes tool
output that isn't displayed in the session buffer, such as edit results.

Useful if you have separate usage limits for opencode vs webchat, so you can let
it gather context and start a plan in opencode, and then iterate on the plan
in webchat."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an OpenCode session"))
  (message "Fetching conversation...")
  (opencode-api-all-messages (opencode-session-id)
      messages
    (let ((transcript (opencode--conversation-to-markdown messages)))
      (kill-new transcript)
      (message "Copied conversation (%d messages, %d characters)"
               (length messages)
               (length transcript)))))

(defconst opencode--btw-instructions
  (concat "The user is asking a quick side question about the conversation so far. "
          "Answer directly and concisely in markdown from what you already know. "
          "Do not call any tools and do not take any actions.")
  "Instructions sent before a side question.
The generate endpoint exposes the session's tools but runs no tool loop, so a
tool call would give an empty answer.")

(defun opencode--show-btw-answer (question answer)
  "Pop up a new buffer showing ANSWER to QUESTION."
  (let ((buffer (generate-new-buffer
                 (format "*OpenCode btw: %s*"
                         (truncate-string-to-width
                          (string-clean-whitespace question) 50 nil nil t))))
        (opencode-open-same-window nil))
    (with-current-buffer buffer
      (insert "> " (string-replace "\n" "\n> " question) "\n\n" answer "\n")
      (markdown-ts-view-mode)
      (visual-line-mode)
      (goto-char (point-min)))
    (opencode--switch-to-buffer buffer)))

(defun opencode-btw (question)
  "Ask QUESTION about the current session without adding to its conversation.
The answer opens in a new buffer.  It can be asked while the session is busy,
then only sees the conversation up to the response in progress."
  (interactive
   (progn
     (unless opencode-session-id
       (user-error "Not in an OpenCode session"))
     (list (read-string "Side question: "))))
  (when (string-blank-p question)
    (user-error "No question given"))
  (message "Asking side question...")
  ;; Don't sync the agent, switching it would record a message in the session.
  (opencode-api-session-generate (opencode-session-id)
      `((prompt . ,(concat opencode--btw-instructions "\n\n" question)))
      result
    :else (lambda (error-msg)
            (message "Side question failed: %s" error-msg))
    (let ((answer (string-trim (or (alist-get 'text result) ""))))
      (if (string-empty-p answer)
          (message "No answer to side question, the model may have tried to call a tool")
        (message nil)
        (opencode--show-btw-answer question answer)))))

(defun opencode-kill-session (&optional session)
  "Kill SESSION."
  (interactive)
  (opencode-api-delete-session ((or (alist-get 'id session)
                                    opencode-session-id))
      _result
    ;; if called from session control buffer, don't kill buffer
    ;; but if called from a session buffer then kill this buffer
    (unless session
      (kill-buffer (current-buffer)))))

(defun opencode--session-cleanup ()
  "Clean up resources for the current session buffer."
  (when-let* ((process (get-buffer-process (current-buffer))))
    (delete-process process))
  (opencode--delete-temp-files opencode--temp-files)
  (setq opencode--temp-files nil))

(defvar opencode-session-loaded-functions nil
  "Abnormal hook run in a session buffer once its session is displayed.
Each function receives the SESSION-ID.  Events are lost when not
connected, so this is where to fetch what is pending for the session.")

(defun opencode--load-session (session &optional callback)
  "Display SESSION in the current buffer and go to a prompt, unless it's busy.
If CALLBACK is given, it will be called with SESSION afterwards."
  (let-alist session
    (opencode-api-all-messages (.id)
        messages
      (opencode--reset-turns)
      (setq opencode--replayed-unfinished-messages nil
            opencode-session-todos nil
            opencode-session-cost (or .cost 0)
            opencode--server-agent
            (or .agent
                ;; sessions migrated from v1 only have it on their messages
                (alist-get 'agent
                           (seq-find (lambda (message)
                                       (equal "assistant" (alist-get 'type message)))
                                     (reverse messages)))
                ;; a session with no agent yet uses the default, listed first
                (alist-get 'id (opencode--default-agent))))
      (opencode--replay-session-messages messages .revert.messageID)
      ;; A new session starts on the default agent, with the models chosen
      ;; for each agent where it was opened from.
      (when messages
        (opencode--adopt-selection opencode--server-agent .model))
      (opencode-api-sessions-active
        active
        (if (assq (intern .id) active)
            (setq opencode-session-status "busy")
          (setq opencode-session-status "idle")
          ;; like one from before idle markers, or that failed to record one
          (opencode--end-turn)
          (opencode--show-prompt))
        (force-mode-line-update)
        (run-hook-with-args 'opencode-session-loaded-functions .id)
        (when callback
          (funcall callback session))))))

(defun opencode--inherited-agents ()
  "Return the server's agents, for a session opened from the current buffer.
Each has the model selected for it in the current buffer when that's a
session on the same server, else its own or `opencode-last-model'."
  (let ((local (and (eq opencode-buffer-connection (opencode-current-connection))
                    ;; the current agent's entry is only updated when cycling away
                    (cons opencode-session-agent opencode-session-agents))))
    (mapcar (lambda (agent)
              (let ((agent (copy-tree agent))
                    (selected (cl-find (alist-get 'id agent) local
                                       :key (lambda (a) (alist-get 'id a))
                                       :test #'equal)))
                (setf (alist-get 'model agent)
                      (copy-alist (or (alist-get 'model selected)
                                      (alist-get 'model agent)
                                      opencode-last-model)))
                agent))
            (opencode--agents))))

(defun opencode--session-project-root (session)
  "Return the root directory of SESSION's project, as a file name on its host.
It's the session's directory without its subpath in the project, or the
directory itself when it's outside the project, such as a worktree."
  (let-alist session
    (let ((directory (directory-file-name .location.directory)))
      (opencode--from-server-path
       (file-name-as-directory
        (if (and .subpath
                 (not (member .subpath '("" ".")))
                 (not (string-prefix-p ".." .subpath))
                 (string-suffix-p (concat "/" .subpath) directory))
            (string-remove-suffix (concat "/" .subpath) directory)
          directory))))))

(defun opencode--trust-worktree (directory)
  "Trust the git worktree of DIRECTORY if its main worktree is trusted.
When the root of the repository's main worktree is in
`safe-local-variable-directories', add the root of DIRECTORY's
worktree to it, for this Emacs session only."
  (let ((default-directory directory))
    (when-let* ((root (magit-toplevel))
                (main (car (car (magit-list-worktrees))))
                (trusted-p (lambda (dir)
                             (seq-some (lambda (safe) (file-equal-p safe dir))
                                       safe-local-variable-directories))))
      (when (and (not (file-equal-p root main))
                 (funcall trusted-p main)
                 (not (funcall trusted-p root)))
        (push (file-name-as-directory root) safe-local-variable-directories)))))

(defun opencode--session-moved (moved)
  "Move the current session buffer to the location of MOVED.
MOVED is the data of a session.moved event, with the session's new
location, project and subpath.  Return non-nil if its directory changed,
as the event is sent to both its old and new locations.  Its
directory-local variables and tags table are kept."
  (let* ((server-directory (map-nested-elt moved '(location directory)))
         (directory (file-name-as-directory
                     (opencode--from-server-path server-directory))))
    (unless (equal directory default-directory)
      (setq default-directory directory
            opencode--project-root (opencode--session-project-root moved))
      (opencode--trust-worktree directory)
      (opencode--download-slash-commands directory)
      (opencode--download-skills directory)
      ;; when it's moved while idle, like from another client
      (let ((prompt (opencode--prompt-showing-p)))
        (when prompt
          (opencode--retract-prompt))
        (opencode--insert-location-switched server-directory)
        (when prompt
          (opencode--show-prompt)))
      t)))

(defun opencode--use-project-tags ()
  "Complete from the TAGS file at the session's project root, if any.
Set `tags-table-list' locally too, so etags doesn't ask whether to add
it to, or replace, the global tags tables.  Leave the global tables in
use when the project has no TAGS file, a directory-local
`tags-file-name' already chose one, or `etags-regen-mode' manages them."
  (unless (or (local-variable-p 'tags-file-name)
              (bound-and-true-p etags-regen-mode))
    (let ((tags (expand-file-name "TAGS" opencode--project-root)))
      (when (file-exists-p tags)
        (setq-local tags-file-name tags
                    tags-table-list (list tags))))))

(defun opencode--offer-relocation (session-id then)
  "Offer to move SESSION-ID, whose directory no longer exists, back to its project.
Such as a session in a worktree that's since been deleted.  It goes to the
same subdirectory of the project's main directory if that exists, otherwise
to its top-level directory.  Once moved, THEN is called with the moved
session, in the buffer current now."
  ;; Requests from the missing directory fail to find its location
  (let ((default-directory (expand-file-name "~/")))
    (opencode-api-session (session-id)
        session
      (let ((default-directory (expand-file-name "~/")))
        (opencode-api-projects projects
          (let-alist session
            (let* ((missing (opencode--from-server-path .location.directory))
                   (project (cl-find .projectID projects
                                     :key (lambda (p) (alist-get 'id p)) :test #'equal))
                   (root (and project (alist-get 'canonical project)
                              (file-name-as-directory
                               (opencode--from-server-path (alist-get 'canonical project)))))
                   (subdirectory (and root .subpath
                                      (not (member .subpath '("" ".")))
                                      (not (string-prefix-p ".." .subpath))
                                      (file-name-as-directory
                                       (expand-file-name .subpath root))))
                   (target (cond ((and subdirectory (file-directory-p subdirectory))
                                  subdirectory)
                                 ((and root (file-directory-p root)) root))))
              (unless target
                (user-error "This session's directory %s no longer exists, nor does its project's"
                            (abbreviate-file-name missing)))
              (unless (y-or-n-p (format "This session's directory %s no longer exists, move it to %s? "
                                        (abbreviate-file-name missing)
                                        (abbreviate-file-name target)))
                (user-error "Session not moved, its directory %s no longer exists"
                            (abbreviate-file-name missing)))
              (let ((default-directory target))
                (opencode-api-move-session (session-id)
                    `((directory . ,(directory-file-name (opencode--to-server-path target))))
                    _result
                  (let ((default-directory target))
                    (opencode-api-session (session-id)
                        moved
                      (funcall then moved))))))))))))

(defun opencode--relocate-session-buffer (then)
  "Offer to move the current buffer's session, if its directory no longer exists.
Return non-nil if it's offered.  Once moved, the buffer follows it, and THEN
is called with no arguments in it, if given."
  (unless (file-directory-p default-directory)
    (opencode--offer-relocation
     opencode-session-id
     (lambda (moved)
       (opencode--session-moved moved)
       (when then
         (funcall then))))
    t))

(defun opencode-send-input ()
  "Send input like `comint-send-input', unless it is empty.
The input is the current input, or with point before it, the old input
`comint-get-old-input' copies, like `comint-send-input' sends.
If the session's directory no longer exists, it offers to move the session
back to its project first, and sends once moved."
  (interactive nil opencode-session-mode)
  (let* ((pmark (process-mark (get-buffer-process (current-buffer))))
         (pending (>= (point) pmark))
         (input (if pending
                    (buffer-substring-no-properties pmark (point-max))
                  (funcall comint-get-old-input))))
    (unless (or (string-empty-p input)
                (opencode--relocate-session-buffer #'opencode-send-input))
      (when (and pending (opencode--steering-p input))
        (opencode--with-pending-input-aside
          (opencode--show-prompt)))
      (comint-send-input))))

(defun opencode-queue-input ()
  "Send input like `opencode-send-input', but queued while a turn runs.
Input sent while a turn runs steers it, the model sees it after its next
step.  Queued input waits until the turn finishes, then starts the next.
It's shown pending until then.  Slash and shell commands aren't queued."
  (interactive nil opencode-session-mode)
  (let ((opencode--input-delivery 'queue))
    (opencode-send-input)))

(cl-defun opencode-open-session (session &key callback)
  "Open comint based shell for SESSION.
Returns the buffer.  SESSION is on the current connection's server, or on
that of its `opencode-connection' if it has one.
If CALLBACK is given, it will be called with the session after it is initialized.
If SESSION's directory no longer exists, offer to move it back to its
project's directory, see `opencode--offer-relocation'.  It's opened once
moved, or if already open, its buffer follows it."
  (opencode--with-connection-of session
    (let-alist session
      (let* ((connection (opencode--conn))
             (old-buffer (opencode--session-buffer .id)))
        (cond
         (old-buffer
          (prog1 (opencode--switch-to-buffer old-buffer)
            (with-current-buffer old-buffer
              (opencode--relocate-session-buffer nil))))
         ((not (file-directory-p (opencode--from-server-path .location.directory)))
          (opencode--offer-relocation
           .id (lambda (moved) (opencode-open-session moved :callback callback)))
          nil)
         (t
          (let ((buffer (generate-new-buffer
                         (format "*OpenCode: %s*" (opencode--session-title session))))
                (agents (opencode--inherited-agents)))
            (with-current-buffer buffer
              (opencode-session-mode)
              (setq opencode-buffer-connection connection
                    opencode-session-id .id
                    opencode-last-session-buffer buffer
                    default-directory (file-name-as-directory
                                       (opencode--from-server-path .location.directory))
                    opencode--project-root (opencode--session-project-root session)
                    opencode-session-agent (opencode--default-agent agents)
                    opencode-session-agents agents
                    opencode-session-title (opencode--session-title session))
              ;; Not over Tramp, as it's slow and can be run from a process filter
              (unless (file-remote-p default-directory)
                (hack-dir-local-variables-non-file-buffer)
                (opencode--use-project-tags))
              (opencode--download-slash-commands default-directory)
              (opencode--download-skills default-directory)
              (puthash .id buffer (opencode-connection-session-buffers connection))
              (let ((proc (start-process "dummy" buffer nil)))
                (set-process-query-on-exit-flag proc nil)
                (add-hook 'kill-buffer-hook #'opencode--session-cleanup nil t)
                (opencode-insert-logo)
                (opencode--load-session session callback))
              (opencode--switch-to-buffer buffer)))))))))

(defun opencode--reload-session ()
  "Redisplay the current session from the server, keeping any typed input.
It keeps its properties, which hold the parts of its labels."
  (let ((input (buffer-substring (opencode--session-process-position)
                                 (point-max))))
    (let ((inhibit-read-only t))
      (remove-overlays)
      (erase-buffer))
    (setq opencode--current-block nil)
    (opencode--forget-queued-inputs)
    (opencode-insert-logo)
    (opencode-api-session (opencode-session-id)
        session
      (opencode--load-session
       session
       (lambda (_session)
         (when (equal opencode-session-status "idle")
           (goto-char (point-max))
           (insert input)))))))

(defun opencode--goto-prompt-input ()
  "Move point to the input start of the prompt at or before point."
  (if (get-char-property (point) 'field)
      (comint-previous-prompt 1)
    (goto-char (field-beginning))))

(defun opencode--prompt-start ()
  "Return the start of the line of the prompt before point."
  (save-excursion
    (opencode--goto-prompt-input)
    (let ((inhibit-field-text-motion t))
      (line-beginning-position))))

(defun opencode--prompt-input ()
  "Return the input of the prompt before point, keeping its labels."
  (save-excursion
    (opencode--goto-prompt-input)
    ;; replayed input ends with a newline
    (string-trim-right (opencode--input-string (point) (field-end)) "\n+")))

(defun opencode--imenu-create-index ()
  "Return an imenu index of the sent user messages in the session buffer.
Comint leaves sent input without a `field', unlike output, and older
Emacs gives it the field `input'.  Input at the prompt isn't sent yet."
  (let ((end (opencode--session-process-position))
        (pos (point-min))
        (n 0)
        index)
    (while (< pos end)
      (let ((next (min end (next-single-char-property-change pos 'field))))
        (when (memq (get-char-property pos 'field) '(nil input))
          (let* ((text (car (opencode--input-text-and-parts pos next)))
                 (line (seq-find (lambda (l) (not (string-blank-p l)))
                                 (split-string text "\n"))))
            (when line
              (push (cons (format "%d: %s" (cl-incf n)
                                  (truncate-string-to-width
                                   (string-clean-whitespace line) 60 nil nil "…"))
                          (if imenu-use-markers (copy-marker pos) pos))
                    index))))
        (setq pos next)))
    (nreverse index)))

(defun opencode--truncate-to-prompt (start text)
  "Delete from START to the end of the buffer, then prompt with TEXT as input.
The turns displayed there are forgotten."
  (opencode--forget-turns-from start)
  (let ((inhibit-read-only t))
    (remove-overlays start (point-max))
    (delete-region start (point-max)))
  (set-marker (process-mark (get-buffer-process (current-buffer))) start)
  (setq opencode--current-block nil)
  (opencode--show-prompt)
  (goto-char (point-max))
  (insert text))

(defun opencode--user-message-id-at-point ()
  "Return the id of the user message of the prompt at or before point.
Its sent input is marked with it, once the server created the message.
Signal a `user-error' if there's none, as before the first prompt or at
the input not sent yet, or if it's a shell command.  Input not sent yet
may still have the id of a message it was copied from, as after a revert."
  (pcase-let ((`(,sent . ,id)
               (save-excursion
                 (opencode--goto-prompt-input)
                 (let ((sent (and (< (point) (opencode--session-process-position))
                                  (memq (get-char-property (point) 'field)
                                        '(nil input)))))
                   (cons sent
                         (and sent
                              (get-text-property (point) 'opencode-user-message)))))))
    (cond ((eq id 'shell) (user-error "Can't do that to a shell command"))
          (id)
          (sent (user-error "No user message at point (not yet sent)"))
          (t (user-error "No user message at point")))))

(defun opencode-rename-session (&optional session)
  "Rename SESSION. If in a session buffer, rename that session."
  (interactive)
  (let* ((old-title (or (alist-get 'title session)
                        (string-remove-prefix
                         "*OpenCode: "
                         (string-trim-right (buffer-name)
                                            "\\*\\(<[0-9]+>\\)?"))))
         (title (read-string "Title: " old-title)))
    (opencode-api-rename-session ((or (alist-get 'id session)
                                      opencode-session-id))
        `((title . ,title))
        _res)))

(defun opencode--set-session-archived (session archived)
  "Archive SESSION and its subagents if ARCHIVED is non-nil, else unarchive them.
If SESSION is nil use the current session buffer's session."
  (let ((id (or (alist-get 'id session)
                opencode-session-id
                (user-error "No session"))))
    (opencode-api-archive-session (id)
        `((archived . ,(if archived t :json-false)))
        _res)
    (opencode-api-session-children (id)
        children
      (dolist (child children)
        (opencode--set-session-archived child archived)))))

(defun opencode-archive-session (&optional session)
  "Archive SESSION.  If in a session buffer, archive that session."
  (interactive)
  (opencode--set-session-archived session t))

(defun opencode-unarchive-session (&optional session)
  "Unarchive SESSION.  If in a session buffer, unarchive that session."
  (interactive)
  (opencode--set-session-archived session nil))

(defun opencode--interrupt-session-tree (id)
  "Interrupt session ID and, recursively, all the subagents it spawned."
  (opencode-api-session-children (id)
      children
    (dolist (child children)
      (opencode--interrupt-session-tree (alist-get 'id child))))
  (opencode-api-interrupt-session (id)
      _result))

(defun opencode-abort-session (&optional arg)
  "Abort a busy session and go back to prompt.
With prefix ARG, also abort all subagents it spawned, recursively."
  (interactive "P")
  (if arg
      (opencode--interrupt-session-tree opencode-session-id)
    (opencode-api-interrupt-session (opencode-session-id)
        result
      (when (opencode--json-falsy (alist-get 'interrupted result))
        (message "Failed to abort session.")))))

(defun opencode-background-all-jobs ()
  "Move the current session's blocking jobs to the background.
This covers running shell commands, subagents, Code Mode scripts and
image or video generation.  The session's turn carries on while they
keep running, and it is woken when they finish.  Does nothing if
there are none."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an OpenCode session"))
  (opencode-api-background-session (opencode-session-id)
      _result))

(defun opencode-compact-session ()
  "Compact the current session."
  (interactive)
  (if opencode-session-id
      (opencode-api-compact-session (opencode-session-id)
          (make-hash-table)
          _result)
    (user-error "Not in an OpenCode session")))

(defun opencode-scroll-to-last-response-start ()
  "Scroll to the top of the last text part of assistant response."
  (interactive)
  (unless opencode-last-response-start
    (user-error "No last response"))
  (goto-char opencode-last-response-start)
  (forward-word 1)
  (backward-word 1)
  (recenter-top-bottom 0))

(provide 'opencode-sessions)
;;; opencode-sessions.el ends here

