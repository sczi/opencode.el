;;; opencode-common.el --- Common code shared through the package  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Common code shared through the package

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'color)
(require 'json)
(require 'map)
(require 'notifications)
(require 'opencode-connection)
(require 'project)
(require 'savehist)
(require 'svg)

(defvar opencode-last-model nil
  "Last model used, as a Model.Ref alist of id, providerID and optional variant.
When nil or not available, connecting sets it to the server's default model.")
(add-to-list 'savehist-additional-variables 'opencode-last-model)

(defvar opencode-alerted-sessions nil
  "List of unvisited idle sessions.
Each has the connection of its server added as `opencode-connection'.")

(defvar opencode-last-session-buffer nil
  "Last active opencode session buffer.")

(defun opencode--normalize-directory (directory)
  "Return DIRECTORY as a canonical directory name.
Symlinks aren't resolved in remote ones, as that needs a connection, which
can't be made from the event stream's process filter."
  (file-name-as-directory (if (file-remote-p directory)
                              (expand-file-name directory)
                            (file-truename directory))))

(defun opencode--longest-backtick-run (text)
  "Return the length of the longest run of backticks in TEXT."
  (let ((start 0)
        (longest 0))
    (while (string-match "`+" text start)
      (setq longest (max longest (- (match-end 0) (match-beginning 0)))
            start (match-end 0)))
    longest))

(defun opencode--code-span (text)
  "Return TEXT as a Markdown code span."
  (let ((delimiter (make-string (1+ (opencode--longest-backtick-run text)) ?`))
        (padding (if (or (string-prefix-p "`" text)
                         (string-suffix-p "`" text))
                     " "
                   "")))
    (concat delimiter padding text padding delimiter)))

(defun opencode--fenced-block (text &optional language)
  "Return TEXT in a Markdown fenced block labeled LANGUAGE."
  (let* ((text (or text ""))
         (fence (make-string (max 3 (1+ (opencode--longest-backtick-run text)))
                             ?`)))
    (concat fence (or language "") "\n"
            text
            (unless (string-suffix-p "\n" text) "\n")
            fence)))

(defun opencode--fenced-output (text)
  "Return tool output TEXT in a fenced block.
A JSON object or array is labeled as JSON, and pretty-printed unless it's
too large to do so quickly."
  (let ((trimmed (string-trim text)))
    (if (and (string-match-p "\\`[[{]" trimmed)
             (ignore-errors (json-parse-string trimmed)))
        (opencode--fenced-block
         (or (ignore-errors
               (with-temp-buffer
                 (insert trimmed)
                 (json-pretty-print-buffer)
                 (buffer-string)))
             text)
         "json")
      (opencode--fenced-block text "text"))))

(defcustom opencode-toast-function 'opencode--default-toast-show
  "Function to use to show toast notifications.
It receives a single argument, PROPERTIES, which is an alist with string keys
for title, message, variant (error/warning/info/success), and duration"
  :type 'function
  :group 'opencode)

(defcustom opencode-open-session-same-window t
  "If t use `switch-to-buffer' when opening sessions, otherwise use `pop-to-buffer'.
This also applies to the session control buffer."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-inline-images t
  "If non-nil, display file references to images as the images.
This applies to backticked paths in assistant text and tool calls,
in graphical frames, for local servers."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-inline-image-max-size '(600 . 400)
  "Maximum (WIDTH . HEIGHT) in pixels of images shown inline."
  :type '(cons (natnum :tag "Width") (natnum :tag "Height"))
  :group 'opencode)

(defcustom opencode-session-control-table-columns
  '(:required (title branch last-update created-at)
    :optional (cost tokens cache))
  "Columns displayed in the session control table.
A plist with keys `:required' and `:optional', each a list of column
symbols.  Columns are displayed in list order, required columns first.
Optional columns are only displayed if the window is wide enough to
show all columns."
  :type (let ((column '(choice (const title)
                        (const branch)
                        (const last-update)
                        (const created-at)
                        (const cost)
                        (const tokens)
                        (const cache))))
          `(list (const :required) (repeat ,column)
            (const :optional) (repeat ,column)))
  :group 'opencode)

(defcustom opencode-claude-five-hour-budgets
  '(("pro" . 12) ("max_5x" . 125) ("max_20x" . 294))
  "Estimated usage in a 5 hour window of each Claude plan, in API priced dollars.
An alist from the planTier reported by `claude auth status' to dollars.  The
cost column of the session control table shows sessions on these plans as a
percentage of the budget as well as dollars, and sessions on other plans as
dollars only."
  :type '(alist :key-type string :value-type number)
  :group 'opencode)

(defconst opencode--apply-patch-file-header-regexp
  "^\\*\\*\\* \\(Add File\\|Update File\\|Delete File\\): \\(.*\\)$"
  "Regexp matching file operation headers in patch tool input.")

(defun opencode--time-ago (opencode-object type)
  "Return .time.TYPE value from OPENCODE-OBJECT, as seconds ago.
Returns nil if the timestamp is not present."
  (when-let* ((timestamp (map-nested-elt opencode-object `(time ,type))))
    (- (float-time) (/ timestamp 1000))))

(defun opencode--format-time-ago (seconds)
  "Format SECONDS as a human-readable duration string.
Returns \"-\" if SECONDS is nil."
  (if seconds
      (seconds-to-string seconds)
    "-"))

(defun opencode--json-falsy (value)
  "Return t if VALUE is null, :null, :false, or 0."
  (cl-case value
    ((nil :null :false 0) t)
    (t nil)))

(defun opencode--truncate-at-max-lines (max-lines)
  "Delete the first half of the buffer if we've reached MAX-LINES."
  (when (> (line-number-at-pos) max-lines)
    (goto-char (point-max))
    (forward-line (- (/ max-lines 2)))
    (delete-region (point-min) (point))))

(cl-defun opencode--annotated-completion (prompt candidates)
  "Read a candidate with PROMPT and annotations.
CANDIDATES is a list of lists, where the first element is the string to show,
the second is the value to return, the third is the annotation to show, and
the optional fourth element is a number to sort by."
  (if candidates
      (let* ((max-length (seq-max
                          (mapcar (lambda (candidate)
                                    (length (car candidate)))
                                  candidates)))
             (candidate-results (make-hash-table :test 'equal))
             (has-sort-values nil)
             (candidates (cl-loop for (name return-value maybe-annotation sort-value) in candidates
                                  for annotation = (or maybe-annotation "")
                                  for candidate = (concat name
                                                          (propertize annotation 'invisible t))
                                  when sort-value do (setf has-sort-values t)
                                  do (puthash candidate return-value candidate-results)
                                  collect (propertize candidate
                                                      'opencode-annotation
                                                      (concat (make-string (+ 5 (- max-length
                                                                                   (length name)))
                                                                           ?\s)
                                                              annotation)
                                                      'opencode-sort-value sort-value)))
             (completion-extra-properties
              `(:annotation-function
                ,(lambda (candidate)
                   (get-text-property 0 'opencode-annotation candidate))
                :display-sort-function
                ,(when has-sort-values
                   (lambda (candidates)
                     (sort candidates
                           (lambda (a b)
                             (< (get-text-property 0 'opencode-sort-value a)
                                (get-text-property 0 'opencode-sort-value b)))))))))
        (gethash
         (completing-read
          prompt
          candidates
          nil t)
         candidate-results))
    (message "%sNo completion candidates" prompt)))

(defun opencode--format-questions (questions)
  "Format QUESTIONS showing ❓ prefix for each question."
  (string-join
   (cl-loop for q being the elements of questions
            for question-text = (alist-get 'question q)
            collect (concat "❓ " question-text "\n"))))

(defvar-local opencode--project-root nil
  "Root directory of the project of the buffer's session or sessions.
It's the one the server found, with the server's Tramp prefix if it's remote.")

(defun opencode--project-root ()
  "Return the root of the current buffer's project, or `default-directory'."
  (file-name-as-directory (expand-file-name (or opencode--project-root default-directory))))

(defun opencode--relative-path-for-display (file)
  "Return FILE from the server relative to project root when possible.
It's done on the strings, as `file-relative-name' of remote files can need
a connection, and this is done for output from a process filter."
  (when file
    (let ((root (opencode--project-root)))
      ;; ~ isn't expanded as for a remote server that needs a connection
      (if (and (file-name-absolute-p file)
               (not (string-prefix-p "~" file)))
          (let ((expanded (expand-file-name (opencode--from-server-path file))))
            (if (string-prefix-p root expanded)
                (substring expanded (length root))
              file))
        file))))

(defun opencode--resolve-file-reference (file)
  "Return absolute path for FILE from the server, or nil if no file exists.
For a remote server it's resolved without checking it exists, as that's
done for all output, from a process filter.  Then it must have a directory
or extension, to not be any word, and not start with ~, which would need a
connection to expand."
  (if (opencode--tramp-prefix)
      (when (and (not (string-prefix-p "~" file))
                 (string-match-p "[/.]" file))
        (if (file-name-absolute-p file)
            (opencode--from-server-path file)
          (expand-file-name file (opencode--project-root))))
    (seq-find (lambda (candidate)
                (and (file-exists-p candidate)
                     (not (file-directory-p candidate))))
              (delete-dups
               (if (file-name-absolute-p file)
                   (list file)
                 (list (expand-file-name file (opencode--project-root))
                       (expand-file-name file default-directory)))))))

(defun opencode--visit-file-location (location)
  "Open LOCATION in another window."
  (let ((file (car location))
        (line (cdr location)))
    (unless (file-exists-p file)
      (user-error "No file %s" file))
    (find-file-other-window file)
    (goto-char (point-min))
    (forward-line (1- line))
    (recenter)))

(defun opencode--inline-image (file)
  "Return an image to display FILE inline, or nil if it shouldn't be."
  (when (and opencode-inline-images
             (display-graphic-p)
             ;; Loading a remote image needs a connection, and this is done
             ;; for output, from a process filter.
             (not (opencode--tramp-prefix))
             (image-supported-file-p file))
    (create-image file nil nil
                  :max-width (car opencode-inline-image-max-size)
                  :max-height (cdr opencode-inline-image-max-size))))

(defun opencode--buttonize-file-references (start end)
  "Turn backticked file references between START and END into buttons.
References to images are displayed as the image, see `opencode-inline-images'."
  (let ((inhibit-read-only t)
        (regexp
         (rx "`"
             (group (any alnum "._~/")
                    (* (any alnum "@%_./~+-")))
             (optional ":"
                       (group (+ digit))
                       (optional (any "-" "–" ",") (+ digit)))
             "`")))
    (save-excursion
      (goto-char start)
      (while (re-search-forward regexp end t)
        (let* ((match-beg (match-beginning 0))
               (match-end (match-end 0))
               (file-beg (match-beginning 1))
               (file (match-string-no-properties 1))
               (line-string (match-string-no-properties 2))
               (line (if line-string
                         (string-to-number line-string)
                       1))
               (path (opencode--resolve-file-reference file)))
          (when (and path (not (button-at match-beg)))
            (make-text-button
             match-beg match-end
             'action #'opencode--visit-file-location
             'button-data (cons path line)
             'follow-link t
             'help-echo (if line-string
                            (format "Open %s:%d in another window" file line)
                          (format "Open %s in another window" file))
             'mouse-face 'highlight)
            (when-let* ((image (opencode--inline-image path)))
              ;; Not over the backticks, which Markdown makes invisible,
              ;; where a display would start.
              (with-silent-modifications
                (put-text-property file-beg (1- match-end)
                                   'display image)))))))))

(defun opencode--default-toast-show (properties)
  "Default notifier to show notification with PROPERTIES from opencode."
  (let-alist properties
    (cond
     ((featurep 'dbusbind)
      (notifications-notify
       :title .title
       :body .message
       :urgency (pcase .variant
                  ((or "error" "warning" )'critical)
                  ((or "info" "success") 'normal))
       :timeout (or .duration 5000)
       :replaces-id 5647
       :app-icon 'none))
     ((eq system-type 'darwin)
      (let ((title (concat (pcase .variant
                             ("error" "❌")
                             ("warning" "⚠️")
                             ((or "info" "success") "ℹ️"))
                           " " .title)))
        (if (executable-find "terminal-notifier")
            (start-process "opencode-notification" nil "terminal-notifier"
                           "-title" title
                           "-message" .message
                           "-group" "opencode-toast"
                           "-activate" "org.gnu.Emacs"
                           "-sound" (pcase .variant
                                      ((or "error" "warning") "Basso")
                                      (_ "default")))
          (start-process "opencode-notification" nil "osascript" "-e"
                         (format "display notification %S with title %S"
                                 .message
                                 title))))))))

(defun opencode--toast-show (properties)
  "Show toast notification with PROPERTIES from opencode."
  (funcall opencode-toast-function properties))

(defun opencode--buffer-active-p (buffer)
  "Return non-nil if BUFFER is visible in the selected window of a focused frame."
  (and buffer
       (eq buffer (window-buffer (selected-window)))
       (frame-focus-state (window-frame (selected-window)))))

(defvar doom-switch-buffer-hook)

(defun opencode--switch-to-buffer (buffer)
  "Display BUFFER and select its window.
Use `switch-to-buffer' if `opencode-open-session-same-window' is non-nil,
otherwise `pop-to-buffer'.

This is often called from async callbacks, after which Emacs restores the
current buffer, so hooks that run on redisplay see the old buffer as
current and think no switch happened.  Doom relies on that to add buffers
to the current workspace, so run its switch buffer hook ourselves.  It
can't be limited to async calls, as `this-command' is left non-nil by a
minibuffer read within the callback.  When called from a command Doom runs
it again on redisplay, which is harmless."
  (if opencode-open-session-same-window
      (switch-to-buffer buffer)
    (pop-to-buffer buffer))
  (when (boundp 'doom-switch-buffer-hook)
    (with-current-buffer buffer
      (run-hooks 'doom-switch-buffer-hook))))

(defvar-local opencode-session-id nil
  "Session id for the current opencode session buffer.")

(defvar-local opencode-session-status "idle"
  "Status of the current opencode session (busy or idle).")

(defvar-local opencode-session-title nil
  "Title of the current opencode session.")

(defun opencode--session-buffer (session-id)
  "Return the live buffer of SESSION-ID on the current connection's server, or nil."
  (when-let* ((connection (opencode-current-connection))
              (buffer (gethash session-id
                               (opencode-connection-session-buffers connection))))
    (and (buffer-live-p buffer) buffer)))

(defmacro opencode--with-session-buffer (session-id &rest body)
  "Run BODY in the live session buffer for SESSION-ID."
  (declare (indent 1))
  (cl-with-gensyms (buffer)
    `(when-let* ((,buffer (opencode--session-buffer ,session-id)))
       (with-current-buffer ,buffer
         ,@body))))

(defun opencode--line-position (line)
  "Return the position of the start of LINE in the current buffer.
It's the last line if there are fewer than LINE."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defun opencode--call-preserving-lines (fn)
  "Call FN, then restore the lines of point in the current buffer and its windows.
The line of each window's start is restored too, so redrawing the whole
buffer doesn't scroll windows it isn't selected in."
  (let ((line (line-number-at-pos))
        (windows (mapcar (lambda (window)
                           (list window
                                 (line-number-at-pos (window-point window))
                                 (line-number-at-pos (window-start window))))
                         (get-buffer-window-list nil nil t))))
    (funcall fn)
    (goto-char (opencode--line-position line))
    (pcase-dolist (`(,window ,point-line ,start-line) windows)
      (when (and (window-live-p window)
                 (eq (window-buffer window) (current-buffer)))
        (set-window-start window (opencode--line-position start-line) t)
        (set-window-point window (opencode--line-position point-line))))))

(defmacro opencode--with-preserved-lines (&rest body)
  "Run BODY, then restore the lines of point in the current buffer and its windows.
For BODY that redraws the whole buffer.  See
`opencode--call-preserving-lines'."
  (declare (indent 0) (debug t))
  `(opencode--call-preserving-lines (lambda () ,@body)))

(defvar opencode-event-log-max-lines nil
  "Maximum number of lines to log in the opencode event log buffer.
Or nil to disable logging.")

(defun opencode--log-event (type event)
  "Log EVENT of TYPE to the opencode log buffer."
  (when opencode-event-log-max-lines
    (with-current-buffer (get-buffer-create "*opencode-event-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "[%s] %s: %s\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S")
                        type
                        event))
        (opencode--truncate-at-max-lines opencode-event-log-max-lines)))))

(defconst opencode--usage-bar-width 20
  "Width in characters of usage bars.")

(defun opencode--usage-bar-face (fraction)
  "Return the face for a usage bar FRACTION full."
  (cond ((>= fraction 0.85) 'error)
        ((>= fraction 0.6) 'warning)
        (t 'success)))

(defun opencode--usage-bar-svg (fraction face &optional background-face)
  "Return an SVG image of a rounded bar FRACTION full, filled in FACE's color.
Its track is blended with the background of BACKGROUND-FACE, by default
`default'."
  (let* ((width (* opencode--usage-bar-width (frame-char-width)))
         (height (max 4 (round (* 0.5 (frame-char-height)))))
         (radius (/ height 2.0))
         (background (color-name-to-rgb
                      (or (face-background (or background-face 'default)
                                           nil 'default)
                          "white")))
         (fill-rgb (or (color-name-to-rgb (face-foreground face nil t))
                       background))
         (blend (lambda (alpha)
                  (apply #'color-rgb-to-hex
                         (append (color-blend fill-rgb background alpha)
                                 '(2)))))
         (filled (* (min 1 (max 0 fraction)) width))
         (svg (svg-create width height)))
    (svg-rectangle svg 0 0 width height :fill (funcall blend 0))
    (svg-rectangle svg 0.5 0.5 (- width 1) (- height 1)
                   :rx (- radius 0.5) :fill (funcall blend 0.12)
                   :stroke (funcall blend 0.25) :stroke-width 1)
    (when (> filled 0)
      (svg-rectangle svg 0 0 (max height filled) height
                     :rx radius :fill (funcall blend 1)))
    (svg-image svg :ascent 'center)))

(defun opencode--usage-bar (fraction &optional background-face)
  "Return a bar showing FRACTION, colored by how full it is.
It is drawn with box drawing characters, displayed as an SVG on graphical
frames.  The SVG blends with the background of BACKGROUND-FACE, by default
`default'."
  (let* ((face (opencode--usage-bar-face fraction))
         (filled (round (* opencode--usage-bar-width (min 1 (max 0 fraction)))))
         (text (concat (propertize (make-string filled ?━) 'face face)
                       (propertize (make-string (- opencode--usage-bar-width
                                                   filled)
                                                ?─)
                                   'face 'shadow))))
    (if (and (display-graphic-p) (image-type-available-p 'svg))
        (propertize text 'display
                    (opencode--usage-bar-svg fraction face background-face))
      text)))

(defun opencode--claude-rate-limit ()
  "Return the current connection's latest Claude rate limit info.
It's a cons of the time it was reported and the rateLimit alist, or nil if
it isn't known.  See `opencode-connection-claude-rate-limit'."
  (when-let* ((connection (opencode-current-connection))
              (rate-limit (opencode-connection-claude-rate-limit connection))
              ((consp rate-limit)))
    rate-limit))

(defun opencode--claude-window-utilization (window)
  "Return the fraction used of Claude rate limit WINDOW.
WINDOW is an alist of utilization and resetsAt, in epoch seconds.  Once it
has reset this is 0."
  (let-alist window
    (if (and .resetsAt (<= .resetsAt (float-time)))
        0
      (or .utilization 0))))

(defun opencode--claude-window-reset (window)
  "Return when Claude rate limit WINDOW resets as a short time.
It is nil if WINDOW has no reset time or it has passed."
  (when-let* ((resets-at (alist-get 'resetsAt window))
              ((> resets-at (float-time))))
    (format-time-string (if (< (- resets-at (float-time)) (* 20 3600))
                            "%H:%M"
                          "%a %H:%M")
                        resets-at)))

(defun opencode--claude-five-hour-window ()
  "Return the Claude 5h usage limit window, nil if unknown."
  (when-let* ((rate-limit (opencode--claude-rate-limit)))
    (map-nested-elt (cdr rate-limit) '(unifiedWindows five_hour))))

(defun opencode--claude-five-hour-utilization ()
  "Return the fraction used of the Claude 5h usage limit, nil if unknown."
  (when-let* ((window (opencode--claude-five-hour-window)))
    (opencode--claude-window-utilization window)))

(provide 'opencode-common)
;;; opencode-common.el ends here
