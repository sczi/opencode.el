;;; opencode-stats.el --- Usage statistics of opencode sessions  -*- lexical-binding: t; -*-

;; Keywords: tools

;;; Commentary:

;; `opencode-stats' shows a dashboard of the server's session statistics, the
;; data of the CLI's `opencode stats': a summary, a calendar heatmap of
;; activity, a chart of usage over time stacked by model, and tables of models,
;; projects and tools.  The server only aggregates over a time range, so the
;; chart and the projects table come from a request for each bar or project.
;; In graphical frames the heatmap and chart are SVG images, whose days and
;; bars show details on hover and narrow the range to them when clicked.

;;; Code:

(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-connection)
(require 'opencode-servers)
(require 'opencode-sessions)
(require 'color)
(require 'svg)
(require 'url-util)

(defgroup opencode-stats nil
  "Usage statistics of opencode sessions."
  :group 'opencode)

(defcustom opencode-stats-timezone nil
  "IANA time zone the server groups activity by day in, like \"Europe/Paris\".
nil means Emacs's local time zone, from TZ or /etc/localtime, else UTC."
  :type '(choice (const :tag "Local" nil) string)
  :group 'opencode-stats)

(defcustom opencode-stats-colors
  '("#4e79a7" "#f28e2b" "#e15759" "#76b7b2" "#59a14f"
    "#edc948" "#b07aa1" "#ff9da7")
  "Colors of the models in the usage chart, the most used first.
The models after these are shown together as other."
  :type '(repeat color)
  :group 'opencode-stats)

(defface opencode-stats-heading
  '((t :inherit bold :height 1.1))
  "Face of section headings in the stats buffer."
  :group 'opencode-stats)

(defface opencode-stats-number
  '((t :inherit bold))
  "Face of numbers in the stats summary."
  :group 'opencode-stats)

(defface opencode-stats-heat
  '((t :inherit success))
  "Face whose foreground is the color of the busiest days in the heatmap."
  :group 'opencode-stats)

(defconst opencode-stats--other-color "#9c9c9c"
  "Color of the models past `opencode-stats-colors' in the usage chart.")

(defconst opencode-stats--metrics
  '((tokens "Tokens" opencode-stats--total-tokens)
    (output "Output tokens" opencode-stats--output-tokens)
    (steps "Steps" opencode-stats--steps)
    (cost "Cost" opencode-stats--cost))
  "Metrics the usage chart can show: (KEY TITLE FUNCTION).
FUNCTION returns the metric of a model's usage, from the stats' models.")

(defconst opencode-stats--max-concurrent 8
  "Most stats requests to make at once.")

(defvar-local opencode-stats--range nil
  "Range of the stats shown, a plist of :from, :to and :label.
:from and :to are times in seconds, :from nil for all time.")

(defvar-local opencode-stats--project nil
  "Project the stats are of, a cons of its id and label, or nil for all.")

(defvar-local opencode-stats--metric 'tokens
  "Key in `opencode-stats--metrics' of the usage chart's metric.")

(defvar-local opencode-stats--history nil
  "Previous ranges and projects, conses of them, most recent first.")

(defvar-local opencode-stats--data nil
  "The fetched stats, nil while loading.
A plist of :stats, the range's stats, :unit, the bars' time unit, :buckets,
a list of (RANGE . STATS) for each bar, and :projects, a list of
\(PROJECT . STATS).  Or (:error MESSAGE) if a request failed.")

(defvar-local opencode-stats--generation 0
  "Incremented on each load, so responses to earlier ones are ignored.")

(defvar-local opencode-stats--width nil
  "Width of the window the stats were last drawn for, in pixels.")

;;; Time ranges

(defun opencode-stats--time (time &optional hour day month)
  "Return the local time in seconds of TIME's HOUR, DAY and MONTH.
Each defaults to TIME's, and may be past its usual bounds, as they're
normalized; the minutes and seconds are 0."
  (let ((decoded (decode-time time)))
    (float-time
     (encode-time (list 0 0
                        (or hour (decoded-time-hour decoded))
                        (or day (decoded-time-day decoded))
                        (or month (decoded-time-month decoded))
                        (decoded-time-year decoded)
                        nil -1 nil)))))

(defun opencode-stats--day-start (time &optional offset)
  "Return the start of the local day OFFSET days after TIME's, in seconds."
  (opencode-stats--time time 0 (+ (decoded-time-day (decode-time time))
                                  (or offset 0))))

(defun opencode-stats--align (time unit)
  "Return the start of the UNIT TIME is in, in seconds.
UNIT is hour, day, week, which starts on Monday, or month."
  (let ((decoded (decode-time time)))
    (pcase unit
      ('hour (opencode-stats--time time))
      ('day (opencode-stats--day-start time))
      ('week (opencode-stats--day-start
              time (- (mod (+ 6 (decoded-time-weekday decoded)) 7))))
      ('month (opencode-stats--time time 0 1)))))

(defun opencode-stats--next (start unit)
  "Return the start of the UNIT after the one starting at START, in seconds."
  (let* ((decoded (decode-time start))
         (next (pcase unit
                 ('hour (opencode-stats--time start (1+ (decoded-time-hour decoded))))
                 ('day (opencode-stats--day-start start 1))
                 ('week (opencode-stats--day-start start 7))
                 ('month (opencode-stats--time start 0 1 (1+ (decoded-time-month decoded)))))))
    ;; An hour repeated as clocks go back would otherwise loop
    (if (> next start) next (+ start 3600))))

(defun opencode-stats--unit (from to)
  "Return the time unit of the bars of the usage chart from FROM to TO."
  (let ((days (/ (- to from) 86400.0)))
    (cond ((<= days 1.1) 'hour)
          ((<= days 62) 'day)
          ((<= days 280) 'week)
          (t 'month))))

(defun opencode-stats--bucket-label (start unit)
  "Return the label of the range of the UNIT starting at START."
  (pcase unit
    ('hour (format-time-string "%F %H:00" start))
    ('day (format-time-string "%a %F" start))
    ('week (format-time-string "week of %F" start))
    ('month (format-time-string "%B %Y" start))))

(defun opencode-stats--buckets (from to unit)
  "Return the ranges of the bars of the usage chart from FROM to TO.
They're of UNIT, aligned to it but clipped to FROM and TO."
  (cl-loop for start = (opencode-stats--align from unit) then end
           for end = (opencode-stats--next start unit)
           while (< start to)
           collect (list :from (max start from) :to (min end to)
                         :start start
                         :label (opencode-stats--bucket-label start unit))))

(defun opencode-stats--days-range (days)
  "Return the range of the last DAYS days, including today."
  (list :from (opencode-stats--day-start nil (- 1 days))
        :to (+ (float-time) 0.001)
        :label (if (= days 1) "today" (format "last %d days" days))
        :refresh (list #'opencode-stats--days-range days)))

(defun opencode-stats--year-range (year)
  "Return the range of YEAR, up to now if it's the current one."
  (let* ((now (float-time))
         (current (decoded-time-year (decode-time now)))
         (start (float-time (encode-time (list 0 0 0 1 1 year nil -1 nil))))
         (end (float-time (encode-time (list 0 0 0 1 1 (1+ year) nil -1 nil)))))
    (list :from start
          :to (if (= year current) (+ now 0.001) end)
          :label (if (= year current) (format "%d so far" year) (number-to-string year))
          :refresh (list #'opencode-stats--year-range year))))

(defun opencode-stats--all-range ()
  "Return the range of all time."
  (list :from nil :to (+ (float-time) 0.001) :label "all time"
        :refresh (list #'opencode-stats--all-range)))

(defun opencode-stats--refresh-range (range)
  "Return RANGE up to date, as those relative to now move with it."
  (if-let* ((refresh (plist-get range :refresh)))
      (apply refresh)
    range))

;;; Requests

(defun opencode-stats--timezone ()
  "Return the IANA time zone to group activity by day in."
  (or opencode-stats-timezone
      (let ((tz (string-remove-prefix ":" (or (getenv "TZ") ""))))
        (and (string-search "/" tz) (not (file-name-absolute-p tz)) tz))
      (let ((file (file-truename "/etc/localtime")))
        (when (string-match "zoneinfo/\\(.+\\)\\'" file)
          (match-string 1 file)))
      "UTC"))

(defun opencode-stats--ms (time)
  "Return TIME in seconds as a string of milliseconds."
  (number-to-string (round (* 1000 time))))

(defun opencode-stats--path (range project tools)
  "Return the path of the stats of RANGE and PROJECT's id, with TOOLS detail."
  (concat "/api/experimental/session/stats?"
          (url-build-query-string
           `(,@(when-let* ((from (plist-get range :from)))
                 `((from ,(opencode-stats--ms from))))
             (to ,(opencode-stats--ms (plist-get range :to)))
             ,@(when project `((project ,project)))
             (timezone ,(opencode-stats--timezone))
             (tools ,tools)))))

(defun opencode-stats--run (tasks then)
  "Run TASKS, `opencode-stats--max-concurrent' at a time, then call THEN.
Each task is a function called with a function to call with its result, and
one to call with an error message.  THEN is called with their results in
order, or if any fail with the error message of one."
  (let ((results (make-vector (length tasks) nil))
        (queue (seq-map-indexed #'cons tasks))
        (pending (length tasks))
        failed)
    (cl-labels ((finish ()
                  (when (zerop (cl-decf pending))
                    (funcall then (or failed (append results nil)))))
                (next ()
                  (when-let* ((item (pop queue)))
                    (funcall (car item)
                             (lambda (result)
                               (aset results (cdr item) result)
                               (next)
                               (finish))
                             (lambda (message)
                               (setq failed message)
                               (next)
                               (finish))))))
      (if (null tasks)
          (funcall then nil)
        (dotimes (_ (min opencode-stats--max-concurrent (length tasks)))
          (next))))))

(defun opencode-stats--fetch (paths then)
  "Request each of PATHS, then call THEN with their results in order.
If any fail, THEN is called with the error message of one instead."
  (opencode-stats--run
   (mapcar (lambda (path)
             (lambda (done fail)
               (opencode-api--request 'get path :then done :else fail)))
           paths)
   then))

(defun opencode-stats--fetch-pages (path stop then fail)
  "Call THEN with the items of PATH's pages until one STOP is true of.
PATH must already have a query string, with any order last.  STOP is called with each page's
items, and its items are included.  Call FAIL with the error message if a
request fails."
  (let (pages)
    (cl-labels ((fetch (cursor)
                  (opencode-api--request
                   'get (if cursor
                            ;; The first page's order continues, and can't be given again
                            (concat (replace-regexp-in-string "&order=[a-z]+" "" path)
                                    "&cursor=" (url-hexify-string cursor))
                          path)
                   :raw t
                   :else fail
                   :then (lambda (result)
                           (let ((page (alist-get 'data result))
                                 (next (map-nested-elt result '(cursor next))))
                             (push page pages)
                             (if (and page
                                      (not (opencode--json-falsy next))
                                      (not (funcall stop page)))
                                 (fetch next)
                               (funcall then (apply #'append (nreverse pages)))))))))
      (fetch nil))))

(defun opencode-stats--ms-time (item)
  "Return the creation time of ITEM, a session or message, in milliseconds."
  (map-nested-elt item '(time created)))

(defun opencode-stats--fetch-sessions (range project then fail)
  "Call THEN with the usage of the sessions active in RANGE.
They're those of PROJECT's id, or of all projects if it's nil.  THEN is called
with the rows of `opencode-stats--session-rows'.  Call FAIL with the error
message if a request fails."
  (let ((from (* 1000 (plist-get range :from)))
        (to (* 1000 (plist-get range :to)))
        (updated (lambda (session) (map-nested-elt session '(time updated)))))
    (opencode-stats--fetch-pages
     (concat "/api/session?limit=200"
             (if project (concat "&project=" (url-hexify-string project)) "")
             "&order=desc")
     ;; Newest updated first, so the rest weren't active since FROM
     (lambda (page) (< (funcall updated (car (last page))) from))
     (lambda (sessions)
       (let ((active (seq-filter (lambda (session)
                                   (and (>= (funcall updated session) from)
                                        (< (opencode-stats--ms-time session) to)))
                                 sessions)))
         (opencode-stats--run
          (mapcar
           (lambda (session)
             (lambda (done fail)
               (opencode-stats--fetch-pages
                (format "/api/session/%s/message?type=assistant&limit=100&order=desc"
                        (alist-get 'id session))
                (lambda (page) (< (opencode-stats--ms-time (car (last page))) from))
                (lambda (messages)
                  ;; A fork's copies of the messages before it are its
                  ;; original's usage, as in the server's stats
                  (let ((from (max from (opencode-stats--ms-time session))))
                    (funcall done (seq-filter
                                   (lambda (message)
                                     (let ((time (opencode-stats--ms-time message)))
                                       (and (<= from time) (< time to))))
                                   messages))))
                fail)))
           active)
          (lambda (results)
            (if (stringp results)
                (funcall fail results)
              (opencode-stats--with-parents
               sessions (cl-mapcar #'cons active results) then fail))))))
     fail)))

(defun opencode-stats--with-parents (sessions usage then fail)
  "Call THEN with the session rows of USAGE, fetching parents not in SESSIONS.
USAGE is a list of (SESSION . MESSAGES).  The parents of subagents are
needed to find the sessions their usage is added to."
  (let ((known (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (puthash (alist-get 'id session) session known))
    (cl-labels ((missing ()
                  (seq-uniq
                   (cl-loop for session being the hash-values of known
                            for parent = (alist-get 'parentID session)
                            when (and parent (not (opencode--json-falsy parent))
                                      (not (gethash parent known)))
                            collect parent)))
                (resolve ()
                  (if-let* ((ids (missing)))
                      (opencode-stats--fetch
                       (mapcar (lambda (id) (format "/api/session/%s" id)) ids)
                       (lambda (results)
                         (if (stringp results)
                             (funcall fail results)
                           (dolist (session results)
                             (puthash (alist-get 'id session) session known))
                           (resolve))))
                    (funcall then (opencode-stats--session-rows known usage)))))
      (resolve))))

(defun opencode-stats--session-rows (sessions usage)
  "Return the rows of the sessions table, busiest first.
SESSIONS is a hash table of sessions by id, including the parents of
subagents.  USAGE is a list of (SESSION . MESSAGES), the session's
assistant messages in the range.  A subagent's usage is added to its root
session's row.  Each row is a plist of :session, :subagents, the number of
them, :steps, :tokens, :cost, :first, the time of its first message, in
milliseconds, and :model, the name of the model of the most steps."
  (let ((rows (make-hash-table :test #'equal))
        (order nil))
    (pcase-dolist (`(,session . ,messages) usage)
      (when messages
        (let* ((root (cl-loop for current = session then parent
                              for parent-id = (alist-get 'parentID current)
                              for parent = (and parent-id
                                                (not (opencode--json-falsy parent-id))
                                                (gethash parent-id sessions))
                              while parent
                              finally return current))
               (id (alist-get 'id root))
               (row (or (gethash id rows)
                        (progn (push id order)
                               (puthash id (list :session root :subagents 0 :steps 0
                                                 :tokens 0 :cost 0 :first nil
                                                 :models nil)
                                        rows)))))
          (unless (eq root session)
            (cl-incf (plist-get row :subagents)))
          (dolist (message messages)
            (cl-incf (plist-get row :steps))
            (cl-incf (plist-get row :tokens) (opencode-stats--total-tokens message))
            (cl-incf (plist-get row :cost) (or (alist-get 'cost message) 0))
            (let ((time (opencode-stats--ms-time message)))
              (when (or (null (plist-get row :first)) (< time (plist-get row :first)))
                (setf (plist-get row :first) time)))
            (when (alist-get 'model message)
              (cl-incf (alist-get (opencode-stats--model-name message)
                                  (plist-get row :models) 0 nil #'equal))))
          (puthash id row rows))))
    (sort (mapcar (lambda (id)
                    (let ((row (gethash id rows)))
                      (plist-put row :model (car (car (sort (plist-get row :models)
                                                            :key #'cdr :reverse t))))))
                  order)
          :key (lambda (row) (list (plist-get row :tokens) (plist-get row :steps)))
          :reverse t)))

(defun opencode-stats--load ()
  "Fetch the stats of the buffer's range and project, then draw them."
  (setq opencode-stats--range (opencode-stats--refresh-range opencode-stats--range))
  (let* ((generation (cl-incf opencode-stats--generation))
         (buffer (current-buffer))
         (range opencode-stats--range)
         (project (car opencode-stats--project))
         (fail (lambda (message)
                 (setq opencode-stats--data (list :error message))
                 (opencode-stats--draw)))
         (current (lambda (function)
                    (lambda (result)
                      (when (and (buffer-live-p buffer)
                                 (= generation (buffer-local-value
                                                'opencode-stats--generation buffer)))
                        (with-current-buffer buffer
                          (if (stringp result)
                              (funcall fail result)
                            (funcall function result))))))))
    (setq opencode-stats--data nil)
    (opencode-stats--draw)
    (cl-labels
        ((details (projects stats)
           (let* ((from (/ (alist-get 'from (alist-get 'range stats)) 1000.0))
                  (to (plist-get range :to))
                  (unit (opencode-stats--unit from to))
                  (buckets (opencode-stats--buckets from to unit)))
             (opencode-stats--fetch
              (append (mapcar (lambda (bucket) (opencode-stats--path bucket project "none"))
                              buckets)
                      (mapcar (lambda (project)
                                (opencode-stats--path range (alist-get 'id project) "none"))
                              projects))
              (funcall current
                       (lambda (results)
                         (let ((data (list :stats stats
                                           :unit unit
                                           :buckets (cl-mapcar #'cons buckets results)
                                           :projects (cl-mapcar #'cons projects
                                                                (nthcdr (length buckets) results)))))
                           ;; Sessions are listed for a day or less.  They take
                           ;; the longest, so the rest is drawn while they load.
                           (when (eq unit 'hour)
                             (let ((sessions (lambda (sessions)
                                               (setf (plist-get opencode-stats--data :sessions)
                                                     sessions)
                                               (opencode-stats--draw))))
                               (setq data (append data (list :sessions 'loading)))
                               (opencode-stats--fetch-sessions
                                (list :from from :to to) project
                                (funcall current sessions)
                                ;; Not `current', which shows strings as the page's error
                                (lambda (message)
                                  (when (and (buffer-live-p buffer)
                                             (= generation (buffer-local-value
                                                            'opencode-stats--generation buffer)))
                                    (with-current-buffer buffer
                                      (funcall sessions message)))))))
                           (setq opencode-stats--data data)
                           (opencode-stats--draw)))))))
         (main (projects)
           (opencode-stats--fetch
            (list (opencode-stats--path range project "detail"))
            (funcall current (lambda (results) (details projects (car results)))))))
      (if project
          (main nil)
        (opencode-api--request 'get "/api/project"
                               :then (funcall current #'main)
                               :else (funcall current fail))))))

;;; Formatting

(defun opencode-stats--grouped (n)
  "Return integer N with its thousands separated by commas."
  (let ((digits (number-to-string (round n))))
    (while (string-match "\\([0-9]\\)\\([0-9]\\{3\\}\\)\\($\\|,\\)" digits)
      (setq digits (replace-match "\\1,\\2\\3" t nil digits)))
    digits))

(defun opencode-stats--compact (n)
  "Return N briefly, like 1.2K or 3M."
  (let ((scaled (lambda (divisor suffix)
                  (concat (string-remove-suffix ".0" (format "%.1f" (/ n divisor)))
                          suffix))))
    (cond ((< (abs n) 1000) (format "%d" (round n)))
          ((< (abs n) 999950) (funcall scaled 1e3 "K"))
          ((< (abs n) 999950000) (funcall scaled 1e6 "M"))
          (t (funcall scaled 1e9 "B")))))

(defun opencode-stats--money (n)
  "Return N dollars."
  (format "$%.2f" n))

(defun opencode-stats--duration (ms)
  "Return MS milliseconds briefly, or - if nil."
  (cond ((null ms) "-")
        ((< ms 1000) (format "%dms" (round ms)))
        ((< ms 60000) (format "%.1fs" (/ ms 1000.0)))
        (t (format "%.1fm" (/ ms 60000.0)))))

(defun opencode-stats--percent (fraction)
  "Return FRACTION as a percentage."
  (let ((percent (* 100 fraction)))
    (if (or (= percent 100) (= percent 0) (>= percent 10))
        (format "%d%%" (round percent))
      (format "%.1f%%" percent))))

(defun opencode-stats--plural (n word)
  "Return N, highlighted, and WORD, pluralized unless N is 1."
  (concat (propertize (opencode-stats--grouped n) 'face 'opencode-stats-number)
          " " word (if (= n 1) "" "s")))

(defun opencode-stats--bar (fraction width &optional face)
  "Return a bar of FRACTION of WIDTH characters in FACE, padded to WIDTH."
  (let* ((eighths (round (* 8 width (max 0 (min 1 fraction)))))
         (bar (concat (make-string (/ eighths 8) ?█)
                      (if (zerop (% eighths 8))
                          ""
                        (string (aref "▏▎▍▌▋▊▉" (1- (% eighths 8))))))))
    (string-pad (propertize bar 'face (or face 'opencode-stats-heat)) width)))

;;; Stats

(defun opencode-stats--tokens-total (tokens)
  "Return the total of TOKENS, the server's token usage."
  ;; Messages of steps that didn't finish may not have them
  (let-alist tokens
    (+ (or .input 0) (or .output 0) (or .reasoning 0)
       (or .cache.read 0) (or .cache.write 0))))

(defun opencode-stats--total-tokens (usage)
  "Return the total tokens of USAGE."
  (opencode-stats--tokens-total (alist-get 'tokens usage)))

(defun opencode-stats--output-tokens (usage)
  "Return the output and reasoning tokens of USAGE."
  (let-alist (alist-get 'tokens usage)
    (+ (or .output 0) (or .reasoning 0))))

(defun opencode-stats--steps (usage)
  "Return the steps of USAGE."
  (alist-get 'steps usage))

(defun opencode-stats--cost (usage)
  "Return the cost of USAGE."
  (alist-get 'cost usage))

(defun opencode-stats--model-name (model)
  "Return the name of MODEL, a model of the stats."
  (let-alist (alist-get 'model model)
    (concat .providerID "/" .id (when (and .variant (not (opencode--json-falsy .variant)))
                                  (concat "#" .variant)))))

(defun opencode-stats--metric-value (usage)
  "Return the chart's metric of USAGE."
  (funcall (nth 1 (alist-get opencode-stats--metric opencode-stats--metrics)) usage))

(defun opencode-stats--format-metric (n)
  "Return N of the chart's metric briefly."
  (if (eq opencode-stats--metric 'cost)
      (opencode-stats--money n)
    (opencode-stats--compact n)))

(defun opencode-stats--series ()
  "Return the models of the chart, a list of (NAME . COLOR), most used first.
The models past `opencode-stats-colors' are together as (nil . COLOR)."
  (let ((totals (make-hash-table :test #'equal)))
    (pcase-dolist (`(,_ . ,stats) (plist-get opencode-stats--data :buckets))
      (dolist (model (alist-get 'models stats))
        (cl-incf (gethash (opencode-stats--model-name model) totals 0)
                 (opencode-stats--metric-value model))))
    (let* ((names (seq-filter (lambda (name) (> (gethash name totals) 0))
                              (sort (hash-table-keys totals)
                                    :key (lambda (name) (gethash name totals))
                                    :reverse t)))
           (shown (seq-take names (length opencode-stats-colors))))
      (append (cl-mapcar #'cons shown opencode-stats-colors)
              (when (length> names (length shown))
                (list (cons nil opencode-stats--other-color)))))))

(defun opencode-stats--bucket-values (stats series)
  "Return the chart's metric of each of SERIES in STATS, a list of numbers."
  (let ((values (make-list (length series) 0))
        (other (seq-position series nil (lambda (item _) (null (car item))))))
    (dolist (model (alist-get 'models stats))
      (when-let* ((index (or (seq-position series (opencode-stats--model-name model)
                                           (lambda (item name) (equal (car item) name)))
                             other)))
        (cl-incf (nth index values) (opencode-stats--metric-value model))))
    values))

;;; Drawing

(defun opencode-stats--pixel-width ()
  "Return the width in pixels to draw charts in."
  (let ((window (get-buffer-window (current-buffer) t)))
    (- (if window (window-body-width window t) (frame-pixel-width))
       (* 2 (frame-char-width)))))

(defun opencode-stats--graphic-p ()
  "Return non-nil if charts are drawn as SVG images."
  (and (display-images-p) (image-type-available-p 'svg)))

(defun opencode-stats--color (face attribute fallback)
  "Return FACE's ATTRIBUTE color as #RRGGBB, else FALLBACK."
  (let ((color (face-attribute face attribute nil t)))
    (if-let* (((stringp color))
              (rgb (color-name-to-rgb color)))
        (apply #'color-rgb-to-hex (append rgb '(2)))
      fallback)))

(defun opencode-stats--blend (from to amount)
  "Return the color AMOUNT of the way from FROM to TO."
  (apply #'color-rgb-to-hex
         (append (cl-mapcar (lambda (a b) (+ a (* amount (- b a))))
                            (color-name-to-rgb from) (color-name-to-rgb to))
                 '(2))))

(defun opencode-stats--heat-colors ()
  "Return the colors of the heatmap's levels 0 to 4."
  (let ((background (opencode-stats--color 'default :background "#ffffff"))
        (foreground (opencode-stats--color 'default :foreground "#000000"))
        (heat (opencode-stats--color 'opencode-stats-heat :foreground "#40c463")))
    (cons (opencode-stats--blend background foreground 0.12)
          (mapcar (lambda (amount) (opencode-stats--blend background heat amount))
                  '(0.3 0.5 0.75 1.0)))))

(defun opencode-stats--svg-text (svg text x y &rest args)
  "Add TEXT at X, Y to SVG in the default face, with ARGS as `svg-text'."
  (apply #'svg-text svg text
         :x x :y y
         :font-family (face-attribute 'default :family nil t)
         :font-size (round (* 0.75 (frame-char-height)))
         :fill (opencode-stats--color 'default :foreground "#000000")
         args))

(defun opencode-stats--insert-svg (svg areas)
  "Insert SVG, whose AREAS show help and narrow the range when clicked.
AREAS is a list of (X0 Y0 X1 Y1 HELP RANGE), with RANGE nil for none."
  (let ((map (make-sparse-keymap))
        (image-map nil))
    (cl-loop for (x0 y0 x1 y1 help range) in areas
             for i from 0
             for id = (intern (format "opencode-stats-area-%d" i))
             do (push `((rect . ((,(round x0) . ,(round y0)) . (,(round x1) . ,(round y1))))
                        ,id (help-echo ,help ,@(when range '(pointer hand))))
                      image-map)
             (when range
               ;; cl-loop reuses its variables' bindings, so bind one per area
               (let ((range range))
                 (keymap-set map (format "<%s> <mouse-1>" id)
                             (lambda ()
                               (interactive)
                               (opencode-stats--set-range range))))))
    (let ((start (point)))
      (insert-image (svg-image svg :scale 1.0 :map (nreverse image-map)) " ")
      (put-text-property start (point) 'keymap map))))

(defun opencode-stats--heat-levels (activity)
  "Return a function from a day's steps to its heat level 0 to 4.
Levels are ranked among the distinct step counts of ACTIVITY, as in the CLI."
  (let* ((counts (sort (seq-uniq (seq-filter #'cl-plusp (mapcar #'cdr activity)))))
         (n (length counts)))
    (lambda (steps)
      (if (zerop steps)
          0
        (max 1 (ceiling (* 4 (/ (1+ (or (seq-position counts steps) 0)) (float n)))))))))

(defun opencode-stats--calendar (from to columns)
  "Return the heatmap's weeks from FROM to TO, at most COLUMNS of the latest.
Each is a list of 7 days, Monday first, each the day's start in seconds or nil
when outside the range.  The second value is whether weeks were left out."
  (let* ((first (opencode-stats--day-start from))
         (last (opencode-stats--day-start (- to 0.001)))
         (monday (opencode-stats--align first 'week))
         (weeks (cl-loop for week = monday then (opencode-stats--day-start week 7)
                         while (<= week last)
                         collect (cl-loop for day below 7
                                          for time = (opencode-stats--day-start week day)
                                          collect (and (<= first time last) time)))))
    (list (last weeks columns) (length> weeks columns))))

(defun opencode-stats--insert-heatmap (stats)
  "Insert the heatmap of STATS's activity."
  (let* ((activity (mapcar (lambda (day) (cons (alist-get 'date day) (alist-get 'steps day)))
                           (alist-get 'activity stats)))
         (level (opencode-stats--heat-levels activity))
         (from (/ (alist-get 'from (alist-get 'range stats)) 1000.0))
         (to (plist-get opencode-stats--range :to))
         (steps (lambda (time)
                  (or (cdr (assoc (format-time-string "%F" time) activity)) 0)))
         (day-range (lambda (time)
                      (list :from time :to (opencode-stats--day-start time 1)
                            :label (format-time-string "%a %F" time))))
         (colors (opencode-stats--heat-colors)))
    (if (opencode-stats--graphic-p)
        (let* ((ch (frame-char-height))
               (left (* 4 (frame-char-width)))
               (gap 3)
               (width (opencode-stats--pixel-width))
               (calendar (opencode-stats--calendar
                          from to (max 1 (/ (- width left) (+ 8 gap)))))
               (weeks (car calendar))
               (cell (max 8 (min (round (* 1.1 ch))
                                 (- (/ (- width left) (length weeks)) gap))))
               (top ch)
               (svg (svg-create (+ left (* (length weeks) (+ cell gap)))
                                (+ top (* 7 (+ cell gap)))))
               areas previous-month)
          (cl-loop for label in '("Mon" "Wed" "Fri")
                   for row in '(0 2 4)
                   do (opencode-stats--svg-text svg label 0 (+ top (* row (+ cell gap)) cell -1)))
          (cl-loop for week in weeks
                   for column from 0
                   for x = (+ left (* column (+ cell gap)))
                   for month = (format-time-string "%b" (or (seq-find #'identity (last week 4))
                                                            (seq-find #'identity week)))
                   do (unless (equal month previous-month)
                        (when (or previous-month (< column (- (length weeks) 3)))
                          (opencode-stats--svg-text svg month x (- top 3)))
                        (setq previous-month month))
                   (cl-loop for time in week
                            for row from 0
                            for y = (+ top (* row (+ cell gap)))
                            when time
                            do (let ((n (funcall steps time)))
                                 (svg-rectangle svg x y cell cell :rx 2
                                                :fill (nth (funcall level n) colors))
                                 (push (list x y (+ x cell) (+ y cell)
                                             (format "%s: %s" (format-time-string "%a %b %-d, %Y" time)
                                                     (opencode-stats--plural n "step"))
                                             (funcall day-range time))
                                       areas))))
          (opencode-stats--insert-svg svg areas)
          (insert "\n")
          (when (cadr calendar)
            (insert (propertize (format "last %d weeks\n" (length weeks)) 'face 'shadow))))
      (let* ((calendar (opencode-stats--calendar from to (max 1 (- (window-max-chars-per-line) 6))))
             (weeks (car calendar)))
        (cl-loop for label in '("Mo" "" "We" "" "Fr" "" "Su")
                 for row from 0
                 do (insert (string-pad label 3))
                 (dolist (week weeks)
                   (if-let* ((time (nth row week)))
                       (let ((n (funcall steps time)))
                         (insert-text-button
                          "■" 'face `(:foreground ,(nth (funcall level n) colors))
                          'help-echo (format "%s: %d steps" (format-time-string "%F" time) n)
                          'action (let ((range (funcall day-range time)))
                                    (lambda (_) (opencode-stats--set-range range)))
                          'follow-link t))
                     (insert " ")))
                 (insert "\n"))))
    (insert "   " (propertize "less " 'face 'shadow))
    (dolist (color colors)
      (insert (propertize "■" 'face `(:foreground ,color))))
    (insert (propertize " more" 'face 'shadow) "\n")))

(defun opencode-stats--nice-step (max)
  "Return a round step for 4 gridlines up to about MAX."
  (if (<= max 0)
      1
    (let* ((raw (/ max 4.0))
           (magnitude (expt 10 (floor (log raw 10))))
           (step (seq-find (lambda (m) (>= (* m magnitude) raw)) '(1 2 2.5 5 10))))
      (* step magnitude))))

(defun opencode-stats--axis-label (start unit first)
  "Return the x axis label of the bar starting at START of UNIT.
FIRST is non-nil for the first labelled bar."
  (pcase unit
    ('hour (format-time-string "%H:00" start))
    ((or 'day 'week) (format-time-string "%b %-d" start))
    ('month (format-time-string (if (or first (= 1 (decoded-time-month (decode-time start))))
                                    "%b %Y" "%b")
                                start))))

(defun opencode-stats--bar-help (range total series values)
  "Return the help of a bar of RANGE, with TOTAL and VALUES of SERIES."
  (string-join
   (cons (format "%s: %s" (plist-get range :label) (opencode-stats--format-metric total))
         (cl-loop for (name . _) in series
                  for value in values
                  when (> value 0)
                  collect (format "  %s %s" (or name "other")
                                  (opencode-stats--format-metric value))))
   "\n"))

(defun opencode-stats--insert-chart ()
  "Insert the chart of the metric over time, stacked by model."
  (let* ((buckets (plist-get opencode-stats--data :buckets))
         (unit (plist-get opencode-stats--data :unit))
         (series (opencode-stats--series))
         (values (mapcar (lambda (bucket) (opencode-stats--bucket-values (cdr bucket) series))
                         buckets))
         (totals (mapcar (lambda (v) (apply #'+ v)) values))
         (max (apply #'max 0 totals))
         (drill (not (eq unit 'hour))))
    (if (opencode-stats--graphic-p)
        (let* ((ch (frame-char-height))
               (cw (frame-char-width))
               (step (opencode-stats--nice-step max))
               (lines (max 1 (ceiling (/ max (float step)))))
               (top-value (* step lines))
               (left (* cw (1+ (length (opencode-stats--format-metric top-value)))))
               (width (opencode-stats--pixel-width))
               (plot-top (/ ch 2))
               (plot-height (* 9 ch))
               (bottom (+ plot-top plot-height))
               (n (max 1 (length buckets)))
               (column (/ (float (- width left)) n))
               (bar (max 1 (* 0.8 column)))
               (label-every (max 1 (ceiling (/ (* cw 8.0) column))))
               (grid (opencode-stats--color 'shadow :foreground "#888888"))
               (svg (svg-create width (+ bottom (* 1.5 ch))))
               areas)
          (dotimes (i (1+ lines))
            (let* ((value (* i step))
                   (y (- bottom (* plot-height (/ value (float top-value))))))
              (svg-line svg left y width y :stroke grid :stroke-width 1
                        :stroke-opacity (if (zerop i) 0.8 0.3))
              (opencode-stats--svg-text svg (opencode-stats--format-metric value)
                                        (- left cw) (+ y (/ ch 3))
                                        :text-anchor "end")))
          (cl-loop for (range . _) in buckets
                   for bucket-values in values
                   for total in totals
                   for i from 0
                   for x = (+ left (* i column) (/ (- column bar) 2))
                   do (let ((y bottom))
                        (cl-loop for value in bucket-values
                                 for (_ . color) in series
                                 for height = (* plot-height (/ value (float top-value)))
                                 when (> height 0)
                                 do (setq y (- y height))
                                 (svg-rectangle svg x y bar height :fill color)))
                   (when (zerop (% i label-every))
                     (opencode-stats--svg-text
                      svg (opencode-stats--axis-label (plist-get range :start) unit (zerop i))
                      (+ x (/ bar 2)) (+ bottom ch) :text-anchor "middle"))
                   (push (list (+ left (* i column)) plot-top (+ left (* (1+ i) column)) bottom
                               (opencode-stats--bar-help range total series bucket-values)
                               (and drill range))
                         areas))
          (opencode-stats--insert-svg svg areas)
          (insert "\n"))
      (let ((label-width (seq-max (mapcar (lambda (b) (length (plist-get (car b) :label)))
                                          buckets)))
            (bar-width (max 10 (- (window-max-chars-per-line) 40))))
        (cl-loop for (range . _) in buckets
                 for total in totals
                 do (insert-text-button (string-pad (plist-get range :label) label-width)
                                        'face 'default
                                        'action (let ((range range))
                                                  (lambda (_)
                                                    (when drill (opencode-stats--set-range range))))
                                        'follow-link t)
                 (insert " " (opencode-stats--bar (if (zerop max) 0 (/ total (float max))) bar-width)
                         " " (opencode-stats--format-metric total) "\n"))))
    (when (opencode-stats--graphic-p)
      (insert "\n")
      (cl-loop for (name . color) in series
               for first = t then nil
               do (insert (if first "" "   ")
                          (propertize "■" 'face `(:foreground ,color))
                          " " (or name "other")))
      (insert "\n"))))

(define-button-type 'opencode-stats-row
  'face nil
  'mouse-face 'highlight
  'follow-link t)

(defun opencode-stats--insert-table (columns rows &optional actions help)
  "Insert a table of ROWS with COLUMNS, a list of (NAME ALIGN).
ALIGN is left or right.  Each row is a list of strings.  ACTIONS is a list
of a function for each row, called with no arguments when its line is
clicked or RET is pressed on it, with HELP as its help."
  (let ((widths (cl-loop for (name) in columns
                         for i from 0
                         collect (apply #'max (string-width name)
                                        (mapcar (lambda (row) (string-width (nth i row)))
                                                rows))))
        (actions (cons nil actions)))
    (dolist (row (cons (mapcar #'car columns) rows))
      (insert "  ")
      (cl-loop for cell in row
               for (_ align) in columns
               for width in widths
               for first = t then nil
               do (insert (if first "" "  ")
                          (if (eq align 'right)
                              (string-pad cell width nil t)
                            (string-pad cell width))))
      (delete-horizontal-space)
      (when-let* ((action (pop actions)))
        (make-text-button (+ 2 (line-beginning-position)) (point)
                          :type 'opencode-stats-row
                          'action (lambda (_) (funcall action))
                          'help-echo help))
      (insert "\n"))
    (save-excursion
      (forward-line (- (1+ (length rows))))
      (put-text-property (point) (line-end-position) 'face 'shadow))))

(defun opencode-stats--insert-heading (title)
  "Insert section heading TITLE."
  (insert "\n" (propertize title 'face 'opencode-stats-heading) "\n"))

(defun opencode-stats--insert-summary (stats)
  "Insert the summary of STATS."
  (let-alist stats
    (let* ((cache-rate (opencode--cache-rate stats))
           (finished (+ (or .tools.totals.succeeded 0) (or .tools.totals.failed 0))))
      (insert (string-join
               (delq nil (list (opencode-stats--plural .sessions "session")
                               (when (> .subagents 0)
                                 (opencode-stats--plural .subagents "subagent"))
                               (opencode-stats--plural .prompts "prompt")
                               (opencode-stats--plural .steps "step")))
               " · ")
              "\n"
              (propertize (opencode-stats--compact (opencode-stats--tokens-total .tokens))
                          'face 'opencode-stats-number)
              " tokens"
              (if (> .cost 0)
                  (concat " · " (propertize (opencode-stats--money .cost)
                                            'face 'opencode-stats-number))
                "")
              (if cache-rate
                  (concat " · " (propertize (opencode-stats--percent cache-rate)
                                            'face 'opencode-stats-number)
                          " cache hit")
                "")
              "\n"
              (propertize (format "input %s · output %s · reasoning %s · cache read %s · cache write %s"
                                  (opencode-stats--compact .tokens.input)
                                  (opencode-stats--compact .tokens.output)
                                  (opencode-stats--compact .tokens.reasoning)
                                  (opencode-stats--compact .tokens.cache.read)
                                  (opencode-stats--compact .tokens.cache.write))
                          'face 'shadow)
              "\n"
              (if (> finished 0)
                  (concat (propertize (opencode-stats--percent (/ .tools.totals.succeeded
                                                                  (float finished)))
                                      'face 'opencode-stats-number)
                          " tool success · ")
                "")
              (opencode-stats--plural .activeDays "active day")
              " · best streak "
              (opencode-stats--plural .streak "day")
              "\n"))))

(defun opencode-stats--insert-models (stats)
  "Insert the table of STATS's models."
  (let* ((models (alist-get 'models stats))
         (series (opencode-stats--series))
         (total (apply #'+ (mapcar #'opencode-stats--total-tokens models)))
         (cost (> (alist-get 'cost stats) 0)))
    (opencode-stats--insert-table
     `(("Model" left) ("Steps" right) ("Tokens" right) ("Output" right) ("Cache hit" right)
       ,@(when cost '(("Cost" right))) ("Share" left))
     (mapcar (lambda (model)
               (let ((name (opencode-stats--model-name model)))
                 `(,(concat (propertize "■" 'face
                                        `(:foreground
                                          ,(or (alist-get name series nil nil #'equal)
                                               opencode-stats--other-color)))
                            " " name)
                   ,(opencode-stats--grouped (opencode-stats--steps model))
                   ,(opencode-stats--compact (opencode-stats--total-tokens model))
                   ,(opencode-stats--compact (opencode-stats--output-tokens model))
                   ,(if-let* ((rate (opencode--cache-rate model)))
                        (opencode-stats--percent rate)
                      "-")
                   ,@(when cost (list (opencode-stats--money (opencode-stats--cost model))))
                   ,(opencode-stats--bar (if (zerop total) 0
                                           (/ (opencode-stats--total-tokens model)
                                              (float total)))
                                         20))))
             models))))

(defun opencode-stats--project-label (project)
  "Return the label of PROJECT, a project of the server."
  (abbreviate-file-name
   (opencode--from-server-path (or (alist-get 'canonical project)
                                   (alist-get 'id project)))))

(defun opencode-stats--insert-projects ()
  "Insert the table of the projects' stats, busiest first."
  (let* ((projects (sort (seq-filter (lambda (item) (> (alist-get 'steps (cdr item)) 0))
                                     (plist-get opencode-stats--data :projects))
                         :key (lambda (item) (alist-get 'steps (cdr item)))
                         :reverse t))
         (max (apply #'max 1 (mapcar (lambda (item) (alist-get 'steps (cdr item))) projects))))
    (opencode-stats--insert-table
     '(("Project" left) ("Sessions" right) ("Steps" right) ("Tokens" right) ("" left))
     (mapcar (pcase-lambda (`(,project . ,stats))
               (list (opencode-stats--project-label project)
                     (opencode-stats--grouped (alist-get 'sessions stats))
                     (opencode-stats--grouped (alist-get 'steps stats))
                     (opencode-stats--compact (opencode-stats--total-tokens stats))
                     (opencode-stats--bar (/ (alist-get 'steps stats) (float max)) 20)))
             projects)
     (mapcar (pcase-lambda (`(,project . ,_))
               (lambda ()
                 (opencode-stats--set-project
                  (cons (alist-get 'id project) (opencode-stats--project-label project)))))
             projects)
     "Show this project's stats")))

(defun opencode-stats--open-session (session)
  "Open SESSION, in a buffer in its directory."
  (let ((default-directory (opencode--from-server-path
                            (file-name-as-directory
                             (map-nested-elt session '(location directory))))))
    (opencode-open-session session)))

(defun opencode-stats--insert-sessions ()
  "Insert the table of the sessions active in the range, busiest first.
The data's :sessions are the rows, `loading', or an error message."
  (let ((rows (plist-get opencode-stats--data :sessions)))
    (cond
     ((eq rows 'loading) (insert "  Loading…\n"))
     ((stringp rows) (insert "  " (propertize rows 'face 'error) "\n"))
     ((null rows) (insert "  No sessions\n"))
     (t
      (opencode-stats--insert-table
       '(("Time" left) ("Session" left) ("Project" left) ("Steps" right)
         ("Tokens" right) ("Model" left))
       (mapcar (lambda (row)
                 (let ((session (plist-get row :session))
                       (subagents (plist-get row :subagents)))
                   (list (format-time-string "%H:%M" (/ (plist-get row :first) 1000.0))
                         (concat (truncate-string-to-width
                                  (opencode--session-title session) 50 nil nil "…")
                                 (if (> subagents 0)
                                     (propertize (format "  +%d subagent%s" subagents
                                                         (if (= subagents 1) "" "s"))
                                                 'face 'shadow)
                                   ""))
                         (abbreviate-file-name
                          (directory-file-name (opencode--session-project-root session)))
                         (opencode-stats--grouped (plist-get row :steps))
                         (opencode-stats--compact (plist-get row :tokens))
                         (or (plist-get row :model) ""))))
               rows)
       (mapcar (lambda (row)
                 (let ((session (plist-get row :session)))
                   (lambda () (opencode-stats--open-session session))))
               rows)
       "Open this session")))))

(defun opencode-stats--insert-tools (stats)
  "Insert the table of STATS's tools."
  (let* ((tools (map-nested-elt stats '(tools usage)))
         (max (apply #'max 1 (mapcar (lambda (tool) (alist-get 'calls tool)) tools))))
    (opencode-stats--insert-table
     '(("Tool" left) ("Calls" right) ("Failed" right) ("Fail rate" right)
       ("Median" right) ("" left))
     (mapcar (lambda (tool)
               (let-alist tool
                 (let* ((finished (+ .succeeded .failed))
                        (rate (if (zerop finished) 0 (/ .failed (float finished)))))
                   (list .name
                         (opencode-stats--grouped .calls)
                         (opencode-stats--grouped .failed)
                         (propertize (opencode-stats--percent rate)
                                     'face (if (> rate 0.05) 'warning 'default))
                         (opencode-stats--duration .durationP50)
                         (opencode-stats--bar (/ .calls (float max)) 20)))))
             tools))))

(defvar-keymap opencode-stats-mode-map
  "w" #'opencode-stats-week
  "m" #'opencode-stats-month
  "y" #'opencode-stats-year
  "a" #'opencode-stats-all
  "D" #'opencode-stats-days
  "p" #'opencode-stats-toggle-project
  "t" #'opencode-stats-cycle-metric
  "l" #'opencode-stats-back
  "TAB" #'forward-button
  "<backtab>" #'backward-button)

(defvar opencode-stats--help-map nil
  "Keymap whose bindings `opencode-stats--keys-help' shows.")

(defvar evil-state)
(declare-function evil-get-auxiliary-keymap "evil-core")

(defun opencode-stats--keys-help ()
  "Return the help line of the stats buffer's keys."
  (let ((opencode-stats--help-map
         (if (bound-and-true-p evil-local-mode)
             ;; Evil's bindings for the mode's map, which shadow its own
             (make-composed-keymap
              (evil-get-auxiliary-keymap opencode-stats-mode-map (or evil-state 'normal))
              opencode-stats-mode-map)
           opencode-stats-mode-map)))
    (substitute-command-keys
     (concat "\\<opencode-stats--help-map>"
             "\\[opencode-stats-week] week  \\[opencode-stats-month] month  "
             "\\[opencode-stats-year] year  \\[opencode-stats-all] all  "
             "\\[opencode-stats-days] days  \\[opencode-stats-toggle-project] project  "
             "\\[opencode-stats-cycle-metric] metric  \\[opencode-stats-back] back  "
             "\\[revert-buffer] refresh"))))

(defun opencode-stats--draw ()
  "Draw the stats in the current buffer."
  (setq opencode-stats--width (opencode-stats--pixel-width))
  (let ((inhibit-read-only t)
        (data opencode-stats--data))
    (opencode--with-preserved-lines
      (erase-buffer)
      (insert (propertize "OpenCode stats" 'face 'opencode-stats-heading)
              (propertize (format " · %s · %s%s"
                                  (plist-get opencode-stats--range :label)
                                  (or (cdr opencode-stats--project) "all projects")
                                  (if-let* ((label (opencode--server-label)))
                                      (format " · %s" label)
                                    ""))
                          'face 'shadow)
              "\n"
              (propertize (opencode-stats--keys-help) 'face 'shadow)
              "\n\n")
      (cond
       ((null data) (insert "Loading…\n"))
       ((plist-get data :error) (insert (propertize (plist-get data :error) 'face 'error) "\n"))
       ((zerop (alist-get 'steps (plist-get data :stats)))
        (insert "No activity in this range\n"))
       (t
        (let ((stats (plist-get data :stats)))
          (opencode-stats--insert-summary stats)
          (when (plist-member data :sessions)
            (opencode-stats--insert-heading "Sessions")
            (opencode-stats--insert-sessions))
          ;; A heatmap of a day or less would be a single cell
          (unless (eq (plist-get data :unit) 'hour)
            (opencode-stats--insert-heading "Activity")
            (opencode-stats--insert-heatmap stats))
          (opencode-stats--insert-heading
           (format "%s per %s" (car (alist-get opencode-stats--metric opencode-stats--metrics))
                   (plist-get data :unit)))
          (opencode-stats--insert-chart)
          (opencode-stats--insert-heading "Models")
          (opencode-stats--insert-models stats)
          (when (plist-get data :projects)
            (opencode-stats--insert-heading "Projects")
            (opencode-stats--insert-projects))
          (when (map-nested-elt stats '(tools usage))
            (opencode-stats--insert-heading "Tools")
            (opencode-stats--insert-tools stats))))))))

(defun opencode-stats--window-size-changed (window)
  "Redraw the stats shown in WINDOW if its width changed."
  (with-current-buffer (window-buffer window)
    (when (and opencode-stats--data
               (not (eql opencode-stats--width (opencode-stats--pixel-width))))
      (opencode-stats--draw))))

;;; Commands

(defun opencode-stats--set-range (range)
  "Show the stats of RANGE, keeping the previous one for `opencode-stats-back'."
  (push (cons opencode-stats--range opencode-stats--project) opencode-stats--history)
  (setq opencode-stats--range range)
  (opencode-stats--load))

(defun opencode-stats--set-project (project)
  "Show the stats of PROJECT, a cons of its id and label, or nil for all."
  (push (cons opencode-stats--range opencode-stats--project) opencode-stats--history)
  (setq opencode-stats--project project)
  (opencode-stats--load))

(defun opencode-stats-week ()
  "Show the stats of the last 7 days."
  (interactive)
  (opencode-stats--set-range (opencode-stats--days-range 7)))

(defun opencode-stats-month ()
  "Show the stats of the last 30 days."
  (interactive)
  (opencode-stats--set-range (opencode-stats--days-range 30)))

(defun opencode-stats-days (days)
  "Show the stats of the last DAYS days, including today."
  (interactive (list (read-number "Days: " 90)))
  (opencode-stats--set-range (opencode-stats--days-range (max 1 days))))

(defun opencode-stats-year (year)
  "Show the stats of this year so far, or with a prefix argument YEAR's."
  (interactive (list (if current-prefix-arg
                         (read-number "Year: " (1- (decoded-time-year (decode-time))))
                       (decoded-time-year (decode-time)))))
  (opencode-stats--set-range (opencode-stats--year-range year)))

(defun opencode-stats-all ()
  "Show the stats of all time."
  (interactive)
  (opencode-stats--set-range (opencode-stats--all-range)))

(defun opencode-stats-toggle-project ()
  "Toggle between the stats of all projects and of `default-directory''s."
  (interactive)
  (if opencode-stats--project
      (opencode-stats--set-project nil)
    (opencode-api--cached-location
     (lambda (location)
       (let-alist location
         (opencode-stats--set-project
          (cons .project.id
                (abbreviate-file-name
                 (opencode--from-server-path (or .project.canonical .directory))))))))))

(defun opencode-stats-cycle-metric ()
  "Cycle the metric of the usage chart."
  (interactive)
  (let ((keys (mapcar #'car opencode-stats--metrics)))
    ;; Cost is all 0 for subscriptions and free models
    (unless (> (or (map-nested-elt opencode-stats--data '(:stats cost)) 0) 0)
      (setq keys (delq 'cost keys)))
    (setq opencode-stats--metric
          (or (cadr (memq opencode-stats--metric keys)) (car keys))))
  (message "Chart shows %s" (downcase (car (alist-get opencode-stats--metric
                                                      opencode-stats--metrics))))
  (when (plist-get opencode-stats--data :buckets)
    (opencode-stats--draw)))

(defun opencode-stats-back ()
  "Show the stats of the previous range and project."
  (interactive)
  (pcase-let ((`(,range . ,project) (or (pop opencode-stats--history)
                                        (user-error "No previous stats"))))
    (setq opencode-stats--range range
          opencode-stats--project project)
    (opencode-stats--load)))

(define-derived-mode opencode-stats-mode special-mode "OpenCode Stats"
  "Mode showing usage statistics of opencode sessions.
Click a day of the heatmap, a bar of the chart or a project to narrow the
stats to it.

\\{opencode-stats-mode-map}"
  (setq-local revert-buffer-function (lambda (&rest _) (opencode-stats--load))
              truncate-lines t
              doom-real-buffer-p t)
  (add-hook 'window-size-change-functions #'opencode-stats--window-size-changed nil t))

(with-eval-after-load 'evil
  (declare-function evil-define-key* "evil-core")
  (evil-define-key* 'normal opencode-stats-mode-map
    "w" #'opencode-stats-week
    "m" #'opencode-stats-month
    "y" #'opencode-stats-year
    "a" #'opencode-stats-all
    "D" #'opencode-stats-days
    "p" #'opencode-stats-toggle-project
    "t" #'opencode-stats-cycle-metric
    "l" #'opencode-stats-back
    "gr" #'revert-buffer
    "q" #'quit-window))

(defun opencode-stats--buffer (connection)
  "Return the stats buffer of CONNECTION's server, creating it if needed."
  (or (seq-find (lambda (buffer)
                  (with-current-buffer buffer
                    (and (derived-mode-p 'opencode-stats-mode)
                         (eq opencode-buffer-connection connection))))
                (buffer-list))
      (generate-new-buffer
       (format "*OpenCode Stats%s*" (opencode--server-tag connection)))))

;;;###autoload
(defun opencode-stats (&optional project)
  "Show usage statistics of opencode sessions for this year so far.
They're of all projects, or with a prefix argument PROJECT of the project of
`default-directory'.  See `opencode-stats-mode' for its keys."
  (interactive "P")
  (let ((directory default-directory))
    (opencode-autoconnect
     (lambda ()
       (let ((connection (opencode--conn)))
         (with-current-buffer (opencode-stats--buffer connection)
           (unless (derived-mode-p 'opencode-stats-mode)
             (opencode-stats-mode)
             (setq opencode-stats--range (opencode-stats--year-range
                                          (decoded-time-year (decode-time)))))
           (setq default-directory directory
                 opencode-buffer-connection connection)
           (opencode--switch-to-buffer (current-buffer))
           (if project
               (progn (setq opencode-stats--project nil)
                      (opencode-stats-toggle-project))
             (opencode-stats--load))))))))

(provide 'opencode-stats)
;;; opencode-stats.el ends here
