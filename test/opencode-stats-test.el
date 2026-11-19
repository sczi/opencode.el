;;; opencode-stats-test.el --- Tests of the opencode stats buffer  -*- lexical-binding: t; -*-
;;; Code:

(require 'ert)
(require 'opencode)
(require 'opencode-stats)

(defmacro opencode-stats-test--in-zone (&rest body)
  "Run BODY with the local time zone a fixed one with DST."
  (declare (indent 0) (debug t))
  `(let ((zone (getenv "TZ")))
     (unwind-protect
         (progn (setenv "TZ" "America/New_York") ,@body)
       (setenv "TZ" zone))))

(defun opencode-stats-test--time (string)
  "Return local time STRING, like \"2026-03-08 00:00\", in seconds."
  (float-time (date-to-time string)))

(ert-deftest opencode-stats-buckets-test ()
  "Bars are aligned to their unit, clipped to the range, and cross DST."
  (opencode-stats-test--in-zone
    (let ((from (opencode-stats-test--time "2026-03-04 10:00"))
          (to (opencode-stats-test--time "2026-03-20 12:00")))
      (should (eq 'day (opencode-stats--unit from to)))
      (let ((weeks (opencode-stats--buckets from to 'week)))
        (should (equal '("week of 2026-03-02" "week of 2026-03-09" "week of 2026-03-16")
                       (mapcar (lambda (b) (plist-get b :label)) weeks)))
        (should (= from (plist-get (car weeks) :from)))
        (should (= to (plist-get (car (last weeks)) :to)))
        ;; Weeks start at local midnight, so the one with the DST change on
        ;; March 8 is an hour short
        (should (= (opencode-stats-test--time "2026-03-09 00:00")
                   (plist-get (nth 1 weeks) :from)))
        (should (= (- (* 7 86400) 3600)
                   (- (plist-get (nth 1 weeks) :start) (plist-get (nth 0 weeks) :start)))))
      (should (equal 23 (length (opencode-stats--buckets
                                 (opencode-stats-test--time "2026-03-08 00:00")
                                 (opencode-stats-test--time "2026-03-09 00:00")
                                 'hour))))
      (should (equal '("January 2026" "February 2026")
                     (mapcar (lambda (b) (plist-get b :label))
                             (opencode-stats--buckets
                              (opencode-stats-test--time "2026-01-15 00:00")
                              (opencode-stats-test--time "2026-02-03 00:00")
                              'month)))))))

(ert-deftest opencode-stats-calendar-test ()
  "The heatmap's weeks start on Monday, with days outside the range nil."
  (opencode-stats-test--in-zone
    (pcase-let ((`(,weeks ,clipped)
                 (opencode-stats--calendar (opencode-stats-test--time "2026-03-04 10:00")
                                           (opencode-stats-test--time "2026-03-10 12:00")
                                           10)))
      (should-not clipped)
      (should (= 2 (length weeks)))
      (should (equal '(nil nil "03-04" "03-05" "03-06" "03-07" "03-08")
                     (mapcar (lambda (time) (and time (format-time-string "%m-%d" time)))
                             (car weeks))))
      (should (equal '("03-09" "03-10" nil nil nil nil nil)
                     (mapcar (lambda (time) (and time (format-time-string "%m-%d" time)))
                             (cadr weeks)))))
    (pcase-let ((`(,weeks ,clipped)
                 (opencode-stats--calendar (opencode-stats-test--time "2026-01-01 00:00")
                                           (opencode-stats-test--time "2026-03-10 00:00")
                                           3)))
      (should clipped)
      (should (= 3 (length weeks)))
      (should (equal "03-09" (format-time-string "%m-%d" (car (car (last weeks)))))))))

(ert-deftest opencode-stats-heat-levels-test ()
  "Heat levels rank a day's steps among the distinct counts, as in the CLI."
  (let ((level (opencode-stats--heat-levels '(("a" . 1) ("b" . 5) ("c" . 5) ("d" . 50)))))
    (should (equal '(0 2 3 4) (mapcar level '(0 1 5 50))))))

(ert-deftest opencode-stats-formatting-test ()
  "Numbers are shown briefly."
  (should (equal "999" (opencode-stats--compact 999)))
  (should (equal "1.5K" (opencode-stats--compact 1500)))
  (should (equal "40M" (opencode-stats--compact 40000000)))
  (should (equal "1M" (opencode-stats--compact 999999)))
  (should (equal "1,234,567" (opencode-stats--grouped 1234567)))
  (should (equal "16ms" (opencode-stats--duration 16)))
  (should (equal "2.5s" (opencode-stats--duration 2500)))
  (should (equal "-" (opencode-stats--duration nil)))
  (should (equal "6.6%" (opencode-stats--percent 0.066)))
  (should (equal "29%" (opencode-stats--percent 0.29)))
  (should (= 250000 (opencode-stats--nice-step 1000000)))
  (should (= 2 (opencode-stats--nice-step 7))))

(ert-deftest opencode-stats-cache-rate-test ()
  "The cache hit rate is the share of input tokens read from cache."
  (should (= 0.75 (opencode--cache-rate
                   '((tokens (input . 10) (output . 99) (reasoning . 0)
                      (cache (read . 60) (write . 10)))))))
  (should-not (opencode--cache-rate
               '((tokens (input . 0) (output . 5) (reasoning . 0)
                  (cache (read . 0) (write . 0)))))))

(ert-deftest opencode-stats-bar-test ()
  "Bars are drawn in eighths of a character and padded to their width."
  (should (equal "██▌       " (substring-no-properties (opencode-stats--bar 0.25 10))))
  (should (equal "    " (substring-no-properties (opencode-stats--bar 0 4)))))

(ert-deftest opencode-stats-svg-areas-test ()
  "Clicking each area of an image narrows the stats to that area's range."
  (with-temp-buffer
    ;; Images with maps can't be made without a window system, as in batch
    (cl-letf (((symbol-function 'svg-image)
               (lambda (_svg &rest props) `(image :type svg :data "" ,@props))))
      (opencode-stats--insert-svg (svg-create 20 10)
                                  '((0 0 10 10 "a" (:label "a"))
                                    (10 0 20 10 "b" (:label "b"))
                                    (0 0 1 1 "none" nil))))
    (let ((map (get-text-property (point-min) 'keymap))
          ranges)
      (cl-letf (((symbol-function 'opencode-stats--set-range)
                 (lambda (range) (push (plist-get range :label) ranges))))
        (funcall (keymap-lookup map "<opencode-stats-area-0> <mouse-1>"))
        (funcall (keymap-lookup map "<opencode-stats-area-1> <mouse-1>")))
      (should (equal '("b" "a") ranges))
      (should-not (commandp (keymap-lookup map "<opencode-stats-area-2> <mouse-1>"))))))

(defun opencode-stats-test--message (time model output)
  "Return an assistant message at TIME in ms with MODEL and OUTPUT tokens."
  `((time (created . ,time))
    (model (providerID . "p") (id . ,model))
    (tokens (input . 1) (output . ,output) (reasoning . 0)
            (cache (read . 0) (write . 0)))
    (cost . 0)))

(ert-deftest opencode-stats-session-rows-test ()
  "Subagents' usage is added to their root session's row, busiest first."
  (let* ((root '((id . "root") (title . "Root") (time (created . 0))))
         (child '((id . "child") (parentID . "root") (time (created . 0))))
         (grandchild '((id . "grandchild") (parentID . "child") (time (created . 0))))
         (other '((id . "other") (title . "Other") (time (created . 0))))
         (idle '((id . "idle") (title . "Idle") (time (created . 0))))
         (sessions (make-hash-table :test #'equal)))
    (dolist (session (list root child grandchild other idle))
      (puthash (alist-get 'id session) session sessions))
    (let ((rows (opencode-stats--session-rows
                 sessions
                 `((,other ,(opencode-stats-test--message 500 "a" 10))
                   (,grandchild ,(opencode-stats-test--message 300 "b" 100)
                                ,(opencode-stats-test--message 400 "b" 100))
                   (,root ,(opencode-stats-test--message 200 "a" 10))
                   (,idle)))))
      (should (equal '("root" "other")
                     (mapcar (lambda (row) (alist-get 'id (plist-get row :session))) rows)))
      (let ((row (car rows)))
        (should (equal 1 (plist-get row :subagents)))
        (should (equal 3 (plist-get row :steps)))
        (should (equal 213 (plist-get row :tokens)))
        (should (equal 200 (plist-get row :first)))
        (should (equal "p/b" (plist-get row :model)))))))

(provide 'opencode-stats-test)
;;; opencode-stats-test.el ends here
