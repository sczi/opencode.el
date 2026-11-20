;;; opencode-sidebar-test.el --- Tests of the session sidebar  -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests of `opencode-sidebar', in fake session buffers.

;;; Code:

(require 'ert)
(require 'opencode-sidebar)
(require 'opencode-test-helpers)

(defvar opencode-sidebar-test--mcps
  '(((name . "docs") (status (status . "connected")))
    ((name . "broken") (status (status . "failed") (error . "boom"))))
  "MCP servers of the test sessions.")

(defvar opencode-sidebar-test--session
  '((id . "ses_test")
    (tokens (input . 1000) (output . 200) (reasoning . 50)
            (cache (read . 3000) (write . 0)))
    (cost . 1.5))
  "Session.Info of the test sessions.")

(defun opencode-sidebar-test--fill-session ()
  "Give the current session buffer everything the sidebar shows.
The data's all there, so the sidebar doesn't fetch any."
  (setq opencode-session-title "Session title"
        opencode-session-tokens 12345
        opencode-session-cost 1.5
        opencode-session-todos (opencode--parse-todos
                                '(((content . "done") (status . "completed"))
                                  ((content . "doing") (status . "in_progress"))
                                  ((content . "to do") (status . "pending"))))
        opencode-sidebar--data (list :session opencode-sidebar-test--session
                                     :diff '(3 10 4)
                                     :mcps opencode-sidebar-test--mcps
                                     :branch "main")))

(defmacro opencode-sidebar-test--with-sidebar (&rest body)
  "Run BODY in a sidebar buffer, bound to `sidebar', not shown in a window.
It's run within a session buffer filled by `opencode-sidebar-test--fill-session',
bound to `session'."
  (declare (indent 0) (debug t))
  `(with-opencode-session-test-buffer
     (opencode-sidebar-test--fill-session)
     (let ((session (current-buffer))
           (sidebar (generate-new-buffer " *opencode-sidebar-test*")))
       (unwind-protect
           (with-current-buffer sidebar
             (opencode-sidebar-mode)
             (opencode-sidebar--set-session sidebar session)
             ,@body)
         (kill-buffer sidebar)))))

(defun opencode-sidebar-test--text ()
  "Return the text of the current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun opencode-sidebar-test--section-at (text)
  "Return the section at the first occurrence of TEXT."
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (magit-current-section)))

(ert-deftest opencode-sidebar-render-test ()
  "The sidebar shows the session's title, context, usage, todos, changes, MCPs and branch."
  (opencode-sidebar-test--with-sidebar
    (should (equal (concat "Session title\n\n"
                           "Changed 3 files +10 -4\n\n"
                           "Context\n 12k tokens\n\n"
                           "Usage\n ↑4k ↓200 🧠50\n 75% cache\n $1.50\n\n"
                           "Todo (2 remaining)\n [✓] done\n [•] doing\n [ ] to do\n\n"
                           "MCP (1 active, 1 error)\n ● docs connected\n ● broken failed\n\n"
                           (abbreviate-file-name
                            (directory-file-name
                             (buffer-local-value 'default-directory session)))
                           ":main\n")
                   (opencode-sidebar-test--text)))
    ;; a failed MCP's error is shown on hover, and RET toggles it
    (goto-char (point-min))
    (search-forward "broken")
    (should (equal "boom" (get-text-property (point) 'help-echo)))
    (should (eq 'opencode-sidebar-toggle-mcp (key-binding (kbd "RET"))))))

(ert-deftest opencode-sidebar-title-wrap-test ()
  "A long session title is wrapped to the sidebar width."
  (opencode-sidebar-test--with-sidebar
    (let ((title (string-join (make-list 12 "wrapping") " ")))
      (with-current-buffer session
        (setq opencode-session-title title))
      (opencode-sidebar--redraw)
      (let ((lines (split-string (opencode-sidebar-test--text) "\n")))
        (should (equal title (string-join (seq-take lines 3) " ")))
        (dolist (line (seq-take lines 3))
          (should (<= (string-width line) opencode-sidebar-width)))))))

(defvar opencode-sidebar-test--messages
  '(((type . "user") (id . "u1") (text . "hello") (time (created . 1000)))
    ((type . "assistant") (id . "a1") (agent . "build")
     (model (id . "m1") (providerID . "prov") (variant . "high"))
     (time (created . 1500) (firstToken . 2000) (streamed . 4000) (completed . 4100))
     (tokens (input . 10) (output . 80) (reasoning . 20) (cache (read . 0) (write . 0)))
     (content ((type . "text") (text . "first answer"))))
    ((type . "assistant") (id . "a2") (agent . "build")
     (model (id . "m1") (providerID . "prov"))
     (time (created . 4200) (firstToken . 4300) (streamed . 5300) (completed . 5400))
     (tokens (input . 10) (output . 50) (reasoning . 0) (cache (read . 0) (write . 0)))
     (content ((type . "text") (text . "second answer"))))
    ((type . "idle") (id . "i1") (outcome . "succeeded") (time (created . 6000)))
    ((type . "user") (id . "u2") (text . "again") (time (created . 7000)))
    ((type . "assistant") (id . "a3") (agent . "plan")
     (model (id . "m2") (providerID . "prov"))
     (time (created . 7100) (firstToken . 7600) (streamed . 8600) (completed . 8600))
     (tokens (input . 10) (output . 30) (reasoning . 0) (cache (read . 0) (write . 0)))
     (content ((type . "text") (text . "third answer"))))
    ((type . "idle") (id . "i2") (outcome . "succeeded") (time (created . 72000))))
  "Messages of two turns, the first of two steps.")

(defun opencode-sidebar-test--step-sections ()
  "Return the agent and activity sections of the current sidebar's text."
  (let ((text (opencode-sidebar-test--text)))
    (substring text (string-search "Agent" text)
               (string-match (rx bol (or "Changed" "Context")) text))))

(ert-deftest opencode-sidebar-step-test ()
  "The sidebar shows the agent and activity of the step at point."
  (opencode-sidebar-test--with-sidebar
    (with-current-buffer session
      (setq opencode-session-agents '(((id . "build") (name . "Build")))
            opencode-session-agent '((id . "build") (name . "Build")
                                     (model (id . "m1") (providerID . "prov")))
            opencode-session-status "idle")
      (opencode--replay-session-messages opencode-sidebar-test--messages)
      (opencode--show-prompt))
    (cl-flet ((sections-at (text)
                (with-current-buffer session
                  (goto-char (point-min))
                  (search-forward text))
                (opencode-sidebar--redraw)
                (opencode-sidebar-test--step-sections)))
      (should (equal (concat "Agent\n Build · 🚀 idle\n prov\n m1 high\n\n"
                             "Activity\n turn 1 · 5.0s\n TTFT 0.5s · 50 tok/s\n\n")
                     (sections-at "first")))
      ;; the prompt is of its turn's first step
      (should (equal (sections-at "hello") (sections-at "first")))
      (should (equal (concat "Agent\n Build · 🚀 idle\n prov\n m1\n\n"
                             "Activity\n turn 1 · 5.0s\n TTFT 0.1s · 50 tok/s\n\n")
                     (sections-at "second")))
      ;; an agent that isn't listed is shown by its id
      (should (equal (concat "Agent\n plan · 🚀 idle\n prov\n m2\n\n"
                             "Activity\n turn 2 · 1m 5s\n TTFT 0.5s · 30 tok/s\n\n")
                     (sections-at "third"))))))

(ert-deftest opencode-sidebar-running-turn-test ()
  "A running turn's time is how long it's run, and its step's agent is shown.
With no step yet, the session's selected agent is shown."
  (opencode-sidebar-test--with-sidebar
    (with-current-buffer session
      (setq opencode-session-agent '((id . "build") (name . "Build")
                                     (model (id . "m1") (providerID . "prov")))
            opencode-session-status "busy")
      (opencode--start-turn (- (opencode--now-ms) 3000)))
    (opencode-sidebar--redraw)
    (should (string-match-p (rx "Agent\n Build · ⏳ busy\n prov\n m1\n\n"
                                "Activity\n turn 1 · 3s\n\n")
                            (opencode-sidebar-test--step-sections)))
    (should opencode-sidebar--running)
    (with-current-buffer session
      (opencode--add-message "a1" "plan" '((id . "m2") (providerID . "prov"))
                             (opencode--now-ms)))
    (opencode-sidebar--redraw)
    (should (string-prefix-p "Agent\n plan · ⏳ busy\n prov\n m2\n\n"
                             (opencode-sidebar-test--step-sections)))))

(ert-deftest opencode-sidebar-duration-test ()
  "Durations show tenths under 10s, or whole seconds rounded with WHOLE."
  (should (equal "3.6s" (opencode-sidebar--duration 3600)))
  (should (equal "4s" (opencode-sidebar--duration 3600 t)))
  (should (equal "42s" (opencode-sidebar--duration 42000)))
  (should (equal "1m 0s" (opencode-sidebar--duration 59600 t)))
  (should (equal "3m 5s" (opencode-sidebar--duration 185000))))

(ert-deftest opencode-sidebar-empty-sections-test ()
  "Sections with nothing to show are omitted."
  (opencode-sidebar-test--with-sidebar
    (with-current-buffer session
      (setq opencode-session-tokens 0
            opencode-session-cost 0
            opencode-session-todos nil
            opencode-sidebar--data '(:session nil :diff (0 0 0) :mcps nil :branch nil)))
    (opencode-sidebar--redraw)
    (should (equal (concat "Session title\n\n"
                           (abbreviate-file-name
                            (directory-file-name
                             (buffer-local-value 'default-directory session)))
                           "\n")
                   (opencode-sidebar-test--text)))))

(ert-deftest opencode-sidebar-context-limit-test ()
  "The context section shows the percent of the model's context used."
  (opencode-sidebar-test--with-sidebar
    (cl-letf (((symbol-function 'opencode--current-model)
               (lambda () '((limit (context . 100000))))))
      (opencode-sidebar--redraw))
    (should (string-search "Context\n 12k tokens\n 12% used\n\n"
                           (opencode-sidebar-test--text)))))

(ert-deftest opencode-sidebar-claude-usage-test ()
  "Claude subscription sessions show their estimated cost and usage windows."
  (opencode-sidebar-test--with-sidebar
    (let* ((now (float-time))
           (opencode--claude-plan-tier "pro")
           (opencode--claude-catalog
            '((claude-opus-4-5 (release_date . "2025-11-24")
               (cost (input . 5) (output . 25)
                     (cache_read . 0.5) (cache_write . 6.25)))))
           (opencode-claude-five-hour-budgets '(("pro" . 10))))
      (with-current-buffer session
        (setq opencode-session-agent
              '((model (id . "opus") (providerID . "claude-agent"))))
        (setf (opencode-connection-claude-rate-limit (opencode--conn))
              (cons now `((unifiedWindows
                           (five_hour (utilization . 0.09) (resetsAt . ,(+ now 3600)))
                           (seven_day (utilization . 0.31) (resetsAt . ,(- now 60)))))))
        (setq opencode-sidebar--data
              (plist-put opencode-sidebar--data :session
                         `((model (id . "opus") (providerID . "claude-agent"))
                           (cost . 0)
                           ,@opencode-sidebar-test--session))))
      (opencode-sidebar--redraw)
      (should (string-match-p
               (rx "Usage\n ↑4k ↓200 🧠50\n 75% cache\n ~$0.01 0.1%\n"
                   "\n 5h   " (+ (any "━─")) " 9%\n"
                   " resets "
                   (literal (opencode--claude-window-reset
                             `((resetsAt . ,(+ now 3600)))))
                   "\n"
                   " week " (+ (any "━─")) " 0%\n\n")
               (opencode-sidebar-test--text))))))

(ert-deftest opencode-sidebar-visibility-test ()
  "A collapsed section stays collapsed on redraw, and for other sessions."
  (opencode-sidebar-test--with-sidebar
    (magit-section-hide (opencode-sidebar-test--section-at "Todo"))
    (opencode-sidebar--redraw)
    (should (oref (opencode-sidebar-test--section-at "Todo") hidden))
    (should-not (oref (opencode-sidebar-test--section-at "MCP") hidden))
    (with-temp-buffer
      (let ((other (current-buffer)))
        (opencode-sidebar-test--fill-session)
        (opencode-sidebar--set-session sidebar other)
        (with-current-buffer sidebar
          (should (oref (opencode-sidebar-test--section-at "Todo") hidden)))))))

(ert-deftest opencode-sidebar-killed-session-test ()
  "The sidebar of a killed session buffer says there's no session."
  (opencode-sidebar-test--with-sidebar
    (with-temp-buffer
      (let ((other (current-buffer)))
        (opencode-sidebar--set-session sidebar other)
        (kill-buffer other)))
    (opencode-sidebar--redraw)
    (should (equal "No session\n" (opencode-sidebar-test--text)))))

(ert-deftest opencode-sidebar-parse-todos-test ()
  "Todos are parsed from a list or a JSON string, dropping malformed ones."
  (should (equal '(((content . "a") (status . "completed"))
                   ((content . "b") (status . "pending")))
                 (opencode--parse-todos
                  "[{\"content\":\"a\",\"status\":\"completed\"},
                    {\"content\":\"b\",\"status\":\"bogus\"},
                    {\"content\":\"\"}, {\"status\":\"pending\"}, 3]")))
  (should (equal '(((content . "c") (status . "in_progress")))
                 (opencode--parse-todos '(((content . "c") (status . "in_progress"))))))
  (should-not (opencode--parse-todos "not json"))
  (should-not (opencode--parse-todos nil)))

(ert-deftest opencode-sidebar-record-todos-test ()
  "A todowrite call replaces the session's todos, other tools don't."
  (with-opencode-session-test-buffer
    (should (opencode--record-todos "todowrite" '((todos ((content . "x"))))))
    (should (equal '(((content . "x") (status . "pending"))) opencode-session-todos))
    (should-not (opencode--record-todos "read" '((path . "a.txt"))))
    (should (equal '(((content . "x") (status . "pending"))) opencode-session-todos))
    (should (opencode--record-todos "todowrite" '((todos))))
    (should-not opencode-session-todos)))

(defmacro opencode-sidebar-test--with-frame-state (&rest body)
  "Run BODY, then remove the selected frame's sidebar and its state.
BODY runs without any `display-buffer-overriding-action' from the caller,
which would take precedence over the sidebar's side window action."
  (declare (indent 0) (debug t))
  `(let ((opencode-sidebar-auto nil)
         (display-buffer-overriding-action '(nil)))
     (unwind-protect
         (progn ,@body)
       (when-let* ((window (opencode-sidebar--window)))
         (delete-window window))
       (when-let* ((buffer (opencode-sidebar--buffer)))
         (kill-buffer buffer))
       (set-frame-parameter nil 'opencode-sidebar nil)
       (set-frame-parameter nil 'opencode-sidebar-dismissed nil)
       (set-frame-parameter nil 'opencode-sidebar-suspended nil))))

(ert-deftest opencode-sidebar-toggle-test ()
  "Toggling shows the session's sidebar in a side window, then hides it."
  (opencode-sidebar-test--with-frame-state
    (with-opencode-session-test-buffer
      (opencode-sidebar-test--fill-session)
      (let ((session (current-buffer)))
        (save-window-excursion
          (switch-to-buffer session)
          (opencode-sidebar-toggle)
          (let ((window (opencode-sidebar--window)))
            (should window)
            (should (eq opencode-sidebar-side (window-parameter window 'window-side)))
            (with-current-buffer (window-buffer window)
              (should (eq session opencode-sidebar--session))
              (should (string-prefix-p "Session title\n" (opencode-sidebar-test--text)))))
          (opencode-sidebar-toggle)
          (should-not (opencode-sidebar--window))
          (should (frame-parameter nil 'opencode-sidebar-dismissed)))))))

(ert-deftest opencode-sidebar-toggle-no-session-test ()
  "Toggling with no session visible in the frame doesn't show the sidebar."
  (opencode-sidebar-test--with-frame-state
    (with-opencode-session-test-buffer
      (opencode-sidebar-test--fill-session)
      (save-window-excursion
        (delete-other-windows)
        (switch-to-buffer (get-buffer-create "*scratch*"))
        (opencode-sidebar-toggle)
        (should-not (opencode-sidebar--window))))))

(ert-deftest opencode-sidebar-follow-test ()
  "A shown sidebar follows the session buffer selected in its frame."
  (opencode-sidebar-test--with-frame-state
    (with-opencode-session-test-buffer
      (opencode-sidebar-test--fill-session)
      (with-temp-buffer
        (opencode-session-mode)
        (opencode-sidebar-test--fill-session)
        (setq opencode-session-title "Other")
        (let ((other (current-buffer)))
          (save-window-excursion
            (delete-other-windows)
            (switch-to-buffer other)
            (opencode-sidebar-toggle)
            (with-current-buffer (opencode-sidebar--buffer)
              (should (eq other opencode-sidebar--session))
              (should (string-prefix-p "Other\n" (opencode-sidebar-test--text))))
            ;; a session in another window keeps it showing that session
            (select-window (split-window))
            (switch-to-buffer (get-buffer-create "*scratch*"))
            (opencode-sidebar--follow (selected-frame))
            (should (opencode-sidebar--window))
            (should (eq other (buffer-local-value 'opencode-sidebar--session
                                                  (opencode-sidebar--buffer))))))))))

(ert-deftest opencode-sidebar-hide-without-session-test ()
  "The sidebar's hidden while no session is visible in its frame.
It comes back when one is, unless it was hidden with the toggle."
  (opencode-sidebar-test--with-frame-state
    (with-opencode-session-test-buffer
      (opencode-sidebar-test--fill-session)
      (let ((session (current-buffer)))
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer session)
          (opencode-sidebar-toggle)
          (should (opencode-sidebar--window))
          (switch-to-buffer (get-buffer-create "*scratch*"))
          (opencode-sidebar--follow (selected-frame))
          (should-not (opencode-sidebar--window))
          (switch-to-buffer session)
          (opencode-sidebar--follow (selected-frame))
          (should (opencode-sidebar--window))
          ;; dismissed, it stays hidden
          (opencode-sidebar-toggle)
          (switch-to-buffer (get-buffer-create "*scratch*"))
          (opencode-sidebar--follow (selected-frame))
          (switch-to-buffer session)
          (opencode-sidebar--follow (selected-frame))
          (should-not (opencode-sidebar--window)))))))

(ert-deftest opencode-sidebar-auto-test ()
  "With `opencode-sidebar-auto' it's shown on selecting a session buffer.
Unless the frame is too narrow, or it was hidden."
  (opencode-sidebar-test--with-frame-state
    (with-opencode-session-test-buffer
      (opencode-sidebar-test--fill-session)
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (let ((opencode-sidebar-auto (1+ (frame-width))))
          (opencode-sidebar--follow (selected-frame))
          (should-not (opencode-sidebar--window)))
        (let ((opencode-sidebar-auto t))
          (opencode-sidebar--follow (selected-frame))
          (should (opencode-sidebar--window))
          (opencode-sidebar-toggle)
          (opencode-sidebar--follow (selected-frame))
          (should-not (opencode-sidebar--window)))))))

(ert-deftest opencode-sidebar-invalidate-test ()
  "Invalidating keys forgets only them, and refetches them if it's shown."
  (with-opencode-session-test-buffer
    (opencode-sidebar-test--fill-session)
    (let (fetched)
      (cl-letf (((symbol-function 'opencode-sidebar--shown-p) #'always)
                ((symbol-function 'opencode-sidebar--fetch-diff)
                 (lambda () (push 'diff fetched)))
                ((symbol-function 'opencode-sidebar--fetch-branch)
                 (lambda () (push 'branch fetched)))
                ((symbol-function 'opencode-sidebar--fetch-mcps)
                 (lambda () (push 'mcps fetched))))
        (opencode-sidebar-session-changed :diff :branch)
        (should (equal '(branch diff) fetched))
        (should (equal opencode-sidebar-test--mcps
                       (plist-get opencode-sidebar--data :mcps)))))))

(provide 'opencode-sidebar-test)
;;; opencode-sidebar-test.el ends here
