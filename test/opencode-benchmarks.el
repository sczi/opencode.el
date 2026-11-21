;;; opencode-benchmarks.el --- Benchmarks of the opencode session display  -*- lexical-binding: t; -*-

;;; Commentary:

;; Commands to profile the session display:
;;
;; `opencode-benchmark-streaming' streams a turn into a session buffer at a
;; given rate of tokens per second, with the CPU profiler running.  The turn's
;; events are recorded once from the test server, with a scripted LLM (see
;; opencode-integration-test-helpers.el), then replayed by a timer, their
;; deltas split into tokens.  Emacs redisplays between the timer's ticks as
;; it would when they come from the server.
;;
;; `opencode--profile-session-loading' profiles opening a session, from the
;; session control buffer.

;;; Code:

(require 'opencode-integration-test-helpers)
(require 'profiler)
(require 'vtable)

;;; Loading a session

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

;;; Streaming

(defconst opencode-benchmark--reasoning
  "The user wants an overview of the parser.  Let me read the main file first,
then explain how the pieces fit together.  I should mention the tokenizer,
the grammar, and how errors are reported, with an example of each.

")

(defconst opencode-benchmark--text
  "# Overview of the parser

The parser is split in **three stages**: tokenizing, parsing, and
*resolving* names.  Each stage is in its own file, see `lexer.el`,
`parser.el` and `resolve.el`.

## Tokenizer

The tokenizer turns the source into a list of tokens:

```elisp
(defun my-tokenize (string)
  \"Return the tokens of STRING.\"
  (let ((pos 0) tokens)
    (while (string-match \"[[:alnum:]]+\\\\|[^[:space:]]\" string pos)
      (push (match-string 0 string) tokens)
      (setq pos (match-end 0)))
    (nreverse tokens)))
```

Some properties of the tokens:

- They keep their position, for error messages.
- Whitespace is dropped, except in strings.
- Comments become a single `comment` token.
  1. Line comments end at the newline.
  2. Block comments can nest.

## Grammar

| Rule       | Produces        | Notes                       |
|------------+-----------------+-----------------------------|
| expression | `term` `op` ... | left associative            |
| term       | `atom` or group | groups are in parentheses   |
| atom       | number, name    | names are resolved later    |

> Precedence is handled by climbing, not by separate rules, which keeps
> the grammar small.

## Errors

Errors are collected rather than signaled, so one run reports all of them:

```python
def report(errors):
    for error in errors:
        print(f\"{error.file}:{error.line}: {error.message}\")
```

That's the gist of it.  Let me know if you want more detail on a stage.

")

(defun opencode-benchmark--response ()
  "Return the scripted LLM's responses for the benchmark turn."
  (list (list (list :reasoning opencode-benchmark--reasoning)
              "I'll read the parser first.\n\n"
              '(:tool "read" ((path . "parser.el"))))
        opencode-benchmark--text))

(defvar opencode-benchmark--events nil
  "Events of the benchmark turn, oldest first, recorded by `opencode-benchmark-record'.")

(defconst opencode-benchmark--event-types
  '("session.execution.started" "session.execution.succeeded"
    "session.step.started" "session.step.first-token" "session.step.streamed"
    "session.step.ended"
    "session.text.delta" "session.text.ended"
    "session.reasoning.delta" "session.reasoning.ended"
    "session.tool.input.started" "session.tool.called"
    "session.tool.success" "session.tool.failed")
  "Types of the events replayed by `opencode-benchmark-streaming'.")

(defun opencode-benchmark-record ()
  "Record the events of the benchmark turn from the test server.
Store them in `opencode-benchmark--events' and return them."
  (interactive)
  (let (events)
    (opencode-test-with-session
        `(("parser.el" . ,(concat ";; The parser\n" opencode-benchmark--text)))
      (apply #'opencode-test-llm-reply (opencode-benchmark--response))
      (opencode-test-send "How does the parser work?")
      (setq events (seq-filter
                    (lambda (event)
                      (and (member (alist-get 'type event) opencode-benchmark--event-types)
                           (equal opencode-session-id
                                  (map-nested-elt event '(data sessionID)))))
                    (reverse opencode-test-events))))
    (setq opencode-benchmark--events (copy-tree events))))

(defun opencode-benchmark--tokens (string)
  "Split STRING into tokens of a word or up to 4 other characters.
Whitespace goes with the token after it."
  (let ((pos 0) tokens)
    (while (string-match "[[:space:]]*\\([[:alpha:]]\\{1,8\\}\\|[^[:alpha:][:space:]]\\{1,4\\}\\)\\|[[:space:]]+"
                         string pos)
      (push (substring-no-properties string pos (match-end 0)) tokens)
      (setq pos (match-end 0)))
    (nreverse tokens)))

(defun opencode-benchmark--copy-event (event repetition)
  "Return a copy of EVENT for REPETITION of the turn, in session \"benchmark\".
Message ids are made unique to the repetition."
  (let* ((event (copy-tree event))
         (data (alist-get 'data event)))
    (setf (alist-get 'sessionID data) "benchmark")
    (when-let* ((id (alist-get 'assistantMessageID data)))
      (setf (alist-get 'assistantMessageID data) (format "%s-%d" id repetition)))
    (setf (alist-get 'data event) data)
    event))

(defun opencode-benchmark--expand (events repetitions)
  "Return EVENTS repeated REPETITIONS times, with deltas split into tokens."
  (let (result)
    (dotimes (repetition repetitions)
      (dolist (event events)
        (let ((event (opencode-benchmark--copy-event event repetition)))
          (if (member (alist-get 'type event)
                      '("session.text.delta" "session.reasoning.delta"))
              (dolist (token (opencode-benchmark--tokens
                              (map-nested-elt event '(data delta))))
                (let ((token-event (copy-tree event)))
                  (setf (alist-get 'delta (alist-get 'data token-event)) token)
                  (push token-event result)))
            (push event result)))))
    (nreverse result)))

(defun opencode-benchmark--token-event-p (event)
  "Return non-nil if EVENT is the delta of a token."
  (member (alist-get 'type event) '("session.text.delta" "session.reasoning.delta")))

(defun opencode-benchmark--make-buffer ()
  "Make the benchmark's session buffer, shown in the selected window, and return it."
  (let ((buffer (get-buffer "*OpenCode: benchmark*")))
    (when buffer
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer))))
  (let ((buffer (get-buffer-create "*OpenCode: benchmark*"))
        (connection (opencode--make-connection :name 'opencode-benchmark)))
    (with-current-buffer buffer
      (setq default-directory
            (file-name-directory (locate-library "opencode-sessions")))
      (opencode-session-mode)
      (start-process "opencode-benchmark" buffer nil)
      (setq opencode-session-id "benchmark"
            opencode-buffer-connection connection
            opencode-session-status "idle")
      (puthash "benchmark" buffer (opencode-connection-session-buffers connection))
      (opencode--show-prompt))
    (switch-to-buffer buffer)
    buffer))

(defun opencode-benchmark--dispatch (event)
  "Handle EVENT in the benchmark's session buffer, as if from the server now."
  (setf (alist-get 'created event) (opencode--now-ms))
  (if (equal (alist-get 'type event) "session.execution.succeeded")
      ;; Without what `opencode--session-finished' does that needs a server
      (opencode--with-session-buffer "benchmark"
        (opencode-session--set-status "benchmark" "idle")
        (opencode--end-turn (opencode--now-ms))
        (opencode--show-prompt))
    (opencode--handle-message event)))

(defvar opencode-benchmark--timer nil
  "Timer of the running `opencode-benchmark-streaming', or nil.")

(defvar opencode-benchmark-result nil
  "Plist of the results of the last `opencode-benchmark-streaming'.")

(defun opencode-benchmark-streaming (&optional tps repetitions)
  "Stream the benchmark turn REPETITIONS times at TPS tokens per second.
The turn is streamed into a new session buffer with the CPU profiler
running, and the profiler report is shown once it's done, along with the
rate achieved and how many redisplays there were.  TPS defaults to 1000
and REPETITIONS to 5.  With a prefix argument, prompt for them.

The turn's events are recorded from the test server the first time, see
`opencode-benchmark-record'."
  (interactive
   (when current-prefix-arg
     (list (read-number "Tokens per second: " 1000)
           (read-number "Repetitions: " 5))))
  (when (timerp opencode-benchmark--timer)
    (user-error "A benchmark is already running"))
  (let* ((tps (or tps 1000))
         (events (opencode-benchmark--expand
                  (or opencode-benchmark--events (opencode-benchmark-record))
                  (or repetitions 5)))
         (tokens (seq-count #'opencode-benchmark--token-event-p events))
         (buffer (opencode-benchmark--make-buffer))
         (connection (buffer-local-value 'opencode-buffer-connection buffer))
         (sent 0)
         (redisplays 0)
         (ticks 0)
         (slowest 0)
         (count-redisplay (lambda (&rest _) (setq redisplays (1+ redisplays))))
         (start (float-time))
         (finish
          (lambda ()
            (cancel-timer opencode-benchmark--timer)
            (setq opencode-benchmark--timer nil)
            (profiler-stop)
            (advice-remove 'redisplay--pre-redisplay-functions count-redisplay)
            (let ((elapsed (- (float-time) start)))
              (setq opencode-benchmark-result
                    (list :tokens tokens :seconds elapsed :tps (/ tokens elapsed)
                          :target-tps tps :ticks ticks :redisplays redisplays
                          :slowest-tick-ms (* 1000 slowest)))
              (profiler-report)
              (message "Streamed %d tokens in %.3fs, %.0f tps of %d targeted, \
in %d ticks with %d redisplays, the slowest tick taking %.1fms"
                       tokens elapsed (/ tokens elapsed) tps ticks redisplays
                       (* 1000 slowest)))))
         (tick
          (lambda ()
            (let ((tick-start (float-time))
                  (due (* tps (- (float-time) start))))
              (condition-case err
                  (let ((opencode--dispatching-connection connection))
                    (while (and events (or (< sent due)
                                           ;; no delay between other events
                                           (not (opencode-benchmark--token-event-p
                                                 (car events)))))
                      (let ((event (pop events)))
                        (when (opencode-benchmark--token-event-p event)
                          (setq sent (1+ sent)))
                        (opencode-benchmark--dispatch event))))
                (error (funcall finish)
                       (signal (car err) (cdr err))))
              (setq ticks (1+ ticks)
                    slowest (max slowest (- (float-time) tick-start)))
              (unless (and events (buffer-live-p buffer))
                (funcall finish))))))
    (advice-add 'redisplay--pre-redisplay-functions :before count-redisplay)
    (profiler-reset)
    (profiler-start 'cpu)
    ;; About a frame at 60Hz, as the server's events are batched by the network
    (setq opencode-benchmark--timer (run-at-time 0 (/ 1.0 60) tick))))

(provide 'opencode-benchmarks)
;;; opencode-benchmarks.el ends here
