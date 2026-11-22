;;; opencode-integration-test.el --- Tests against a real opencode server  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests of opencode.el against a real opencode server, with a scripted LLM.
;; See `opencode-integration-test-helpers'.

;;; Code:

(require 'ert)
(require 'opencode)
(require 'opencode-integration-test-helpers)
(require 'opencode-stats)

(defun opencode-integration-test--file (file)
  "Return the contents of FILE in the session's directory, or nil if missing.
The server may briefly remove a file while restoring it, as on revert."
  (with-temp-buffer
    (when (ignore-error file-missing
            (insert-file-contents (expand-file-name file)))
      (buffer-string))))

(ert-deftest opencode-integration-text-response-test ()
  "A prompt is sent to the LLM, and its streamed answer shown before a prompt.
The buffer is renamed after the title the server generates.  Text of a step
the server continues is spaced from the next step's.  An error from the LLM
provider is shown, then a prompt."
  (opencode-test-with-session nil
    (opencode-test-llm-reply '("Hello" " from the" " LLM."))
    (opencode-test-send "Say hello")
    (should (equal "Say hello" (opencode-test-llm-user-text
                                (car opencode-test-llm-requests))))
    (should (equal "❯ Say hello\n\nHello from the LLM.\n\n❯ "
                   (opencode-test-output)))
    ;; the title
    (opencode-test-wait-until (equal opencode-session-title opencode-test-title))
    (should (equal (format "*OpenCode: %s*" opencode-test-title) (buffer-name)))
    ;; a continued step
    (opencode-test-llm-reply '("Cut off" (:finish "length"))
                             "Next text")
    (opencode-test-send "Go on")
    (should (string-suffix-p "❯ Go on\n\nCut off\n\nNext text\n\n❯ "
                             (opencode-test-output)))
    ;; a provider error
    (opencode-test-llm-reply
     '((:raw ((error . ((message . "Bad request from provider")
                        (type . "invalid_request_error")))))
       (:finish nil)))
    (opencode-test-send "Fail")
    (should (string-suffix-p "❯ Fail\n\nBad request from provider\n\n❯ "
                             (opencode-test-output)))
    (should (opencode-test-error-p "Bad request from provider"))))

(ert-deftest opencode-integration-stats-test ()
  "The stats of the session's project show its prompts, steps, model and tools.
Today's stats list its sessions with their steps, and RET opens one."
  ;; Projects are identified by their first commit, which is unique only
  ;; with unique files, so the stats don't include other tests' sessions
  (opencode-test-with-session `(("a.txt" . "alpha\n") ("unique" . ,(format "%s" (random))))
    (opencode-test-llm-reply '((:tool "read" ((path . "a.txt")))) "It says alpha.")
    (opencode-test-send "What's in a.txt?")
    (let ((first opencode-session-id)
          (second (with-current-buffer (opencode-test-new-session default-directory)
                    (opencode-test-llm-reply "Hi.")
                    (opencode-test-send "Hello")
                    opencode-session-id))
          opened)
      (opencode-stats t)
      (with-current-buffer (opencode-stats--buffer opencode-test--connection)
        (opencode-test-wait-until (and opencode-stats--project opencode-stats--data))
        ;; the project's stats
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should-not (plist-get opencode-stats--data :error))
          (should (string-search "2 sessions · 2 prompts · 3 steps" text))
          (should (string-search "100% tool success · 1 active day" text))
          (should (string-match-p "^  read +1 +0 +0% " text))
          ;; The chart's bars together have them all
          (should (equal 3 (apply #'+ (mapcar (lambda (bucket) (alist-get 'steps (cdr bucket)))
                                              (plist-get opencode-stats--data :buckets))))))
        ;; today's sessions
        (opencode-stats-days 1)
        (opencode-test-wait-until
         (and opencode-stats--data
              (not (eq (plist-get opencode-stats--data :sessions) 'loading))))
        (should-not (plist-get opencode-stats--data :error))
        (should (equal `((,first . 2) (,second . 1))
                       (mapcar (lambda (row)
                                 (cons (alist-get 'id (plist-get row :session))
                                       (plist-get row :steps)))
                               (plist-get opencode-stats--data :sessions))))
        (goto-char (point-min))
        (search-forward "\nSessions\n")
        (forward-line 2)
        (cl-letf (((symbol-function 'opencode--switch-to-buffer)
                   (lambda (buffer) (setq opened buffer))))
          (push-button (+ 4 (point))))
        (should (eq opened (opencode--session-buffer second)))))))

(ert-deftest opencode-integration-read-image-test ()
  "A read of an image shows the image instead of its text."
  (skip-unless (image-type-available-p 'png))
  (opencode-test-with-session
      `(("pixel.png"
         . ,(base64-decode-string
             "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8DwHwAFBQIAX8jx0gAAAABJRU5ErkJggg==")))
    (opencode-test-llm-reply '((:tool "read" ((path . "pixel.png")))) "A pixel.")
    ;; Only images are displayed as if graphical, as the rest of the session
    ;; can't be in batch mode.
    (cl-letf (((symbol-function 'opencode--inline-image)
               (let ((inline-image (symbol-function 'opencode--inline-image)))
                 (lambda (&rest args)
                   (cl-letf (((symbol-function 'display-graphic-p) #'always))
                     (apply inline-image args))))))
      (opencode-test-send "Look at pixel.png"))
    (should-not (string-search "Image read successfully" (opencode-test-output)))
    (goto-char (point-min))
    (search-forward "read pixel.png\n```text\n")
    (should (eq (car-safe (get-text-property (point) 'display)) 'image))))

(defun opencode-test--pending-display ()
  "Return the text displayed before the pending input, or nil."
  (when opencode--pending-input-overlay
    (substring-no-properties
     (or (overlay-get opencode--pending-input-overlay 'before-string) ""))))

(ert-deftest opencode-integration-input-test ()
  "Input is queued, steers a running turn, and aborts it or switches agent.
Queued input is sent while idle, and pending while a turn runs.  It's
delivered after the turn, with input typed meanwhile kept after it, and
shown pending again when the session is reloaded.

Input sent while a turn runs is shown after a prompt, as input sent idle.
It's separated from the output before it, and has the request margin.

Aborting while the LLM's answering shows it was interrupted, then a prompt.
Aborting with a prefix argument also aborts the session's subagents.

`opencode-build-go' switches to the build agent and sends \"go\".  Pending
input is kept, and the plan agent with its settings is saved.

Killing the session deletes it on the server, and kills its buffer."
  (opencode-test-with-session nil
    ;; queued input
    (opencode-test-llm-reply "Answer"
                             '("First answer" (:hold)) "Queued answer"
                             '("Third answer" (:hold)) "Fourth answer")
    ;; while idle
    (let ((since (length opencode-test-events)))
      (opencode-test-type "Hello")
      (opencode-queue-input)
      (opencode-test-wait-finished since))
    (should-not opencode--queued-inputs)
    (should (string-match-p "\\`❯ Hello\n\nAnswer\n\n❯ \\'" (opencode-test-output)))
    ;; while a turn runs
    (let ((since (length opencode-test-events)))
      (opencode-test-send "First" t)
      (opencode-test-wait-for-output "First answer")
      (opencode-test-type "Second")
      (opencode-queue-input)
      (should-not (string-search "Second" (buffer-string)))
      (should (string-search "Second  (queued)" (opencode-test--pending-display)))
      (opencode-test-wait-until (opencode--queued-id (car opencode--queued-inputs)))
      (opencode-test-type "draft")
      (backward-char 2)
      (opencode-test-llm-finish (opencode-test-llm-wait-held))
      (opencode-test-wait-finished since))
    (should-not opencode--queued-inputs)
    (should (string-suffix-p "❯ First\n\nFirst answer\n\n❯ Second\n\nQueued answer\n\n❯ "
                             (opencode-test-output)))
    ;; point stays in the pending input
    (should (looking-at-p "ft\\'"))
    (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
      (should (equal "draft" (buffer-substring-no-properties pmark (point-max))))
      (delete-region pmark (point-max)))
    (should (stringp (get-text-property (opencode-test--search "Second")
                                        'opencode-user-message)))
    (should (equal "Second" (opencode-test-llm-user-text
                             (car (last opencode-test-llm-requests)))))
    ;; the queued input is of the turn it was delivered to, as when replayed
    (should (length= opencode--turns 2))
    ;; reloaded while it's queued
    (let ((since (length opencode-test-events)))
      (opencode-test-send "Third" t)
      (opencode-test-wait-for-output "Third answer")
      (opencode-test-type "Fourth")
      (opencode-queue-input)
      (opencode-test-wait-until (opencode--queued-id (car opencode--queued-inputs)))
      (opencode--reload-session)
      (should-not opencode--queued-inputs)
      (opencode-test-wait-until opencode--queued-inputs)
      (should (equal "Fourth" (opencode--queued-text (car opencode--queued-inputs))))
      (should (string-search "Fourth  (queued)" (opencode-test--pending-display)))
      (opencode-test-llm-finish (opencode-test-llm-wait-held))
      (opencode-test-wait-finished since))
    (should-not opencode--queued-inputs)
    (should (string-suffix-p "❯ Fourth\n\nFourth answer\n\n❯ " (opencode-test-output)))
    ;; steering input is delivered after the held response, as the next turn
    (opencode-test-llm-reply '("Held answer" (:hold)) "Steered answer")
    (let ((since (length opencode-test-events)))
      (opencode-test-send "Fifth" t)
      (opencode-test-wait-for-output "Held answer")
      (opencode-test-type "Steer")
      (opencode-send-input)
      (should (string-suffix-p "Held answer\n\n❯ Steer\n\n" (opencode-test-output)))
      ;; at the start of its line, where it's displayed
      (let ((start (save-excursion (goto-char (opencode-test--search "Steer"))
                                   (line-beginning-position))))
        (should (eq 'opencode-request-margin-highlight
                    (seq-some (lambda (ov)
                                (when-let* ((prefix (overlay-get ov 'line-prefix)))
                                  (get-text-property
                                   0 'face (cadr (get-text-property 0 'display prefix)))))
                              (overlays-at start)))))
      (opencode-test-llm-finish (opencode-test-llm-wait-held))
      (opencode-test-wait-finished since))
    (opencode-test-wait-for-output "Steered answer")
    (opencode-test-wait-until (opencode-test-at-prompt-p))
    (should (equal "Steer" (opencode-test-llm-user-text
                            (car (last opencode-test-llm-requests)))))
    (should (string-search "❯ Steer\n" (opencode-test-output)))
    ;; aborted
    (opencode-test-llm-reply '("Partial answer" (:hold)))
    (let ((since (length opencode-test-events)))
      (opencode-test-send "Take your time" t)
      (opencode-test-wait-for-output "Partial answer")
      (should (equal "busy" opencode-session-status))
      (opencode-abort-session)
      (opencode-test-wait-finished since))
    (should (opencode-test-error-p "Interrupted"))
    (should (string-suffix-p "Partial answer\n\nInterrupted\n\n❯ " (opencode-test-output)))
    ;; with subagents; the aborted request is no longer held
    (setq opencode-test-llm-held nil)
    (let ((reply (lambda (body)
                   (if (string-search "You are a subagent"
                                      (opencode-test-llm-user-text body))
                       '("Child working" (:hold))
                     "Started"))))
      (opencode-test-llm-reply
       '((:tool "subagent" ((agent . "general") (description . "Long task")
                            (prompt . "Work slowly"))))
       reply reply))
    (opencode-test-send "Spawn a subagent")
    (opencode-test-llm-wait-held)
    (let ((child (opencode-test-wait-until
                  (seq-some (lambda (event)
                              (let ((id (map-nested-elt event '(data sessionID))))
                                (and (equal "session.execution.started" (alist-get 'type event))
                                     (not (equal id opencode-session-id))
                                     id)))
                            opencode-test-events))))
      (should-not (opencode-test-event-p "session.execution.interrupted" child))
      ;; The parent is told its subagent was cancelled
      (opencode-test-llm-reply "Subagent cancelled")
      (let ((since (length opencode-test-events)))
        (opencode-abort-session '(4))
        (opencode-test-wait-until
         (opencode-test-event-p "session.execution.interrupted" child))
        (opencode-test-wait-finished since))
      (should (string-search "state=\"cancelled\""
                             (opencode-test-llm-user-text
                              (car (last opencode-test-llm-requests))))))
    (setq opencode-test-llm-held nil)
    ;; build go
    (opencode-test-llm-reply "A plan." "Building.")
    (while (not (equal "plan" (alist-get 'id opencode-session-agent)))
      (opencode-cycle-session-agent))
    (setq opencode-session-agent (copy-alist opencode-session-agent))
    (opencode-test-send "Make a plan")
    (should (equal "plan" opencode--server-agent))
    (opencode-test-type "draft")
    (backward-char 1)
    (let ((since (length opencode-test-events))
          (plan opencode-session-agent))
      (opencode-build-go)
      (should (memq plan opencode-session-agents))
      (opencode-test-wait-finished since))
    (should (equal "go" (opencode-test-llm-user-text
                         (car (last opencode-test-llm-requests)))))
    (should (equal "build" opencode--server-agent))
    (should (equal "build" (alist-get 'id opencode-session-agent)))
    (should (string-search "❯ go\n\nBuilding.\n\n❯ " (opencode-test-output)))
    (should (equal "draft" (buffer-substring-no-properties
                            (process-mark (get-buffer-process (current-buffer)))
                            (point-max))))
    (should (looking-at-p "t\\'"))
    (should (equal "go" (ring-ref comint-input-ring 0)))
    ;; killed
    (let ((id opencode-session-id)
          (buffer (current-buffer)))
      (opencode-kill-session)
      (opencode-test-wait-until (not (buffer-live-p buffer)))
      (opencode-test-wait-until (opencode-test-event-p "session.deleted" id))
      (let (result)
        (opencode-api-session (id)
            session
          :else (lambda (error-msg) (setq result (list 'error error-msg)))
          (setq result (list 'found session)))
        (opencode-test-wait-until result)
        (should (eq 'error (car result)))))))

(ert-deftest opencode-integration-background-shell-test ()
  "A turn can finish while its shell keeps running in the background.
A session isn't alerted as finished until its background shell finishes.
Backgrounding a running shell lets the turn finish while it keeps running."
  (opencode-test-with-session nil
    ;; not alerted until a background shell finishes
    (let (job-lists)
      (cl-letf* ((request (symbol-function 'opencode-api--request))
                 ((symbol-function 'opencode-api--request)
                  (lambda (method path &rest args)
                    (if (not (string-prefix-p "/api/experimental/job" path))
                        (apply request method path args)
                      (let ((then (plist-get args :then)))
                        (apply request method path
                               :then (lambda (jobs)
                                       (funcall then jobs)
                                       (push jobs job-lists))
                               args)))))
                 ((symbol-function 'opencode--buffer-active-p) #'ignore))
        (opencode-test-llm-reply
         '((:tool "shell" ((command . "while [ ! -e go ]; do sleep 0.1; done; echo went")
                           (background . t))))
         "Started it."
         "It went.")
        (opencode-test-send "Run it in the background")
        (opencode-test-wait-until job-lists)
        (should (car job-lists))
        (should-not opencode-alerted-sessions)
        (let ((since (length opencode-test-events)))
          (write-region "" nil "go")
          (opencode-test-wait-until
           (opencode-test-event-p "session.execution.started" nil since))
          (opencode-test-wait-finished since))
        (opencode-test-wait-until opencode-alerted-sessions)
        (should (equal 2 (length job-lists)))
        (should-not (car job-lists))
        (should (equal 1 (length opencode-alerted-sessions)))
        (should (string-search "It went." (opencode-test-output)))))
    ;; backgrounded while it runs
    (opencode-test-llm-reply
     '((:tool "shell" ((command . "touch started; while [ ! -e go-on ]; do sleep 0.1; done; echo went"))))
     "Backgrounded it."
     "It finished.")
    (let ((since (length opencode-test-events)))
      (opencode-test-send "Run it" t)
      (opencode-test-wait-until (file-exists-p "started"))
      (opencode-background-all-jobs)
      (opencode-test-wait-finished since))
    (should (string-search "Backgrounded it." (opencode-test-output)))
    (should-not (string-search "It finished." (opencode-test-output)))
    (let ((since (length opencode-test-events)))
      (write-region "" nil "go-on")
      (opencode-test-wait-until
       (opencode-test-event-p "session.execution.started" nil since))
      (opencode-test-wait-finished since))
    (should (string-search "It finished." (opencode-test-output)))))

(ert-deftest opencode-integration-replay-test ()
  "Tools' and shell commands' output is shown, and the same once reloaded.
Reasoning and a read's output are folded, and the result goes back to the
LLM.  Parallel tool calls each have their output after them.  An edit
changes the file, and its diff is shown.  A tool that fails shows its
error.  A shell command's output is shown if it fails, and folded
otherwise.  A user's shell command shows its output, as an error if it
fails, and isn't sent to the LLM.  The session is shown the same when
reloaded from the server as it was live."
  (opencode-test-with-session '(("a.txt" . "alpha\nbeta\n") ("b.txt" . "gamma\n"))
    ;; reasoning and a read
    (opencode-test-llm-reply
     '((:reasoning "Let me read it\nto see") (:tool "read" ((path . "a.txt"))))
     "It says alpha.")
    (opencode-test-send "What's in a.txt?")
    (should (equal (concat "❯ What's in a.txt?\n\n"
                           "Let me read it\nto see\n\n"
                           "read a.txt\n```text\nRead file a.txt, lines 1-2\n1: alpha\n2: beta\n```\n\n"
                           "It says alpha.\n\n❯ ")
                   (opencode-test-output)))
    (should (opencode-test-folded-p "Let me read it"))
    (should (opencode-test-folded-p "read a.txt"))
    (should (opencode-test-visible-p "It says alpha."))
    (let ((tool-result (car (last (alist-get 'messages (cadr opencode-test-llm-requests))))))
      (should (equal "tool" (alist-get 'role tool-result)))
      (should (string-search "1: alpha" (format "%s" (alist-get 'content tool-result)))))
    ;; parallel tool calls
    (opencode-test-llm-reply
     '((:tool "grep" ((pattern . "alpha"))) (:tool "grep" ((pattern . "gamma"))))
     "Found both.")
    (opencode-test-send "Find them")
    (should (string-match-p
             (concat "🔍 grep `alpha`\n```text\nFound 1 matches\n.*/a\\.txt:\n  Line 1: alpha\n```\n\n"
                     "🔍 grep `gamma`\n```text\nFound 1 matches\n.*/b\\.txt:\n  Line 1: gamma\n```\n\n"
                     "Found both\\.")
             (opencode-test-output)))
    (should (zerop (hash-table-count opencode--tool-calls)))
    ;; an edit
    (opencode-test-llm-reply
     '((:tool "edit" ((path . "a.txt") (oldString . "beta") (newString . "BETA"))))
     "Edited.")
    (opencode-test-send "Shout beta")
    (should (equal "alpha\nBETA\n" (opencode-integration-test--file "a.txt")))
    (should (string-search "✏ edit a.txt:\n```diff\n@@ -1 +1 @@\n-beta\n+BETA\n```\n\nEdited."
                           (opencode-test-output)))
    (should (opencode-test-visible-p "+BETA"))
    ;; a tool error
    (opencode-test-llm-reply '((:tool "read" ((path . "missing.txt")))) "No file.")
    (opencode-test-send "Read it")
    (should (string-search "read missing.txt\n```text\nFile not found: missing.txt\n```"
                           (opencode-test-output)))
    (should (opencode-test-error-p "File not found"))
    (should (opencode-test-visible-p "File not found"))
    ;; shell tools
    (opencode-test-llm-reply '((:tool "shell" ((command . "echo fine"))))
                             '((:tool "shell" ((command . "echo oops; exit 3"))))
                             "Done.")
    (opencode-test-send "Run them")
    (should (string-search "```bash\necho oops; exit 3\n```\n```text\noops\n\nExit code: 3\n```"
                           (opencode-test-output)))
    (should (opencode-test-visible-p "Exit code: 3"))
    (should (opencode-test-folded-p "echo fine"))
    ;; the user's shell commands
    (let ((requests (length opencode-test-llm-requests)))
      (opencode-test-send "!echo ok")
      (should (string-suffix-p "❯ !echo ok\n\n```text\nok\n```\n\n❯ " (opencode-test-output)))
      (should-not (opencode-test-error-p "ok"))
      (opencode-test-send "!echo bad; exit 2")
      (should (string-suffix-p "❯ !echo bad; exit 2\n\n```text\nbad\n```\n\n❯ "
                               (opencode-test-output)))
      (should (opencode-test-error-p "bad"))
      (should (opencode-test-visible-p "bad"))
      (should (equal requests (length opencode-test-llm-requests))))
    ;; all the same once reloaded
    (let ((live (opencode-test-output)))
      (opencode--reload-session)
      (opencode-test-wait-until (and (opencode-test-at-prompt-p)
                                     (string-search "❯ !echo bad" (opencode-test-output))))
      (should (equal live (opencode-test-output)))
      (should (opencode-test-folded-p "Let me read it"))
      (should (opencode-test-folded-p "echo fine"))
      (should (opencode-test-visible-p "+BETA"))
      (should (opencode-test-error-p "File not found"))
      (should (opencode-test-error-p "bad")))))

(ert-deftest opencode-integration-ask-user-test ()
  "The LLM's questions and tools' permissions are asked of the user.
Questions from the LLM are asked in menus, and their answers shown unfolded.
A tool that needs permission waits for it, and is labeled with the answer.
A denied tool isn't run."
  (opencode-test-with-session
      '(("opencode.json"
         . "{\"permissions\": [{\"action\": \"shell\", \"resource\": \"*\", \"effect\": \"ask\"}]}"))
    (let (menus prompts)
      (cl-letf (((symbol-function 'opencode-forms--show-menu)
                 (lambda (selection) (push selection menus)))
                ((symbol-function 'opencode-permission--prompt)
                 (lambda (&rest args) (push args prompts)))
                ((symbol-function 'opencode--buffer-active-p)
                 (lambda (buffer) (eq buffer opencode-test-session-buffer))))
        ;; questions
        (opencode-test-llm-reply
         '((:tool "question"
            ((questions . [((question . "Pick one?") (header . "Pick")
                            (options . [((label . "Red") (description . "Warm"))
                                        ((label . "Blue") (description . "Cool"))]))
                           ((question . "Which sizes?") (header . "Sizes") (multiple . t)
                            (options . [((label . "S") (description . "Small"))
                                        ((label . "M") (description . "Medium"))
                                        ((label . "L") (description . "Large"))]))
                           ((question . "Anything else?") (header . "Else")
                            (options . [((label . "No") (description . "No"))]))]))))
         "Blue it is.")
        (let ((since (length opencode-test-events)))
          (opencode-test-send "Ask me" t)
          (opencode-test-wait-until menus)
          (should (equal "Pick one?" (alist-get 'description (car opencode-forms--fields))))
          (opencode-forms--answer "Blue")
          (opencode-forms--answer ["S" "L"])
          (opencode-forms--answer nil)
          (opencode-test-wait-finished since))
        (should (equal (concat "❯ Ask me\n\n"
                               "❓ Pick one?\n❓ Which sizes?\n❓ Anything else?\n"
                               "```text\n→ Blue\n→ S, L\n→ Unanswered\n```\n\n"
                               "Blue it is.\n\n❯ ")
                       (opencode-test-output)))
        (should (opencode-test-visible-p "→ Blue"))
        (should (string-search "Blue" (format "%s" (alist-get 'content (car (last (alist-get 'messages (cadr opencode-test-llm-requests))))))))
        ;; permission
        (opencode-test-llm-reply '((:tool "shell" ((command . "echo allowed")))) "Ran it.")
        (let ((since (length opencode-test-events)))
          (opencode-test-send "Run it" t)
          (opencode-test-wait-until prompts)
          (should (equal '("shell" "Shell command: echo allowed" "echo *")
                         (seq-take (car prompts) 3)))
          (should (opencode-test-visible-p "Shell command: echo allowed"))
          (should-not (opencode-test-event-p "session.tool.success" nil since))
          (opencode--send-permission-response "once")
          (opencode-test-wait-finished since))
        (should (string-search "Shell command: echo allowed [accepted]" (opencode-test-output)))
        (should (string-search "```text\nallowed\n```" (opencode-test-output)))
        (should-not opencode-session-pending-permission)
        ;; denied
        (opencode-test-llm-reply '((:tool "shell" ((command . "touch ran")))))
        (let ((since (length opencode-test-events)))
          (opencode-test-send "Run that" t)
          (opencode-test-wait-until opencode-session-pending-permission)
          (opencode--send-permission-response "reject")
          (opencode-test-wait-finished since))
        (should (string-search "Shell command: touch ran [denied]" (opencode-test-output)))
        (should-not (file-exists-p "ran"))))))

(ert-deftest opencode-integration-user-messages-test ()
  "Prompts are of their user messages, which can be reverted and resent.
Sent prompts and slash commands are of their user messages, as when
replayed.  A shell command isn't a user message.  A reverted slash command
resent is of its new user message, not the old one.  A message's turn diff
has only the files that turn changed, before and after.  Reverting a
message undoes its edits and those after it, and puts its text back in the
prompt."
  (let ((opencode-test-snapshots t))
    (opencode-test-with-session '((".opencode/command/greet.md" . "Greet $ARGUMENTS\n")
                                  ("a.txt" . "one\n"))
      (opencode-test-llm-reply
       "Hi." "Hello bob." "Hello again bob."
       '((:tool "edit" ((path . "a.txt") (oldString . "one") (newString . "two"))))
       "Edited."
       '((:tool "write" ((path . "b.txt") (content . "new\n"))))
       "Wrote."
       "Answered again.")
      (opencode-test-send "Hello")
      (opencode-test-send "/greet bob")
      (let* ((id-at (lambda (text)
                      (goto-char (point-min))
                      (search-forward (concat "❯ " text))
                      (opencode--user-message-id-at-point)))
             (user-ids (lambda ()
                         (let (users)
                           (opencode-api-all-messages (opencode-session-id)
                               messages
                             (setq users (cl-loop for message in messages
                                                  when (equal "user" (alist-get 'type message))
                                                  collect (alist-get 'id message))))
                           (opencode-test-wait-until users))))
             (pmark (process-mark (get-buffer-process (current-buffer))))
             (old (funcall id-at "/greet bob"))
             users)
        ;; a reverted slash command, resent
        (opencode-revert-message)
        (opencode-test-wait-until
         (equal "/greet bob" (buffer-substring-no-properties pmark (point-max))))
        ;; not sent yet, so it's of no message
        (goto-char (point-max))
        (should-error (opencode--user-message-id-at-point) :type 'user-error)
        (opencode-test-send "")
        (opencode-test-send "!echo ok")
        (setq users (funcall user-ids))
        (should (equal 2 (length users)))
        (should-not (member old users))
        (should (equal users (mapcar id-at '("Hello" "/greet bob"))))
        (goto-char (opencode-test--search "!echo ok"))
        (should-error (opencode--user-message-id-at-point) :type 'user-error)
        (opencode--reload-session)
        (opencode-test-wait-until (and (opencode-test-at-prompt-p)
                                       (string-search "❯ !echo" (opencode-test-output))))
        ;; the server expands the command into the text of its message
        (should (equal users (mapcar id-at '("Hello" "Greet bob"))))
        ;; turn diffs
        (let ((before (opencode-test-output))
              (turns (length opencode--turns))
              (number (opencode--turn-number (car opencode--turns)))
              (messages (hash-table-count opencode--messages))
              diffs)
          (opencode-test-send "Edit it")
          (opencode-test-send "Write it")
          (should (equal "two\n" (opencode-integration-test--file "a.txt")))
          (goto-char (opencode-test--search "Edit it"))
          (opencode--turn-file-diffs (opencode--user-message-id-at-point)
                                     (lambda (result) (setq diffs (list result))))
          (opencode-test-wait-until diffs)
          (pcase-let ((`((,diff)) diffs))
            (should (equal "a.txt" (alist-get 'file diff)))
            (should (equal "modified" (alist-get 'status diff)))
            (should (equal '("one\n" . "two\n")
                           (opencode--patch-sides (alist-get 'patch diff)))))
          ;; reverted with the turn after it
          (let ((reverted (mapcan (lambda (turn) (copy-sequence (opencode--turn-messages turn)))
                                  (seq-take opencode--turns 2))))
            (goto-char (opencode-test--search "Edit it"))
            (opencode-revert-message)
            (opencode-test-wait-until (equal "one\n" (opencode-integration-test--file "a.txt")))
            (opencode-test-wait-until (not (file-exists-p "b.txt")))
            (opencode-test-wait-until (not (string-search "Edited." (opencode-test-output))))
            (should (equal before (opencode-test-output)))
            (should (equal "Edit it" (buffer-substring-no-properties pmark (point-max))))
            ;; the reverted turns and their messages are forgotten
            (should (equal turns (length opencode--turns)))
            (should-not (seq-some #'opencode--message-info reverted))
            (should (equal messages (hash-table-count opencode--messages)))
            ;; so the turn resent is numbered after the ones left
            (opencode-test-send "")
            (should (equal (1+ number) (opencode--turn-number (car opencode--turns))))))))))

(defun opencode-integration-test--git (&rest args)
  "Run git with ARGS in `default-directory', signaling an error if it fails."
  (unless (zerop (apply #'call-process "git" nil nil nil args))
    (error "Git %s failed in %s" (car args) default-directory)))

(ert-deftest opencode-integration-worktree-moved-and-deleted-test ()
  "A session follows a move to a worktree, and is moved back when it's deleted.
Moved to a worktree, its directory and project root are the worktree's, its
shell commands run there, and a notice of the move is shown once, also when
it's reloaded.  The worktree is trusted, as the main one is.

Once the worktree is deleted, sending input in the open session offers a
move back to the project, and sends the input once moved; switching to an
open session's buffer offers it too.  A closed session reopened is offered
it, going to the same subdirectory of the main worktree.  Where it goes
otherwise is tested by `opencode-relocation-target-test'."
  (opencode-test-with-server
    (let* ((project (opencode-test-make-project '(("a.txt" . "alpha\n")
                                                  ("src/b.txt" . "beta\n"))))
           (process-environment (opencode-test--environment))
           (safe-local-variable-directories (list project))
           (add-worktree (lambda (suffix)
                           (let ((worktree (file-name-as-directory
                                            (concat (directory-file-name project) suffix)))
                                 (default-directory project))
                             (opencode-integration-test--git
                              "worktree" "add" "-q" "--detach" (directory-file-name worktree))
                             worktree)))
           (remove-worktree (lambda (worktree)
                              (let ((default-directory project))
                                (opencode-integration-test--git
                                 "worktree" "remove" "--force"
                                 (directory-file-name worktree)))))
           (worktree (funcall add-worktree "-moved"))
           (notice-count (lambda ()
                           (let ((output (opencode-test-output))
                                 (count 0)
                                 (start 0))
                             (while (setq start (string-search "📁 Moved to" output start))
                               (cl-incf count)
                               (cl-incf start))
                             count)))
           asked)
      ;; Moved to a worktree
      (with-current-buffer (opencode-test-new-session project)
        (opencode-test-llm-reply "Hi.")
        (opencode-test-send "Hello")
        (opencode-api-move-session (opencode-session-id)
            `((directory . ,(opencode--to-server-path (directory-file-name worktree))))
            _result)
        (opencode-test-wait-until (equal worktree default-directory))
        (should (equal worktree opencode--project-root))
        (should (seq-some (lambda (dir) (file-equal-p dir worktree))
                          safe-local-variable-directories))
        (opencode-test-wait-until (opencode-test-at-prompt-p))
        (should (string-suffix-p (format "Hi.\n\n📁 Moved to %s\n\n❯ "
                                         (abbreviate-file-name worktree))
                                 (opencode-test-output)))
        (should (= 1 (funcall notice-count)))
        (opencode-test-send "!pwd")
        (should (string-search (concat (directory-file-name worktree) "\n")
                               (opencode-test-output)))
        (opencode--reload-session)
        (opencode-test-wait-until (and (opencode-test-at-prompt-p)
                                       (string-search "❯ !pwd" (opencode-test-output))))
        (should (= 1 (funcall notice-count))))
      ;; Open sessions whose worktree was deleted.  Not the session above:
      ;; the server answers the shell command's message when that's moved.
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt) (setq asked prompt) t)))
        (let ((deleted-session
               (lambda (suffix)
                 (let ((worktree (funcall add-worktree suffix)))
                   (prog1 (with-current-buffer (opencode-test-new-session worktree)
                            ;; Let the requests made on loading it finish first
                            (opencode-test-wait-until
                             (and (opencode--directory-cache-entry
                                   'slash-commands default-directory)
                                  (opencode--directory-cache-entry
                                   'skills default-directory)))
                            (opencode-test-llm-reply "Hi.")
                            (opencode-test-send "Hello")
                            (current-buffer))
                     (funcall remove-worktree worktree))))))
          ;; Sending input moves it back
          (with-current-buffer (funcall deleted-session "-send")
            (opencode-test-llm-reply "Moved and answered.")
            (goto-char (point-max))
            (insert "Hello")
            (opencode-send-input)
            (opencode-test-wait-until (equal project default-directory))
            (should (string-search "no longer exists" asked))
            (opencode-test-wait-for-output "Moved and answered.")
            (opencode-test-wait-until (opencode-test-at-prompt-p))
            (should (string-search "📁 Moved to" (opencode-test-output))))
          ;; Switching to its buffer moves it back
          (setq asked nil)
          (with-current-buffer (funcall deleted-session "-switch")
            (let ((default-directory project))
              (opencode-api-session (opencode-session-id)
                  session
                (opencode-open-session session)))
            (opencode-test-wait-until (equal project default-directory))
            (should (string-search "no longer exists" asked)))))
      ;; Reopening a closed session whose worktree was deleted moves it back
      (let* ((worktree (funcall add-worktree "-gone"))
             (buffer (opencode-test-new-session (expand-file-name "src/" worktree)))
             (session-id (buffer-local-value 'opencode-session-id buffer))
             opened)
        (setq asked nil)
        (kill-buffer buffer)
        (funcall remove-worktree worktree)
        ;; It's asked from a response handler
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (prompt) (setq asked prompt) t)))
          (opencode-api-session (session-id)
              session
            (opencode-open-session
             session :callback (lambda (_) (setq opened (current-buffer)))))
          (opencode-test-wait-until opened))
        (should (string-search "no longer exists" asked))
        (with-current-buffer opened
          (should (equal session-id opencode-session-id))
          (opencode-test-wait-until (opencode-test-at-prompt-p))
          (should (equal (file-name-concat project "src/") default-directory)))))))

(ert-deftest opencode-integration-new-worktree-test ()
  "A new worktree is made by the server, and a session moved or opened in it.
It's detached, with the name given or one the server picks.  A session is
moved to the same subdirectory of it, with the tree's uncommitted changes
if wanted: then the changes are gone from the original tree, and it's the
same session.  Declining leaves the changes in the original tree.  Outside
a session, a session is opened in it.  It's trusted if the main worktree
is.  See also `opencode-worktree-test' for moving the changes, trusting
the worktree, and busy sessions."
  (opencode-test-with-server
    (let* ((project (opencode-test-make-project '(("a.txt" . "alpha\n")
                                                  ("src/b.txt" . "beta\n"))))
           (process-environment (opencode-test--environment))
           (safe-local-variable-directories (list project))
           (trusted-p (lambda (directory)
                        (seq-some (lambda (dir) (file-equal-p dir directory))
                                  safe-local-variable-directories))))
      (with-current-buffer (opencode-test-new-session (file-name-concat project "src/"))
        ;; Moved with its uncommitted changes
        (let ((session-id opencode-session-id))
          (with-temp-file "b.txt" (insert "changed\n"))
          (with-temp-file "c.txt" (insert "new\n"))
          (let (asked)
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) (setq asked t))))
              (opencode-new-worktree "feature"))
            (should asked))
          (opencode-test-wait-until (string-suffix-p "/feature/src/" default-directory))
          (should (equal session-id opencode-session-id))
          (should (funcall trusted-p opencode--project-root))
          (should (equal "changed\n" (opencode-integration-test--file "b.txt")))
          (should (file-exists-p "c.txt"))
          (let ((default-directory project))
            (should-not (magit-git-lines "status" "--porcelain")))
          (opencode-test-wait-until (opencode-test-at-prompt-p))
          (opencode-test-send "!pwd")
          (should (string-search (concat (directory-file-name default-directory) "\n")
                                 (opencode-test-output)))))
      ;; Moved without its uncommitted changes, untrusted
      (with-current-buffer (opencode-test-new-session project)
        (let ((safe-local-variable-directories nil))
          (with-temp-file "a.txt" (insert "changed\n"))
          (cl-letf (((symbol-function 'y-or-n-p) #'ignore))
            (opencode-new-worktree "kept"))
          (opencode-test-wait-until (string-suffix-p "/kept/" default-directory))
          (should-not safe-local-variable-directories)
          (should (equal "alpha\n" (opencode-integration-test--file "a.txt")))
          (let ((default-directory project))
            (should (equal '(" M a.txt") (magit-git-lines "status" "--porcelain")))
            (should-not (magit-git-lines "for-each-ref" "refs/opencode/")))))
      ;; Outside a session a session is opened in it, named by the server
      (let ((default-directory project)
            (new-session (symbol-function 'opencode-new-session))
            buffer)
        (cl-letf (((symbol-function 'opencode-new-session)
                   (lambda ()
                     (funcall new-session
                              :callback (lambda (_session)
                                          (setq buffer (current-buffer)))))))
          (opencode-new-worktree nil)
          (opencode-test-wait-until buffer))
        (with-current-buffer buffer
          (opencode-test-wait-until (opencode-test-at-prompt-p))
          (should-not (member (file-name-nondirectory (directory-file-name default-directory))
                              '("feature" "kept" "")))
          (should (equal default-directory opencode--project-root))
          (should (file-exists-p "a.txt"))
          (should (funcall trusted-p default-directory))
          ;; A linked worktree, with a detached HEAD
          (should (file-regular-p ".git"))
          (should-not (zerop (call-process "git" nil nil nil
                                           "symbolic-ref" "-q" "HEAD"))))))))

(defun opencode-integration-test--usage (input output)
  "Return an LLM response part reporting INPUT and OUTPUT tokens used."
  `(:raw ((id . "usage") (object . "chat.completion.chunk")
          (choices . [])
          (usage (prompt_tokens . ,input) (completion_tokens . ,output)
                 (total_tokens . ,(+ input output))))))

(ert-deftest opencode-integration-sidebar-test ()
  "The sidebar shows the session's todos, changes, usage, and the step at point.
The changes are those of all its turns, and the usage is refetched when a
step ends.  The times of steps recorded from events are those the server
records.  They're all the same once the session is reloaded.

It shows the session's running subagents and background shells.  RET on a
shell shows its output, followed while it runs, and on a subagent opens its
session.  x cancels them, and they're no longer shown."
  (let ((opencode-test-snapshots t))
    (opencode-test-with-session '(("a.txt" . "one\n"))
      (opencode-test-llm-reply
       '((:reasoning "Let me plan")
         (:tool "todowrite" ((todos . [((content . "Edit a.txt") (status . "completed"))
                                       ((content . "Write b.txt") (status . "pending"))]))))
       '((:tool "edit" ((path . "a.txt") (oldString . "one") (newString . "two"))))
       `("Edited." ,(opencode-integration-test--usage 1000 200))
       '((:tool "write" ((path . "b.txt") (content . "new\nfile\n"))))
       "Wrote.")
      (opencode-test-send "Edit it")
      (opencode-test-send "Write it")
      (let ((session (current-buffer))
            (sidebar (generate-new-buffer " *opencode-sidebar-test*")))
        (unwind-protect
            (cl-flet* ((sidebar-text ()
                         (with-current-buffer sidebar
                           (opencode-sidebar--redraw)
                           (buffer-substring-no-properties (point-min) (point-max))))
                       (goto-job (text)
                         (with-current-buffer sidebar
                           (goto-char (point-min))
                           (search-forward text)))
                       (times (id)
                         (let ((info (opencode--message-info id)))
                           (list (plist-get info :created) (plist-get info :first-token)
                                 (plist-get info :streamed) (plist-get info :completed)
                                 (plist-get info :tokens))))
                       (turn-times ()
                         ;; each turn's steps' ids and times, in order
                         (mapcar (lambda (turn)
                                   (mapcar (lambda (id) (cons id (times id)))
                                           (reverse (opencode--turn-messages turn))))
                                 (reverse opencode--turns))))
              ;; the steps' times recorded from events are the server's
              (let ((live (turn-times))
                    messages)
                (should (equal '(3 2) (mapcar #'length live)))
                (should (equal '(1 2) (mapcar #'opencode--turn-number
                                              (reverse opencode--turns))))
                (should-not (opencode--running-turn))
                (pcase-dolist (`(,_id ,created ,first-token ,streamed ,completed ,_tokens)
                               (apply #'append live))
                  (should (and created first-token streamed completed)))
                (opencode-api-all-messages (opencode-session-id)
                    result
                  (setq messages result))
                (opencode-test-wait-until messages)
                (pcase-dolist (`(,id . ,times) (apply #'append live))
                  (let-alist (seq-find (lambda (message) (equal id (alist-get 'id message)))
                                       messages)
                    (should (equal (list .time.created .time.firstToken .time.streamed
                                         .time.completed .tokens)
                                   times)))))
              (with-current-buffer sidebar
                (opencode-sidebar-mode))
              (cl-letf (((symbol-function 'opencode-sidebar--shown-p)
                         (lambda (buffer) (eq buffer session))))
                (opencode-sidebar--set-session sidebar session)
                ;; todos and the changes of all turns
                (opencode-test-wait-until (string-search "Changed" (sidebar-text)))
                (should (string-search "Todo (1 remaining)\n [✓] Edit a.txt\n [ ] Write b.txt\n"
                                       (sidebar-text)))
                (should (string-search "Changed 2 files +3 -1\n" (sidebar-text)))
                ;; the agent and activity of the step at point
                (goto-char (point-min))
                (search-forward "Edited.")
                (should (string-match-p
                         (rx "Agent\n " (+ nonl) "\n " (+ nonl) "\n " (+ nonl) "\n\n"
                             "Activity\n turn 1 · " (+ (any "0-9.ms ")) "\n"
                             " TTFT " (+ (any "0-9.ms ")) " · " (+ digit) " tok/s\n")
                         (sidebar-text)))
                ;; the prompt of the second turn is of its first step
                (search-forward "Write it")
                (should (string-search "Activity\n turn 2 · " (sidebar-text)))
                (let* ((turn (car opencode--turns))
                       (first-step (car (last (opencode--turn-messages turn)))))
                  (should (equal first-step (cdr (opencode--message-at (point)))))
                  ;; so is its prompt string, though point is moved off it after commands
                  (let ((inhibit-field-text-motion t))
                    (should (equal (cons turn first-step)
                                   (opencode--message-at (line-beginning-position))))))
                ;; before the first prompt is of the first turn's first step
                (goto-char (point-min))
                (should (string-search "Activity\n turn 1 · " (sidebar-text)))
                (let ((turn (car (last opencode--turns))))
                  (should (equal (cons turn (car (last (opencode--turn-messages turn))))
                                 (opencode--message-at (point)))))
                ;; every turn has the session's status
                (goto-char (point-max))
                (should (string-search "· 🚀 idle\n" (sidebar-text)))
                (search-backward "Edited.")
                (should (string-search "· 🚀 idle\n" (sidebar-text)))
                ;; the usage is the session's, refetched when a step ends
                (should (string-search "Usage\n ↑1k ↓200 🧠0\n" (sidebar-text)))
                (opencode-test-llm-reply
                 `("Again." ,(opencode-integration-test--usage 1000 200)))
                (opencode-test-send "Once more")
                (opencode-test-wait-until
                 (string-search "Usage\n ↑2k ↓400 🧠0\n" (sidebar-text)))
                ;; all the same once reloaded
                (let ((live (turn-times))
                      loaded)
                  (setq opencode-session-todos nil
                        opencode-sidebar--data nil)
                  (add-hook 'opencode-session-loaded-functions
                            (lambda (_id) (setq loaded t)) nil t)
                  (opencode--reload-session)
                  (opencode-test-wait-until loaded)
                  (should (equal live (turn-times))))
                (opencode-test-wait-until (string-search "Changed" (sidebar-text)))
                (should (string-search "Todo (1 remaining)\n" (sidebar-text)))
                (should (string-search "Changed 2 files +3 -1\n" (sidebar-text)))
                (opencode-test-wait-until
                 (string-search "Usage\n ↑2k ↓400 🧠0\n" (sidebar-text)))
                ;; running jobs
                (let ((reply (lambda (body)
                               (if (string-search "You are a subagent"
                                                  (opencode-test-llm-user-text body))
                                   '("Child working" (:hold))
                                 "Noted.")))
                      (shown nil))
                  (opencode-test-llm-reply
                   '((:tool "shell" ((command . "echo hello; while [ ! -e go ]; do sleep 0.1; done")
                                     (background . t)))
                     (:tool "subagent" ((agent . "general") (description . "Long task")
                                        (prompt . "Work slowly"))))
                   reply reply
                   ;; woken by the shell, then the subagent, being cancelled
                   reply reply)
                  ;; the buffers shown, and whether in the same window
                  (cl-letf (((symbol-function 'opencode--switch-to-buffer)
                             (lambda (buffer)
                               (push (cons buffer opencode-open-same-window) shown))))
                    (opencode-test-send "Start them")
                    (opencode-test-llm-wait-held)
                    (opencode-test-wait-until (string-search "Jobs (2 running)" (sidebar-text)))
                    (should (string-match-p (rx "\n $ echo hello; while " (+ nonl) " " (+ digit) "s\n")
                                            (sidebar-text)))
                    (should (string-match-p (rx "\n ● @general Long task " (+ digit) "s\n")
                                            (sidebar-text)))
                    ;; the shell's output
                    (goto-job "echo hello")
                    (let* ((shell (with-current-buffer sidebar
                                    (prog1 (alist-get 'shellID (car (opencode-sidebar--job-at-point)))
                                      (opencode-sidebar-visit-job))))
                           (output (opencode-test-wait-until
                                    (seq-find (lambda (buffer)
                                                (equal shell (buffer-local-value
                                                              'opencode-shell-output--id buffer)))
                                              (buffer-list)))))
                      (with-current-buffer output
                        (opencode-test-wait-until (string-search "\nhello\n" (buffer-string)))
                        (should (timerp opencode-shell-output--timer)))
                      ;; in another window
                      (should (equal (cons output nil) (car shown)))
                      ;; the subagent's session
                      (goto-job "Long task")
                      (let ((child (with-current-buffer sidebar
                                     (alist-get 'childSessionID
                                                (car (opencode-sidebar--job-at-point))))))
                        (with-current-buffer sidebar
                          (opencode-sidebar-visit-job))
                        (opencode-test-wait-until (opencode--session-buffer child))
                        (should (equal (cons (opencode--session-buffer child) nil) (car shown)))
                        ;; cancelling the shell ends its output
                        (let ((since (length opencode-test-events)))
                          (goto-job "echo hello")
                          (with-current-buffer sidebar
                            (opencode-sidebar-cancel-job))
                          (opencode-test-wait-until (string-search "Jobs (1 running)" (sidebar-text)))
                          (with-current-buffer output
                            (opencode-test-wait-until (not opencode-shell-output--timer))
                            (should (string-match-p (rx "\n[" (or "killed" "exited") (* nonl) "]\n" eos)
                                                    (buffer-string))))
                          (opencode-test-wait-until
                           (opencode-test-event-p "session.execution.started" nil since))
                          (opencode-test-wait-finished since))
                        (should-not (string-search "echo hello" (sidebar-text)))
                        ;; and the subagent
                        (let ((since (length opencode-test-events)))
                          (goto-job "Long task")
                          (with-current-buffer sidebar
                            (opencode-sidebar-cancel-job))
                          (opencode-test-wait-until
                           (opencode-test-event-p "session.execution.interrupted" child))
                          (setq opencode-test-llm-held nil)
                          (opencode-test-wait-until (not (string-search "Jobs" (sidebar-text))))
                          (opencode-test-wait-until
                           (opencode-test-event-p "session.execution.started" nil since))
                          (opencode-test-wait-finished since))))))))
          (kill-buffer sidebar))))))

(ert-deftest opencode-integration-session-preview-test ()
  "A session that isn't open is previewed with its last reply's text."
  (skip-unless (require 'consult nil t))
  (opencode-test-with-session nil
    (opencode-test-llm-reply '((:reasoning "Thinking") "The answer is 42."))
    (opencode-test-send "What is the answer?")
    (let ((session `((id . ,opencode-session-id) (title . "Answer")))
          (state (opencode--session-preview-state))
          (kill-buffer-query-functions nil))
      (kill-buffer (current-buffer))
      (funcall state 'setup nil)
      (funcall state 'preview session)
      (let ((preview (get-buffer opencode--session-preview-buffer-name)))
        (should (eq preview (window-buffer (selected-window))))
        (opencode-test-wait-until
         (with-current-buffer preview
           (string-search "The answer is 42." (buffer-string))))
        (with-current-buffer preview
          (should (derived-mode-p 'markdown-ts-view-mode))
          (should (string-prefix-p "# Answer" (buffer-string)))
          (should-not (string-search "Thinking" (buffer-string))))
        (funcall state 'preview nil)
        (funcall state 'exit nil)
        (funcall state 'return nil)
        (should-not (buffer-live-p preview))))))

(ert-deftest opencode-integration-finished-editing-diagnostics-test ()
  "Diagnostics reported after a turn's edits are sent to the LLM in a new turn."
  (let ((opencode-test-snapshots t))
    (opencode-test-with-session '(("a.txt" . "alpha\n"))
      (let* ((reported nil)
             (opencode-files-finished-editing-functions
              (list (lambda (_files done)
                      (if reported
                          (funcall done)
                        (setq reported t)
                        (funcall done "- a.txt:1:1: error: ALPHA is wrong"))))))
        (opencode-test-llm-reply
         '((:tool "edit" ((path . "a.txt") (oldString . "alpha") (newString . "ALPHA"))))
         "Edited."
         "Fixed.")
        (opencode-test-send "Shout alpha")
        (opencode-test-wait-for-output "Fixed.")
        (opencode-test-wait-until (opencode-test-at-prompt-p))
        (let ((text (opencode-test-llm-user-text (car (last opencode-test-llm-requests)))))
          (should (string-prefix-p "Diagnostics were reported after editing files." text))
          (should (string-search "- a.txt:1:1: error: ALPHA is wrong" text)))
        ;; The diagnostics are the input of the latest turn
        (should (equal 2 (length opencode--turns)))
        (should (string-prefix-p "Diagnostics were reported"
                                 (buffer-substring-no-properties
                                  (opencode--turn-start (car opencode--turns))
                                  (point-max))))))))

(provide 'opencode-integration-test)
;;; opencode-integration-test.el ends here
