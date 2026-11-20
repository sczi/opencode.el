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
  "A prompt is sent to the LLM, and its streamed answer shown before a prompt."
  (opencode-test-with-session nil
    (opencode-test-llm-reply '("Hello" " from the" " LLM."))
    (opencode-test-send "Say hello")
    (should (equal "Say hello" (opencode-test-llm-user-text
                                (car opencode-test-llm-requests))))
    (should (equal "❯ Say hello\n\nHello from the LLM.\n\n❯ "
                   (opencode-test-output)))))

(ert-deftest opencode-integration-stats-test ()
  "The stats of the session's project show its prompt, steps, model and tools."
  ;; Projects are identified by their first commit, which is unique only
  ;; with unique files, so the stats don't include other tests' sessions
  (opencode-test-with-session `(("a.txt" . "alpha\n") ("unique" . ,(format "%s" (random))))
    (opencode-test-llm-reply '((:tool "read" ((path . "a.txt")))) "It says alpha.")
    (opencode-test-send "What's in a.txt?")
    (opencode-stats t)
    (with-current-buffer (opencode-stats--buffer opencode-test--connection)
      (opencode-test-wait-until (and opencode-stats--project opencode-stats--data))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should-not (plist-get opencode-stats--data :error))
        (should (string-search "1 session · 1 prompt · 2 steps" text))
        (should (string-search "100% tool success · 1 active day" text))
        (should (string-match-p "^  read +1 +0 +0% " text))
        ;; The chart's bars together have them all
        (should (equal 2 (apply #'+ (mapcar (lambda (bucket) (alist-get 'steps (cdr bucket)))
                                            (plist-get opencode-stats--data :buckets)))))))))

(ert-deftest opencode-integration-stats-sessions-test ()
  "Today's stats list its sessions with their steps, and RET opens one."
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

(ert-deftest opencode-integration-title-test ()
  "The buffer is renamed after the title the server generates."
  (opencode-test-with-session nil
    (opencode-test-llm-reply "Hi.")
    (opencode-test-send "Hello")
    (opencode-test-wait-until (equal opencode-session-title opencode-test-title))
    (should (equal (format "*OpenCode: %s*" opencode-test-title) (buffer-name)))))

(ert-deftest opencode-integration-reasoning-and-tool-test ()
  "Reasoning and a read's output are folded, and the result goes back to the LLM."
  (opencode-test-with-session '(("a.txt" . "alpha\nbeta\n"))
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
      (should (string-search "1: alpha" (format "%s" (alist-get 'content tool-result)))))))

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

(ert-deftest opencode-integration-parallel-tools-test ()
  "Parallel tool calls each have their output after them."
  (opencode-test-with-session '(("a.txt" . "alpha\n") ("b.txt" . "gamma\n"))
    (opencode-test-llm-reply
     '((:tool "grep" ((pattern . "alpha"))) (:tool "grep" ((pattern . "gamma"))))
     "Found both.")
    (opencode-test-send "Find them")
    (should (string-match-p
             (concat "🔍 grep `alpha`\n```text\nFound 1 matches\n.*/a\\.txt:\n  Line 1: alpha\n```\n\n"
                     "🔍 grep `gamma`\n```text\nFound 1 matches\n.*/b\\.txt:\n  Line 1: gamma\n```\n\n"
                     "Found both\\.")
             (opencode-test-output)))
    (should (zerop (hash-table-count opencode--tool-calls)))))

(ert-deftest opencode-integration-edit-test ()
  "An edit changes the file, and its diff is shown."
  (opencode-test-with-session '(("a.txt" . "alpha\nbeta\n"))
    (opencode-test-llm-reply
     '((:tool "edit" ((path . "a.txt") (oldString . "beta") (newString . "BETA"))))
     "Edited.")
    (opencode-test-send "Shout beta")
    (should (equal "alpha\nBETA\n" (opencode-integration-test--file "a.txt")))
    (should (string-search "✏ edit a.txt:\n```diff\n@@ -1 +1 @@\n-beta\n+BETA\n```\n\nEdited."
                           (opencode-test-output)))
    (should (opencode-test-visible-p "+BETA"))))

(ert-deftest opencode-integration-tool-error-test ()
  "A tool that fails shows its error."
  (opencode-test-with-session nil
    (opencode-test-llm-reply '((:tool "read" ((path . "missing.txt")))) "No file.")
    (opencode-test-send "Read it")
    (should (string-search "read missing.txt\n```text\nFile not found: missing.txt\n```"
                           (opencode-test-output)))
    (should (opencode-test-error-p "File not found"))
    (should (opencode-test-visible-p "File not found"))))

(ert-deftest opencode-integration-shell-tool-test ()
  "A shell command's output is shown if it fails, and folded otherwise."
  (opencode-test-with-session nil
    (opencode-test-llm-reply '((:tool "shell" ((command . "echo fine"))))
                             '((:tool "shell" ((command . "echo oops; exit 3"))))
                             "Done.")
    (opencode-test-send "Run them")
    (should (string-search "```bash\necho oops; exit 3\n```\n```text\noops\n\nExit code: 3\n```"
                           (opencode-test-output)))
    (should (opencode-test-visible-p "Exit code: 3"))
    (should (opencode-test-folded-p "echo fine"))))

(ert-deftest opencode-integration-user-shell-test ()
  "A user's shell command shows its output, as an error if it fails."
  (opencode-test-with-session nil
    (opencode-test-send "!echo ok")
    (should (string-suffix-p "❯ !echo ok\n\n```text\nok\n```\n\n❯ " (opencode-test-output)))
    (should-not (opencode-test-error-p "ok"))
    (opencode-test-send "!echo bad; exit 2")
    (should (string-suffix-p "❯ !echo bad; exit 2\n\n```text\nbad\n```\n\n❯ "
                             (opencode-test-output)))
    (should (opencode-test-error-p "bad"))
    (should (opencode-test-visible-p "bad"))
    (should-not opencode-test-llm-requests)))

(ert-deftest opencode-integration-abort-test ()
  "Aborting while the LLM's answering shows it was interrupted, then a prompt."
  (opencode-test-with-session nil
    (opencode-test-llm-reply '("Partial answer" (:hold)))
    (let ((since (length opencode-test-events)))
      (opencode-test-send "Take your time" t)
      (opencode-test-wait-for-output "Partial answer")
      (should (equal "busy" opencode-session-status))
      (opencode-abort-session)
      (opencode-test-wait-finished since))
    (should (opencode-test-error-p "Interrupted"))
    (should (string-suffix-p "Partial answer\n\nInterrupted\n\n❯ " (opencode-test-output)))))

(ert-deftest opencode-integration-abort-subagents-test ()
  "Aborting with a prefix argument also aborts the session's subagents."
  (opencode-test-with-session nil
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
                              (car (last opencode-test-llm-requests))))))))

(ert-deftest opencode-integration-alert-after-background-test ()
  "A session isn't alerted as finished until its background shell finishes."
  (opencode-test-with-session nil
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
        (should (string-search "It went." (opencode-test-output)))))))

(ert-deftest opencode-integration-continued-step-test ()
  "Text of a step the server continues is spaced from the next step's."
  (opencode-test-with-session nil
    (opencode-test-llm-reply '("Cut off" (:finish "length"))
                             "Next text")
    (opencode-test-send "Go on")
    (should (string-suffix-p "❯ Go on\n\nCut off\n\nNext text\n\n❯ "
                             (opencode-test-output)))))

(ert-deftest opencode-integration-provider-error-test ()
  "An error from the LLM provider is shown, then a prompt."
  (opencode-test-with-session nil
    (opencode-test-llm-reply
     '((:raw ((error . ((message . "Bad request from provider")
                        (type . "invalid_request_error")))))
       (:finish nil)))
    (opencode-test-send "Fail")
    (should (string-suffix-p "❯ Fail\n\nBad request from provider\n\n❯ "
                             (opencode-test-output)))
    (should (opencode-test-error-p "Bad request from provider"))))

(ert-deftest opencode-integration-replay-test ()
  "A session is shown the same when reloaded from the server as it was live."
  (opencode-test-with-session '(("a.txt" . "alpha\nbeta\n"))
    (opencode-test-llm-reply
     '((:reasoning "Thinking\nmore thoughts") (:tool "grep" ((pattern . "alpha"))))
     '((:tool "edit" ((path . "a.txt") (oldString . "beta") (newString . "BETA"))))
     '((:tool "read" ((path . "missing.txt"))))
     '((:tool "shell" ((command . "echo oops; exit 3"))))
     "All done.")
    (opencode-test-send "Do things")
    (opencode-test-send "!echo hi; exit 2")
    (let ((live (opencode-test-output)))
      (opencode--reload-session)
      (opencode-test-wait-until (and (opencode-test-at-prompt-p)
                                     (string-search "❯ !echo" (opencode-test-output))))
      (should (equal live (opencode-test-output)))
      (should (opencode-test-folded-p "Thinking"))
      (should (opencode-test-visible-p "+BETA"))
      (should (opencode-test-error-p "File not found")))))

(ert-deftest opencode-integration-question-test ()
  "Questions from the LLM are asked in menus, and their answers shown unfolded."
  (opencode-test-with-session nil
    (let (menus)
      (cl-letf (((symbol-function 'opencode-forms--show-menu)
                 (lambda (selection) (push selection menus)))
                ((symbol-function 'opencode--buffer-active-p)
                 (lambda (buffer) (eq buffer opencode-test-session-buffer))))
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
        (should (string-search "Blue" (format "%s" (alist-get 'content (car (last (alist-get 'messages (cadr opencode-test-llm-requests))))))))))))

(ert-deftest opencode-integration-permission-test ()
  "A tool that needs permission waits for it, and is labeled with the answer."
  (opencode-test-with-session
      '(("opencode.json"
         . "{\"permissions\": [{\"action\": \"shell\", \"resource\": \"*\", \"effect\": \"ask\"}]}"))
    (let (prompts)
      (cl-letf (((symbol-function 'opencode-permission--prompt)
                 (lambda (&rest args) (push args prompts)))
                ((symbol-function 'opencode--buffer-active-p)
                 (lambda (buffer) (eq buffer opencode-test-session-buffer))))
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
        (should-not opencode-session-pending-permission)))))

(ert-deftest opencode-integration-permission-denied-test ()
  "A denied tool isn't run."
  (opencode-test-with-session
      '(("opencode.json"
         . "{\"permissions\": [{\"action\": \"shell\", \"resource\": \"*\", \"effect\": \"ask\"}]}"))
    (cl-letf (((symbol-function 'opencode-permission--prompt) #'ignore))
      (opencode-test-llm-reply '((:tool "shell" ((command . "touch ran")))))
      (let ((since (length opencode-test-events)))
        (opencode-test-send "Run it" t)
        (opencode-test-wait-until opencode-session-pending-permission)
        (opencode--send-permission-response "reject")
        (opencode-test-wait-finished since))
      (should (string-search "Shell command: touch ran [denied]" (opencode-test-output)))
      (should-not (file-exists-p "ran")))))

(ert-deftest opencode-integration-revert-test ()
  "Reverting a message undoes its edits, and puts its text back in the prompt."
  (opencode-test-with-session '(("a.txt" . "one\n"))
    (opencode-test-llm-reply
     "First answer."
     '((:tool "edit" ((path . "a.txt") (oldString . "one") (newString . "two"))))
     "Edited.")
    (opencode-test-send "Hello")
    (opencode-test-send "Edit it")
    (should (equal "two\n" (opencode-integration-test--file "a.txt")))
    (goto-char (opencode-test--search "Edit it"))
    (opencode-revert-message)
    (opencode-test-wait-until (equal "one\n" (opencode-integration-test--file "a.txt")))
    (opencode-test-wait-until (not (string-search "Edited." (opencode-test-output))))
    (should (equal "❯ Hello\n\nFirst answer.\n\n❯ " (opencode-test-output)))
    (should (equal "Edit it" (buffer-substring-no-properties
                              (process-mark (get-buffer-process (current-buffer)))
                              (point-max))))))

(ert-deftest opencode-integration-user-message-id-test ()
  "Sent prompts and slash commands are of their user messages, as when replayed.
A shell command isn't a user message."
  (opencode-test-with-session '((".opencode/command/greet.md" . "Greet $ARGUMENTS\n"))
    (opencode-test-llm-reply "Hi." "Hello bob.")
    (opencode-test-send "Hello")
    (opencode-test-send "/greet bob")
    (opencode-test-send "!echo ok")
    (let ((id-at (lambda (text)
                   (goto-char (point-min))
                   (search-forward (concat "❯ " text))
                   (opencode--user-message-id-at-point)))
          users)
      (opencode-api-all-messages (opencode-session-id)
          messages
        (setq users (cl-loop for message in messages
                             when (equal "user" (alist-get 'type message))
                             collect (alist-get 'id message))))
      (opencode-test-wait-until users)
      (should (equal 2 (length users)))
      (should (equal users (mapcar id-at '("Hello" "/greet bob"))))
      (goto-char (opencode-test--search "!echo ok"))
      (should-error (opencode--user-message-id-at-point) :type 'user-error)
      (opencode--reload-session)
      (opencode-test-wait-until (and (opencode-test-at-prompt-p)
                                     (string-search "❯ !echo" (opencode-test-output))))
      ;; the server expands the command into the text of its message
      (should (equal users (mapcar id-at '("Hello" "Greet bob")))))))

(ert-deftest opencode-integration-turn-file-diffs-test ()
  "A message's turn diff has only the files that turn changed, before and after."
  (opencode-test-with-session '(("a.txt" . "one\n"))
    (opencode-test-llm-reply
     '((:tool "edit" ((path . "a.txt") (oldString . "one") (newString . "two"))))
     "Edited."
     '((:tool "write" ((path . "b.txt") (content . "new\n"))))
     "Wrote.")
    (opencode-test-send "Edit it")
    (opencode-test-send "Write it")
    (goto-char (opencode-test--search "Edit it"))
    (let (diffs)
      (opencode--turn-file-diffs (opencode--user-message-id-at-point)
                                 (lambda (result) (setq diffs (list result))))
      (opencode-test-wait-until diffs)
      (pcase-let ((`((,diff)) diffs))
        (should (equal "a.txt" (alist-get 'file diff)))
        (should (equal "modified" (alist-get 'status diff)))
        (should (equal '("one\n" . "two\n")
                       (opencode--patch-sides (alist-get 'patch diff))))))))

(ert-deftest opencode-integration-sidebar-test ()
  "The sidebar shows the session's todos and the files changed over all its turns.
They're shown again when the session is reloaded."
  (opencode-test-with-session '(("a.txt" . "one\n"))
    (opencode-test-llm-reply
     '((:tool "todowrite" ((todos . [((content . "Edit a.txt") (status . "completed"))
                                     ((content . "Write b.txt") (status . "pending"))]))))
     '((:tool "edit" ((path . "a.txt") (oldString . "one") (newString . "two"))))
     "Edited."
     '((:tool "write" ((path . "b.txt") (content . "new\nfile\n"))))
     "Wrote.")
    (opencode-test-send "Edit it")
    (opencode-test-send "Write it")
    (let ((session (current-buffer))
          (sidebar (generate-new-buffer " *opencode-sidebar-test*")))
      (unwind-protect
          (cl-flet ((sidebar-text ()
                      (with-current-buffer sidebar
                        (opencode-sidebar--redraw)
                        (buffer-substring-no-properties (point-min) (point-max)))))
            (with-current-buffer sidebar
              (opencode-sidebar-mode))
            (cl-letf (((symbol-function 'opencode-sidebar--shown-p)
                       (lambda (buffer) (eq buffer session))))
              (opencode-sidebar--set-session sidebar session)
              (opencode-test-wait-until (string-search "Changed" (sidebar-text)))
              (should (string-search "Todo (1 remaining)\n [✓] Edit a.txt\n [ ] Write b.txt\n"
                                     (sidebar-text)))
              (should (string-search "Changed 2 files +3 -1\n" (sidebar-text)))
              (setq opencode-session-todos nil
                    opencode-sidebar--data nil)
              (let (loaded)
                (add-hook 'opencode-session-loaded-functions
                          (lambda (_id) (setq loaded t)) nil t)
                (opencode--reload-session)
                (opencode-test-wait-until loaded))
              (opencode-test-wait-until (string-search "Changed" (sidebar-text)))
              (should (string-search "Todo (1 remaining)\n" (sidebar-text)))
              (should (string-search "Changed 2 files +3 -1\n" (sidebar-text)))))
        (kill-buffer sidebar)))))

(ert-deftest opencode-integration-sidebar-usage-test ()
  "The sidebar's usage is the session's, refetched when a step ends."
  (opencode-test-with-session nil
    (opencode-test-llm-reply
     "First."
     '("Second." (:raw ((id . "usage") (object . "chat.completion.chunk")
                        (choices . [])
                        (usage (prompt_tokens . 1000) (completion_tokens . 200)
                               (total_tokens . 1200))))))
    (opencode-test-send "One")
    (let ((session (current-buffer))
          (sidebar (generate-new-buffer " *opencode-sidebar-test*")))
      (unwind-protect
          (cl-flet ((sidebar-text ()
                      (with-current-buffer sidebar
                        (opencode-sidebar--redraw)
                        (buffer-substring-no-properties (point-min) (point-max)))))
            (with-current-buffer sidebar
              (opencode-sidebar-mode))
            (cl-letf (((symbol-function 'opencode-sidebar--shown-p)
                       (lambda (buffer) (eq buffer session))))
              (opencode-sidebar--set-session sidebar session)
              (opencode-test-wait-until (string-search "Usage\n" (sidebar-text)))
              (opencode-test-send "Two")
              (opencode-test-wait-until
               (string-search "Usage\n ↑1k ↓200 🧠0\n" (sidebar-text)))))
        (kill-buffer sidebar)))))

(ert-deftest opencode-integration-sidebar-activity-test ()
  "The sidebar shows the agent and activity of the step at point.
The times of steps recorded from events are those the server records,
and are the same once the session is reloaded."
  (opencode-test-with-session '(("a.txt" . "alpha\n"))
    (opencode-test-llm-reply
     '((:reasoning "Let me look") (:tool "read" ((path . "a.txt"))))
     '("It says alpha." (:raw ((id . "usage") (object . "chat.completion.chunk")
                               (choices . [])
                               (usage (prompt_tokens . 1000) (completion_tokens . 200)
                                      (total_tokens . 1200)))))
     "Second answer.")
    (opencode-test-send "What's in a.txt?")
    (opencode-test-send "And now?")
    (let ((session (current-buffer))
          (sidebar (generate-new-buffer " *opencode-sidebar-test*"))
          (live (mapcar (lambda (turn)
                          (mapcar (lambda (id)
                                    (cons id (opencode--message-info id)))
                                  (reverse (opencode--turn-messages turn))))
                        (reverse opencode--turns))))
      (unwind-protect
          (cl-flet ((sidebar-text ()
                      (with-current-buffer sidebar
                        (opencode-sidebar--redraw)
                        (buffer-substring-no-properties (point-min) (point-max))))
                    (times (info)
                      (list (plist-get info :created) (plist-get info :first-token)
                            (plist-get info :streamed) (plist-get info :completed)
                            (plist-get info :tokens))))
            ;; the tool call's step and the answer's, then the second answer's
            (should (equal '(2 1) (mapcar #'length live)))
            (should (equal '(1 2) (mapcar #'opencode--turn-number (reverse opencode--turns))))
            (should-not (opencode--running-turn))
            (pcase-dolist (`(,_id . ,info) (apply #'append live))
              (should (plist-get info :first-token))
              (should (plist-get info :streamed))
              (should (plist-get info :completed)))
            (let (messages)
              (opencode-api-all-messages (opencode-session-id)
                  result
                (setq messages result))
              (opencode-test-wait-until messages)
              (pcase-dolist (`(,id . ,info) (apply #'append live))
                (let-alist (seq-find (lambda (message) (equal id (alist-get 'id message)))
                                     messages)
                  (should (equal (list .time.created .time.firstToken .time.streamed
                                       .time.completed .tokens)
                                 (times info))))))
            (with-current-buffer sidebar
              (opencode-sidebar-mode))
            (opencode-sidebar--set-session sidebar session)
            (goto-char (point-min))
            (search-forward "It says alpha")
            (should (string-match-p
                     (rx "Agent\n " (+ nonl) "\n " (+ nonl) "\n " (+ nonl) "\n\n"
                         "Activity\n turn 1 · " (+ (any "0-9.ms ")) "\n"
                         " TTFT " (+ (any "0-9.ms ")) " · " (+ digit) " tok/s\n")
                     (sidebar-text)))
            ;; the prompt of the second turn is of its step
            (search-forward "And now?")
            (should (string-search "Activity\n turn 2 · " (sidebar-text)))
            (should (equal (car (opencode--turn-messages (car opencode--turns)))
                           (cdr (opencode--message-at (point)))))
            ;; so is its prompt string, though point is moved off it after commands
            (let ((inhibit-field-text-motion t))
              (should (equal (cons (car opencode--turns)
                                   (car (opencode--turn-messages (car opencode--turns))))
                             (opencode--message-at (line-beginning-position)))))
            ;; before the first prompt is of the first turn
            (goto-char (point-min))
            (should (string-search "Activity\n turn 1 · " (sidebar-text)))
            ;; every turn has the session's status
            (goto-char (point-max))
            (should (string-search "· 🚀 idle\n" (sidebar-text)))
            (search-backward "It says alpha")
            (should (string-search "· 🚀 idle\n" (sidebar-text)))
            (let (loaded)
              (add-hook 'opencode-session-loaded-functions
                        (lambda (_id) (setq loaded t)) nil t)
              (opencode--reload-session)
              (opencode-test-wait-until loaded))
            (should (equal (mapcar (lambda (turn)
                                     (mapcar (lambda (step) (times (cdr step))) turn))
                                   live)
                           (mapcar (lambda (turn)
                                     (mapcar (lambda (id) (times (opencode--message-info id)))
                                             (reverse (opencode--turn-messages turn))))
                                   (reverse opencode--turns)))))
        (kill-buffer sidebar)))))

(ert-deftest opencode-integration-kill-session-test ()
  "Killing a session deletes it on the server, and kills its buffer."
  (opencode-test-with-session nil
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
                                (point-max)))))))

(provide 'opencode-integration-test)
;;; opencode-integration-test.el ends here
