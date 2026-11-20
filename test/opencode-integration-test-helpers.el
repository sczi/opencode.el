;;; opencode-integration-test-helpers.el --- Run tests against a real opencode server  -*- lexical-binding: t; -*-

;;; Commentary:

;; Helpers for tests run against a real opencode server, with a scripted LLM.
;;
;; One server is shared by the tests of a run.  It's started on first use with
;; its home, config, data, cache, state and database in a temporary directory,
;; so it never touches the user's own.  It runs in opencode's simulation mode:
;; all its outbound HTTP is answered by a simulated network, and requests to the
;; LLM are sent to us over a websocket, to be answered by what the test queued
;; with `opencode-test-llm-reply'.  Each test gets a fresh project directory.
;;
;; `opencode-test-stop-server' stops it, as does exiting Emacs.

;;; Code:

(require 'ert)
(require 'opencode)
(require 'opencode-test-helpers)
(require 'websocket)

(defvar opencode-test-port nil
  "Port of the test server, picked by it when it starts.")

(defvar opencode-test--drive-url nil
  "URL of the test server's simulation websocket.")

(defconst opencode-test--drive-instance "opencode-el-test"
  "Name of the test server's Drive instance, whose manifest sets its websockets.")

(defconst opencode-test-model "sim/sim-model"
  "The test server's default model, answered by the scripted LLM.")

(defconst opencode-test--server-script
  ;; A background job's stdin is /dev/null, so the watcher gets it as fd 3
  (concat "exec 3<&0; "
          "\"$0\" serve --port \"$1\" --hostname 127.0.0.1 </dev/null 3<&- & server=$!; "
          "(cat <&3 >/dev/null; kill $server 2>/dev/null) & watcher=$!; "
          "wait $server; status=$?; kill $watcher 2>/dev/null; exit $status")
  "Shell script running the test server, $0 with port $1, until stdin closes.")

(defvar opencode-test--root nil
  "Temporary directory holding the test server's files, or nil if it's not started.")

(defvar opencode-test--process nil
  "The test server's process.")

(defvar opencode-test--password nil
  "The test server's password.")

(defvar opencode-test--connection nil
  "Connection to the test server.")

(defvar opencode-test--ws nil
  "Websocket to the test server's simulated LLM.")

(defvar opencode-test--rpc-id 0
  "Id of the last JSON-RPC request sent to the simulated LLM.")

(defvar opencode-test--rpc-results nil
  "Hash table of JSON-RPC request ids to their response.")

(defvar opencode-test--project-counter 0
  "Number of projects created by `opencode-test-make-project'.")

(defvar opencode-test--tool-call-counter 0
  "Number of tool calls sent by the scripted LLM.")

(defvar opencode-test--llm-script nil
  "Responses queued for the scripted LLM's requests, next first.")

(defvar opencode-test-llm-requests nil
  "Bodies of the requests to the scripted LLM in this test, oldest first.
Those to generate a session title aren't included.")

(defvar opencode-test-llm-held nil
  "Ids of the LLM requests the test holds, finished by it, latest first.")

(defvar opencode-test-events nil
  "Events from the test server handled in this test, latest first.")

(defvar opencode-test--errors nil
  "Errors in async code during this test, which fail it.")

(defvar opencode-test-session-buffer nil
  "The buffer of the session `opencode-test-with-session' opened.")

(defvar opencode-test-title "Test title"
  "Title the scripted LLM gives sessions.")

;;; Server

(defun opencode-test--environment ()
  "Return `process-environment' for the test server and git."
  (append
   (mapcar
    (lambda (pair) (concat (car pair) "=" (cdr pair)))
    `(("HOME" . ,opencode-test--root)
      ("OPENCODE_TEST_HOME" . ,opencode-test--root)
      ("OPENCODE_DB" . ,(file-name-concat opencode-test--root "opencode.db"))
      ("XDG_CONFIG_HOME" . ,(file-name-concat opencode-test--root "config"))
      ("XDG_DATA_HOME" . ,(file-name-concat opencode-test--root "data"))
      ("XDG_CACHE_HOME" . ,(file-name-concat opencode-test--root "cache"))
      ("XDG_STATE_HOME" . ,(file-name-concat opencode-test--root "state"))
      ("OPENCODE_PASSWORD" . ,opencode-test--password)
      ("OPENCODE_SIMULATE" . "1")
      ("OPENCODE_DRIVE" . ,opencode-test--drive-instance)
      ("OPENCODE_DISABLE_AUTOUPDATE" . "1")
      ("OPENCODE_DISABLE_CLAUDE_CODE" . "1")
      ;; The todowrite tool is experimental
      ("OPENCODE_FLAGS" . "todo")
      ("GIT_AUTHOR_NAME" . "opencode-test")
      ("GIT_AUTHOR_EMAIL" . "opencode-test@example.com")
      ("GIT_COMMITTER_NAME" . "opencode-test")
      ("GIT_COMMITTER_EMAIL" . "opencode-test@example.com")))
   ;; Our own settings for opencode mustn't reach the test server, nor
   ;; git's, eg. GIT_LITERAL_PATHSPECS in a hook run by magit breaks the
   ;; server's snapshots
   (seq-remove (lambda (variable)
                 (string-match-p "\\`\\(OPENCODE\\|GIT\\)_" variable))
               process-environment)))

(defun opencode-test--write-config ()
  "Write the test server's global config."
  (let ((file (file-name-concat opencode-test--root "config" "opencode" "opencode.json")))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert
       (json-encode
        `((model . ,opencode-test-model)
          (update . "disable")
          (permissions . [((action . "*") (resource . "*") (effect . "allow"))])
          (providers
           . ((sim . ((name . "Sim")
                      (package . "@opencode/ai/providers/openai-compatible")
                      ;; The simulated network sends its requests to us
                      (settings . ((baseURL . "https://api.openai.com/v1")
                                   (apiKey . "sim")))
                      (models . ((sim-model . ((name . "Sim Model")))))))))))))))

(defun opencode-test--free-port ()
  "Return a port on 127.0.0.1 that's free now."
  (let ((server (make-network-process :name "opencode-test-free-port"
                                      :server t :host "127.0.0.1" :service t
                                      :noquery t)))
    (prog1 (process-contact server :service)
      (delete-process server))))

(defun opencode-test--write-drive-manifest ()
  "Write the test server's Drive manifest, giving its websockets free ports.
Set `opencode-test--drive-url' to the one of its simulated LLM."
  (let ((file (file-name-concat opencode-test--root "state" "opencode-drive" "instances"
                                (concat opencode-test--drive-instance ".json"))))
    (setq opencode-test--drive-url (format "ws://127.0.0.1:%d" (opencode-test--free-port)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert
       (json-encode
        `((endpoints
           ;; The server doesn't open the UI's websocket, but it must have one
           . ((ui . ,(format "ws://127.0.0.1:%d" (opencode-test--free-port)))
              (backend . ,opencode-test--drive-url)))))))))

(defun opencode-test--server-live-p ()
  "Return non-nil if the test server is running and its LLM is attached."
  (and (process-live-p opencode-test--process)
       opencode-test--ws
       (websocket-openp opencode-test--ws)))

(defun opencode-test-ensure-server ()
  "Start the test server unless it's running, skipping the test if it can't be."
  (unless (executable-find opencode-command)
    (ert-skip (format "%s isn't installed" opencode-command)))
  (unless (opencode-test--server-live-p)
    (opencode-test-stop-server)
    (opencode-test--start-server)))

(defconst opencode-test--root-prefix "opencode-el-test-server-"
  "Prefix of the name of the test server's temporary directory.")

(defun opencode-test--start-server ()
  "Start the test server, and attach to its simulated LLM."
  (setq opencode-test--root (file-name-as-directory
                             (file-truename (make-temp-file opencode-test--root-prefix t)))
        opencode-test--password (format "%x" (random most-positive-fixnum))
        opencode-test--project-counter 0
        opencode-test-port nil)
  (opencode-test--write-config)
  (opencode-test--write-drive-manifest)
  (let* ((buffer (get-buffer-create " *opencode-test-server*"))
         (process-environment (opencode-test--environment))
         (default-directory opencode-test--root))
    (with-current-buffer buffer
      (erase-buffer))
    (setq opencode-test--process
          (make-process
           :name "opencode-test-server"
           :buffer buffer
           ;; The server is killed when our end of its stdin closes, so it
           ;; dies with Emacs even if Emacs is killed without cleaning up.
           :command (list "sh" "-c" opencode-test--server-script
                          opencode-command "0")
           :noquery t
           :connection-type 'pipe))
    (opencode-test--wait
     (lambda ()
       (unless (process-live-p opencode-test--process)
         (error "The test opencode server exited: %s"
                (with-current-buffer buffer (buffer-string))))
       (with-current-buffer buffer
         (let ((output (buffer-string)))
           (and (string-search "drive backend websocket" output)
                (string-match "server listening on http://127\\.0\\.0\\.1:\\([0-9]+\\)" output)
                (setq opencode-test-port (string-to-number (match-string 1 output)))))))
     "the test server to start" 30))
  (setq opencode-test--connection
        (opencode--make-connection
         :name 'opencode-test
         :url (format "http://127.0.0.1:%d" opencode-test-port)
         :password opencode-test--password))
  (opencode-test--attach-llm))

(defun opencode-test-stop-server ()
  "Stop the test server and delete its files."
  (interactive)
  (when opencode-test--ws
    (ignore-errors (websocket-close opencode-test--ws)))
  (when (process-live-p opencode-test--process)
    (delete-process opencode-test--process))
  (when (and opencode-test--root (file-directory-p opencode-test--root))
    (delete-directory opencode-test--root t))
  (setq opencode-test--ws nil
        opencode-test--process nil
        opencode-test--root nil
        opencode-test--connection nil))

(add-hook 'kill-emacs-hook #'opencode-test-stop-server)

;;; Scripted LLM

(defun opencode-test--rpc-send (method params)
  "Send the JSON-RPC request METHOD with PARAMS to the simulated LLM.
Return its id."
  (let ((id (cl-incf opencode-test--rpc-id)))
    (websocket-send-text
     opencode-test--ws
     (json-encode `((jsonrpc . "2.0") (id . ,id) (method . ,method) (params . ,params))))
    id))

(defun opencode-test--rpc-call (method params)
  "Send the JSON-RPC request METHOD with PARAMS, and return its result."
  (let* ((id (opencode-test--rpc-send method params))
         (response (opencode-test--wait
                    (lambda () (gethash id opencode-test--rpc-results))
                    (format "the simulated LLM's reply to %s" method))))
    (when-let* ((error (alist-get 'error response)))
      (error "Simulated LLM %s failed: %S" method error))
    (alist-get 'result response)))

(defun opencode-test--attach-llm ()
  "Connect to the test server's simulated LLM and attach to its requests."
  (setq opencode-test--rpc-results (make-hash-table))
  (let (opened)
    (setq opencode-test--ws
          (websocket-open opencode-test--drive-url
                          :on-open (lambda (_ws) (setq opened t))
                          :on-message (lambda (_ws frame)
                                        (opencode-test--llm-message
                                         (websocket-frame-text frame)))
                          :on-error (lambda (_ws type err)
                                      (push (list 'websocket type err)
                                            opencode-test--errors))))
    (opencode-test-wait-until opened))
  (opencode-test--rpc-call
   "simulation.handshake"
   '((client . ((name . "opencode.el") (version . "1")))
     (expectedRole . "backend")
     (offeredVersions . [1])
     (requiredCapabilities . ["llm.attach" "llm.request" "llm.chunk" "llm.finish"])
     (optionalCapabilities . [])))
  (opencode-test--rpc-call "llm.attach" nil))

(defun opencode-test--llm-message (text)
  "Handle the message TEXT from the simulated LLM."
  (condition-case err
      (let ((message (json-parse-string text :object-type 'alist :array-type 'list
                                        :null-object nil :false-object nil)))
        (cond
         ((equal (alist-get 'method message) "llm.request")
          (opencode-test--llm-request (alist-get 'params message)))
         ((alist-get 'id message)
          (puthash (alist-get 'id message) message opencode-test--rpc-results)
          (when-let* ((error (alist-get 'error message)))
            (push (list 'simulated-llm error) opencode-test--errors)))))
    (error (push (list 'simulated-llm-message err text) opencode-test--errors))))

(defun opencode-test--title-request-p (body)
  "Return non-nil if BODY is a request to generate a session's title."
  (when-let* ((system (car (alist-get 'messages body)))
              (content (alist-get 'content system)))
    (and (stringp content)
         (string-search "You name conversations" content))))

(defun opencode-test--llm-request (invocation)
  "Answer INVOCATION, a request to the simulated LLM."
  (let-alist invocation
    (if (opencode-test--title-request-p .body)
        (opencode-test-llm-respond .id opencode-test-title)
      (setq opencode-test-llm-requests (append opencode-test-llm-requests (list .body)))
      (if-let* ((response (pop opencode-test--llm-script)))
          (opencode-test-llm-respond
           .id (if (functionp response) (funcall response .body) response))
        (push (list 'unscripted-llm-request .body) opencode-test--errors)
        (opencode-test-llm-respond .id "Unscripted LLM request")))))

(defun opencode-test--llm-parts (response)
  "Return the list of parts of RESPONSE."
  (if (or (stringp response) (keywordp (car-safe response)))
      (list response)
    response))

(defun opencode-test-llm-respond (id response)
  "Answer the LLM request ID with RESPONSE.
RESPONSE is a part or a list of parts.  A part is one of:
  TEXT                   text, a string
  (:reasoning TEXT)      reasoning
  (:tool NAME INPUT)     a call of the tool NAME with the alist INPUT
  (:raw CHUNK)           an OpenAI chat completion chunk
  (:finish REASON)       the finish reason, by default \"tool-calls\" if it
                         calls a tool, else \"stop\".  With nil it isn't
                         finished, for a raw chunk that ends it, like an error
  (:disconnect)          the provider disconnects
  (:hold)                it isn't finished, the test does that, with the
                         request's id from `opencode-test-llm-held'"
  (let ((index -1) items (finish 'default) disconnect hold)
    (dolist (part (opencode-test--llm-parts response))
      (pcase part
        ((pred stringp) (push `((type . "textDelta") (text . ,part)) items))
        (`(:reasoning ,text) (push `((type . "reasoningDelta") (text . ,text)) items))
        (`(:tool ,name ,input)
         (push `((type . "toolCall") (index . ,(cl-incf index))
                 (id . ,(format "call_%d" (cl-incf opencode-test--tool-call-counter)))
                 (name . ,name) (input . ,input))
               items)
         (when (eq finish 'default) (setq finish "tool-calls")))
        (`(:raw ,chunk) (push `((type . "raw") (chunk . ,chunk)) items))
        (`(:finish ,reason) (setq finish reason))
        (`(:disconnect) (setq disconnect t))
        (`(:hold) (setq hold t))
        (_ (error "Unknown LLM response part %S" part))))
    (when items
      (opencode-test--rpc-send "llm.chunk" `((id . ,id) (items . ,(vconcat (nreverse items))))))
    (cond
     (hold (push id opencode-test-llm-held))
     (disconnect (opencode-test--rpc-send "llm.disconnect" `((id . ,id))))
     ((null finish))
     (t (opencode-test--rpc-send
         "llm.finish" `((id . ,id) (reason . ,(if (eq finish 'default) "stop" finish))))))))

(defun opencode-test-llm-reply (&rest responses)
  "Queue RESPONSES to the next requests to the LLM, in order.
Each is a response for `opencode-test-llm-respond', or a function called
with the request's body that returns one."
  (setq opencode-test--llm-script (append opencode-test--llm-script responses)))

(defun opencode-test-llm-wait-held ()
  "Wait for the LLM to hold a request, and return its id."
  (opencode-test-wait-until (car opencode-test-llm-held)))

(defun opencode-test-llm-finish (id &optional reason)
  "Finish the held LLM request ID with REASON, \"stop\" by default."
  (setq opencode-test-llm-held (delete id opencode-test-llm-held))
  (opencode-test--rpc-call "llm.finish" `((id . ,id) (reason . ,(or reason "stop")))))

(defun opencode-test-llm-user-text (body)
  "Return the text of the last user message in the LLM request BODY."
  (let ((content (alist-get 'content
                            (seq-find (lambda (message) (equal "user" (alist-get 'role message)))
                                      (reverse (alist-get 'messages body))))))
    (if (stringp content)
        content
      (mapconcat (lambda (part) (or (alist-get 'text part) "")) content ""))))

;;; Tests

(defun opencode-test--record-event (handle event)
  "Call HANDLE with EVENT, recording it and its errors if it's from the test server."
  (if (not (and opencode-test--connection
                (eq opencode--dispatching-connection opencode-test--connection)))
      (funcall handle event)
    (push event opencode-test-events)
    (condition-case err
        (funcall handle event)
      (error (push (list 'event event err) opencode-test--errors)
             (signal (car err) (cdr err))))))

(defconst opencode-test--kept-settings '(opencode-tool-formatters)
  "Options opencode.el sets itself when it's loaded, so their value is kept.")

(defun opencode-test--options (&optional group)
  "Return the options in customization GROUP and its subgroups.
GROUP defaults to `opencode'.  `opencode-test--kept-settings' are left out."
  (let (options)
    (pcase-dolist (`(,member ,type) (get (or group 'opencode) 'custom-group))
      (pcase type
        ('custom-variable
         (unless (memq member opencode-test--kept-settings)
           (push member options)))
        ('custom-group
         (setq options (nconc (opencode-test--options member) options)))))
    options))

(defun opencode-test--standard-settings ()
  "Return the opencode options and their standard values.
Those whose standard value can't be computed are left out, as are
`opencode-test--kept-settings'."
  (let (settings)
    (dolist (symbol (opencode-test--options))
      (condition-case nil
          (push (cons symbol (eval (car (get symbol 'standard-value)) t)) settings)
        (error nil)))
    settings))

(defun opencode-test--cleanup ()
  "Kill the test's buffers and disconnect from the test server."
  (dolist (buffer (buffer-list))
    (when (eq opencode-test--connection
              (buffer-local-value 'opencode-buffer-connection buffer))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer))))
  (when opencode-test--connection
    (clrhash (opencode-connection-session-buffers opencode-test--connection))
    (opencode--disconnect opencode-test--connection)))

(defun opencode-test--call-with-server (body)
  "Call BODY connected to the test server, isolated from the user's settings."
  (opencode-test-ensure-server)
  (let* ((settings (opencode-test--standard-settings))
         (connection opencode-test--connection)
         (opencode-test-llm-requests nil)
         (opencode-test-llm-held nil)
         (opencode-test-events nil)
         (opencode-test--errors nil)
         (opencode-test--llm-script nil))
    (cl-progv (mapcar #'car settings) (mapcar #'cdr settings)
      (let ((opencode-servers `((opencode-test :host "127.0.0.1" :port ,opencode-test-port
                                 :auto-start nil)))
            (opencode-default-server 'opencode-test)
            (opencode-project-servers nil)
            (opencode-connections (list connection))
            (opencode-default-connection connection)
            (opencode-toast-function #'ignore)
            (opencode-file-edited-functions nil)
            (opencode-files-finished-editing-functions nil)
            (opencode-project-files-finished-editing-functions nil)
            (opencode-alerted-sessions nil)
            (opencode-last-model nil)
            (opencode-last-session-buffer nil)
            (opencode--tool-calls (make-hash-table :test 'equal))
            (opencode-forms--queue nil)
            (opencode-forms--current nil)
            (opencode-forms--fields nil)
            (opencode-forms--answers nil)
            (opencode-forms--left nil)
            (opencode-forms--reading nil)
            (opencode-forms--kept-selection nil)
            (opencode-test-session-buffer nil)
            (inhibit-message t))
        (advice-add 'opencode--handle-message :around #'opencode-test--record-event)
        (cl-letf (((symbol-function 'opencode--switch-to-buffer) #'ignore))
          (unwind-protect
              (let (connected)
                (opencode--connect connection (lambda () (setq connected t)))
                (opencode-test-wait-until connected)
                (prog1 (funcall body)
                  (should-not opencode-test--errors)
                  (should-not opencode-test--llm-script)))
            (advice-remove 'opencode--handle-message #'opencode-test--record-event)
            (opencode-test--cleanup)))))))

(defmacro opencode-test-with-server (&rest body)
  "Run BODY connected to the shared test server.
The user's settings for opencode are bound to their standard values, and
those that would reach outside the test are turned off.  The test fails if
async code it ran had an error, or responses it queued for the LLM weren't
used."
  (declare (indent 0) (debug t))
  `(opencode-test--call-with-server (lambda () ,@body)))

(defun opencode-test-make-project (&optional files)
  "Make a new git project on the test server, and return its directory.
FILES is an alist of file names to their contents, committed in it."
  (let ((directory (file-name-as-directory
                    (file-name-concat opencode-test--root "projects"
                                      (format "project-%d"
                                              (cl-incf opencode-test--project-counter)))))
        (process-environment (opencode-test--environment)))
    (make-directory directory t)
    (pcase-dolist (`(,file . ,contents) files)
      (let ((file (expand-file-name file directory)))
        (make-directory (file-name-directory file) t)
        (with-temp-file file (insert contents))))
    (let ((default-directory directory))
      (dolist (args '(("init" "-q" "-b" "main")
                      ("add" "-A")
                      ("commit" "-q" "--allow-empty" "-m" "Initial commit")))
        (unless (zerop (apply #'call-process "git" nil nil nil args))
          (error "Git %s failed in %s" (car args) directory))))
    directory))

(defun opencode-test-new-session (&optional directory)
  "Open a new session in DIRECTORY, a new project by default, and return its buffer.
It waits until the session is loaded."
  (let ((default-directory (or directory (opencode-test-make-project)))
        buffer)
    (opencode-new-session :callback (lambda (_session) (setq buffer (current-buffer))))
    (opencode-test-wait-until buffer)
    (with-current-buffer buffer
      (opencode-test-wait-until (opencode-test-at-prompt-p)))
    buffer))

(defmacro opencode-test-with-session (files &rest body)
  "Run BODY in a new session's buffer on the test server.
The session's in a new project with FILES, an alist of file names to their
contents.  See `opencode-test-with-server'."
  (declare (indent 1) (debug t))
  `(opencode-test-with-server
     (let ((buffer (opencode-test-new-session (opencode-test-make-project ,files))))
       (setq opencode-test-session-buffer buffer)
       (with-current-buffer buffer
         ,@body))))

(defun opencode-test-output ()
  "Return the session's output after its logo, without the pending input."
  (let ((end (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char (point-min))
      ;; The logo ends with a blank line
      (search-forward "\n\n" end)
      (buffer-substring-no-properties (point) end))))

(defun opencode-test-at-prompt-p ()
  "Return non-nil if the session is idle at a prompt."
  (and (equal opencode-session-status "idle")
       (string-suffix-p "❯ " (buffer-substring-no-properties
                              (point-min)
                              (process-mark (get-buffer-process (current-buffer)))))))

(defun opencode-test-event-p (type &optional session-id since)
  "Return the latest event of TYPE for SESSION-ID, after the SINCE latest ones.
SESSION-ID defaults to the current buffer's session."
  (let ((session-id (or session-id opencode-session-id))
        (events (if since
                    (butlast opencode-test-events since)
                  opencode-test-events)))
    (seq-find (lambda (event)
                (and (equal type (alist-get 'type event))
                     (equal session-id (map-nested-elt event '(data sessionID)))))
              events)))

(defun opencode-test-type (text)
  "Type TEXT at the end of the session buffer's input."
  (goto-char (point-max))
  (insert text))

(defun opencode-test-send (text &optional no-wait)
  "Send TEXT from the session buffer, and wait until it's back at a prompt.
With NO-WAIT, return once it's sent."
  (let ((since (length opencode-test-events)))
    (opencode-test-type text)
    (comint-send-input)
    (unless no-wait
      (if (string-prefix-p "!" text)
          ;; A shell command isn't an execution
          (progn (opencode-test-wait-until
                  (opencode-test-event-p "session.shell.ended" nil since))
                 (opencode-test-wait-until (opencode-test-at-prompt-p)))
        (opencode-test-wait-finished since)))))

(defun opencode-test-wait-finished (since)
  "Wait until the session's execution ends after the SINCE latest events.
Then until it's back at a prompt."
  (opencode-test-wait-until
   (seq-some (lambda (type) (opencode-test-event-p type nil since))
             '("session.execution.succeeded" "session.execution.failed"
               "session.execution.interrupted")))
  (opencode-test-wait-until (opencode-test-at-prompt-p)))

(defun opencode-test-wait-for-output (string)
  "Wait until the session's output contains STRING."
  (opencode-test--wait (lambda () (string-search string (opencode-test-output)))
                       (format "output %S" string)))

(defun opencode-test--search (text)
  "Return the start of the last TEXT in the buffer, or nil if it has none."
  (save-excursion
    (goto-char (point-max))
    (when (search-backward text nil t)
      (point))))

(defun opencode-test-folded-p (text)
  "Return non-nil if the last block whose first line contains TEXT is folded."
  (when-let* ((pos (opencode-test--search text)))
    (save-excursion
      (goto-char pos)
      (eq 'hs (get-char-property (pos-eol) 'invisible)))))

(defun opencode-test-visible-p (text)
  "Return non-nil if the last TEXT in the buffer is visible."
  (when-let* ((pos (opencode-test--search text)))
    (not (invisible-p pos))))

(defun opencode-test-error-p (text)
  "Return non-nil if the last TEXT in the buffer has the `error' face."
  (when-let* ((pos (opencode-test--search text)))
    (let ((face (get-char-property pos 'face)))
      (or (eq face 'error) (and (listp face) (memq 'error face))))))

(provide 'opencode-integration-test-helpers)
;;; opencode-integration-test-helpers.el ends here
