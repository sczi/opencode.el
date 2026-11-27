;;; opencode-servers.el --- Starting and connecting to opencode servers  -*- lexical-binding: t; -*-

;; Keywords: internal

;;; Commentary:

;; Starting, connecting to and disconnecting from opencode servers, and
;; commands to switch between and list the connections.

;;; Code:

(require 'format-spec)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-connection)
(require 'opencode-events)
(require 'opencode-session-control)
(require 'plz)
(require 'plz-event-source)
(require 'plz-media-type)
(require 'vtable)

(defcustom opencode-host "localhost"
  "Hostname of opencode servers in `opencode-servers' without a :host."
  :type 'string
  :group 'opencode)

(defcustom opencode-port 4096
  "Port of opencode servers in `opencode-servers' without a :port."
  :type 'integer
  :group 'opencode)

(defcustom opencode-command "opencode"
  "Base command for the opencode executable.
Used to construct the serve command of servers without a :command, when
`opencode-serve-command' is nil."
  :type 'string
  :group 'opencode)

(defcustom opencode-serve-command nil
  "Command to start opencode servers in `opencode-servers' without a :command.
When nil, the command is constructed from `opencode-command'.  %h and %p
in it are replaced by the server's host and port.

Set this to a custom command for special cases like nix:
  \"nix run github:numtide/nix-ai-tools#opencode -- serve --port %p --hostname %h\""
  :type '(choice (const :tag "Construct from opencode-command" nil)
          (string :tag "Custom command"))
  :group 'opencode)

(defun opencode--server-url (plist)
  "Return the URL of the server with PLIST from `opencode-servers'."
  (format "http://%s:%d"
          (or (plist-get plist :host) opencode-host)
          (or (plist-get plist :port) opencode-port)))

(defun opencode--connection-for-url (url name)
  "Return the connection to URL, creating it named NAME if there's none.
If a server in `opencode-servers' has URL, it's named after that instead."
  (or (seq-find (lambda (connection) (equal url (opencode-connection-url connection)))
                opencode-connections)
      (let ((connection
             (opencode--make-connection
              :url url
              :name (or (car (seq-find (lambda (server)
                                         (equal url (opencode--server-url (cdr server))))
                                       opencode-servers))
                        name))))
        (setq opencode-connections (append opencode-connections (list connection)))
        connection)))

(defun opencode--server-connection (name)
  "Return the connection to the server NAME in `opencode-servers'."
  (let ((server (or (assq name opencode-servers)
                    (user-error "No server %s in `opencode-servers'" name))))
    (opencode--connection-for-url (opencode--server-url (cdr server)) name)))

(defun opencode--server-running-p (connection)
  "Return non-nil if CONNECTION's server is running.
If it needs a password that isn't known, ask for it."
  (condition-case err
      (plz 'get (concat (opencode-connection-url connection) "/api/info")
        :headers (delq nil (list (opencode--auth-header connection)))
        :timeout 1)
    (plz-http-error
     (when-let* ((plz-error (cl-third err))
                 (response (plz-error-response plz-error))
                 ((= 401 (plz-response-status response))))
       (setf (opencode-connection-password connection)
             (read-passwd (format "Password for opencode at %s: "
                                  (opencode-connection-url connection))))
       (opencode--server-running-p connection)))
    (error nil)))

(defun opencode--serve-command (connection)
  "Return the command to start CONNECTION's server."
  (let ((plist (alist-get (opencode-connection-name connection) opencode-servers)))
    (format-spec (or (plist-get plist :command)
                     opencode-serve-command
                     (concat opencode-command " serve --port %p --hostname %h"))
                 `((?h . ,(or (plist-get plist :host) opencode-host))
                   (?p . ,(or (plist-get plist :port) opencode-port))))))

(defun opencode--server-auto-start-p (connection)
  "Return non-nil if CONNECTION's server should be started when not running.
It's its :auto-start in `opencode-servers' if that's given, else whether its
host is local.  It's nil if it isn't in `opencode-servers'."
  (when-let* ((server (assq (opencode-connection-name connection) opencode-servers)))
    (let ((plist (cdr server)))
      (if (plist-member plist :auto-start)
          (plist-get plist :auto-start)
        (member (or (plist-get plist :host) opencode-host)
                '("localhost" "127.0.0.1" "::1"))))))

(defun opencode--server-buffer-name (connection)
  "Return the name of the output buffer of CONNECTION's server."
  (format "*opencode-serve%s*" (opencode--server-tag connection)))

(defun opencode--start-server (connection on-connect)
  "Start CONNECTION's server and connect to it, then run ON-CONNECT."
  (unless (assq (opencode-connection-name connection) opencode-servers)
    (user-error "Can't start the server at %s, it's not in `opencode-servers'"
                (opencode-connection-url connection)))
  (unless (opencode--connection-password connection)
    (setf (opencode-connection-password connection)
          (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
            (apply #'string
                   (cl-loop repeat 32
                            collect (aref chars (random (length chars))))))))
  (let* ((buffer (get-buffer-create (opencode--server-buffer-name connection)))
         (process
          (let ((process-environment (cons
                                      (format "OPENCODE_PASSWORD=%s"
                                              (opencode--connection-password connection))
                                      process-environment)))
            (start-process-shell-command
             "opencode" buffer (opencode--serve-command connection)))))
    (setf (opencode-connection-process connection) process)
    (set-process-filter
     process
     (lambda (process output)
       (when (buffer-live-p (process-buffer process))
         (with-current-buffer (process-buffer process)
           (goto-char (process-mark process))
           (insert output)
           (set-marker (process-mark process) (point))))
       (when (string-search "server listening on" output)
         ;; Remove filter to avoid repeated callbacks
         (set-process-filter process nil)
         (opencode--connect connection on-connect))))
    (set-process-sentinel
     process
     (lambda (process event)
       (when (memq (process-status process) '(exit signal))
         (when (eq process (opencode-connection-process connection))
           (opencode--disconnect connection event))
         (let ((buffer (process-buffer process)))
           ;; A killed buffer means the server was deliberately killed with it
           (unless (or (zerop (process-exit-status process))
                       (not (buffer-live-p buffer)))
             (pop-to-buffer buffer)
             (display-warning
              'opencode
              (format "OpenCode server %s failed, %s"
                      (opencode-connection-name connection)
                      (string-trim-right event "\n"))
              :error))))))))

(defun opencode-autoconnect (on-connect &optional connection no-start)
  "Run ON-CONNECT once connected to CONNECTION's server.
CONNECTION defaults to `opencode-current-connection', or the connection to
`opencode-default-server' if there's none.  The connection to the server
for `default-directory', from `opencode-project-servers' or whose :tramp is
its host, is created first if it should be the current one.  It connects if needed, starting the server first if it
isn't running, NO-START is nil and its :auto-start in `opencode-servers'
allows it.  ON-CONNECT runs with CONNECTION as its current connection."
  (unless (or connection opencode--dispatching-connection opencode-buffer-connection)
    (when-let* ((server (opencode--server-for-directory default-directory)))
      (opencode--server-connection server)))
  (let* ((connection (or connection
                         (opencode-current-connection)
                         (opencode--server-connection opencode-default-server)))
         (run (opencode--in-command-context
               (lambda ()
                 (let ((opencode--dispatching-connection connection))
                   (funcall on-connect))))))
    (opencode--disconnect-if-stream-dead connection)
    (cond
     ((and (opencode-connection-connected-p connection)
           (opencode-connection-loading connection))
      (user-error "OpenCode server %s is still connecting"
                  (opencode-connection-name connection)))
     ((opencode-connection-connected-p connection)
      (funcall run))
     ((opencode--server-running-p connection)
      (opencode--connect connection run))
     ((process-live-p (opencode-connection-process connection))
      (user-error "OpenCode server %s is still starting"
                  (opencode-connection-name connection)))
     ((and (not no-start) (opencode--server-auto-start-p connection))
      (opencode--start-server connection run))
     (t
      (user-error "No opencode server running at %s, start it or set :auto-start in `opencode-servers'"
                  (opencode-connection-url connection))))))

(defun opencode--read-server ()
  "Read the name of a server in `opencode-servers'."
  (intern (completing-read
           (format-prompt "Server" opencode-default-server)
           (mapcar (lambda (server) (symbol-name (car server))) opencode-servers)
           nil t nil nil (symbol-name opencode-default-server))))

(defvar opencode--host-history nil
  "History of hosts given to `opencode-connect'.")

(defvar opencode--port-history nil
  "History of ports given to `opencode-connect'.")

(defun opencode-connect (host port)
  "Connect to the opencode server at HOST and PORT.
It's named after the server in `opencode-servers' with that host and port,
if there is one, otherwise HOST:PORT.  It becomes the default connection.
The server isn't started if it isn't running."
  (interactive
   (list (read-string (format-prompt "Host" opencode-host)
                      nil 'opencode--host-history opencode-host)
         (read-number "Port: " opencode-port 'opencode--port-history)))
  (let ((connection (opencode--connection-for-url
                     (format "http://%s:%d" host port) (format "%s:%d" host port))))
    (opencode-autoconnect (lambda () (opencode--set-default-connection connection))
                          connection t)))

(defun opencode--connect (connection on-connect)
  "Connect to CONNECTION's running server, then call ON-CONNECT.
It becomes the default connection.  CONNECTION must be disconnected."
  (opencode--subscribe-events connection)
  (opencode--set-default-connection connection)
  (setf (opencode-connection-loading connection) t)
  (let ((opencode--dispatching-connection connection))
    (opencode--wait-for-plugins
     (lambda ()
       (opencode--fetch-models
        (lambda ()
          (opencode--fetch-agents
           (lambda ()
             (setf (opencode-connection-loading connection) nil)
             (funcall on-connect))))))))
  (message "Connected to opencode %s at %s"
           (opencode-connection-name connection) (opencode-connection-url connection)))

(defun opencode--set-default-connection (connection)
  "Make CONNECTION the default connection."
  (setq opencode-default-connection connection)
  (opencode--connections-changed))

(defun opencode--connections-changed ()
  "Update what shows the connections, after one connected or disconnected."
  ;; For %s in session mode lines
  (force-mode-line-update t)
  (when-let* ((buffer (get-buffer "*OpenCode Connections*")))
    (with-current-buffer buffer
      (opencode--draw-connection-list))))

(defun opencode--subscribe-events (connection)
  "Subscribe to the event stream of CONNECTION's server."
  (let* (stream
         (ended (lambda (&optional event)
                  (when (eq stream (opencode-connection-stream connection))
                    (opencode--stream-ended connection event)))))
    (setq stream (plz-media-type-request
                   'get (concat (opencode-connection-url connection) "/api/event?"
                                (mapconcat (lambda (type) (concat "types=" type))
                                           opencode--event-types "&"))
                   :as `(media-types
                         ((text/event-stream
                           . ,(plz-event-source:text/event-stream
                               :events `((open . ,(lambda (event)
                                                    (opencode--log-event "OPEN" event)))
                                         (message . ,(lambda (event)
                                                       (opencode--handle-event
                                                        connection event)))
                                         (close . ,ended))))))
                   :headers (delq nil (list (opencode--auth-header connection)))
                   :then ended
                   :else ended))
    (setf (opencode-connection-stream connection) stream)
    (set-process-query-on-exit-flag stream nil)))

(defun opencode--stream-ended (connection event)
  "Disconnect CONNECTION after its event stream ends with EVENT."
  (opencode--disconnect connection event)
  (display-warning
   'opencode
   (format "Lost connection to opencode %s at %s, reconnect with M-x opencode"
           (opencode-connection-name connection) (opencode-connection-url connection))
   :error))

(defun opencode-disconnect (&optional all)
  "Disconnect from the current connection's opencode server.
With prefix argument ALL, disconnect from all of them.  If Emacs started the
server, it's killed."
  (interactive "P")
  (if all
      (mapc #'opencode--disconnect opencode-connections)
    (opencode--disconnect (opencode--conn))))

(defun opencode--disconnect (connection &optional event)
  "Disconnect CONNECTION, optionally log EVENT.
If another connection is connected, it becomes the default."
  (opencode--log-event "DISCONNECT" (list (opencode-connection-name connection) event))
  ;; Cleared first so the stream's further reports of ending are ignored
  (let ((stream (opencode-connection-stream connection))
        (process (opencode-connection-process connection)))
    (setf (opencode-connection-stream connection) nil)
    (when (process-live-p stream)
      (delete-process stream))
    (when (process-live-p process)
      (set-process-sentinel process nil)
      (kill-process process)))
  (setf (opencode-connection-loading connection) nil
        (opencode-connection-slash-commands connection) nil
        (opencode-connection-skills connection) nil
        (opencode-connection-locations connection) nil)
  (when (eq connection opencode-default-connection)
    (setq opencode-default-connection
          (or (car (last (opencode--connected-connections))) connection)))
  (opencode--connections-changed))

(defun opencode--disconnect-if-stream-dead (connection)
  "Disconnect CONNECTION if its event stream died without reporting it ended.
plz ignores some ways curl can die, such as a hangup when its buffer is
killed, so the stream's callbacks never run."
  (when-let* ((stream (opencode-connection-stream connection))
              ((not (process-live-p stream))))
    (opencode--disconnect connection "event stream process died")))

(defun opencode--wait-for-plugins (callback &optional deadline)
  "Call CALLBACK once the server lists plugins for `default-directory'.
The server answers catalog reads like models and agents with whatever is
loaded so far, and lists no plugins until the location is first activated,
so this waits for that as opencode's own CLI does.  It gives up waiting at
DEADLINE, a `float-time', which defaults to 5 seconds from now.  CALLBACK
runs with the same `default-directory'."
  (let* ((deadline (or deadline (+ (float-time) 5)))
         (connection (opencode--conn))
         (directory default-directory))
    (opencode-api-plugins plugins
      :else (lambda (_error) (funcall callback))
      (if (or plugins (> (float-time) deadline))
          (funcall callback)
        (run-at-time 0.025 nil
                     (lambda ()
                       (let ((opencode--dispatching-connection connection)
                             (default-directory directory))
                         (opencode--wait-for-plugins callback deadline))))))))

;;; Connection commands

(defun opencode--cycle-connection (step)
  "Make the connection STEP places after the default one the default."
  (let* ((connections (or (opencode--connected-connections)
                          (user-error "Not connected to opencode")))
         (position (or (cl-position opencode-default-connection connections) -1))
         (connection (nth (mod (+ position step) (length connections)) connections)))
    (opencode--set-default-connection connection)
    (message "Default opencode connection: %s" (opencode-connection-name connection))))

(defun opencode-next-connection ()
  "Make the next connected opencode server the default one."
  (interactive)
  (opencode--cycle-connection 1))

(defun opencode-prev-connection ()
  "Make the previous connected opencode server the default one."
  (interactive)
  (opencode--cycle-connection -1))

(defun opencode--restart-server (connection)
  "Restart CONNECTION's server, which Emacs started."
  (let ((process (opencode-connection-process connection)))
    (unless (process-live-p process)
      (user-error "Can't restart %s, it wasn't started by Emacs"
                  (opencode-connection-name connection)))
    (opencode--disconnect connection)
    ;; Wait so the new one can listen on its port
    (with-timeout (5 (user-error "Server %s didn't exit" (opencode-connection-name connection)))
      (while (process-live-p process)
        (accept-process-output process 0.1)))
    (opencode--start-server connection #'ignore)))

(defvar opencode-connection-list-mode-map
  (define-keymap
    "c" #'opencode-connect
    "g" #'opencode-list-connections
    "SPC" nil))

(define-derived-mode opencode-connection-list-mode special-mode "Connections"
  "Mode listing the connections to opencode servers.")

(with-eval-after-load 'evil
  (declare-function evil-define-key* "evil-core")
  (evil-define-key* 'normal opencode-connection-list-mode-map
    "c" #'opencode-connect
    "gr" #'opencode-list-connections))

(defun opencode--connection-status (connection)
  "Return a description of CONNECTION's status."
  (concat (if (opencode-connection-connected-p connection) "connected" "disconnected")
          (when (process-live-p (opencode-connection-process connection))
            ", started by Emacs")))

(defun opencode--draw-connection-list ()
  "Draw the connection list in the current buffer."
  (let ((inhibit-read-only t))
    (opencode--with-preserved-lines
      (erase-buffer)
      (if (not opencode-connections)
          (insert "No opencode connections")
        (make-vtable
         :columns '((:name "" :width 1) (:name "Name" :min-width 5) "URL" "Status"
                    (:name "Sessions" :align right :min-width 8))
         :objects opencode-connections
         :getter (lambda (connection column _vtable)
                   (pcase column
                     (0 (if (eq connection opencode-default-connection) "*" ""))
                     (1 (format "%s" (opencode-connection-name connection)))
                     (2 (opencode-connection-url connection))
                     (3 (opencode--connection-status connection))
                     (4 (cl-loop for buffer being the hash-values
                                 of (opencode-connection-session-buffers connection)
                                 count (buffer-live-p buffer)))))
         :actions (list "RET" (lambda (connection)
                                (opencode-autoconnect #'opencode--read-project connection))
                        "d" #'opencode--set-default-connection
                        "x" #'opencode--disconnect
                        "R" #'opencode--restart-server)
         ;; So its g, which redraws from `opencode-connections', wins over
         ;; vtable's, which just refreshes current entries, doesn't check for new ones
         :keymap opencode-connection-list-mode-map)))))

(defun opencode-list-connections ()
  "List the connections to opencode servers.
The default connection is marked with *."
  (interactive)
  (let ((directory default-directory))
    (with-current-buffer (get-buffer-create "*OpenCode Connections*")
      (unless (derived-mode-p 'opencode-connection-list-mode)
        (opencode-connection-list-mode))
      (setq default-directory directory)
      (opencode--draw-connection-list)
      (pop-to-buffer (current-buffer)))))

(provide 'opencode-servers)
;;; opencode-servers.el ends here
