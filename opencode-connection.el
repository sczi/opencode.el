;;; opencode-connection.el --- Connections to opencode servers  -*- lexical-binding: t; -*-

;; Keywords: internal

;;; Commentary:

;; Connections to opencode servers, and which one to use for a request.

;;; Code:

(require 'cl-lib)
(require 'cl-print)
(require 'seq)

(defcustom opencode-servers '((local))
  "Opencode servers to connect to, an alist of (NAME . PLIST).
NAME is a symbol.  PLIST can have these keys, all optional:

  :host      hostname, by default `opencode-host'
  :port      port, by default `opencode-port'
  :command   shell command to start it, by default `opencode-serve-command'
             or one constructed from `opencode-command'.  %h and %p in it
             are replaced by the host and port.
  :password  password, by default `opencode-server-password'
  :auto-start  whether to start it with :command when it isn't running, by
             default t if its host is localhost, 127.0.0.1 or ::1, else nil.
             Set it to nil for a port forwarded to a remote server.
  :tramp     Tramp prefix of the host the server's files are on, such as
             \"/ssh:user@host:\", for a server on another host.  Its paths
             are visited with it prepended, and \\[opencode] in a buffer with
             a `default-directory' on that host uses this server.

For example:
  ((local)
   (other :port 4097)
   (tunnel :port 4099 :auto-start nil :tramp \"/ssh:user@host:\")
   (nix :port 4098
        :command \"nix run github:numtide/nix-ai-tools#opencode -- serve --port %p --hostname %h\"))

\\[universal-argument] \\[opencode] asks which one to connect to."
  :type '(alist :key-type symbol
          :value-type (plist :options ((:host string)
                                       (:port integer)
                                       (:command string)
                                       (:password string)
                                       (:auto-start boolean)
                                       (:tramp string))))
  :group 'opencode)

(defcustom opencode-default-server 'local
  "Name of the server in `opencode-servers' that \\[opencode] connects to.
That's when it isn't run from a buffer of a server that's already been
connected to, and no server is connected.  Buffers of other servers have
their name in their buffer names.  `opencode-project-servers' overrides it
for the directories in it."
  :type 'symbol
  :group 'opencode)

(defcustom opencode-project-servers nil
  "Alist of directories to the name of the server in `opencode-servers' to use there.
\\[opencode] run from a buffer whose `default-directory' is one of the
directories, or below it, connects to that server instead of
`opencode-default-server'.  When directories are nested, the most specific
one wins.  Directories may be remote file names."
  :type '(alist :key-type directory :value-type symbol)
  :group 'opencode)

(defcustom opencode-server-password nil
  "Password to connect to opencode server."
  :type 'string
  :group 'opencode)

(cl-defstruct (opencode-connection (:constructor opencode--make-connection)
                                   (:copier nil))
  "A connection to an opencode server, and what's known about the server.
It's kept after disconnecting, so reconnecting to the same URL reuses it
and the buffers tied to it keep working."
  (name nil :documentation "Name of the server in `opencode-servers', or \"HOST:PORT\".")
  (url nil :documentation "Base URL of the server.")
  (password nil :documentation "Password generated for or entered for it, if any.")
  (process nil :documentation "Server process when started by Emacs.")
  (stream nil :documentation "SSE event stream process, non-nil when connected.")
  (agents nil :documentation "Available agents excluding hidden agents.")
  (models nil :documentation "Enabled models.")
  (models-by-ref nil :documentation "Hash table of models by (providerID . id).
Set with `opencode--set-models'.")
  (slash-commands nil :documentation "Alist of normalized directories to slash commands.")
  (skills nil :documentation "Alist of normalized directories to skills.")
  (locations nil :documentation "Alist of normalized directories to their location.")
  (session-buffers (make-hash-table :test 'equal)
                   :documentation "Hash table of session ids to their buffers.")
  (session-control-buffers nil :documentation "Its session control buffers.
Some may have been killed.")
  (claude-rate-limit 'unfetched
                     :documentation "The latest Claude subscription rate limit info.
It's nil if there is none.  A cons of the time in seconds it was reported and
the rateLimit alist that claude-agent puts in a step's native metadata.  The
symbol `unfetched' before the latest stored one has been requested."))

(cl-defmethod cl-print-object ((connection opencode-connection) stream)
  "Print CONNECTION to STREAM briefly, as its caches are large."
  (princ (format "#<opencode-connection %s %s%s>"
                 (opencode-connection-name connection)
                 (opencode-connection-url connection)
                 (if (opencode-connection-stream connection) "" " disconnected"))
         stream))

(defvar opencode-connections nil
  "Connections to opencode servers, connected or not, oldest first.")

(defvar opencode-default-connection nil
  "Connection used where no other is tied to the buffer or command.")

(defvar-local opencode-buffer-connection nil
  "Connection of the server the current buffer's session or project is on.")
(put 'opencode-buffer-connection 'permanent-local t)

(defvar opencode--dispatching-connection nil
  "Connection of the event or API response being handled.
Bound, never set, so the requests made while handling it go to its server.")

(defun opencode-current-connection ()
  "Return the connection to use now, or nil if there's none.
It's the one of the event or response being handled, else of the current
buffer, else that to the server for `default-directory' if there is one,
else the default connection.  The server for a directory is the one in
`opencode-project-servers', else the one whose :tramp in `opencode-servers'
is the directory's host."
  (or opencode--dispatching-connection
      opencode-buffer-connection
      (opencode--directory-connection)
      opencode-default-connection))

(defun opencode--directory-connection ()
  "Return the connection to the server for remote `default-directory', or nil.
That's the server whose :tramp is its host, if a connection to it exists."
  (when-let* ((server (opencode--server-for-directory default-directory)))
    (seq-find (lambda (connection) (eq server (opencode-connection-name connection)))
              opencode-connections)))

(defun opencode--conn ()
  "Return the connection to use now, or signal an error if there's none."
  (or (opencode-current-connection)
      (error "Not connected to opencode")))

(defmacro opencode--with-connection-of (item &rest body)
  "Run BODY with the `opencode-connection' of alist ITEM as current, if it has one."
  (declare (indent 1) (debug t))
  `(let ((opencode--dispatching-connection
          (or (alist-get 'opencode-connection ,item) opencode--dispatching-connection)))
     ,@body))

(defun opencode-connection-connected-p (connection)
  "Return non-nil if CONNECTION is connected."
  (and connection (opencode-connection-stream connection) t))

(defun opencode--connected-connections ()
  "Return the connected connections, oldest first."
  (seq-filter #'opencode-connection-connected-p opencode-connections))

(defun opencode--server-label (&optional connection)
  "Return the name of CONNECTION's server if more than one is connected, else nil.
CONNECTION defaults to `opencode-current-connection'."
  (when-let* ((connection (or connection (opencode-current-connection)))
              ((length> (opencode--connected-connections) 1)))
    (format "%s" (opencode-connection-name connection))))

(defun opencode--server-tag (connection)
  "Return \"[NAME]\" of CONNECTION's server for buffer names, or \"\".
It's empty for `opencode-default-server'."
  (if (eq (opencode-connection-name connection) opencode-default-server)
      ""
    (format "[%s]" (opencode-connection-name connection))))

(defun opencode--agents ()
  "Return the agents of the current connection's server, excluding hidden agents."
  (when-let* ((connection (opencode-current-connection)))
    (opencode-connection-agents connection)))

(defun opencode--models ()
  "Return the enabled models of the current connection's server."
  (when-let* ((connection (opencode-current-connection)))
    (opencode-connection-models connection)))

(defun opencode--connection-password (connection)
  "Return the password of CONNECTION's server, or nil if it has none.
It's the one generated or entered for it, else its :password in
`opencode-servers', else `opencode-server-password'."
  (or (opencode-connection-password connection)
      (plist-get (alist-get (opencode-connection-name connection) opencode-servers)
                 :password)
      opencode-server-password))

(defun opencode--tramp-prefix (&optional connection)
  "Return the Tramp prefix of CONNECTION's server's host, or nil if it's local.
It's its :tramp in `opencode-servers'.  CONNECTION defaults to
`opencode-current-connection'."
  (when-let* ((connection (or connection (opencode-current-connection)))
              (prefix (plist-get (alist-get (opencode-connection-name connection)
                                            opencode-servers)
                                 :tramp)))
    (file-remote-p prefix)))

(defun opencode--to-server-path (path &optional connection)
  "Return PATH as CONNECTION's server sees it, or nil if it's not on its host.
CONNECTION defaults to `opencode-current-connection'."
  (let ((prefix (opencode--tramp-prefix connection))
        (path (expand-file-name path)))
    (if prefix
        (when (equal prefix (file-remote-p path))
          (file-remote-p path 'localname))
      (unless (file-remote-p path)
        path))))

(defun opencode--from-server-path (path &optional connection)
  "Return PATH from CONNECTION's server as a file name on its host.
CONNECTION defaults to `opencode-current-connection'."
  (if-let* ((prefix (opencode--tramp-prefix connection))
            ((not (file-remote-p path))))
      (concat prefix path)
    path))

(defun opencode--server-for-directory (directory)
  "Return the name of the server in `opencode-servers' for DIRECTORY, or nil.
It's that of the most specific directory in `opencode-project-servers'
DIRECTORY is in, else for remote DIRECTORY the one whose :tramp is its host."
  (or (opencode--project-server directory)
      (when-let* ((prefix (file-remote-p directory)))
        (car (seq-find (lambda (server)
                         (when-let* ((tramp (plist-get (cdr server) :tramp)))
                           (equal prefix (file-remote-p tramp))))
                       opencode-servers)))))

(defun opencode--project-server (directory)
  "Return the server in `opencode-project-servers' for DIRECTORY, or nil.
It's that of the longest directory that DIRECTORY is in or below."
  (let ((directory (file-name-as-directory (expand-file-name directory)))
        best best-length)
    (pcase-dolist (`(,dir . ,server) opencode-project-servers)
      (let ((dir (file-name-as-directory (expand-file-name dir))))
        (when (and (string-prefix-p dir directory)
                   (or (null best-length) (> (length dir) best-length)))
          (setq best server
                best-length (length dir)))))
    best))

(defun opencode--auth-header (connection)
  "Return the auth header for CONNECTION's server, or nil if it needs none."
  (when-let* ((password (opencode--connection-password connection)))
    (cons "Authorization"
          (concat "Basic "
                  (base64-encode-string
                   (format "opencode:%s" password) t)))))

(provide 'opencode-connection)
;;; opencode-connection.el ends here
