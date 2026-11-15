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

For example:
  ((local)
   (other :port 4097)
   (nix :port 4098
        :command \"nix run github:numtide/nix-ai-tools#opencode -- serve --port %p --hostname %h\"))

\\[universal-argument] \\[opencode] asks which one to connect to."
  :type '(alist :key-type symbol
          :value-type (plist :options ((:host string)
                                       (:port integer)
                                       (:command string)
                                       (:password string))))
  :group 'opencode)

(defcustom opencode-default-server 'local
  "Name of the server in `opencode-servers' that \\[opencode] connects to.
That's when it isn't run from a buffer of a server that's already been
connected to, and no server is connected.  Buffers of other servers have
their name in their buffer names."
  :type 'symbol
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
                   :documentation "Hash table of session ids to their buffers."))

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
  "Return the connection to use now, or nil if there's none."
  (or opencode--dispatching-connection
      opencode-buffer-connection
      opencode-default-connection))

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

(defun opencode--auth-header (connection)
  "Return the auth header for CONNECTION's server, or nil if it needs none."
  (when-let* ((password (opencode--connection-password connection)))
    (cons "Authorization"
          (concat "Basic "
                  (base64-encode-string
                   (format "opencode:%s" password) t)))))

(provide 'opencode-connection)
;;; opencode-connection.el ends here
