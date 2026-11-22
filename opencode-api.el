;;; opencode-api.el --- Wrapper for opencode api     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Wrapper for opencode api

;;; Code:

(require 'opencode-common)
(require 'plz)
(require 'json)
(require 'url-util)

(defvar opencode-api-log-max-lines nil
  "Maximum number of lines of opencode api requests and responses to log.
Or nil (default) to turn off logging.")

(defun opencode-api--log (label value)
  "Log VALUE under LABEL to the api log, if logging is on."
  (when opencode-api-log-max-lines
    (with-current-buffer (get-buffer-create "*opencode-api-log*")
      (save-excursion
        (goto-char (point-max))
        (insert label ": ")
        (if (stringp value)
            (insert value "\n")
          (pp value (current-buffer)))
        (opencode--truncate-at-max-lines opencode-api-log-max-lines)))))

(defun opencode-api--error-message (path err)
  "Return a message for ERR, the `plz-error' from requesting PATH.
Shows the _tag and message of the server's JSON error body, if it has one."
  (let* ((response (plz-error-response err))
         (body (and response
                    (ignore-errors
                      (json-parse-string (plz-response-body response)
                                         :object-type 'alist))))
         (message (and (consp body) (alist-get 'message body))))
    (if message
        (format "error requesting %s: %s: %s" path (alist-get '_tag body) message)
      (format "error requesting %s: %s" path err))))

(cl-defun opencode-api--request (method path &key (body nil body-p) raw then else)
  "Request PATH with METHOD, sending BODY encoded as JSON if given.
Call THEN with the result in the buffer current now, unless it's been killed.
The result's {data} envelope is removed unless RAW is non-nil.
Call ELSE with the error message if given, in that buffer if it's still live,
otherwise signal it.
The request goes to the server of `opencode-current-connection', which THEN
and ELSE run with as their current connection too."
  (let* ((buffer (current-buffer))
         (connection (opencode--conn))
         ;; Without it the server uses its default location
         (directory (opencode--to-server-path default-directory connection))
         (then (and then (opencode--in-command-context then)))
         (else (and else (opencode--in-command-context else))))
    (opencode-api--log "REQUEST" path)
    (when body-p
      (opencode-api--log "REQUEST BODY" body))
    (apply
     #'plz method (concat (opencode-connection-url connection) path)
     :as (lambda () (unless (string-empty-p (buffer-string))
                      (json-parse-buffer :array-type 'list
                                         :object-type 'alist)))
     :headers (delq nil
                    (list '("Content-Type" . "application/json")
                          (when directory
                            (cons "x-opencode-directory"
                                  (url-hexify-string
                                   (directory-file-name directory))))
                          (opencode--auth-header connection)))
     :then (lambda (result)
             (opencode-api--log "RESPONSE" (or result "<empty>"))
             (let ((result (if-let* ((data (and (not raw)
                                                (consp result)
                                                (assq 'data result))))
                               (cdr data)
                             result))
                   (opencode--dispatching-connection connection))
               (if (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (when then
                       (funcall then result)))
                 (message "Buffer closed so opencode api response handler not run"))))
     :else (lambda (err)
             (let ((error-msg (opencode-api--error-message path err))
                   (opencode--dispatching-connection connection))
               (opencode-api--log "ERROR" error-msg)
               (cond
                (else (if (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (funcall else error-msg))
                        (funcall else error-msg)))
                ((opencode-connection-connected-p connection) (error "%s" error-msg))
                (t (error "Not connected to opencode")))))
     (when body-p
       (list :body (json-encode body))))))

(defun opencode-api--fetch-all-pages (path then &optional else)
  "Call THEN with the items of every page of PATH, in the server's order.
PATH must already have a query string.
Call ELSE with the error message if given and a request fails."
  (let (pages)
    (cl-labels ((fetch (cursor)
                  (opencode-api--request
                   'get (if cursor
                            (concat path "&cursor=" (url-hexify-string cursor))
                          path)
                   :raw t
                   :else else
                   :then (lambda (result)
                           (let ((page (alist-get 'data result))
                                 (next (map-nested-elt result '(cursor next))))
                             (push page pages)
                             (if (and page (not (opencode--json-falsy next)))
                                 (fetch next)
                               (funcall then (apply #'append (nreverse pages)))))))))
      (fetch nil))))

(defun opencode-api--fetch-all-messages (session-id then &optional else)
  "Call THEN with all of SESSION-ID's messages, oldest first.
The server returns them newest first, a page at a time.
Call ELSE with the error message if given and a request fails."
  (opencode-api--fetch-all-pages
   (format "/api/session/%s/message?limit=200" session-id)
   (lambda (messages) (funcall then (reverse messages)))
   else))

(defun opencode-api--fetch-all-sessions (project-id include-subagents then
                                                    &optional include-archived)
  "Call THEN with all of PROJECT-ID's sessions.
Subagent sessions are only included if INCLUDE-SUBAGENTS is non-nil,
and archived ones only if INCLUDE-ARCHIVED is non-nil."
  (opencode-api--fetch-all-pages
   (format "/api/session?project=%s%s&limit=200%s"
           project-id
           (if include-archived "" "&archived=false")
           (if include-subagents "" "&parentID=null"))
   then))

(eval-and-compile
  (defun opencode-api--expand (method path args data return-var body)
    "Return a request with METHOD for PATH formatted with ARGS, sending DATA.
BODY is run with the result bound to RETURN-VAR.  If BODY starts with
:else HANDLER, HANDLER is called with the error message on failure."
    (let ((else (when (eq (car body) :else)
                  (prog1 (cadr body) (setq body (cddr body))))))
      `(opencode-api--request
        ',method ,(if args `(format ,path ,@args) path)
        ,@(when data `(:body ,data))
        :then (lambda (,return-var) ,@body)
        ,@(when else `(:else ,else))))))

(defmacro opencode-api--define-wrappers (&rest endpoints)
  "Define a macro for each of ENDPOINTS, which are ([METHOD] [nodata] NAME PATH).
METHOD defaults to get.  The macro opencode-api-NAME takes the ARGS to format
PATH with if it has any, the DATA to send if it's a post or patch that isn't
nodata, then a variable to bind the result to and a body to run.  The body
may start with :else HANDLER to handle errors."
  `(progn
     ,@(mapcar
        (lambda (endpoint)
          (let* ((method (if (memq (car endpoint) '(get patch post delete))
                             (pop endpoint)
                           'get))
                 (nodata (when (eq (car endpoint) 'nodata) (pop endpoint)))
                 (path (cadr endpoint))
                 (has-args (string-search "%" path))
                 (has-data (and (memq method '(post patch)) (not nodata))))
            `(defmacro ,(intern (format "opencode-api-%s" (car endpoint)))
                 (,@(when has-args '(args)) ,@(when has-data '(data))
                  return-var &rest body)
               ,(format "Wrapper for opencode %s endpoint." path)
               (declare (indent ,(cond ((and has-args has-data) 3)
                                       ((or has-args has-data) 2)
                                       (t 'defun))))
               (opencode-api--expand ',method ,path
                                     ,(when has-args 'args) ,(when has-data 'data)
                                     return-var body))))
        endpoints)))

(opencode-api--define-wrappers
 (info "/api/info")
 (location "/api/location")
 (post nodata reload-locations "/api/location/reload")
 (projects "/api/project")
 (models "/api/model")
 (default-model "/api/model/default")
 (agents "/api/agent")
 (commands "/api/command")
 (skills "/api/skill")
 (mcps "/api/mcp")
 (post nodata enable-mcp "/api/experimental/mcp/%s/connect")
 (post nodata disable-mcp "/api/experimental/mcp/%s/disconnect")
 (session "/api/session/%s")
 (root-sessions-for-project
  "/api/session?project=%s&archived=false&limit=200&parentID=null")
 (session-children "/api/session?parentID=%s")
 (sessions-active "/api/session/active")
 (session-running-jobs
  "/api/experimental/job?sessionID=%s&descendants=true&status=running")
 (shell "/api/shell/%s")
 (delete remove-shell "/api/shell/%s")
 (shell-output "/api/shell/%s/output?cursor=%d&limit=%d")
 (post create-session "/api/session")
 (patch rename-session "/api/session/%s")
 (patch archive-session "/api/session/%s")
 (delete delete-session "/api/session/%s")
 (post fork-session "/api/session/%s/fork")
 (post compact-session "/api/session/%s/compact")
 (post nodata interrupt-session "/api/session/%s/interrupt")
 (post nodata background-session "/api/session/%s/background")
 (post session-prompt "/api/session/%s/prompt")
 (post session-command "/api/session/%s/command")
 (post session-shell "/api/session/%s/shell")
 (session-inbox "/api/session/%s/inbox")
 (post session-model "/api/session/%s/model")
 (post session-agent "/api/session/%s/agent")
 (post session-generate "/api/session/%s/generate")
 (recent-assistant-messages
  "/api/session/%s/message?type=assistant&order=desc&limit=50")
 (last-assistant-message
  "/api/session/%s/message?type=assistant&order=desc&limit=1")
 (first-user-message "/api/session/%s/message?type=user&order=asc&limit=1")
 (last-user-message "/api/session/%s/message?type=user&order=desc&limit=1")
 (session-diff "/api/session/%s/diff?context=0")
 (session-range-diff "/api/session/%s/diff?from=%s&to=%s&context=0")
 ;; Without context the patches cover the whole files
 (turn-diff "/api/session/%s/diff?from=%s")
 (post revert-message "/api/session/%s/revert/stage")
 (delete unrevert-all "/api/session/%s/revert")
 (session-permissions "/api/session/%s/permission")
 (post respond-permission-request "/api/session/%s/permission/%s/reply")
 (forms "/api/form")
 (session-forms "/api/session/%s/form")
 (post reply-form "/api/session/%s/form/%s/reply")
 (delete cancel-form "/api/session/%s/form/%s")
 (delete cancel-form-with-message "/api/session/%s/form/%s?message=%s"))

(defmacro opencode-api-all-messages (args return-var &rest body)
  "Run BODY with all of a session's messages, oldest first, bound to RETURN-VAR.
ARGS is a list of the session id.  BODY may start with :else HANDLER to
handle errors."
  (declare (indent 2))
  (let ((else (when (eq (car body) :else)
                (prog1 (cadr body) (setq body (cddr body))))))
    `(opencode-api--fetch-all-messages
      ,@args (lambda (,return-var) ,@body) ,@(when else (list else)))))

(defun opencode-api--cached-location (then)
  "Call THEN with the location of `default-directory'.
Only the first call for a directory requests it from the server, which also
caches it until `opencode-reload-locations'."
  (let ((directory (directory-file-name (expand-file-name default-directory)))
        (connection (opencode--conn)))
    (if-let* ((location (alist-get directory (opencode-connection-locations connection)
                                   nil nil #'equal)))
        (funcall then location)
      (opencode-api-location location
        (setf (alist-get directory (opencode-connection-locations connection)
                         nil nil #'equal)
              location)
        (funcall then location)))))

(provide 'opencode-api)
;;; opencode-api.el ends here
