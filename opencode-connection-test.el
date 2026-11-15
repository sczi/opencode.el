;;; opencode-connection-test.el --- Tests of connections to opencode servers  -*- lexical-binding: t; -*-
;;; Code:

(require 'ert)
(require 'opencode)
(require 'opencode-test-helpers)

(ert-deftest opencode-connection-lookup-order-test ()
  "The dispatching connection beats the buffer's, which beats the default."
  (let* ((a (opencode--make-connection :name 'a))
         (b (opencode--make-connection :name 'b))
         (c (opencode--make-connection :name 'c))
         (opencode-default-connection a))
    (with-temp-buffer
      (should (eq a (opencode-current-connection)))
      (setq opencode-buffer-connection b)
      (should (eq b (opencode-current-connection)))
      (let ((opencode--dispatching-connection c))
        (should (eq c (opencode-current-connection)))))
    (let ((opencode-default-connection nil))
      (should-error (opencode--conn)))))

(ert-deftest opencode-connection-prints-briefly-test ()
  "A connection's caches are left out when printed with `cl-prin1'."
  (let ((connection (opencode--make-connection
                     :name 'local :url "http://localhost:4096"
                     :skills (list (cons "/" (make-list 100 "a skill"))))))
    (should (equal "#<opencode-connection local http://localhost:4096 disconnected>"
                   (cl-prin1-to-string connection)))))

(ert-deftest opencode-request-callback-connection-test ()
  "A response is handled with the connection its request was made on."
  (let* ((a (opencode--make-connection :name 'a :url "http://a"))
         (b (opencode--make-connection :name 'b :url "http://b"))
         (opencode-default-connection a)
         urls seen)
    (cl-letf (((symbol-function 'plz)
               (lambda (_method url &rest args)
                 (push url urls)
                 ;; the default changes before the response arrives
                 (let ((opencode-default-connection b))
                   (funcall (plist-get args :then) nil)))))
      (opencode-api--request 'get "/api/x"
                             :then (lambda (_) (setq seen (opencode-current-connection)))))
    (should (equal '("http://a/api/x") urls))
    (should (eq a seen))))

(ert-deftest opencode-event-routing-test ()
  "Events of a server only reach the session buffers of its connection."
  (let* ((a (opencode--make-connection :name 'a))
         (b (opencode--make-connection :name 'b))
         (opencode-default-connection a)
         (buffer-a (generate-new-buffer " *a*"))
         (buffer-b (generate-new-buffer " *b*")))
    (unwind-protect
        (progn
          (puthash "ses" buffer-a (opencode-connection-session-buffers a))
          (puthash "ses" buffer-b (opencode-connection-session-buffers b))
          (opencode--handle-event
           b (plz-event-source-event
              :data (json-encode '((type . "session.renamed")
                                   (data . ((sessionID . "ses") (title . "New")))))))
          (should (equal "*OpenCode: New*" (buffer-name buffer-b)))
          (should (equal " *a*" (buffer-name buffer-a))))
      (kill-buffer buffer-a)
      (kill-buffer buffer-b))))

(ert-deftest opencode-connection-for-url-test ()
  "Connections are found by URL, and named after the configured server."
  (let ((opencode-connections nil)
        (opencode-servers '((local) (other :port 4097)))
        (opencode-host "localhost")
        (opencode-port 4096))
    (let ((local (opencode--server-connection 'local))
          (other (opencode--connection-for-url "http://localhost:4097" "localhost:4097"))
          (adhoc (opencode--connection-for-url "http://box:4096" "box:4096")))
      (should (eq 'local (opencode-connection-name local)))
      (should (eq 'other (opencode-connection-name other)))
      (should (equal "box:4096" (opencode-connection-name adhoc)))
      (should (eq local (opencode--connection-for-url "http://localhost:4096" "x")))
      (should (length= opencode-connections 3)))))

(ert-deftest opencode-connection-list-test ()
  "The connection list shows each connection, and marks the default."
  (let* ((a (opencode--make-connection :name 'a :url "http://a:1" :stream t))
         (b (opencode--make-connection :name 'b :url "http://b:2"))
         (opencode-connections (list a b))
         (opencode-default-connection a))
    (with-temp-buffer
      (opencode-connection-list-mode)
      (opencode--draw-connection-list)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p (rx bol "*" (+ space) "a" (+ space) "http://a:1"
                                    (+ space) "connected")
                                text))
        (should (string-match-p (rx bol (+ space) "b" (+ space) "http://b:2"
                                    (+ space) "disconnected")
                                text)))
      (goto-char (point-min))
      (search-forward "http://b")
      (should (eq b (vtable-current-object))))))

(ert-deftest opencode-session-control-buffer-server-name-test ()
  "Control buffers name their server unless it's `opencode-default-server'."
  (let* ((a (opencode--make-connection :name 'a :stream t))
         (b (opencode--make-connection :name 'b :stream t))
         (opencode-connections (list a b))
         (opencode-default-connection a)
         (opencode-default-server 'a)
         opencode--session-control-buffers buffers)
    (cl-letf (((symbol-function 'opencode-sessions-redisplay) #'ignore)
              ((symbol-function 'switch-to-buffer)
               (lambda (buffer &rest _) (push buffer buffers)))
              ((symbol-function 'opencode-api--request)
               (lambda (_method _path &rest args)
                 (funcall (plist-get args :then)
                          '((project . ((id . "repo") (directory . "/src/repo")
                                        (canonical . "/src/repo"))))))))
      (unwind-protect
          (progn
            (opencode-open-project "/src/repo/")
            (let ((opencode-default-connection b))
              (opencode-open-project "/src/repo/"))
            (should (equal '("*OpenCode Sessions[b]: repo*" "*OpenCode Sessions: repo*")
                           (mapcar #'buffer-name buffers))))
        (mapc #'kill-buffer buffers)))))

(ert-deftest opencode-server-spec-test ()
  "%s shows the server name only when more than one server is connected."
  (with-opencode-session-test-buffer
    (cl-letf (((symbol-function 'opencode--current-model) #'ignore))
      (setf (opencode-connection-stream opencode-default-connection) t)
      (should (equal "[]" (opencode--mode-line-expand "[%s]")))
      (let ((opencode-connections
             (list opencode-default-connection
                   (opencode--make-connection :name 'other :stream t))))
        (should (equal "[test ]" (opencode--mode-line-expand "[%s]")))))))

(provide 'opencode-connection-test)
;;; opencode-connection-test.el ends here
