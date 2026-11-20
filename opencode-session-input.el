;;; opencode-session-input.el --- Composing and sending opencode prompts  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; The prompt of session buffers: attachments, slash commands, and the
;; agent and model it's sent with.

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'dired)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-session-output)
(require 'project)
(require 'url-util)

(defvar-local opencode-session-agent nil
  "Currently active agent for this buffer's session.")

(defvar-local opencode-session-agents nil
  "List of agents for the current session.
Buffer local so we can configure models and variants per agent per session.")

(defvar-local opencode--server-agent nil
  "Id of the agent the server has selected for this buffer's session.")

(defvar-local opencode--temp-files nil
  "Temporary files to delete after sending or killing a session buffer.")

(defun opencode--selectable-agent-p (agent)
  "Return non-nil if AGENT can be the agent of a session."
  (not (equal "subagent" (alist-get 'mode agent))))

(defun opencode-cycle-session-agent ()
  "Switch to the next agent in `opencode-session-agents'."
  (interactive)
  (let* ((id (alist-get 'id opencode-session-agent))
         (agents opencode-session-agents)
         (pos (and id (cl-position-if
                       (lambda (agent)
                         (string= id (alist-get 'id agent)))
                       agents))))
    (when pos
      (setcar (nthcdr pos agents) opencode-session-agent)
      (when-let* ((next (seq-find
                         #'opencode--selectable-agent-p
                         (append (nthcdr (1+ pos) agents)
                                 (seq-take agents (1+ pos))))))
        (setq opencode-session-agent next))))
  (force-mode-line-update))

(defun opencode--model-ref (model)
  "Return the Model.Ref for MODEL, a Model.Info."
  `((id . ,(alist-get 'id model))
    (providerID . ,(alist-get 'providerID model))))

(defun opencode--set-models (connection models)
  "Set the enabled MODELS of CONNECTION's server, and index them by Model.Ref.
The mode line looks up the current model on every update."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (model models)
      (let ((key (cons (alist-get 'providerID model) (alist-get 'id model))))
        (unless (gethash key table)
          (puthash key model table))))
    (setf (opencode-connection-models connection) models
          (opencode-connection-models-by-ref connection) table)))

(defun opencode--find-model (ref)
  "Return the enabled Model.Info that the Model.Ref REF refers to, or nil."
  (when-let* ((connection (opencode-current-connection))
              (table (opencode-connection-models-by-ref connection)))
    (gethash (cons (alist-get 'providerID ref) (alist-get 'id ref)) table)))

(defun opencode--collect-all-models ()
  "Collect all of the server's enabled models as a list.
Each element is (display-name model-ref provider-id)."
  (mapcar (lambda (model)
            (list (alist-get 'name model)
                  (opencode--model-ref model)
                  (alist-get 'providerID model)))
          (opencode--models)))

(defun opencode--model-variant-p (model variant)
  "Return non-nil if MODEL, a Model.Info, has a variant with id VARIANT."
  (seq-find (lambda (candidate)
              (equal variant (alist-get 'id candidate)))
            (alist-get 'variants model)))

(defun opencode-select-model ()
  "Select a model for the current session and agent using completion."
  (interactive)
  (unless opencode-session-agent
    (user-error "not in a session"))
  (when-let* ((ref (opencode--annotated-completion
                    "Model: "
                    (opencode--collect-all-models))))
    (let ((variant (map-nested-elt opencode-session-agent '(model variant))))
      (when (opencode--model-variant-p (opencode--find-model ref) variant)
        (setf (alist-get 'variant ref) variant))
      (setf (alist-get 'model opencode-session-agent) ref
            opencode-last-model (copy-alist ref)))))

(defun opencode--current-model ()
  "Return the active model for this session, as a Model.Info."
  (opencode--find-model (alist-get 'model opencode-session-agent)))

(defun opencode--cycle-variant (step)
  "Move STEP places through the current model's variants, wrapping around.
With no variant selected, start from the first or last variant."
  (unless opencode-session-agent
    (user-error "not in a session"))
  (let* ((ids (mapcar (lambda (variant) (alist-get 'id variant))
                      (alist-get 'variants (opencode--current-model))))
         (current (map-nested-elt opencode-session-agent '(model variant)))
         (pos (cl-position current ids :test #'equal)))
    (if (null ids)
        (message "No variants")
      (let ((variant (nth (if pos
                              (mod (+ pos step) (length ids))
                            (if (> step 0) 0 (1- (length ids))))
                          ids)))
        (setf (alist-get 'variant (alist-get 'model opencode-session-agent))
              variant)
        (force-mode-line-update)))))

(defun opencode-cycle-variant ()
  "Switch to the next variant of the current model."
  (interactive)
  (opencode--cycle-variant 1))

(defun opencode-cycle-variant-backwards ()
  "Switch to the previous variant of the current model."
  (interactive)
  (opencode--cycle-variant -1))

(defconst opencode--directory-fetchers
  `((slash-commands . ,(lambda (then) (opencode-api-commands commands (funcall then commands))))
    (skills . ,(lambda (then) (opencode-api-skills skills (funcall then skills)))))
  "Alist of connection slots cached per directory to functions fetching them.
Each function calls its argument with the fetched value.")

(defun opencode--directory-cache-entry (slot directory)
  "Return the (DIRECTORY . VALUE) cached in SLOT of the current connection, or nil."
  (when-let* ((connection (opencode-current-connection)))
    (assoc (opencode--normalize-directory directory)
           (cl-struct-slot-value 'opencode-connection slot connection))))

(defun opencode--download-for-directory (slot directory &optional force)
  "Download SLOT of `opencode--directory-fetchers' for DIRECTORY.
Unless it already was downloaded, or FORCE."
  (let ((directory (opencode--normalize-directory directory))
        (connection (opencode--conn)))
    (when (or force (not (opencode--directory-cache-entry slot directory)))
      (let ((default-directory directory))
        (funcall (alist-get slot opencode--directory-fetchers)
                 (lambda (value)
                   (setf (alist-get directory
                                    (cl-struct-slot-value 'opencode-connection slot connection)
                                    nil nil #'string=)
                         value)))))))

(defun opencode--download-slash-commands (directory &optional force)
  "Download slash commands for DIRECTORY, unless already downloaded or FORCE."
  (opencode--download-for-directory 'slash-commands directory force))

(defun opencode--download-skills (directory &optional force)
  "Download skills available in DIRECTORY, unless they already were or FORCE."
  (opencode--download-for-directory 'skills directory force))

(defun opencode--directory-slash-commands ()
  "Return the slash commands of `default-directory' on the server."
  (cdr (opencode--directory-cache-entry 'slash-commands default-directory)))

(defun opencode--directory-skills ()
  "Return the skills of `default-directory' on the server."
  (cdr (opencode--directory-cache-entry 'skills default-directory)))

(defun opencode-insert-slash-command ()
  "Insert an opencode slash command."
  (interactive)
  (if (= (point) (cdr comint-last-prompt))
      (let* ((commands (opencode--directory-slash-commands))
             (command (opencode--annotated-completion
                       "Slash command: "
                       (cl-loop for command in commands
                                collect (let-alist command
                                          (list
                                           .name
                                           .name
                                           .description))))))
        (when command
          (insert (concat "/" command " "))))
    (call-interactively #'self-insert-command)))

(defun opencode--attachable-file-p (file)
  "Return non-nil if the model can be sent FILE.
The server silently drops all but directories, text, PDFs and some images."
  (or (file-directory-p file)
      (let ((bytes (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (insert-file-contents-literally file nil 0 (* 1024 1024))
                     (buffer-string))))
        (or (string-match-p "\\`\\(?:\x89PNG\\|\xff\xd8\xff\\|GIF8\\|BM\\|%PDF-\\|RIFF[^z-a]\\{4\\}WEBP\\)"
                            bytes)
            (and (not (string-search "\0" bytes))
                 (not (memq 'eight-bit
                            (find-charset-string
                             (decode-coding-string
                              ;; the size limit can cut the last character in half
                              (replace-regexp-in-string "[\xc0-\xff][\x80-\xbf]*\\'" "" bytes)
                              'utf-8 t)))))))))

(defun opencode--insert-label (name part)
  "Insert a label showing NAME that sends PART with the prompt.
The label is a single character, so it is deleted as a unit, and undo
restores it together with PART."
  (insert (propertize "￼"
                      'display (propertize name 'face 'markdown-ts-code-span)
                      'opencode-label-name name
                      'opencode-part part
                      'rear-nonsticky t)))

(defun opencode--input-text-and-parts (start end)
  "Return (TEXT . PARTS) for the input between START and END.
PARTS are those of the labels in the input, which TEXT shows as code."
  (let ((pos start)
        chunks
        parts)
    (while (< pos end)
      (if-let* ((name (get-text-property pos 'opencode-label-name)))
          (progn
            (push (opencode--code-span name) chunks)
            (push (get-text-property pos 'opencode-part) parts)
            (cl-incf pos))
        (let ((next (next-single-property-change pos 'opencode-part nil end)))
          (push (buffer-substring-no-properties pos next) chunks)
          (setq pos next))))
    (cons (apply #'concat (nreverse chunks))
          ;; a label yanked twice shares its part
          (delete-dups (nreverse parts)))))

(defun opencode--file-attachment (file name)
  "Return a label part attaching FILE as NAME.
If FILE isn't on the current connection's server's host, its contents are
sent instead, which can't be done for a directory."
  `(files . ((uri . ,(if-let* ((path (opencode--to-server-path file)))
                         (concat "file://"
                                 (url-hexify-string path url-path-allowed-chars))
                       (when (file-directory-p file)
                         (user-error "Can't attach %s, it's not on the host of opencode server %s"
                                     file (opencode-connection-name (opencode--conn))))
                       ;; The server detects the type from the contents
                       (concat "data:application/octet-stream;base64,"
                               (with-temp-buffer
                                 (set-buffer-multibyte nil)
                                 (insert-file-contents-literally file)
                                 (base64-encode-region (point-min) (point-max) t)
                                 (buffer-string)))))
             (name . ,name))))

(defun opencode--text-attachment (text name)
  "Return a label part attaching TEXT as NAME."
  `(files . ((uri . ,(concat "data:text/plain;base64,"
                             (base64-encode-string
                              (encode-coding-string text 'utf-8) t)))
             (name . ,name))))

(defun opencode--add-file-to-context (file &optional display-name)
  "Add FILE to context with optional DISPLAY-NAME."
  (unless (opencode--attachable-file-p file)
    (user-error "Can't attach %s: unsupported file type" file))
  (let ((display-name (or display-name
                          (opencode--relative-path-for-display file))))
    (opencode--insert-label display-name
                            (opencode--file-attachment file display-name))))

(defun opencode--image-extension (mime)
  "Return a file extension for image MIME."
  (or (cadr (split-string mime "[/;+]" t))
      "img"))

(defun opencode--yank-image (type data)
  "Add pasted image DATA of MIME TYPE to the next prompt."
  (unless (comint-after-pmark-p)
    (user-error "Images can only be pasted into the current prompt"))
  (let* ((mime (symbol-name type))
         (extension (opencode--image-extension mime))
         (file (make-temp-file "opencode-image-" nil
                               (concat "." extension))))
    (condition-case error
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region data nil file nil 'silent))
          (push file opencode--temp-files)
          (opencode--add-file-to-context file (file-name-nondirectory file)))
      (error
       (ignore-errors (delete-file file))
       (signal (car error) (cdr error))))))

(defun opencode--delete-temp-files (files)
  "Delete temporary FILES created by opencode."
  (dolist (file files)
    (when (and file (file-exists-p file))
      (ignore-errors (delete-file file)))))

(defmacro with-last-opencode-session (&rest body)
  "Run BODY with the last used opencode session buffer active."
  `(if opencode-last-session-buffer
       (if (buffer-live-p opencode-last-session-buffer)
           (with-current-buffer opencode-last-session-buffer
             (unless (comint-after-pmark-p)
               (goto-char (point-max)))
             ,@body)
         (user-error "Last selected OpenCode session buffer is no longer active"))
     (user-error "Open an OpenCode session first")))

(defun opencode-add-file (&optional file prompt-directories)
  "Add a FILE to context, or prompt for file in current project.
With a prefix argument (PROMPT-DIRECTORIES), show project directories instead of files."
  (interactive (list nil current-prefix-arg))
  (with-last-opencode-session
   (let* ((project (project-current t))
          (root (project-root project))
          (all-files (project-files project)))
     (opencode--add-file-to-context
      (or file
          (project--read-file-name
           project
           "Add to context"
           (if prompt-directories
               (delete root
                       (delete-dups
                        (cons
                         (concat root "./")
                         (mapcar #'file-name-directory all-files))))
             all-files)
           nil
           'file-name-history))))))

(defun opencode-add-file-dwim ()
  "If in Dired, add all marked files, or file at point if none marked.
Otherwise add the current buffer's file.
Otherwise prompt for file in current project."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (if-let* ((files (dired-get-marked-files)))
          (mapc #'opencode-add-file files)
        (if-let* ((file (dired-get-filename nil t)))
            (opencode-add-file file)
          (user-error "No file at point, and no marked files")))
    (opencode-add-file (buffer-file-name))))

(defun opencode-add-buffer-dwim ()
  "Add current buffer or prompt for one to add."
  (interactive)
  (let ((buffer (if opencode-session-id
                    (read-buffer "Add to context: ")
                  (buffer-name))))
    (with-last-opencode-session
     (opencode--insert-label
      buffer
      (opencode--text-attachment
       (with-current-buffer buffer
         (buffer-substring-no-properties (point-min) (point-max)))
       buffer)))))

(defun opencode--region-location (beg end)
  "Return (END START-LINE START-COL END-LINE END-COL) for BEG to END.
An END at the start of a line is moved back over the preceding newline,
so the returned END is where the region's text should stop.  Columns
are 1-based and the end column is that of the last included character.
A column is nil when its side of the region is on a line boundary."
  (save-excursion
    (when (and (< beg end) (eq (char-before end) ?\n))
      (cl-decf end))
    (goto-char beg)
    (let ((start-line (line-number-at-pos nil t))
          (start-col (unless (bolp) (1+ (- (point) (line-beginning-position))))))
      (goto-char end)
      (list end start-line start-col
            (line-number-at-pos nil t)
            (unless (eolp) (- (point) (line-beginning-position)))))))

(defun opencode--region-with-line-numbers (beg end file path)
  "Return (LABEL . TEXT) for the region BEG to END of FILE, shown as PATH.
FILE is the path on the server.  TEXT is formatted like the output of the
read tool."
  (pcase-let* ((`(,end ,sl ,sc ,el ,ec) (opencode--region-location beg end))
               (lines (split-string (buffer-substring-no-properties beg end)
                                    "\n"))
               (`(,range . ,location)
                (cond ((not (or sc ec))
                       (cons (format "lines %d-%d" sl el)
                             (if (= sl el)
                                 (format "%d" sl)
                               (format "%d-%d" sl el))))
                      ((= sl el)
                       (let ((sc (or sc 1))
                             (ec (or ec (save-excursion
                                          (goto-char end)
                                          (- (point) (line-beginning-position))))))
                         (cons (format "line %d columns %d-%d" sl sc ec)
                               (format "%d:%d-%d" sl sc ec))))
                      (t
                       (cons (format "from line %d%s to line %d%s"
                                     sl (if sc (format " column %d" sc) "")
                                     el (if ec (format " column %d" ec) ""))
                             (format "%d%s-%d%s"
                                     sl (if sc (format ":%d" sc) "")
                                     el (if ec (format ":%d" ec) ""))))))
               (label (concat path ":" location)))
    (cons label
          (mapconcat #'identity
                     (cons (format "Read file %s, %s" file range)
                           (seq-map-indexed
                            (lambda (line i) (format "%d: %s" (+ sl i) line))
                            lines))
                     "\n"))))

(defun opencode--add-region-text (beg end)
  "Add the text from BEG to END of the current buffer to context.
If the buffer visits a file, the text is sent with its file name and
line numbers, if it's on the host of the last session's server."
  (pcase-let* ((file buffer-file-name)
               (`(,server-file . ,path)
                (when (and file (buffer-live-p opencode-last-session-buffer))
                  (with-current-buffer opencode-last-session-buffer
                    (when-let* ((server-file (opencode--to-server-path file)))
                      (cons server-file (opencode--relative-path-for-display file))))))
               (`(,name . ,text)
                (if server-file
                    (opencode--region-with-line-numbers beg end server-file path)
                  (let ((region (buffer-substring-no-properties beg end)))
                    (cons (format "region: %s"
                                  (truncate-string-to-width
                                   region 24 0 nil (truncate-string-ellipsis)))
                          region)))))
    (with-last-opencode-session
     (opencode--insert-label name (opencode--text-attachment text name)))))

(defun opencode-add-region ()
  "Add the active region to context.
If the buffer visits a file, the region is sent with its file name and
line numbers."
  (interactive)
  (unless (use-region-p)
    (user-error "No active region"))
  (opencode--add-region-text (region-beginning) (region-end)))

(defun opencode-add-defun ()
  "Add the defun at point to context, as a region.
If the buffer visits a file, the defun is sent with its file name and
line numbers."
  (interactive)
  (pcase (bounds-of-thing-at-point 'defun)
    (`(,beg . ,end) (opencode--add-region-text beg end))
    (_ (user-error "No defun at point"))))

(defun opencode-add-mention ()
  "Insert a mention of a skill or subagent into the current input."
  (interactive)
  (call-interactively #'self-insert-command)
  (when (comint-after-pmark-p)
    (let ((candidates
           (append
            (cl-loop for skill in (opencode--directory-skills)
                     collect (let-alist skill
                               (list .id
                                     (cons .id `(skills . ((id . ,.id))))
                                     (concat "skill     " (or .description "")))))
            (cl-loop for agent in (opencode--agents)
                     when (string= "subagent" (alist-get 'mode agent))
                     collect (let-alist agent
                               (list .name
                                     (cons .name `(agents . ((name . ,.name))))
                                     (concat "subagent  " (or .description ""))))))))
      (unless candidates
        (user-error "No skills or subagents available"))
      (pcase-let ((`(,name . ,part)
                   (opencode--annotated-completion "Mention: " candidates)))
        (delete-char -1)
        (opencode--insert-label (concat "@" name) part)
        (insert " ")))))

(defun opencode--adopt-selection (agent model)
  "Make the local agent and model AGENT and MODEL, if non-nil."
  (when (and agent (not (equal agent (alist-get 'id opencode-session-agent))))
    (setq opencode-session-agent
          (or (seq-find (lambda (candidate)
                          (equal agent (alist-get 'id candidate)))
                        opencode-session-agents)
              ;; a hidden agent
              `((id . ,agent) (name . ,agent)))))
  (when model
    (setf (alist-get 'model opencode-session-agent) (copy-alist model)))
  (force-mode-line-update))

(defun opencode--send-failed (error-msg)
  "Display ERROR-MSG of input that wasn't accepted, and go back to a prompt.
No events will follow to do so."
  (opencode--insert-error error-msg)
  (opencode--show-prompt))

(defun opencode--sync-agent-and-model (callback)
  "Make the server select the local agent and model, then run CALLBACK.
The server ignores a switch to the same model, but a switch of agent always
records a message, so that's only done if it differs."
  (let ((agent (alist-get 'id opencode-session-agent))
        (model (alist-get 'model opencode-session-agent)))
    (cl-flet ((sync-model ()
                (if model
                    (opencode-api-session-model (opencode-session-id)
                        `((model . ,model))
                        _response
                      :else #'opencode--send-failed
                      (funcall callback))
                  (funcall callback))))
      (if (or (null agent) (equal agent opencode--server-agent))
          (sync-model)
        (opencode-api-session-agent (opencode-session-id)
            `((agent . ,agent))
            _response
          :else #'opencode--send-failed
          (setq opencode--server-agent agent)
          (sync-model))))))

(defun opencode--unless-session-active (session-id function)
  "Call FUNCTION unless the server is still running SESSION-ID."
  (opencode-api-sessions-active
    active
    (unless (assq (intern session-id) active)
      (funcall function))))

(defun opencode--prompt-body (text parts)
  "Return the body of a prompt with TEXT and the label PARTS."
  (let ((files (cl-loop for (kind . part) in parts
                        when (eq kind 'files) collect part))
        (agents (cl-loop for (kind . part) in parts
                         when (eq kind 'agents) collect part))
        (skills (cl-loop for (kind . part) in parts
                         when (eq kind 'skills) collect part)))
    `((text . ,text)
      ,@(when files `((files . ,(vconcat files))))
      ,@(when agents `((agents . ,(vconcat agents))))
      ,@(when skills `((skills . ,(vconcat skills)))))))

(defun opencode--user-message-enqueued (id)
  "Mark the last sent input, a slash command, as that of the user message ID.
`/command' doesn't return the id of the message it creates."
  (let ((start comint-last-input-start))
    (when (and (eq (char-after start) ?/)
               (not (get-text-property start 'opencode-user-message)))
      (opencode--mark-input start comint-last-input-end id))))

(defun opencode--send-input (_proc string)
  "Send STRING as input to current opencode session.
The input is marked as of its user message once the server creates it."
  (opencode--highlight-input)
  (opencode--output "\n")
  ;; STRING is the buffer text of the input, where each label is one character
  (pcase-let* ((start (marker-position comint-last-input-start))
               (end (+ start (length string)))
               (`(,string . ,extra-parts) (opencode--input-text-and-parts start end))
               (temp-files opencode--temp-files))
    (setf opencode--temp-files nil)
    (cond
     ((string-prefix-p "/" string)
      (let ((space-pos (seq-position string ?\s)))
        (opencode--sync-agent-and-model
         (lambda ()
           (opencode-api-session-command (opencode-session-id)
               `((name . ,(substring string 1 space-pos))
                 (text . ,(if space-pos
                              (substring string (1+ space-pos))
                            "")))
               _response
             :else #'opencode--send-failed
             ;; Commands can just run a callback, then no events follow to do so.
             (opencode--unless-session-active
              opencode-session-id
              (lambda ()
                (unless (equal opencode-session-status "busy")
                  (opencode--show-prompt))))))))
      (opencode--delete-temp-files temp-files))
     ((string-prefix-p "!" string)
      (opencode--mark-input start end 'shell)
      (opencode-api-session-shell (opencode-session-id)
          `((command . ,(substring string 1)))
          _response
        :else #'opencode--send-failed)
      (opencode--delete-temp-files temp-files))
     (t
      (setq start (copy-marker start)
            end (copy-marker end))
      (opencode--sync-agent-and-model
       (lambda ()
         (opencode-api-session-prompt (opencode-session-id)
             (opencode--prompt-body string extra-parts)
             inbox-item
           :else (lambda (error-msg)
                   (opencode--delete-temp-files temp-files)
                   (opencode--send-failed error-msg))
           (opencode--delete-temp-files temp-files)
           (opencode--mark-input start end (alist-get 'id inbox-item))
           (set-marker start nil)
           (set-marker end nil)
           (opencode--insert-skills
            (map-nested-elt inbox-item '(payload skills))))))))))

(defun opencode-send-input ()
  "Send input like `comint-send-input', unless it is empty.
The input is the current input, or with point before it, the old input
`comint-get-old-input' copies, like `comint-send-input' sends."
  (interactive nil opencode-session-mode)
  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (unless (string-empty-p
             (if (>= (point) pmark)
                 (buffer-substring-no-properties pmark (point-max))
               (funcall comint-get-old-input)))
      (comint-send-input))))

(defun opencode-session--send-synthetic-input (string)
  "Send STRING to the current opencode session.
Preserve any pending input context while sending STRING as a plain prompt."
  (let* ((process (get-buffer-process (current-buffer)))
         (pending-prompt (copy-marker (process-mark process) t))
         (original-point (copy-marker (point) t))
         (prompt-start (opencode--session-process-position))
         start
         end)
    (unwind-protect
        (progn
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char prompt-start)
              (setq start (point))
              (insert string)
              (setq end (point))))
          (set-marker (process-mark process) end)
          (let ((comint-last-input-start start)
                (comint-last-input-end end)
                (opencode--temp-files nil))
            (opencode--send-input process string)))
      (set-marker (process-mark process) pending-prompt)
      (goto-char original-point)
      (set-marker pending-prompt nil)
      (set-marker original-point nil))))

(provide 'opencode-session-input)
;;; opencode-session-input.el ends here
