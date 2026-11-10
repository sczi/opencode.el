;;; opencode-forms.el --- Answer forms from the opencode server  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Arthur Heymans

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Keywords: internal

;;; Commentary:

;; The server asks the user for input with forms: the question tool, web
;; search consent, and MCP elicitations.  The last aren't tied to a session.
;;
;; Forms wait in a queue until they can be shown, which is right away for
;; global forms and when their session's buffer is active for the others.
;; Fields are prompted for one at a time: choices in a transient menu, text
;; and numbers in the minibuffer.  The form is replied to after the last
;; field, and dismissing any prompt cancels it.

;;; Code:

(require 'seq)
(require 'transient)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-sessions)

(defvar opencode-forms--queue nil
  "Forms waiting to be shown, oldest first.
Each is a Form.Info with the `directory' it belongs to added, and the `error'
from replying if it's being asked again.")

(defvar opencode-forms--current nil
  "The form being answered, or nil.")

(defvar opencode-forms--fields nil
  "The fields of the current form left to answer, starting with the current one.")

(defvar opencode-forms--answers nil
  "Alist of field key to answer for the current form, latest first.")

(defvar opencode-forms--after-menu nil
  "Function to call once the menu has exited, or nil to cancel the form.")

(defconst opencode-forms--supported-types
  '("string" "number" "integer" "boolean" "multiselect" "external")
  "The field types that can be prompted for.")

(defconst opencode-forms--option-keys "123456789abcdefghijklmnoprtuvwxyz"
  "Keys for the options of a menu, leaving out the ones used by its actions.")

;;; Fields

(defun opencode-forms--field ()
  "Return the field being answered."
  (car opencode-forms--fields))

(defun opencode-forms--flag-p (field flag)
  "Return non-nil if the JSON boolean FLAG of FIELD is true."
  (not (opencode--json-falsy (alist-get flag field))))

(defun opencode-forms--default (field)
  "Return the default answer of FIELD, or nil if it has none."
  (when-let* ((default (assq 'default field)))
    (cond ((equal (alist-get 'type field) "multiselect") (vconcat (cdr default)))
          ((eq (cdr default) :false) :json-false)
          (t (cdr default)))))

(defun opencode-forms--same-value-p (answer value)
  "Return non-nil if ANSWER is the VALUE of a `when' condition."
  (if (and (numberp answer) (numberp value))
      (= answer value)
    (or (equal answer value)
        (and (eq answer :json-false) (eq value :false)))))

(defun opencode-forms--active-p (field answers)
  "Return non-nil if every `when' condition of FIELD holds for ANSWERS.
A condition on an unanswered field doesn't hold."
  (seq-every-p
   (lambda (condition)
     (let-alist condition
       (when-let* ((answer (assoc .key answers)))
         (let ((hit (seq-some (lambda (item)
                                (opencode-forms--same-value-p item .value))
                              (if (vectorp (cdr answer))
                                  (cdr answer)
                                (list (cdr answer))))))
           (if (equal .op "eq") hit (not hit))))))
   (alist-get 'when field)))

(defun opencode-forms--label (field)
  "Return what FIELD is asking, or nil for an external field that doesn't say."
  (let-alist field
    (or .description .title (unless (equal .type "external") .key))))

(defun opencode-forms--heading (form field)
  "Return the text to prompt for FIELD of FORM with."
  (let-alist form
    (string-join
     (delq nil (list (and .error (propertize .error 'face 'error))
                     (unless (equal .metadata.kind "question") .title)
                     .metadata.message
                     (opencode-forms--label field)))
     "\n")))

;;; Replying

(defun opencode-forms--settled-error-p (error-msg)
  "Return non-nil if ERROR-MSG says the form was already answered or cancelled."
  (string-search "FormAlreadySettledError" error-msg))

(defun opencode-forms--cancel-form (form)
  "Cancel FORM."
  (let-alist form
    (let ((default-directory .directory))
      (opencode-api-cancel-form (.sessionID .id)
          _result
        :else (lambda (error-msg)
                (unless (opencode-forms--settled-error-p error-msg)
                  (opencode--log-event "WARNING FORM CANCEL" error-msg)))))))

(defun opencode-forms--cancel ()
  "Cancel the current form, and show the next."
  (opencode-forms--cancel-form opencode-forms--current)
  (setq opencode-forms--current nil)
  (opencode-forms--prompt-next))

(defun opencode-forms--reply-failed (form error-msg)
  "Handle ERROR-MSG from replying to FORM.
Ask again if the answer was invalid and the user can change it, as only the
server checks patterns and formats.  Otherwise cancel so the session won't hang."
  (cond
   ((opencode-forms--settled-error-p error-msg))
   ((and (string-search "FormInvalidAnswerError" error-msg)
         (not (seq-every-p (lambda (field) (opencode-forms--flag-p field 'hidden))
                           (alist-get 'fields form))))
    (push (cons (cons 'error error-msg) form) opencode-forms--queue)
    (opencode-forms--prompt-next))
   (t
    (message "opencode: %s" error-msg)
    (opencode-forms--cancel-form form))))

(defun opencode-forms--reply ()
  "Send the answers of the current form, and show the next."
  (let ((form opencode-forms--current)
        (answers (reverse opencode-forms--answers)))
    (setq opencode-forms--current nil)
    (let-alist form
      (let ((default-directory .directory))
        (opencode-api-reply-form (.sessionID .id)
            ;; Without answers an alist would encode as null rather than {}.
            `((answer . ,(or answers (make-hash-table))))
            _result
          :else (lambda (error-msg)
                  (opencode-forms--reply-failed form error-msg)))))
    (opencode-forms--prompt-next)))

;;; Prompting

(defun opencode-forms--answer (value)
  "Answer the current field with VALUE, or skip it if nil, and go to the next."
  (let ((key (alist-get 'key (pop opencode-forms--fields))))
    (when value
      (push (cons key value) opencode-forms--answers))
    (opencode-forms--next)))

(defun opencode-forms--next ()
  "Prompt for the next field of the current form, or reply if none remain."
  (let* ((field (opencode-forms--field))
         (default (opencode-forms--default field))
         (type (alist-get 'type field)))
    (cond
     ((null field) (opencode-forms--reply))
     ((not (opencode-forms--active-p field opencode-forms--answers))
      (opencode-forms--answer nil))
     ((and (opencode-forms--flag-p field 'hidden)
           (or default (not (opencode-forms--flag-p field 'required))))
      (opencode-forms--answer default))
     ((or (member type '("number" "integer"))
          (and (equal type "string") (not (alist-get 'options field))))
      (opencode-forms--answer-from-minibuffer))
     (t (opencode-forms--show-menu
         (when (equal type "multiselect")
           (mapcar (lambda (option) (alist-get 'label option))
                   (seq-filter (lambda (option)
                                 (seq-contains-p default (alist-get 'value option)))
                               (alist-get 'options field)))))))))

;;; Minibuffer

(defun opencode-forms--problem (field text)
  "Return why TEXT can't be the answer to FIELD, or nil if it can.
An empty TEXT skips an optional field."
  (let-alist field
    (let* ((numeric (member .type '("number" "integer")))
           (number (and numeric
                        (string-match-p
                         "\\`[-+]?\\([0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\([eE][-+]?[0-9]+\\)?\\'"
                         text)
                        (string-to-number text))))
      (cond
       ((string-empty-p text)
        (and (opencode-forms--flag-p field 'required) "required"))
       ((and .minLength (< (length text) .minLength))
        (format "at least %d characters" .minLength))
       ((and .maxLength (> (length text) .maxLength))
        (format "at most %d characters" .maxLength))
       ((not numeric) nil)
       ((null number) "enter a number")
       ((and (equal .type "integer") (/= number (truncate number)))
        "enter a whole number")
       ((and .minimum (< number .minimum)) (format "at least %s" .minimum))
       ((and .maximum (> number .maximum)) (format "at most %s" .maximum))))))

(defun opencode-forms--answer-from-minibuffer ()
  "Answer the current field from the minibuffer, which cancels the form if quit.
Leaving it empty skips the field."
  (let* ((field (opencode-forms--field))
         (heading (opencode-forms--heading opencode-forms--current field)))
    (let-alist field
      (condition-case nil
          (let* ((read (lambda (note)
                         (read-string (concat heading note ": ")
                                      (and .default (format "%s" .default)))))
                 (text (funcall read (if .placeholder (format " [%s]" .placeholder) "")))
                 problem)
            (while (setq problem (opencode-forms--problem field text))
              (setq text (funcall read (format " (%s)" problem))))
            (opencode-forms--answer
             (cond ((string-empty-p text) nil)
                   ((member .type '("number" "integer")) (string-to-number text))
                   (t text))))
        (quit (opencode-forms--cancel))))))

;;; Menu

(defun opencode-forms--suffix (function &rest args)
  "Return a menu command calling FUNCTION with ARGS once the menu has exited."
  (lambda ()
    (interactive)
    (setq opencode-forms--after-menu (lambda () (apply function args)))))

(defun opencode-forms--selection (field args)
  "Return the vector of values selected for multiselect FIELD by menu ARGS."
  (vconcat
   (mapcar (lambda (option) (alist-get 'value option))
           (seq-filter (lambda (option) (member (alist-get 'label option) args))
                       (alist-get 'options field)))
   (when-let* ((custom (transient-arg-value "custom=" args)))
     (list custom))))

(defun opencode-forms--selection-problem (field count)
  "Return why multiselect FIELD can't have COUNT selected, or nil if it can."
  (let-alist field
    (cond
     ((zerop count)
      (and (opencode-forms--flag-p field 'required) "Select at least one option"))
     ((and .minItems (< count .minItems)) (format "Select at least %d" .minItems))
     ((and .maxItems (> count .maxItems)) (format "Select at most %d" .maxItems)))))

(defun opencode-forms--confirm ()
  "Answer the current multiselect field with the selected options."
  (interactive)
  (let* ((args (transient-args 'opencode-forms--menu))
         (field (opencode-forms--field))
         (selection (opencode-forms--selection field args))
         (problem (opencode-forms--selection-problem field (length selection))))
    (setq opencode-forms--after-menu
          (cond (problem (lambda ()
                           (opencode-forms--show-menu args)
                           (message "%s" problem)))
                ((length= selection 0) (apply-partially #'opencode-forms--answer nil))
                (t (apply-partially #'opencode-forms--answer selection))))))

(defun opencode-forms--option-key (index)
  "Return the menu key of the option at INDEX."
  (let ((count (length opencode-forms--option-keys)))
    (if (< index count)
        (string (aref opencode-forms--option-keys index))
      (format "M-%c" (aref opencode-forms--option-keys (- index count))))))

(defun opencode-forms--option-spec (field option index)
  "Return the menu spec for OPTION of FIELD, at INDEX."
  (let-alist option
    (let ((key (opencode-forms--option-key index))
          (note (and .description (propertize .description 'face 'shadow))))
      (if (equal (alist-get 'type field) "multiselect")
          `(,key ,(or note "") ,.label :class transient-switch :format " %k %v  %d")
        `(,key ,(concat .label
                        (and note (concat "  " note))
                        (and (equal .value (opencode-forms--default field))
                             " (default)"))
          ,(opencode-forms--suffix #'opencode-forms--answer .value))))))

(defun opencode-forms--menu-children (_children)
  "Return the menu suffixes for the current field.
_CHILDREN is the statically-defined children, of which there are none."
  (let* ((field (opencode-forms--field))
         (type (alist-get 'type field))
         (custom (opencode-forms--flag-p field 'custom))
         (options (if (equal type "boolean")
                      '(((value . t) (label . "Yes"))
                        ((value . :json-false) (label . "No")))
                    (alist-get 'options field))))
    (transient-parse-suffixes
     'opencode-forms--menu
     (append
      (seq-map-indexed (lambda (option index)
                         (opencode-forms--option-spec field option index))
                       options)
      (pcase type
        ("external"
         (let ((url (alist-get 'url field)))
           `(("o" ,(format "Open %s" url)
              ,(lambda () (interactive) (browse-url url))
              :transient t)
             ("C-c C-c" "Done" ,(opencode-forms--suffix #'opencode-forms--answer t)))))
        ("multiselect"
         `(,@(when custom
               '(("!" "Type your own answer" "custom=" :prompt "Custom answer: ")))
           ("C-c C-c" "Confirm" opencode-forms--confirm)))
        (_ (when custom
             `(("!" "Type your own answer"
                ,(opencode-forms--suffix #'opencode-forms--answer-from-minibuffer))))))
      (unless (or (equal type "external")
                  (opencode-forms--flag-p field 'required))
        `(("s" "Skip" ,(opencode-forms--suffix #'opencode-forms--answer nil))))
      '(("q" "Cancel" transient-quit-one))))))

(transient-define-prefix opencode-forms--menu ()
  "Answer a field of a form from the opencode server."
  [:class transient-column
   :description (lambda ()
                  (opencode-forms--heading opencode-forms--current
                                           (opencode-forms--field)))
   :setup-children opencode-forms--menu-children])

(defun opencode-forms--menu-exited ()
  "Do what was chosen in the menu, or cancel the form if nothing was."
  (remove-hook 'transient-exit-hook #'opencode-forms--menu-exited)
  (when opencode-forms--current
    (run-at-time 0 nil (or opencode-forms--after-menu #'opencode-forms--cancel))))

(defun opencode-forms--show-menu (selection)
  "Show the menu for the current field with the arguments SELECTION enabled."
  (setq opencode-forms--after-menu nil)
  (add-hook 'transient-exit-hook #'opencode-forms--menu-exited)
  (transient-setup 'opencode-forms--menu nil nil :value selection))

;;; Queue

(defun opencode-forms--ready-p (form)
  "Return non-nil if FORM can be shown now."
  (let-alist form
    (or (equal .sessionID "global")
        (opencode--buffer-active-p (gethash .sessionID opencode-session-buffers)))))

(defun opencode-forms--show-next ()
  "Start answering the first form that's ready, once the user isn't in a prompt."
  (when-let* (((null opencode-forms--current))
              (form (seq-find #'opencode-forms--ready-p opencode-forms--queue)))
    (if (or (transient-active-prefix) (active-minibuffer-window))
        (run-at-time 1 nil #'opencode-forms--show-next)
      (setq opencode-forms--queue (delq form opencode-forms--queue)
            opencode-forms--current form
            opencode-forms--fields (alist-get 'fields form)
            opencode-forms--answers nil)
      (opencode-forms--next))))

(defun opencode-forms--prompt-next ()
  "Show the next form that's ready, if any.
This can be called from a process filter, where it's too early to prompt."
  (when opencode-forms--queue
    (run-at-time 0 nil #'opencode-forms--show-next)))

(defun opencode-forms--cancel-unsupported (form)
  "Tell the user FORM can't be displayed, and cancel it so the session won't hang."
  (let-alist form
    (opencode--toast-show `((title . "OpenCode Form")
                            (message . ,(format "Can't display form: %s" .title))
                            (variant . "warning")))
    (opencode-api-cancel-form-with-message
        (.sessionID .id (url-hexify-string "opencode.el can't display this form"))
        _response
      :else (lambda (error-msg)
              (opencode--log-event "WARNING FORM CANCEL" error-msg)))))

(defun opencode--form-request (form)
  "Queue FORM, a Form.Info of `default-directory', and show it when it's ready.
If it has a session without a buffer, open it so the form gets shown."
  (let-alist form
    (cond
     ((seq-some (lambda (known) (equal .id (alist-get 'id known)))
                (cons opencode-forms--current opencode-forms--queue)))
     ((not (seq-every-p (lambda (field)
                          (member (alist-get 'type field)
                                  opencode-forms--supported-types))
                        .fields))
      (opencode-forms--cancel-unsupported form))
     (t
      (setq opencode-forms--queue
            (append opencode-forms--queue
                    (list (cons (cons 'directory default-directory) form))))
      (if (opencode-forms--ready-p form)
          (opencode-forms--prompt-next)
        (opencode--toast-show
         `((title . ,(if (equal .metadata.kind "question") "OpenCode Question" .title))
           (message . ,(or (opencode-forms--label (car .fields)) .title))
           (variant . "info")))
        (unless (buffer-live-p (gethash .sessionID opencode-session-buffers))
          (opencode-api-session (.sessionID)
              session
            (push session opencode-alerted-sessions)
            (opencode-open-session session))))))))

(defun opencode--fetch-pending-forms (session-id)
  "Queue the pending forms of SESSION-ID and those not tied to a session.
Events are lost when not connected, so this is the only way to see them."
  (opencode-api-session-forms (session-id)
      forms
    (mapc #'opencode--form-request forms))
  (opencode-api-forms
    forms
    (dolist (form forms)
      (when (equal (alist-get 'sessionID form) "global")
        (opencode--form-request form)))))

(add-hook 'opencode-session-loaded-functions #'opencode--fetch-pending-forms)

(defun opencode--form-settled (form-id)
  "Forget FORM-ID, which was answered or cancelled, and close its menu if it's open.
If its minibuffer prompt is open that's left for the user to finish, and the
server ignores the answer."
  (setq opencode-forms--queue
        (seq-remove (lambda (form) (equal form-id (alist-get 'id form)))
                    opencode-forms--queue))
  (when (and (equal form-id (alist-get 'id opencode-forms--current))
             (transient-active-prefix 'opencode-forms--menu)
             (not (active-minibuffer-window)))
    (setq opencode-forms--current nil)
    (transient--emergency-exit)
    (opencode-forms--prompt-next)))

(provide 'opencode-forms)
;;; opencode-forms.el ends here
