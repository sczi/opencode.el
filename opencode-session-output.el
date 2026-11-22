;;; opencode-session-output.el --- Rendering of opencode session buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Display streamed and replayed session messages, tool calls and
;; their output in session buffers.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'comint)
(require 'hideshow)
(require 'markdown-ts-mode)
(require 'opencode-common)
(require 'opencode-format-tool-calls)
(require 'opencode-latex)

(defcustom opencode-shown-outputs '(shell-non-zero error edit patch question)
  "Outputs in session buffers to show unfolded when first displayed.
A list of symbols: `reasoning', a tool name like `shell' or `grep',
`shell-non-zero' to show shell tool output only if the command failed, or
`error' to show the error of any tool call that failed.
Shown outputs can still be folded manually with `hs-toggle-hiding'."
  :type '(repeat (choice (const reasoning)
                         (const shell-non-zero)
                         (const error)
                         (symbol :tag "Tool name")))
  :group 'opencode)

(defface opencode-request-margin-highlight
  '((t :inherit outline-1 :height reset))
  "OpenCode margin face to apply to user requests."
  :group 'opencode-faces)

(defface opencode-reasoning-margin-highlight
  '((t :inherit outline-2 :height reset))
  "OpenCode margin face to apply to reasoning blocks."
  :group 'opencode-faces)

(defface opencode-tool-margin-highlight
  '((t :inherit outline-5 :height reset))
  "OpenCode margin face to apply to tool call blocks."
  :group 'opencode-faces)

(defvar-local opencode-session-tokens 0
  "Tokens consumed by the current session.")

(defvar-local opencode-session-cost 0
  "Cost of the current session in dollars, as reported by the server.")

(defvar-local opencode-session-todos nil
  "The current session's todo list, as of its last todowrite tool call.
A list of alists with a content and a status.")

(defun opencode--parse-todos (todos)
  "Return the todos of TODOS, the todos input of a todowrite tool call.
Models sometimes send it as a JSON string.  Malformed todos are dropped."
  (let ((todos (if (stringp todos)
                   (ignore-errors (json-parse-string todos :object-type 'alist
                                                     :array-type 'list))
                 todos)))
    (cl-loop for todo in (and (listp todos) todos)
             for content = (and (consp todo) (alist-get 'content todo))
             when (and (stringp content) (not (string-empty-p content)))
             collect `((content . ,content)
                       (status . ,(let ((status (alist-get 'status todo)))
                                    (if (member status '("pending" "in_progress"
                                                         "completed" "cancelled"))
                                        status
                                      "pending")))))))

(defun opencode--record-todos (tool input)
  "Record the todo list of a completed call of TOOL with INPUT, if it's todowrite.
Return non-nil if it was."
  (when (equal tool "todowrite")
    (setq opencode-session-todos (opencode--parse-todos (alist-get 'todos input)))
    t))

(defvar-local opencode--table-scan-marker nil
  "Marker at the unprocessed Markdown in the current assistant text part.")

(defvar-local opencode--buttonize-marker nil
  "Marker at the text not yet buttonized in the current assistant text part.")

(defvar-local opencode--reasoning-start nil
  "Start of the streaming reasoning block, to fold once the next block starts.")

(defvar opencode--tool-calls
  (make-hash-table :test 'equal)
  "A mapping of `opencode--tool-key's to plists of in-progress tool calls.
They have :name and :input, as finishing events don't carry them, and
:marker at the end of the call if it's displayed, where its output goes.
Global so that file edit hooks also run for sessions with no buffer.")

(defvar-local opencode--current-block nil
  "(TYPE . KEY) of the block last output in this step, or nil.
TYPE is a symbol like text, reasoning or tool.  KEY identifies the block,
or is nil for blocks that no deltas continue.")

(defvar-local opencode--blocks-with-output nil
  "KEYs of the text and reasoning blocks of this step that have output.")

(defvar-local opencode--replayed-unfinished-messages nil
  "Ids of the messages still being generated when they were replayed.
Their blocks had output in the replay, without this client seeing any delta.")

(defvar-local opencode--prompt nil
  "(START . END) markers of the last prompt `opencode--show-prompt' showed.")

(defvar-local opencode--pending-input-overlay nil
  "Overlay separating input typed while busy from the output before it.
It also shows `opencode--queued-inputs'.")

(defvar opencode--inhibit-pending-input-update nil
  "Non-nil while `opencode--update-pending-input' shouldn't run.
Bound around buffer changes that update it afterwards themselves.")

(defvar-local opencode--queued-inputs-cache nil
  "(INPUTS . STRING): a copy of `opencode--queued-inputs' and their display.")

(cl-defstruct (opencode--queued (:constructor opencode--queued-create)
                                (:copier nil))
  "Input queued until the session's running work finishes."
  (id nil :documentation "The id of its inbox item, nil until it's enqueued.")
  (text nil :documentation "The input to show, which can have labels.")
  (sent nil :documentation "The text sent from this buffer, if it was.")
  (skills nil :documentation "The skills loaded with it."))

(defvar-local opencode--queued-inputs nil
  "The `opencode--queued' input of the session, oldest first.
It's shown pending, and inserted when it's delivered.")

(defvar-local opencode-last-response-start nil
  "Position of the start of the latest assistant text block.")

(cl-defstruct (opencode--turn (:constructor opencode--make-turn)
                              (:copier nil))
  "A turn of a session: its prompt, and the steps until it was idle again.
NUMBER counts the turns from 1.  START is a marker at its prompt, or its
output if it has none.  CREATED and ENDED are times in ms, and ENDED is
nil while it runs.  MESSAGES are the ids of its assistant messages,
newest first."
  number start created ended messages)

(defvar-local opencode--turns nil
  "The `opencode--turn's displayed in the buffer, newest first.")

(defvar-local opencode--messages nil
  "Hash table of the displayed assistant messages' ids to plists about them.
They have :turn, :agent, the Model.Ref :model, the times in ms :created,
:first-token, :streamed and :completed, and the TokenUsage.Info :tokens.")

(defvar-local opencode--output-message nil
  "Id of the assistant message whose output is being displayed, or nil.
The output is marked with it in the `opencode-message' text property.")

(defvar opencode--replaying-legacy nil
  "Non-nil while replaying messages with no idle markers, from before them.
A turn of those ends at the next prompt.")

(defvar opencode-code-block-languages-without-background '(diff)
  "Fenced code block languages whose background overlay face is removed.
Use for blocks whose own faces draw backgrounds, like diff line faces.")

(defun opencode--fontify-code-block (mode beg end)
  "Collect MODE faces for BEG..END without sharing session text properties."
  (let ((text (buffer-substring-no-properties beg end))
        faces)
    (with-temp-buffer
      (insert text)
      (delay-mode-hooks (funcall mode))
      (font-lock-ensure)
      (let ((pos (point-min)))
        (while (< pos (point-max))
          (let ((next (next-single-property-change pos 'face nil (point-max)))
                (face (get-text-property pos 'face)))
            (when face
              (push (list (+ beg (1- pos)) (+ beg (1- next)) face) faces))
            (setq pos next)))))
    (nreverse faces)))

(defun opencode--fontify-markdown (start end &optional loudly)
  "Fontify Markdown between START and END, optionally LOUDLY.
Isolate conventional code-block fontification from session properties."
  (let ((collect (symbol-function 'markdown-ts--fontify-non-ts-collect-faces)))
    (cl-letf (((symbol-function 'markdown-ts--fontify-non-ts-collect-faces)
               (lambda (mode beg finish)
                 (if (memq mode markdown-ts-code-block-force-conventional-modes)
                     (funcall collect mode beg finish)
                   (opencode--fontify-code-block mode beg finish)))))
      (treesit-font-lock-fontify-region start end loudly)))
  (opencode--render-latex start end)
  ;; Show all markup of the input being written.
  (when-let* ((process (get-buffer-process (current-buffer)))
              (input-start (max start (process-mark process)))
              ((< input-start end)))
    (with-silent-modifications
      (remove-list-of-text-properties input-start end '(invisible))))
  ;; An overlay's background would hide the backgrounds a block draws itself,
  ;; like diff line faces.  Fontifying just a fence line resets its block's
  ;; overlay, so look past it.
  (dolist (overlay (overlays-in (save-excursion (goto-char start) (pos-bol 0))
                                (save-excursion (goto-char end) (pos-bol 3))))
    (when (memq (overlay-get overlay 'markdown-ts-code-block-language)
                opencode-code-block-languages-without-background)
      (overlay-put overlay 'face nil))))

(defun opencode--fold-beginning ()
  "Move to the start of the foldable block containing point, if any.
Folds don't nest, so this is the nearest block start before point."
  (when-let* ((start (previous-single-property-change (1+ (point)) 'opencode-fold))
              (start (if (get-text-property start 'opencode-fold)
                         start
                       (previous-single-property-change start 'opencode-fold)))
              ((>= (get-text-property start 'opencode-fold) (point))))
    (goto-char start)))

(defun opencode--fold-line-count (beg end)
  "Return how many visible lines follow the first line of a fold.
The fold spans BEG, at the end of its first line, to END.  END already
excludes a final closing fence, so skip only the fences at the start: a
tool input's closing fence, if any, and the output's opening fence."
  (save-excursion
    (goto-char beg)
    (let ((start (if (looking-at "\n\\(?:[`~]\\{3,\\}[[:blank:]]*\n\\)?[`~]\\{3,\\}[^`\n]*\n?")
                     (match-end 0)
                   (1+ beg))))
      (count-lines (min start end) end))))

(defun opencode--fold-label (overlay)
  "Return the label for the fold OVERLAY, from its `display' ellipsis."
  (let ((ellipsis (overlay-get overlay 'display))
        (lines (opencode--fold-line-count (overlay-start overlay)
                                          (overlay-end overlay))))
    (concat (apply #'propertize
                   (format "%d %s" lines (if (= lines 1) "line" "lines"))
                   (text-properties-at 0 ellipsis))
            ellipsis)))

(defun opencode--fold (beg &optional end shown)
  "Make the output from BEG to END foldable, except its first line.
END defaults to the process marker.  Fold it now unless SHOWN is non-nil."
  (with-silent-modifications
    (put-text-property beg (1+ beg) 'opencode-fold
                       (copy-marker (opencode--output-content-end end))))
  (unless shown
    (save-excursion
      (goto-char beg)
      (hs-hide-block-at-point))))

(defun opencode--highlight-input (&optional _proc _string)
  "Highlight last prompt input."
  ;; Hide the sent input's markup, as in the rest of the output.
  (font-lock-flush comint-last-input-start comint-last-input-end)
  (opencode--add-margin comint-last-input-start
                        comint-last-input-end
                        'opencode-request-margin-highlight))

(defun opencode--block-spacing (pos)
  "Return the newlines needed before POS for a blank line before a block."
  (cond ((not (eq (char-before pos) ?\n)) "\n\n")
        ((not (eq (char-before (1- pos)) ?\n)) "\n")
        (t "")))

(defun opencode--maybe-insert-block-spacing ()
  "Output the newlines needed for a blank line before a block."
  (opencode--output (opencode--block-spacing (opencode--session-process-position))))

(defun opencode--output (string)
  "Output STRING as comint output.
It's marked as the output of `opencode--output-message', if any."
  (let ((start (and opencode--output-message (opencode--session-process-position))))
    ;; Updated once below, not on each change comint makes.
    (let ((opencode--inhibit-pending-input-update t))
      (comint-output-filter (get-buffer-process (current-buffer)) string))
    (when start
      (with-silent-modifications
        (put-text-property start (opencode--session-process-position)
                           'opencode-message opencode--output-message))))
  (opencode--update-pending-input))

(defun opencode--start-text-lines ()
  "Start processing the lines of an assistant text part at the process mark."
  (let ((pos (opencode--session-process-position)))
    (setq opencode--table-scan-marker (copy-marker pos)
          opencode--buttonize-marker (copy-marker pos))))

(defun opencode--buttonize-finished-lines (&optional finished)
  "Buttonize file references in the complete lines of the text part.
If FINISHED is non-nil, also in its last line, and stop processing it."
  (when opencode--buttonize-marker
    (let ((end (if finished
                   (opencode--session-process-position)
                 (save-excursion
                   (goto-char (opencode--session-process-position))
                   (let ((inhibit-field-text-motion t))
                     (line-beginning-position))))))
      (when (> end opencode--buttonize-marker)
        (opencode--buttonize-file-references opencode--buttonize-marker end)
        (set-marker opencode--buttonize-marker end))
      (when finished
        (set-marker opencode--buttonize-marker nil)
        (setq opencode--buttonize-marker nil)))))

(defun opencode--finish-text-lines (&optional finished)
  "Process the complete lines of the assistant text part.
If FINISHED is non-nil, the part has ended, so also its last line."
  (opencode--buttonize-finished-lines finished)
  (opencode--render-finished-tables finished))

(defconst opencode--table-min-column-width 6
  "Narrowest a rendered table column is shrunk to, to fit its window.")

(defvar-local opencode--table-width nil
  "Columns available to the rendered tables, when they were last rendered.")

(defun opencode--table-available-width ()
  "Return the columns available to tables, in the narrowest window showing them.
Without a window, use `fill-column'."
  (let ((widths (mapcar #'window-max-chars-per-line
                        (get-buffer-window-list nil nil t))))
    ;; The margin bar is outside the text area, but leave a column so a
    ;; line exactly as wide as the window isn't wrapped.
    (if widths (1- (apply #'min widths)) fill-column)))

(defconst opencode--table-button-properties
  '(button category action button-data follow-link help-echo mouse-face)
  "Properties of buttons that their text keeps in rendered tables.
`push-button' activates buttons in display strings clicked with the mouse.")

(defun opencode--table-cell-text (beg end)
  "Return the visible text of the table cell from BEG to END.
Keep its faces, and its buttons for mouse clicks."
  (let ((pos beg)
        parts)
    (while (< pos end)
      (let ((next (min (next-single-char-property-change pos 'invisible nil end)
                       (next-single-property-change pos 'face nil end)
                       (next-single-property-change pos 'button nil end))))
        (unless (invisible-p pos)
          (let ((text (propertize (buffer-substring-no-properties pos next)
                                  'face (get-text-property pos 'face))))
            (when (get-text-property pos 'button)
              (dolist (prop opencode--table-button-properties)
                (put-text-property 0 (length text) prop
                                   (get-text-property pos prop) text))
              ;; Only mouse events use the keymaps of display strings.
              (put-text-property 0 (length text) 'keymap button-map text))
            (push text parts)))
        (setq pos next)))
    ;; In tables, an escaped pipe is a pipe even inside a code span.
    (replace-regexp-in-string
     (rx "\\|") "|" (string-trim (apply #'concat (nreverse parts))) t t)))

(defun opencode--table-alignment (delimiter-cell)
  "Return the justification of DELIMITER-CELL: left, right or center."
  (let ((types (mapcar #'treesit-node-type (treesit-node-children delimiter-cell))))
    (cond ((not (member "pipe_table_align_right" types)) 'left)
          ((member "pipe_table_align_left" types) 'center)
          (t 'right))))

(defun opencode--table-rows (table)
  "Return the rows of the TABLE node, as (KIND BEG END CELLS).
KIND is header, delimiter or row.  CELLS are the alignments of the
delimiter row, and the cell texts of other rows."
  (mapcan
   (lambda (row)
     (let ((kind (pcase (treesit-node-type row)
                   ("pipe_table_header" 'header)
                   ("pipe_table_delimiter_row" 'delimiter)
                   ("pipe_table_row" 'row))))
       (when kind
         (list
          (list kind (treesit-node-start row) (treesit-node-end row)
                (mapcan
                 (lambda (cell)
                   (pcase (treesit-node-type cell)
                     ("pipe_table_delimiter_cell"
                      (list (opencode--table-alignment cell)))
                     ("pipe_table_cell"
                      (list (opencode--table-cell-text
                             (treesit-node-start cell) (treesit-node-end cell))))))
                 (treesit-node-children row)))))))
   (treesit-node-children table)))

(defun opencode--table-column-widths (natural available)
  "Return column widths fitting AVAILABLE columns of text, from NATURAL widths.
Cap the widest columns at the same width, but at no less than
`opencode--table-min-column-width'."
  (let ((cap (apply #'max natural)))
    (while (and (> cap opencode--table-min-column-width)
                (> (apply #'+ (mapcar (lambda (width) (min width cap)) natural))
                   available))
      (cl-decf cap))
    (mapcar (lambda (width) (min width cap)) natural)))

(defun opencode--table-wrap (string width)
  "Return the lines of STRING word-wrapped to WIDTH columns, keeping its faces.
Break words wider than WIDTH."
  (let (lines line)
    (dolist (word (split-string string "[ \t]+" t))
      (cond ((null line))
            ((<= (+ (string-width line) 1 (string-width word)) width)
             (setq word (concat line " " word)))
            (t (push line lines)))
      (while (> (string-width word) width)
        (let ((n (max 1 (length (truncate-string-to-width word width)))))
          (push (substring word 0 n) lines)
          (setq word (substring word n))))
      (setq line word))
    (nreverse (cons (or line "") lines))))

(defun opencode--table-pad (string width alignment)
  "Pad STRING to WIDTH columns, justified according to ALIGNMENT."
  (let* ((gap (max 0 (- width (string-width string))))
         (before (pcase alignment
                   ('right gap)
                   ('center (/ gap 2))
                   (_ 0))))
    (concat (make-string before ?\s) string (make-string (- gap before) ?\s))))

(defun opencode--table-border (widths left middle right)
  "Return a border line for columns of WIDTHS, joined by LEFT, MIDDLE and RIGHT."
  (propertize (concat left
                      (mapconcat (lambda (width) (make-string (+ width 2) ?─))
                                 widths middle)
                      right)
              'face 'shadow))

(defun opencode--table-row-lines (cells widths alignments)
  "Return the display lines of a row of CELLS, wrapped to column WIDTHS.
Justify cells by their column's ALIGNMENTS."
  (let* ((wrapped (cl-mapcar #'opencode--table-wrap cells widths))
         (height (apply #'max (mapcar #'length wrapped)))
         (bar (propertize "│" 'face 'shadow)))
    (mapcar (lambda (i)
              (concat bar " "
                      (mapconcat #'identity
                                 (cl-mapcar (lambda (lines width alignment)
                                              (opencode--table-pad
                                               (or (nth i lines) "") width alignment))
                                            wrapped widths alignments)
                                 (concat " " bar " "))
                      " " bar))
            (number-sequence 0 (1- height)))))

(defun opencode--table-at (pos)
  "Return the Markdown table node on the line at POS, or nil."
  (save-excursion
    (goto-char pos)
    (skip-chars-forward " \t")
    (treesit-parent-until (treesit-node-at (point) 'markdown)
                          "\\`pipe_table\\'" t)))

(defun opencode--render-table (table)
  "Display the Markdown TABLE node wrapped to fit its window, with box borders.
Each source row gets an overlay displaying it, so the text stays the
Markdown that was received."
  (setq opencode--table-width (opencode--table-available-width))
  (save-excursion
    (let ((start (treesit-node-start table)))
      (font-lock-ensure start (treesit-node-end table))
      ;; Fontifying widens the buffer, which reparses it if narrowed.
      (setq table (opencode--table-at start)))
    (let* ((rows (opencode--table-rows table))
           (alignments (nth 3 (assq 'delimiter rows)))
           (columns (length alignments))
           (rows (mapcar (lambda (row)
                           (if (eq (car row) 'delimiter)
                               row
                             (let ((cells (take columns (nth 3 row))))
                               (append (take 3 row)
                                       (list (append cells
                                                     (make-list (- columns (length cells))
                                                                "")))))))
                         rows))
           (natural (apply #'cl-mapcar
                           (lambda (&rest cells)
                             (max 1 (apply #'max (mapcar #'string-width cells))))
                           (mapcar (lambda (row) (nth 3 row))
                                   (seq-remove (lambda (row) (eq (car row) 'delimiter))
                                               rows))))
           (widths (opencode--table-column-widths
                    natural
                    ;; The borders, and the padding around each column.
                    (- (opencode--table-available-width) 1 (* 3 columns))))
           (middle (opencode--table-border widths "├" "┼" "┤"))
           (bottom (opencode--table-border widths "└" "┴" "┘"))
           (last (car (last rows))))
      (dolist (row rows)
        (pcase-let* ((`(,kind ,beg ,end ,cells) row)
                     (lines (unless (eq kind 'delimiter)
                              (opencode--table-row-lines cells widths alignments)))
                     (end-border (if (eq row last) bottom middle))
                     (overlay (make-overlay (progn (goto-char beg) (pos-bol))
                                            (progn (goto-char end) (pos-eol)))))
          ;; The header's overlay finds the table to render it again.
          (overlay-put overlay 'opencode-table kind)
          (overlay-put overlay 'evaporate t)
          (overlay-put overlay 'display
                       (string-join
                        (pcase kind
                          ('header (cons (opencode--table-border widths "┌" "┬" "┐")
                                         (if (eq row last)
                                             (append lines (list bottom))
                                           lines)))
                          ('delimiter (list end-border))
                          (_ (append lines (list end-border))))
                        "\n")))))))

(defun opencode--rerender-tables ()
  "Render all tables again, to fit the current width."
  (let (starts)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when-let* ((kind (overlay-get overlay 'opencode-table)))
        (when (eq kind 'header)
          (push (copy-marker (overlay-start overlay)) starts))
        (delete-overlay overlay)))
    (dolist (start starts)
      (when-let* ((node (opencode--table-at start)))
        (opencode--render-table node))
      (set-marker start nil))
    (setq opencode--table-width (opencode--table-available-width))))

(defun opencode--tables-window-size-change (window)
  "Render the tables of WINDOW's buffer again if their width changed."
  (with-current-buffer (window-buffer window)
    (unless (eql opencode--table-width (opencode--table-available-width))
      (opencode--rerender-tables))))

(defun opencode--mark-as-output (beg end)
  "Mark text from BEG to END, inserted outside `comint-output-filter', as output."
  (comint--mark-as-output beg end)
  (when comint-prompt-read-only
    (with-silent-modifications
      (put-text-property beg end 'read-only t))))

(defvar opencode--table-query nil
  "Compiled query for Markdown tables, compiled on first use.")

(defun opencode--next-table (beg end)
  "Return the first Markdown table ending after BEG and starting before END."
  ;; Querying the tree reparses it, which is costly in large sessions, and
  ;; there can't be a table without a pipe.
  (when (save-excursion
          (goto-char beg)
          (search-forward "|" end t))
    (seq-find (lambda (node) (> (treesit-node-end node) beg))
              ;; An uncompiled query is recompiled on every call.
              (treesit-query-capture
               'markdown
               (or opencode--table-query
                   (setq opencode--table-query
                         (treesit-query-compile
                          'markdown '((pipe_table) @table) t)))
               beg end t))))

(defun opencode--render-finished-tables (&optional finished)
  "Render tables whose end is confirmed by subsequent complete output lines.
If FINISHED is non-nil, also render a table at the end of the text part.
Retain one line of context for headers whose delimiter has yet to arrive."
  (when opencode--table-scan-marker
    (save-excursion
      (save-restriction
        ;; Pending comint input is not part of the assistant's Markdown.
        (narrow-to-region (point-min) (opencode--session-process-position))
        (let* ((inhibit-field-text-motion t)
               (limit (copy-marker (point-max)))
               (complete-end
                (copy-marker
                 (if finished
                     limit
                   (goto-char limit)
                   (line-beginning-position))))
               table waiting)
          (unwind-protect
              (progn
                (while (and (not waiting)
                            (setq table (opencode--next-table
                                         opencode--table-scan-marker
                                         complete-end)))
                  (let ((beg (treesit-node-start table))
                        (end (treesit-node-end table)))
                    (if (or finished
                            (< (save-excursion
                                 (goto-char end)
                                 (if (bolp) (point)
                                   (line-beginning-position 2)))
                               complete-end))
                        (progn
                          (opencode--render-table table)
                          (set-marker opencode--table-scan-marker end))
                      (set-marker opencode--table-scan-marker beg)
                      (setq waiting t))))
                (if finished
                    (progn
                      (set-marker opencode--table-scan-marker nil)
                      (setq opencode--table-scan-marker nil))
                  (unless waiting
                    (goto-char complete-end)
                    (forward-line -1)
                    (when (> (point) opencode--table-scan-marker)
                      (set-marker opencode--table-scan-marker (point))))))
            (set-marker limit nil)
            (set-marker complete-end nil)))))))

(defun opencode--logo-render (line face)
  "Render logo LINE using FACE, expanding the TUI's shadow markers.
`_' is a shadow-background space, `^' is `▀' on a shadow background,
`~' is a shadow `▀' and `,' is a shadow `▄'.  The shadow color is
FACE's foreground blended 25% into the default background.  It's
unspecified when the colors aren't known, like in batch mode."
  (let* ((fg (or (face-foreground face nil t)
                 (face-foreground 'default nil t)))
         (bg (face-background 'default nil t))
         (fg-rgb (and fg (color-name-to-rgb fg)))
         (bg-rgb (and bg (color-name-to-rgb bg)))
         (shadow (and fg-rgb bg-rgb
                      (apply #'color-rgb-to-hex
                             (append (color-blend fg-rgb bg-rgb 0.25) '(2)))))
         (weight (and (eq face 'bold) '(:weight bold))))
    (mapconcat
     (lambda (ch)
       (pcase ch
         (?_ (propertize " " 'font-lock-face `(:background ,shadow)))
         (?^ (propertize "▀" 'font-lock-face `(:foreground ,fg :background ,shadow ,@weight)))
         (?~ (propertize "▀" 'font-lock-face `(:foreground ,shadow)))
         (?, (propertize "▄" 'font-lock-face `(:foreground ,shadow)))
         (_ (propertize (string ch) 'font-lock-face face))))
     line "")))

(defun opencode-insert-logo ()
  "Insert the opencode logo."
  (let ((logo-left '("                   " "█▀▀█ █▀▀█ █▀▀█ █▀▀▄"
                     "█__█ █__█ █^^^ █__█" "▀▀▀▀ █▀▀▀ ▀▀▀▀ ▀~~▀"))
        (logo-right '("             ▄     " "█▀▀▀ █▀▀█ █▀▀█ █▀▀█"
                      "█___ █__█ █__█ █^^^" "▀▀▀▀ ▀▀▀▀ ▀▀▀▀ ▀▀▀▀")))
    (cl-loop for left in logo-left
             for right in logo-right
             do (opencode--output (opencode--logo-render left 'shadow))
             (opencode--output " ")
             (opencode--output (opencode--logo-render right 'bold))
             (opencode--output "\n"))
    (opencode--output "\n")))

(defun opencode--show-prompt ()
  "Show an invisible prompt with a margin after displaying output.
The prompt isn't Markdown markup.  It gives the margin's `line-prefix'
a character on an empty prompt line, and a field boundary before the
input, which commands acting on the input line, like evil's `dd', expect."
  (opencode--maybe-insert-block-spacing)
  (let ((start (opencode--session-process-position)))
    (opencode--output "❯ ")
    (overlay-put (make-overlay start (opencode--session-process-position))
                 'invisible t)
    (setq opencode--prompt (cons (copy-marker start)
                                 (copy-marker (opencode--session-process-position)))))
  (opencode--add-margin (car comint-last-prompt)
                        (cdr comint-last-prompt)
                        'opencode-request-margin-highlight)
  ;; queued input shows before the prompt
  (opencode--update-pending-input))

(defun opencode--prompt-showing-p ()
  "Return non-nil if the last prompt is showing, with no input sent since."
  (and opencode--prompt
       (marker-position (cdr opencode--prompt))
       (= (cdr opencode--prompt) (opencode--session-process-position))))

(defun opencode--retract-prompt ()
  "Delete the showing prompt, so output goes where it was.
Pending input after it stays, and a later prompt goes before it."
  (when (opencode--prompt-showing-p)
    (pcase-let ((`(,start . ,end) opencode--prompt)
                (inhibit-read-only t))
      (dolist (ov (overlays-in start end))
        (when (and (>= (overlay-start ov) start)
                   (<= (overlay-end ov) end))
          (delete-overlay ov)))
      (delete-region start end)
      (set-marker (process-mark (get-buffer-process (current-buffer))) start)
      (set-marker start nil)
      (set-marker end nil)
      (setq opencode--prompt nil
            comint-last-prompt nil)
      (opencode--update-pending-input))))

(defun opencode--queued-inputs-string ()
  "Return the display of `opencode--queued-inputs', a line for each.
It's cached, as it's called on each output."
  (cond
   ((not opencode--queued-inputs) "")
   ((equal (car opencode--queued-inputs-cache) opencode--queued-inputs)
    (cdr opencode--queued-inputs-cache))
   (t
    (let* ((margin (opencode--margin-string 'opencode-request-margin-highlight))
           (string
            (mapconcat (lambda (queued)
                         (let ((text (copy-sequence (opencode--queued-text queued))))
                           (add-face-text-property 0 (length text) 'shadow t text)
                           (concat margin
                                   (string-replace "\n" (concat "\n" margin) text)
                                   (propertize "  (queued)" 'face '(shadow italic))
                                   "\n")))
                       opencode--queued-inputs)))
      ;; Copy the structs too, as they're modified in place.
      (setq opencode--queued-inputs-cache
            (cons (mapcar #'copy-sequence opencode--queued-inputs) string))
      string))))

(defun opencode--update-pending-input (&rest _)
  "Separate input typed while busy from the output before it.
Display a blank line and the request margin before the pending input.
`opencode--queued-inputs' are shown before it, or before the prompt if
one is showing."
  (unless opencode--inhibit-pending-input-update
    (opencode--update-pending-input-1)))

(defun opencode--update-pending-input-1 ()
  "Do the work of `opencode--update-pending-input'."
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (and proc (marker-position (process-mark proc))))
         (busy (equal opencode-session-status "busy"))
         (start (cond ((not pmark) nil)
                      ((and opencode--queued-inputs (opencode--prompt-showing-p))
                       ;; The newline of the blank line before the invisible
                       ;; prompt, which would hide a string before it.
                       (let ((prompt (car opencode--prompt)))
                         (if (eq (char-before prompt) ?\n) (1- prompt) prompt)))
                      ((and busy (or opencode--queued-inputs (< pmark (point-max))))
                       pmark))))
    (if start
        (progn
          (if opencode--pending-input-overlay
              (move-overlay opencode--pending-input-overlay start (point-max))
            (setq opencode--pending-input-overlay
                  (make-overlay start (point-max) nil t t)))
          (let ((string
                 (let ((queued (opencode--queued-inputs-string)))
                   (if (/= start pmark)
                       ;; before the blank line, and the prompt has
                       ;; its own margin
                       (concat "\n" queued)
                     (concat (opencode--block-spacing pmark)
                             (and opencode--queued-inputs (concat queued "\n"))
                             (opencode--margin-string
                              'opencode-request-margin-highlight))))))
            ;; overlay-put with a non-eq value forces redisplay`
            (unless (equal-including-properties
                     string (overlay-get opencode--pending-input-overlay
                                         'before-string))
              (overlay-put opencode--pending-input-overlay
                           'before-string string))))
      (when opencode--pending-input-overlay
        (delete-overlay opencode--pending-input-overlay)
        (setq opencode--pending-input-overlay nil)))))

(defun opencode-session--finish-block ()
  "Finish the last block: process its last text lines and fold its reasoning.
Every block ends with a newline, as comint makes an unfinished last
line a prompt."
  (opencode--finish-text-lines t)
  (unless (eq (char-before (opencode--session-process-position)) ?\n)
    (opencode--output "\n"))
  (when opencode--reasoning-start
    (opencode--fold opencode--reasoning-start nil
                    (memq 'reasoning opencode-shown-outputs))
    (setq opencode--reasoning-start nil)))

(defun opencode-session--start-block (type &optional key)
  "Start the block of TYPE identified by KEY, finishing the last one.
Every block is preceded by a blank line.  KEY is only needed for blocks
that later deltas continue."
  (let ((last-type (car opencode--current-block)))
    (opencode-session--finish-block)
    (opencode--maybe-insert-block-spacing)
    (when (and (eq type 'text)
               (not (eq last-type 'text)))
      (setf opencode-last-response-start
            (opencode--session-process-position)))
    (when (eq type 'reasoning)
      (setq opencode--reasoning-start
            (opencode--session-process-position)))
    (when (eq type 'text)
      (opencode--start-text-lines))
    (setq opencode--current-block (cons type key))))

(defun opencode-session--block-delta (session-id key delta)
  "Output DELTA of the text or reasoning block KEY of SESSION-ID.
KEY is (TYPE MESSAGE-ID ORDINAL)."
  (opencode--with-session-buffer session-id
    (let* ((type (car key))
           (new-block-p (not (equal key (cdr opencode--current-block))))
           ;; don't display margins on extra whitespace
           (delta (if new-block-p (string-trim-left delta) delta)))
      ;; blocks start once they have visible content
      (unless (string-empty-p delta)
        (when new-block-p
          (opencode-session--start-block type key))
        (cl-pushnew key opencode--blocks-with-output :test #'equal)
        (if (eq type 'reasoning)
            (opencode--insert-reasoning-block delta)
          (opencode--output delta))
        (when (and (eq type 'text) (string-search "\n" delta))
          (opencode--finish-text-lines nil))))))

(defun opencode-session--block-ended (session-id key text)
  "Finish the text or reasoning block KEY of SESSION-ID with final TEXT.
TEXT is only output if the block had no delta, as some providers only
send the whole block once it's done."
  (opencode--with-session-buffer session-id
    (when (and text
               (not (member key opencode--blocks-with-output))
               (not (member (cadr key) opencode--replayed-unfinished-messages)))
      (opencode-session--block-delta session-id key text))
    (when (and (eq (car key) 'text)
               (equal key (cdr opencode--current-block)))
      (opencode--finish-text-lines t))))

(defun opencode--tokens-total (tokens)
  "Return the sum of the fields of TOKENS, a TokenUsage.Info."
  (let-alist tokens
    (+ .input .output .reasoning .cache.read .cache.write)))

(defun opencode--format-tokens (n)
  "Return token count N in compact form, like 3.2k."
  (file-size-human-readable (or n 0) 'si))

(defun opencode--tool-key (message-id id)
  "Return the key of tool call ID, as they are only unique within MESSAGE-ID."
  (concat message-id "/" id))

(defun opencode-session--display-tool-call (key)
  "Start a block for the tool call KEY, unless it has one."
  (let* ((call (gethash key opencode--tool-calls))
         (name (or (plist-get call :name) "tool")))
    (unless (plist-get call :marker)
      (opencode-session--start-block 'tool key)
      (puthash key
               (plist-put call :marker
                          (opencode--insert-tool-block name (plist-get call :input)))
               opencode--tool-calls))))

(defun opencode-session--tool-finished (session-id key state)
  "Display the result of the tool call KEY of SESSION-ID, finished with STATE.
Return the plist of the call, from `opencode--tool-calls'."
  (let* ((call (gethash key opencode--tool-calls))
         (name (or (plist-get call :name) "tool")))
    (opencode--with-session-buffer session-id
      ;; A call with malformed input fails without ever being called.
      (opencode-session--display-tool-call key)
      (opencode--maybe-insert-tool-output
       name state (plist-get (gethash key opencode--tool-calls) :marker)))
    (remhash key opencode--tool-calls)
    call))

(defun opencode--insert-shell-output (output failed)
  "Insert OUTPUT of a user's shell command, styled as an error if FAILED."
  (unless (string-empty-p output)
    (opencode-session--start-block 'shell)
    (let ((start (opencode--session-process-position)))
      (opencode--insert-block-with-margin
       (opencode--fenced-block output "text")
       (if failed 'error 'opencode-tool-margin-highlight))
      (when failed
        (opencode--add-error-face (save-excursion
                                    (goto-char start)
                                    (forward-line 1)
                                    (point))
                                  (opencode--output-content-end)))
      (opencode--output "\n"))))

(defun opencode--insert-compaction-started ()
  "Insert the status line for a compaction starting."
  (opencode-session--start-block 'compaction)
  (opencode--output (propertize "🗜 Compacting session...\n" 'font-lock-face 'shadow)))

(defun opencode--insert-compaction-summary (summary)
  "Insert the SUMMARY of a finished compaction, folded."
  (opencode-session--start-block 'compaction-summary)
  (let ((start (opencode--session-process-position)))
    (opencode--output (concat "🗜 Compaction summary\n" (string-trim summary) "\n"))
    (opencode--fold start))
  (setq opencode-session-tokens 0)
  (force-mode-line-update))

(defun opencode--insert-skills (skills)
  "Insert the SKILLS loaded with a prompt, each with its text folded."
  (dolist (skill skills)
    (let-alist skill
      (opencode-session--start-block 'skill .name)
      (let ((start (opencode--session-process-position)))
        (opencode--output (format "→ Skill \"%s\"\n%s\n" .name (string-trim .text)))
        (opencode--fold start)))))

;;; Turns and the messages of their steps

(defun opencode--reset-turns ()
  "Forget the buffer's turns and messages, for displaying it anew."
  (dolist (turn opencode--turns)
    (set-marker (opencode--turn-start turn) nil))
  (setq opencode--turns nil
        opencode--messages nil
        opencode--output-message nil))

(defun opencode--forget-turns-from (pos)
  "Forget the turns starting at or after POS, and the messages of their steps.
As when their text is deleted by a revert."
  (while (and opencode--turns
              (>= (opencode--turn-start (car opencode--turns)) pos))
    (let ((turn (pop opencode--turns)))
      (dolist (id (opencode--turn-messages turn))
        (when (equal id opencode--output-message)
          (setq opencode--output-message nil))
        (when opencode--messages
          (remhash id opencode--messages)))
      (set-marker (opencode--turn-start turn) nil))))

(defun opencode--message-info (id)
  "Return the plist about the displayed assistant message ID, or nil."
  (and opencode--messages (gethash id opencode--messages)))

(defun opencode--update-message (id &rest properties)
  "Set PROPERTIES of the plist about the assistant message ID."
  (unless opencode--messages
    (setq opencode--messages (make-hash-table :test 'equal)))
  (let ((info (gethash id opencode--messages)))
    (while properties
      (setq info (plist-put info (pop properties) (pop properties))))
    (puthash id info opencode--messages)))

(defun opencode--running-turn ()
  "Return the turn still running, or nil."
  (when-let* ((turn (car opencode--turns)))
    (unless (opencode--turn-ended turn)
      turn)))

(defun opencode--end-turn (&optional ended)
  "End the running turn, at ENDED in ms.
ENDED defaults to when its last step completed."
  (when-let* ((turn (opencode--running-turn)))
    (setf (opencode--turn-ended turn)
          (or ended
              (plist-get (opencode--message-info (car (opencode--turn-messages turn)))
                         :completed)
              (opencode--turn-created turn)))))

(defun opencode--start-turn (created)
  "Start a turn at CREATED in ms, ending the running one.
It starts at the last input, unless that's of an earlier turn, as when
the session is woken by a background job finishing."
  (opencode--end-turn)
  (let* ((previous (car opencode--turns))
         (pmark (opencode--session-process-position))
         (input (and (markerp comint-last-input-start)
                     (marker-position comint-last-input-start)))
         (start (if (and input (<= input pmark)
                         (or (not previous) (> input (opencode--turn-start previous))))
                    input
                  pmark)))
    (push (opencode--make-turn :number (1+ (length opencode--turns))
                               :start (copy-marker start)
                               :created created)
          opencode--turns)))

(defun opencode--add-message (id agent model created)
  "Record the step of assistant message ID, by AGENT with MODEL from CREATED.
It's a step of the running turn, or a new one if none is.  A step that's
retried starts anew, forgetting when it was streamed."
  (unless (opencode--running-turn)
    (opencode--start-turn created))
  (let ((turn (car opencode--turns)))
    (unless (member id (opencode--turn-messages turn))
      (push id (opencode--turn-messages turn)))
    (opencode--update-message id :turn turn :agent agent :model model :created created
                              :first-token nil :streamed nil :completed nil)))

(defun opencode--now-ms ()
  "Return the current time in ms."
  (* 1000 (float-time)))

(defun opencode--server-now-ms ()
  "Return the current time in ms on the clock of the current connection's server.
That's the clock of the times it records, like of turns."
  (let ((connection (opencode-current-connection)))
    (- (opencode--now-ms)
       (if connection (opencode-connection-clock-offset connection) 0))))

(defun opencode--turn-prompt-start (turn)
  "Return where the line of TURN's prompt starts.
TURN starts after its prompt, which can be invisible, and point moved
onto it is only moved past it after `post-command-hook'."
  (save-excursion
    (goto-char (opencode--turn-start turn))
    (let ((inhibit-field-text-motion t))
      (line-beginning-position))))

(defun opencode--turn-at (pos)
  "Return the turn displayed at POS, or the first one if POS is before them.
The prompt of a turn is of it."
  (or (seq-find (lambda (turn) (<= (opencode--turn-prompt-start turn) pos))
                opencode--turns)
      (car (last opencode--turns))))

(defun opencode--marked-message-before (pos limit)
  "Return the message the output at or before POS is of, back to LIMIT."
  (or (and (< pos (point-max))
           (get-text-property pos 'opencode-message))
      (let (found)
        (while (and (not found) (> pos limit))
          (unless (setq found (get-text-property (1- pos) 'opencode-message))
            (setq pos (previous-single-property-change pos 'opencode-message nil limit))))
        found)))

(defun opencode--message-at (pos)
  "Return (TURN . MESSAGE-ID) of the step displayed at POS, or nil.
Output not of a step, like a tool's, is of the step before it.  The
prompt of a turn, and anywhere before the first turn, is of the turn's
first step.  MESSAGE-ID is nil for a turn with no steps yet."
  (when-let* ((turn (opencode--turn-at pos)))
    (let ((start (opencode--turn-start turn)))
      (cons turn
            (or (and (>= pos start)
                     (opencode--marked-message-before pos start))
                (car (last (opencode--turn-messages turn))))))))

(defun opencode--margin-string (face)
  "Return a string displaying the margin bar with FACE in the left margin."
  (propertize " " 'display `((margin left-margin) ,(propertize "▎" 'face face))))

(defun opencode--add-margin (start end face)
  "Display margin from START (inclusive) to END (exclusive) with FACE."
  ;; a reversed range would highlight the lines before START
  (when (> end start)
    (let ((ov (make-overlay start (1- end)))
          (margin (opencode--margin-string face)))
      (overlay-put ov 'line-prefix margin)
      (overlay-put ov 'wrap-prefix margin))))

(defun opencode--mark-input (start end id)
  "Mark the sent input from START to END as that of the user message ID.
ID is `shell' for a shell command, which isn't a user message."
  (with-silent-modifications
    (put-text-property start end 'opencode-user-message id)))

(defun opencode--replay-input (text &optional id)
  "Replay a prompt with input TEXT, of the user message ID.
ID is `shell' for a shell command."
  (opencode--show-prompt)
  (insert text)
  (let ((comint-input-sender #'opencode--highlight-input))
    (comint-send-input))
  (opencode--mark-input comint-last-input-start comint-last-input-end id)
  (opencode--output "\n"))

(defmacro opencode--with-pending-input-aside (&rest body)
  "Run BODY at the end of the buffer with the pending input taken out.
The pending input is put back after what BODY inserts, with point where
it was in it.  Taking it out and back isn't undoable, as output going
before it isn't."
  (declare (indent 0) (debug t))
  (let ((pmark (make-symbol "pmark"))
        (pending (make-symbol "pending"))
        (offset (make-symbol "offset")))
    `(let* ((,pmark (process-mark (get-buffer-process (current-buffer))))
            (,pending (buffer-substring ,pmark (point-max)))
            (,offset (and (>= (point) ,pmark) (- (point) ,pmark)))
            (inhibit-read-only t))
       (let ((buffer-undo-list t))
         (delete-region ,pmark (point-max)))
       (unwind-protect
           (save-excursion
             (goto-char (point-max))
             ,@body)
         (let ((buffer-undo-list t))
           (save-excursion
             (goto-char (point-max))
             (insert ,pending)))
         (when ,offset
           (goto-char (+ ,pmark ,offset)))
         (opencode--update-pending-input)))))

(defun opencode--insert-queued-input (queued)
  "Insert the input QUEUED, an element of `opencode--queued-inputs'.
It's inserted as if just sent, before any pending input, as its user
message was just delivered to the running turn."
  (setq opencode--queued-inputs (delq queued opencode--queued-inputs))
  (opencode--with-pending-input-aside
    ;; it's in the history since it was first sent
    (let ((comint-input-filter #'ignore))
      (opencode--replay-input (opencode--queued-text queued)
                              (opencode--queued-id queued)))
    (opencode--insert-skills (opencode--queued-skills queued))))

(defun opencode--replay-user-request (message)
  "Replay a user request MESSAGE.
It starts a turn, unless it was queued while one ran."
  (opencode--replay-input (alist-get 'text message) (alist-get 'id message))
  (when (or opencode--replaying-legacy (not (opencode--running-turn)))
    (opencode--start-turn (map-nested-elt message '(time created))))
  (opencode--insert-skills (alist-get 'skills message)))

(defun opencode--insert-block-with-margin (text face)
  "Insert TEXT with FACE margin highlight."
  (unless (string-empty-p text)
    (let ((beginning (opencode--session-process-position)))
      (opencode--output text)
      (opencode--add-margin beginning (1+ (opencode--output-content-end)) face))))

(defun opencode--output-content-end (&optional pos)
  "Return the end of the last visible line of output before POS.
POS defaults to the process marker.  Markdown hides closing fence lines
along with their newline, so they are excluded, or they would show as an
extra margin line, and the line after a fold would join its first line."
  (save-excursion
    (goto-char (or pos (opencode--session-process-position)))
    (skip-chars-backward "\r\n[:blank:]")
    (let ((end (point))
          (inhibit-field-text-motion t))
      (beginning-of-line)
      (if (looking-at-p "[`~]\\{3,\\}[[:blank:]]*$")
          (1- (point))
        end))))

(defun opencode--insert-reasoning-block (text)
  "Insert TEXT as reasoning block."
  (opencode--insert-block-with-margin text 'opencode-reasoning-margin-highlight))

(defun opencode--session-process-position ()
  "Return position of process marker."
  (marker-position
   (process-mark
    (get-buffer-process
     (current-buffer)))))

(defun opencode--refine-diff-hunks (start)
  "Refine and hide diff hunk headers from START to the process marker."
  (let ((inhibit-read-only t)
        (inhibit-field-text-motion t))
    (save-excursion
      (save-restriction
        (narrow-to-region start (opencode--session-process-position))
        (goto-char (point-min))
        (while (re-search-forward diff-hunk-header-re nil t)
          (let* ((beg (match-beginning 0))
                 (header-end (line-beginning-position 2))
                 (end (progn
                        (goto-char beg)
                        ;; patch tool headers have synthetic line counts.
                        (diff-end-of-hunk nil t)
                        (point))))
            (when diff-refine
              (diff--refine-hunk beg end t))
            ;; Font-lock can clear invisible text properties on refontification.
            (let ((overlay (make-overlay beg header-end)))
              (overlay-put overlay 'invisible t)
              (overlay-put overlay 'evaporate t))
            (goto-char (max header-end end))))))))

(defun opencode--add-shell-prompt (start)
  "Display a `$ ' prompt before the shell command in the block at START."
  (save-excursion
    (goto-char start)
    ;; Skip the hidden opening fence line
    (forward-line 1)
    (let ((overlay (make-overlay (point) (1+ (point)))))
      (overlay-put overlay 'before-string
                   (propertize "$ " 'face 'comint-highlight-prompt))
      (overlay-put overlay 'evaporate t))))

(defun opencode--insert-tool-block (tool input)
  "Insert TOOL call with INPUT as margin-highlighted block.
Return a marker at the end of the call, where its output goes."
  (let ((start (opencode--session-process-position))
        (call (opencode--format-tool-call tool input)))
    ;; The call ends with a newline, so its end is before the process mark.
    (opencode--insert-block-with-margin
     (if (string-suffix-p "\n" call) call (concat call "\n"))
     'opencode-tool-margin-highlight)
    (opencode--buttonize-file-references
     start (opencode--session-process-position))
    (prog1 (save-excursion
             (goto-char (opencode--session-process-position))
             (skip-chars-backward "\n")
             (point-marker))
      (when (and (equal tool "shell") (alist-get 'command input))
        (opencode--add-shell-prompt start))
      (when (member tool '("edit" "patch"))
        (opencode--refine-diff-hunks start)
        (opencode--fold start nil (memq (intern tool) opencode-shown-outputs))))))

(defun opencode--shell-failed-p (tool info)
  "Return non-nil if INFO is of a failed shell TOOL.
INFO has an optional status and exit, like the metadata of a tool call."
  (let-alist info
    (and (string= tool "shell")
         (or (member .status '("timeout" "killed"))
             (and (numberp .exit) (/= .exit 0))))))

(defun opencode--tool-output-shown-p (tool state)
  "Return non-nil if the output of the TOOL call with STATE starts unfolded."
  (let-alist state
    (or (memq (intern tool) opencode-shown-outputs)
        (and (string= .status "error")
             (memq 'error opencode-shown-outputs))
        (and (memq 'shell-non-zero opencode-shown-outputs)
             (opencode--shell-failed-p tool .metadata)))))

(defconst opencode--ignore-output-for-tools '("edit" "write" "patch" "todowrite")
  "Don't show the output for these tools.")

(defun opencode--format-question-answers (answers)
  "Return ANSWERS of a question tool call, one line per question."
  (mapconcat (lambda (answer)
               (concat "→ " (if (seq-empty-p answer)
                                "Unanswered"
                              (string-join answer ", "))))
             answers "\n"))

(defun opencode--tool-content-image (item)
  "Return the image of tool output content ITEM to display, or nil.
It's displayed as its name, with the image as its `display'."
  (let-alist item
    (when (and (equal .type "file")
               (string-prefix-p "image/" .mime)
               .uri
               (string-match "\\`data:[^,]*;base64," .uri))
      (when-let* ((image (opencode--inline-image
                          (ignore-errors
                            (base64-decode-string (substring .uri (match-end 0))))
                          t)))
        (propertize (or .name "image") 'display image)))))

(defun opencode--tool-content-text (tool content)
  "Return the output CONTENT of TOOL joined, or nil if there's none.
That's its text and images.  As a read's text only says that an image
was read, it's omitted when there's an image."
  (let* ((items (cl-loop for item in content
                         if (equal "text" (alist-get 'type item))
                         collect (alist-get 'text item)
                         else if (opencode--tool-content-image item)
                         collect it))
         (items (if (and (equal tool "read")
                         (seq-some (lambda (item) (get-text-property 0 'display item))
                                   items))
                    (seq-filter (lambda (item) (get-text-property 0 'display item))
                                items)
                  items)))
    (when items
      (string-join items "\n"))))

(defun opencode--add-error-face (beg end)
  "Style BEG to END with the `error' face."
  ;; Font-lock can clear text properties on refontification.
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'face 'error)
    (overlay-put overlay 'evaporate t)))

(defun opencode--maybe-insert-tool-output (tool state call-end)
  "Insert the output or error of the finished TOOL call with STATE at CALL-END.
Parallel tool calls can complete after later output, so it's inserted
after its call, foldable onto the call's last line, and folded unless
`opencode--tool-output-shown-p'.  Edits show only their diff, unless
they failed.  Errors and the output of shell tool calls that exited with a
non-zero status are highlighted with the `error' face."
  (let-alist state
    (when (and (member .status '("completed" "error"))
               call-end
               (marker-position call-end))
      (let* ((failed (string= .status "error"))
             (output (cond
                      (failed .error.message)
                      ((and (string= tool "question") .metadata.answers)
                       (opencode--format-question-answers .metadata.answers))
                      ((not (member tool opencode--ignore-output-for-tools))
                       (opencode--tool-content-text tool .content))))
             (errorp (or failed (opencode--shell-failed-p tool .metadata))))
        (when output
          (let ((inhibit-read-only t)
                (end (copy-marker call-end t)))
            (save-excursion
              (goto-char call-end)
              (insert "\n" (if failed
                               (opencode--fenced-block output "text")
                             (opencode--fenced-output output))))
            (opencode--mark-as-output call-end end)
            ;; The first visible output line starts at the call's hidden
            ;; closing fence, so that's where its margin is read from.
            (opencode--add-margin (1+ (opencode--output-content-end call-end))
                                  (1+ (opencode--output-content-end end))
                                  (if errorp 'error 'opencode-tool-margin-highlight))
            (when errorp
              (opencode--add-error-face (save-excursion
                                          (goto-char call-end)
                                          (forward-line 2)
                                          (point))
                                        (opencode--output-content-end end)))
            (opencode--fold (save-excursion
                              (goto-char (opencode--output-content-end call-end))
                              (let ((inhibit-field-text-motion t))
                                (line-beginning-position)))
                            end
                            (opencode--tool-output-shown-p tool state))
            (set-marker end nil))))
      (set-marker call-end nil))))

(defconst opencode--migrated-tool-names
  '(("bash" . "shell") ("apply_patch" . "patch") ("task" . "subagent"))
  "Old names of tools, that sessions migrated from v1 still have.")

(defconst opencode--migrated-tool-inputs
  '((filePath . path) (subagent_type . agent))
  "Old names of tool inputs, that sessions migrated from v1 still have.")

(defun opencode--migrate-tool-input (input)
  "Return tool INPUT with the current names of its arguments."
  (if (listp input)
      (mapcar (lambda (pair)
                (cons (alist-get (car pair) opencode--migrated-tool-inputs (car pair))
                      (cdr pair)))
              input)
    input))

(defun opencode--replay-tool (message-id tool)
  "Display the TOOL content item of the assistant message MESSAGE-ID.
Calls not yet finished are recorded so that later events finish them."
  (let-alist tool
    (let ((key (opencode--tool-key message-id .id))
          (name (or (cdr (assoc .name opencode--migrated-tool-names)) .name))
          (input (opencode--migrate-tool-input .state.input)))
      (pcase .state.status
        ;; its input is still incomplete, so it's displayed once called
        ("streaming" (puthash key (list :name name) opencode--tool-calls))
        ("running"
         (puthash key (list :name name :input input) opencode--tool-calls)
         (opencode-session--display-tool-call key))
        (_ (opencode-session--start-block 'tool key)
           (when (equal .state.status "completed")
             (opencode--record-todos name input))
           (opencode--maybe-insert-tool-output
            name .state (opencode--insert-tool-block name input)))))))

(defun opencode--replay-assistant-message (message)
  "Display the assistant MESSAGE, as its blocks are when streamed."
  (let-alist message
    (unless .time.completed
      (push .id opencode--replayed-unfinished-messages))
    (opencode--add-message .id .agent .model .time.created)
    (opencode--update-message .id :first-token .time.firstToken :streamed .time.streamed
                              :completed .time.completed :tokens .tokens)
    ;; like a new step, it starts a new response
    (setq opencode--current-block nil)
    (setq opencode--output-message .id)
    (unwind-protect
        (seq-do-indexed
         (lambda (item index)
           (let ((type (alist-get 'type item))
                 (text (string-trim (or (alist-get 'text item) ""))))
             (cond
              ((string= type "tool") (opencode--replay-tool .id item))
              ((and (member type '("text" "reasoning"))
                    (not (string-empty-p text)))
               (let ((type (intern type)))
                 (opencode-session--start-block type (list type .id index))
                 (if (eq type 'reasoning)
                     (opencode--insert-reasoning-block text)
                   (opencode--output text)))))))
         .content)
      (setq opencode--output-message nil))
    (when .tokens
      (setq opencode-session-tokens (opencode--tokens-total .tokens)))))

(defun opencode--replay-message (message)
  "Display the past session MESSAGE."
  (let-alist message
    (pcase .type
      ("user" (opencode--replay-user-request message))
      ("assistant" (opencode--replay-assistant-message message))
      ("shell"
       (opencode--replay-input (concat "!" .command) 'shell)
       (opencode--insert-shell-output (or .output.output "")
                                      (opencode--shell-failed-p "shell" message)))
      ("compaction"
       (opencode--insert-compaction-started)
       (pcase .status
         ("completed" (opencode--insert-compaction-summary .summary))
         ("failed" (opencode--insert-error .error.message))))
      ("idle"
       (opencode--end-turn .time.created)
       (pcase .outcome
         ("failed" (opencode--insert-error (or .error.message "Session failed")))
         ("interrupted" (opencode--insert-error "Interrupted")))))))

(defun opencode--replay-session-messages (messages &optional revert-message-id)
  "Display past session MESSAGES.
Those from REVERT-MESSAGE-ID on have a staged revert, so are hidden."
  (let ((inhibit-redisplay t)
        ;; collect once afterwards, rather than many times during the replay
        (gc-cons-threshold most-positive-fixnum)
        (comint-output-filter-functions
         (remove #'comint-postoutput-scroll-to-bottom
                 comint-output-filter-functions))
        (opencode--replaying-legacy
         (not (seq-some (lambda (message) (equal "idle" (alist-get 'type message)))
                        messages))))
    ;; Run the change hooks, like jit-lock's, once for the whole replay
    ;; rather than for every insertion.
    (combine-change-calls (point-min) (point-max)
      ;; Replayed history isn't undoable.  Recording it would be wasted
      ;; work, and anything resetting the undo list meanwhile, like Doom's
      ;; `comint-output-filter' advice, makes `combine-change-calls' warn
      ;; that it was truncated.
      (let ((buffer-undo-list t))
        (dolist (message messages)
          (unless (and revert-message-id
                       (not (string< (alist-get 'id message) revert-message-id)))
            (opencode--replay-message message)))
        (opencode-session--finish-block)))
    (setq opencode--current-block nil)))

(defun opencode--insert-error (message)
  "Insert error MESSAGE in the current session buffer."
  (opencode-session--start-block 'error)
  (let ((start (opencode--session-process-position)))
    (opencode--output message)
    (opencode--add-error-face start (opencode--session-process-position)))
  (opencode--output "\n"))

(defun opencode-session--display-error (session-id message)
  "Display error MESSAGE in SESSION-ID."
  (opencode--with-session-buffer session-id
    (opencode--insert-error message)))

(provide 'opencode-session-output)
;;; opencode-session-output.el ends here
