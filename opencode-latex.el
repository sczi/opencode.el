;;; opencode-latex.el --- Render LaTeX math in session buffers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Display $...$ and $$...$$ math in session buffers as SVG images,
;; typeset by latex and converted by dvisvgm in the background.  The
;; TeX stays in the buffer, the image is only its `display'.

;;; Code:

(require 'cl-lib)
(require 'treesit)
(require 'opencode-common)

(defcustom opencode-render-latex t
  "If non-nil, display LaTeX math in session buffers as images.
This needs the programs latex and dvisvgm, and a graphical frame."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-latex-preamble
  "\\documentclass{article}\n\\usepackage{amsmath,amssymb,bm}"
  "LaTeX preamble used to typeset math, up to \\begin{document}."
  :type 'string
  :group 'opencode)

(defcustom opencode-latex-scale 1.0
  "Size of rendered math relative to the buffer's text."
  :type 'number
  :group 'opencode)

(defvar opencode--latex-cache (make-hash-table :test 'equal)
  "Map from (TEX . DISPLAY-P) to its rendering.
A plist with :svg, the image data, :height and :depth in TeX points,
or `pending' while it's rendered, or `error' if it couldn't be.")

(defvar opencode--latex-queue nil
  "Keys of `opencode--latex-cache' waiting to be rendered, oldest last.")

(defvar opencode--latex-waiting (make-hash-table :test 'equal)
  "Map from a key of `opencode--latex-cache' to math waiting for it.
The math is a list of (START . END) markers, to refontify once it's done.")

(defvar opencode--latex-busy nil
  "Non-nil while a formula is being rendered.")

(defconst opencode--latex-timeout 10
  "Seconds to wait for latex or dvisvgm before giving up on a formula.")

(defun opencode--latex-available-p ()
  "Return non-nil if math can be rendered in the current buffer."
  (and opencode-render-latex
       (display-graphic-p)
       (image-type-available-p 'svg)
       (executable-find "latex")
       (executable-find "dvisvgm")))

(defun opencode--latex-document (tex display-p)
  "Return a LaTeX document typesetting TEX, as display math if DISPLAY-P.
It's shipped out as a page exactly the size of the formula's box, whose
height and depth are logged to align its baseline with the text."
  (concat opencode-latex-preamble "\n"
          "\\begin{document}\n"
          "\\hoffset=-1in \\voffset=-1in\n"
          "\\setbox0\\hbox{$" (if display-p "\\displaystyle" "\\textstyle")
          " " tex "\n$}%\n"
          "\\typeout{OPENCODE-BOX \\the\\ht0,\\the\\dp0}%\n"
          "\\shipout\\hbox{\\special{papersize=\\the\\wd0,"
          "\\the\\dimexpr\\ht0+\\dp0\\relax}\\box0}%\n"
          "\\end{document}\n"))

(defun opencode--latex-run (program args dir then)
  "Run PROGRAM with ARGS in DIR, then call THEN with whether it succeeded."
  (let* ((default-directory dir)
         (process (make-process
                   :name (format "opencode-%s" program)
                   :command (cons program args)
                   :connection-type 'pipe
                   :noquery t
                   :buffer nil
                   :sentinel (lambda (process _event)
                               (unless (process-live-p process)
                                 (cancel-timer (process-get process 'timer))
                                 (funcall then (and (eq (process-status process) 'exit)
                                                    (zerop (process-exit-status process)))))))))
    (process-put process 'timer
                 (run-at-time opencode--latex-timeout nil
                              (lambda ()
                                (when (process-live-p process)
                                  (delete-process process)))))))

(defun opencode--latex-log-error (dir tex)
  "Log why TEX couldn't be typeset, from the latex log in DIR."
  (let ((log (expand-file-name "math.log" dir)))
    (opencode--log-event
     "LATEX ERROR"
     (format "%s\n%s" tex
             (or (and (file-exists-p log)
                      (with-temp-buffer
                        (insert-file-contents log)
                        (when (re-search-forward "^! .*\\(?:\n.*\\)\\{0,2\\}" nil t)
                          (match-string 0))))
                 "latex or dvisvgm failed")))))

(defun opencode--latex-render (key then)
  "Render the math KEY, (TEX . DISPLAY-P), then call THEN with its rendering."
  (let ((dir (make-temp-file "opencode-latex-" t))
        (tex (car key)))
    (with-temp-file (expand-file-name "math.tex" dir)
      (insert (opencode--latex-document tex (cdr key))))
    (let ((finish (lambda (rendering)
                    (unless rendering
                      (opencode--latex-log-error dir tex))
                    (delete-directory dir t)
                    (funcall then (or rendering 'error)))))
      (opencode--latex-run
       "latex" '("-interaction=nonstopmode" "-halt-on-error" "-no-shell-escape"
                 "math.tex")
       dir
       (lambda (ok)
         (if (not ok)
             (funcall finish nil)
           (opencode--latex-run
            "dvisvgm" '("--no-fonts" "--bbox=papersize" "--output=math.svg" "math.dvi")
            dir
            (lambda (ok)
              (funcall
               finish
               (when ok
                 (with-temp-buffer
                   (insert-file-contents (expand-file-name "math.log" dir))
                   (when (re-search-forward
                          "^OPENCODE-BOX \\([0-9.]+\\)pt,\\([0-9.]+\\)pt" nil t)
                     (let ((height (string-to-number (match-string 1)))
                           (depth (string-to-number (match-string 2))))
                       (when (> (+ height depth) 0)
                         (list :svg (with-temp-buffer
                                      (set-buffer-multibyte nil)
                                      (insert-file-contents-literally
                                       (expand-file-name "math.svg" dir))
                                      (buffer-string))
                               :height height
                               :depth depth)))))))))))))))

(defun opencode--latex-render-next ()
  "Render the next queued formula, if none is being rendered."
  (unless opencode--latex-busy
    (when-let* ((key (car (last opencode--latex-queue))))
      (setq opencode--latex-queue (butlast opencode--latex-queue)
            opencode--latex-busy t)
      (opencode--latex-render
       key
       (lambda (rendering)
         (puthash key rendering opencode--latex-cache)
         (dolist (range (gethash key opencode--latex-waiting))
           (when-let* ((buffer (marker-buffer (car range))))
             (with-current-buffer buffer
               (font-lock-flush (car range) (cdr range))))
           (set-marker (car range) nil)
           (set-marker (cdr range) nil))
         (remhash key opencode--latex-waiting)
         (setq opencode--latex-busy nil)
         (opencode--latex-render-next))))))

(defun opencode--latex-image (rendering)
  "Return an image displaying RENDERING with the current buffer's text."
  (let* ((height (plist-get rendering :height))
         (depth (plist-get rendering :depth))
         (total (+ height depth))
         ;; LaTeX's 10pt text has a 12pt line height.
         (font-height (if (face-font 'default)
                          (default-font-height)
                        (frame-char-height)))
         (pixels (max 1 (round (* opencode-latex-scale font-height
                                  (/ total 12.0))))))
    (create-image (plist-get rendering :svg) 'svg t
                  :height pixels
                  :ascent (round (* 100 (/ height total)))
                  :foreground (face-foreground 'default nil t)
                  :css "svg {fill: currentcolor;}")))

(defun opencode--latex-math (node)
  "Return (TEX DISPLAY-P START . END) of the math in latex_block NODE.
START and END delimit the TeX between its delimiters.  Return nil if
NODE isn't math, like the text between the dollars of two prices."
  (let* ((children (treesit-node-children node))
         (open (car children))
         (close (car (last children))))
    (when (and (cdr children)
               (equal (treesit-node-type open) "latex_span_delimiter")
               (equal (treesit-node-type close) "latex_span_delimiter"))
      (let* ((start (treesit-node-end open))
             (end (treesit-node-start close))
             (display-p (> (- start (treesit-node-start open)) 1))
             (tex (buffer-substring-no-properties start end)))
        ;; Like Pandoc, inline math can't start or end with a space, or be
        ;; followed by a digit.
        (when (and (< start end)
                   (or display-p
                       (not (or (string-match-p "\\`[[:space:]]\\|[[:space:]]\\'" tex)
                                (memq (char-after (treesit-node-end close))
                                      '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))))))
          (cl-list* (string-trim tex) display-p start end))))))

(defun opencode--latex-nodes (start end)
  "Return the latex_block nodes between START and END.
Each Markdown inline has its own local parser, so math never spans them."
  (cl-loop for parser in (treesit-local-parsers-on start end 'markdown-inline)
           append (mapcar #'cdr (treesit-query-capture
                                 parser '((latex_block) @math) start end))))

(defun opencode--render-latex (start end)
  "Display the math between START and END as images, rendering it if needed.
Math rendered before that's no longer math is displayed as text again."
  (with-silent-modifications
    ;; Clear whole formulas, which may extend past START and END.
    (let ((pos start))
      (while (< pos end)
        (let ((next (next-single-property-change pos 'opencode-latex nil end)))
          (when (get-text-property pos 'opencode-latex)
            (remove-list-of-text-properties
             (or (previous-single-property-change (1+ pos) 'opencode-latex)
                 (point-min))
             (next-single-property-change pos 'opencode-latex nil (point-max))
             '(display opencode-latex)))
          (setq pos next))))
    (when (opencode--latex-available-p)
      (let ((input-start (when-let* ((process (get-buffer-process (current-buffer))))
                           (marker-position (process-mark process)))))
        (dolist (node (opencode--latex-nodes start end))
          (pcase (opencode--latex-math node)
            (`(,tex ,display-p ,beg . ,finish)
             ;; Pending input shows its markup.
             (unless (and input-start (> finish input-start))
               (let* ((key (cons tex display-p))
                      (rendering (gethash key opencode--latex-cache)))
                 (pcase rendering
                   ('nil
                    (puthash key 'pending opencode--latex-cache)
                    (push key opencode--latex-queue)
                    (push (cons (copy-marker beg) (copy-marker finish))
                          (gethash key opencode--latex-waiting))
                    (run-at-time 0 nil #'opencode--latex-render-next))
                   ('pending
                    (push (cons (copy-marker beg) (copy-marker finish))
                          (gethash key opencode--latex-waiting)))
                   ('error nil)
                   (_
                    ;; Not over the delimiters, which Markdown makes
                    ;; invisible, where a display would start.
                    (add-text-properties
                     beg finish
                     `(display ,(opencode--latex-image rendering)
                       opencode-latex t)))))))))))))

(provide 'opencode-latex)
;;; opencode-latex.el ends here
