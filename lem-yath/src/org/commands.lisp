;;;; Org editing commands and buffer-aware Evil/Vi integration.

(in-package :lem-yath)

;;; --- folding --------------------------------------------------------------

(define-command lem-yath-org-cycle () ()
  "Cycle a heading through folded, direct children, and full subtree.
In a table, align it and advance to the next cell."
  (cond
    ((org-table-line-p (current-point))
     (org-table-next-cell))
    ((org-heading-line-p (current-point))
     (org-cycle-heading (current-point))
     (redraw-display))
    (t
     (message "Org cycle: point is not on a heading or table"))))

(define-command lem-yath-org-shift-tab () ()
  "Move to the previous table cell, or cycle global Org visibility."
  (if (org-table-line-p (current-point))
      (org-table-previous-cell)
      (progn
        (org-cycle-global-visibility)
        (redraw-display))))

;;; --- heading navigation ---------------------------------------------------

(defun org-move-to-heading (heading)
  (when heading
    (move-point (current-point) heading)
    (line-start (current-point))
    t))

(lem-vi-mode:define-motion lem-yath-org-forward-element (&optional (count 1))
    (:universal)
  (:type :exclusive)
  (dotimes (_ (or count 1))
    (unless (org-move-to-heading (org-next-heading-point))
      (return))))

(lem-vi-mode:define-motion lem-yath-org-backward-element (&optional (count 1))
    (:universal)
  (:type :exclusive)
  (dotimes (_ (or count 1))
    (unless (org-move-to-heading (org-previous-heading-point))
      (return))))

(lem-vi-mode:define-motion lem-yath-org-up-element (&optional (count 1))
    (:universal)
  (:type :exclusive)
  (dotimes (_ (or count 1))
    (unless (org-move-to-heading (org-parent-heading-point))
      (return))))

(lem-vi-mode:define-motion lem-yath-org-down-element (&optional (count 1))
    (:universal)
  (:type :exclusive)
  (dotimes (_ (or count 1))
    (unless (org-move-to-heading (org-first-child-heading-point))
      (return))))

(lem-vi-mode:define-motion lem-yath-org-top (&optional (count 1)) (:universal)
  (:type :exclusive)
  (let ((wanted (or count 1)))
    (with-point ((point (current-point)))
      (loop
        (when (= wanted (or (org-heading-level-at point) 0))
          (return (org-move-to-heading point)))
        (unless (line-offset point -1)
          (return))))))

(lem-vi-mode:define-motion lem-yath-org-next-visible-line (&optional (count 1))
    (:universal)
  (:type :line)
  (let ((column (point-charpos (current-point))))
    (when (lem-core::move-to-next-visible-line (current-point) (or count 1))
      (move-to-column (current-point) column))))

(lem-vi-mode:define-motion lem-yath-org-previous-visible-line
    (&optional (count 1)) (:universal)
  (:type :line)
  (let ((column (point-charpos (current-point))))
    (when (lem-core::move-to-previous-visible-line
           (current-point) (or count 1))
      (move-to-column (current-point) column))))

;;; --- TODO workflow --------------------------------------------------------

(defun org-heading-todo-bounds (heading)
  "Return TODO start/end columns and state for HEADING."
  (let ((line (line-string heading)))
    (multiple-value-bind (start end register-starts register-ends)
        (cl-ppcre:scan
         (format nil "^\\*+\\s+(~a)(?:\\s|$)" *org-todo-keyword-pattern*)
         line)
      (declare (ignore start end))
      (when (and register-starts (aref register-starts 0))
        (let ((from (aref register-starts 0))
              (to (aref register-ends 0)))
          (values from to (subseq line from to)))))))

(defun org-set-heading-todo-state (heading state)
  "Set HEADING to STATE, where NIL removes its TODO keyword."
  (multiple-value-bind (todo-start todo-end old-state)
      (org-heading-todo-bounds heading)
    (with-point ((point heading))
      (line-start point)
      (cond
        (old-state
         (character-offset point todo-start)
         (let ((length (- todo-end todo-start)))
           (when (and (null state)
                      (eql (character-at point length) #\Space))
             (incf length))
           (delete-character point length)
           (when state
             (insert-string point state))))
        (state
         (character-offset point (1+ (org-heading-level-at heading)))
         (insert-string point (concatenate 'string state " ")))))
    state))

(defun org-next-todo-state (state)
  (if state
      (let ((tail (member state *org-todo-keywords* :test #'string=)))
        (second tail))
      (first *org-todo-keywords*)))

(define-command lem-yath-org-todo () ()
  "Cycle the configured TODO sequence and immediately save a file buffer."
  (alexandria:if-let ((heading (org-current-heading-point)))
    (multiple-value-bind (start end state) (org-heading-todo-bounds heading)
      (declare (ignore start end))
      (let ((next (org-next-todo-state state)))
        (org-clear-folds (current-buffer))
        (org-set-heading-todo-state heading next)
        ;; The Emacs configuration advises org-todo to persist immediately.
        (when (buffer-filename (current-buffer))
          (save-buffer (current-buffer)))
        (message "TODO state: ~a" (or next "none"))))
    (message "No Org heading at point")))

;;; --- checkboxes and lists -------------------------------------------------

(defun org-list-prefix (point)
  "Return the reusable list prefix on POINT's line, or NIL."
  (let ((line (line-string point)))
    (multiple-value-bind (start end register-starts register-ends)
        (cl-ppcre:scan
         "^(\\s*)((?:[-+] |[0-9]+[.)] ))(?:((?:\\[[ Xx-]\\]))\\s*)?"
         line)
      (declare (ignore start))
      (when end
        (let* ((indent (subseq line (aref register-starts 0)
                               (aref register-ends 0)))
               (bullet (subseq line (aref register-starts 1)
                               (aref register-ends 1)))
               (checkbox-start (and (>= (length register-starts) 3)
                                    (aref register-starts 2))))
          (concatenate 'string indent bullet
                       (if checkbox-start "[ ] " "")))))))

(defun org-enter-insert-state ()
  (setf (lem-vi-mode/core:buffer-state)
        'lem-vi-mode/states:insert))

(defun org-open-list-or-table (above-p)
  (let ((prefix (org-list-prefix (current-point))))
    (cond
      (prefix
       (org-clear-folds (current-buffer))
       (if above-p
           (progn
             (line-start (current-point))
             (insert-string (current-point)
                            (concatenate 'string prefix (string #\Newline)))
             ;; INSERT-STRING leaves point at the original line.  Move back
             ;; into the newly inserted item so insert state edits that item.
             (line-offset (current-point) -1)
             (line-end (current-point)))
           (progn
             (line-end (current-point))
             (insert-string (current-point)
                            (concatenate 'string (string #\Newline) prefix))))
       (org-enter-insert-state)
       t)
      ((org-table-line-p (current-point))
       (org-table-insert-row above-p)
       (org-enter-insert-state)
       t)
      (t nil))))

(define-command lem-yath-org-open-below () ()
  (unless (org-open-list-or-table nil)
    (call-command 'lem-vi-mode/commands:vi-open-below nil)))

(define-command lem-yath-org-open-above () ()
  (unless (org-open-list-or-table t)
    (call-command 'lem-vi-mode/commands:vi-open-above nil)))

(define-command lem-yath-org-toggle-checkbox () ()
  "Toggle the first checkbox on the current Org line."
  (let ((line (line-string (current-point))))
    (multiple-value-bind (start end) (cl-ppcre:scan "\\[[ Xx-]\\]" line)
      (if (null start)
          (message "No checkbox on this line")
          (with-point ((point (current-point)))
            (line-start point)
            (character-offset point start)
            (let ((checked (member (char line (1+ start)) '(#\X #\x))))
              (delete-character point (- end start))
              (insert-string point (if checked "[ ]" "[X]"))))))))

;;; --- heading and subtree transforms --------------------------------------

(defun org-insert-heading-after-subtree (&key todo)
  (let* ((heading (org-current-heading-point))
         (level (or (and heading (org-heading-level-at heading)) 1))
         (target (if heading
                     (org-subtree-end-point heading)
                     (copy-point (current-point) :temporary)))
         (prefix (format nil "~v@{*~} ~:[~;TODO ~]" level todo))
         (before-next-heading-p
           (and heading (not (end-buffer-p target)))))
    (org-clear-folds (current-buffer))
    (move-point (current-point) target)
    (cond
      (before-next-heading-p
       ;; SUBTREE-END is the next sibling's line start.  Insert a complete
       ;; line before it, then return point to the end of the new prefix.
       (insert-string (current-point)
                      (concatenate 'string prefix (string #\Newline)))
       (line-offset (current-point) -1)
       (line-end (current-point)))
      (t
       (unless (start-line-p (current-point))
         (insert-character (current-point) #\Newline))
       (insert-string (current-point) prefix)))
    (org-enter-insert-state)))

(define-command lem-yath-org-insert-heading () ()
  (org-insert-heading-after-subtree))

(define-command lem-yath-org-insert-todo-heading () ()
  (org-insert-heading-after-subtree :todo t))

(define-command lem-yath-org-meta-return () ()
  (unless (org-open-list-or-table nil)
    (org-insert-heading-after-subtree)))

(defun org-list-item-line-p (point)
  (not (null (org-list-prefix point))))

(defun org-adjust-list-indent (delta)
  (line-start (current-point))
  (cond
    ((plusp delta)
     (insert-string (current-point) "  "))
    ((and (minusp delta)
          (eql (character-at (current-point)) #\Space)
          (eql (character-at (current-point) 1) #\Space))
     (delete-character (current-point) 2))))

(defun org-adjust-subtree-level (delta)
  (alexandria:if-let ((heading (org-current-heading-point)))
    (let ((level (org-heading-level-at heading)))
      (when (and (minusp delta) (= level 1))
        (message "A level-1 heading cannot be promoted")
        (return-from org-adjust-subtree-level nil))
      (let ((end (org-subtree-end-point heading)))
        (org-clear-folds (current-buffer))
        (with-point ((point heading))
          (loop :while (point< point end)
                :when (org-heading-line-p point)
                  :do (line-start point)
                      (if (plusp delta)
                          (insert-character point #\*)
                          (delete-character point 1))
                :unless (line-offset point 1)
                  :do (return))))
      t)
    (message "No Org heading at point")))

(define-command lem-yath-org-metaleft () ()
  (if (org-list-item-line-p (current-point))
      (org-adjust-list-indent -1)
      (org-adjust-subtree-level -1)))

(define-command lem-yath-org-metaright () ()
  (if (org-list-item-line-p (current-point))
      (org-adjust-list-indent 1)
      (org-adjust-subtree-level 1)))

(defun org-swap-subtree (direction)
  (alexandria:if-let ((heading (org-current-heading-point)))
    (let ((sibling (org-same-level-sibling heading direction)))
      (unless sibling
        (message "No same-level sibling in that direction")
        (return-from org-swap-subtree nil))
      (org-clear-folds (current-buffer))
      (let ((offset (- (position-at-point (current-point))
                       (position-at-point heading))))
        (if (plusp direction)
            (let* ((heading-end (org-subtree-end-point heading))
                   (sibling-end (org-subtree-end-point sibling))
                   (first (points-to-string heading heading-end))
                   (second (points-to-string sibling sibling-end)))
              (delete-between-points heading sibling-end)
              (insert-string heading (concatenate 'string second first))
              (move-point (current-point) heading)
              (character-offset (current-point) (+ (length second) offset)))
            (let* ((sibling-end (org-subtree-end-point sibling))
                   (first (points-to-string sibling sibling-end))
                   (second (points-to-string heading (org-subtree-end-point heading))))
              (delete-between-points sibling (org-subtree-end-point heading))
              (insert-string sibling (concatenate 'string second first))
              (move-point (current-point) sibling)
              (character-offset (current-point) offset))))
      t)
    (message "No Org heading at point")))

(define-command lem-yath-org-metaup () ()
  (org-swap-subtree -1))

(define-command lem-yath-org-metadown () ()
  (org-swap-subtree 1))

;;; --- links ---------------------------------------------------------------

(defun org-link-at-point (&optional (point (current-point)))
  "Return the bracket-link target covering POINT, or NIL."
  (let ((line (line-string point))
        (column (point-charpos point)))
    (cl-ppcre:do-scans (start end register-starts register-ends
                        "\\[\\[([^]\\n]+)\\](?:\\[[^]\\n]*\\])?\\]" line)
      (when (<= start column end)
        (return-from org-link-at-point
          (subseq line (aref register-starts 0) (aref register-ends 0)))))))

(defun org-split-link-search (target)
  (let ((separator (search "::" target)))
    (if separator
        (values (subseq target 0 separator) (subseq target (+ separator 2)))
        (values target nil))))

(defun org-relative-link-path (path)
  (let ((pathname (pathname path)))
    (if (uiop:absolute-pathname-p pathname)
        pathname
        (merge-pathnames
         pathname
         (or (and (buffer-filename (current-buffer))
                  (uiop:pathname-directory-pathname
                   (buffer-filename (current-buffer))))
             (buffer-directory (current-buffer)))))))

(defun org-visit-search-suffix (suffix)
  (when (and suffix (plusp (length suffix)))
    (let ((point (current-point)))
      (buffer-start point)
      (cond
        ((alexandria:starts-with-subseq "*" suffix)
         (search-forward-regexp
          point
          (format nil "(?m)^\\*+\\s+~a\\s*$"
                  (cl-ppcre:quote-meta-chars
                   (string-trim '(#\Space #\*) suffix)))))
        ((alexandria:starts-with-subseq "#" suffix)
         (search-forward-regexp
          point
          (format nil "(?mi)^:CUSTOM_ID:\\s*~a\\s*$"
                  (cl-ppcre:quote-meta-chars (subseq suffix 1)))))
        (t
         (search-forward-regexp point (cl-ppcre:quote-meta-chars suffix)))))))

(defun org-id-file (id)
  (let* ((root (workdir))
         (output (ignore-errors
                   (uiop:run-program
                    (list "rg" "--files-with-matches" "--glob" "*.org"
                          "--fixed-strings" (format nil ":ID: ~a" id)
                          (namestring root))
                    :output :string :ignore-error-status t)))
         (line (first (uiop:split-string (or output "")
                                         :separator (string #\Newline)))))
    (and line (plusp (length line)) (pathname line))))

(defun org-open-id-link (id)
  (alexandria:if-let ((file (org-id-file id)))
    (progn
      (find-file file)
      (let ((point (current-point)))
        (buffer-start point)
        (when (search-forward-regexp
               point
               (format nil "(?mi)^:ID:\\s*~a\\s*$"
                       (cl-ppcre:quote-meta-chars id)))
          (alexandria:when-let ((heading (org-current-heading-point point)))
            (move-point point heading)))))
    (message "No Org heading with ID ~a under ~a" id (workdir))))

(define-command lem-yath-org-open-at-point () ()
  "Open the Org bracket link at point."
  (alexandria:if-let ((target (org-link-at-point)))
    (cond
      ((or (alexandria:starts-with-subseq "http://" target)
           (alexandria:starts-with-subseq "https://" target)
           (alexandria:starts-with-subseq "mailto:" target))
       (open-with-xdg target))
      ((alexandria:starts-with-subseq "id:" target)
       (org-open-id-link (subseq target 3)))
      (t
       (multiple-value-bind (path suffix)
           (org-split-link-search
            (if (alexandria:starts-with-subseq "file:" target)
                (subseq target 5)
                target))
         (find-file (org-relative-link-path path))
         (org-visit-search-suffix suffix))))
    (message "No Org link at point")))

(define-command lem-yath-org-insert-link () ()
  "Insert an Org bracket link after prompting for target and description."
  (let ((target (prompt-for-string "Link target: ")))
    (when (plusp (length target))
      (let ((description (prompt-for-string "Description (optional): ")))
        (insert-string
         (current-point)
         (if (plusp (length description))
             (format nil "[[~a][~a]]" target description)
             (format nil "[[~a]]" target)))))))

;;; --- tables ---------------------------------------------------------------

(defun org-table-line-p (&optional (point (current-point)))
  (not (null (cl-ppcre:scan "^\\s*\\|" (line-string point)))))

(defun org-table-separator-line-p (line)
  (not (null (cl-ppcre:scan "^\\s*\\|[-+:|\\s]+\\|\\s*$" line))))

(defun org-table-cells (line)
  (let ((cells (cl-ppcre:split "\\|" (string-trim '(#\Space #\Tab) line))))
    (when (and cells (string= (first cells) ""))
      (setf cells (rest cells)))
    (when (and cells (string= (car (last cells)) ""))
      (setf cells (butlast cells)))
    (mapcar (lambda (cell) (string-trim '(#\Space #\Tab) cell)) cells)))

(defun org-table-bounds (&optional (origin (current-point)))
  (when (org-table-line-p origin)
    (with-point ((start origin)
                 (end origin))
      (line-start start)
      (line-start end)
      (loop :while (with-point ((previous start))
                     (and (line-offset previous -1)
                          (org-table-line-p previous)
                          (progn (move-point start previous) t))))
      (loop :while (with-point ((next end))
                     (and (line-offset next 1)
                          (org-table-line-p next)
                          (progn (move-point end next) t))))
      (line-end end)
      (values (copy-point start :temporary)
              (copy-point end :temporary)))))

(defun org-table-row-lines (start end)
  (let ((lines '()))
    (with-point ((point start))
      (loop
        (push (line-string point) lines)
        (when (same-line-p point end)
          (return))
        (unless (line-offset point 1)
          (return))))
    (nreverse lines)))

(defun org-table-column-widths (lines)
  (let ((widths '()))
    (dolist (line lines)
      (unless (org-table-separator-line-p line)
        (loop :for cell :in (org-table-cells line)
              :for index :from 0
              :do (let ((width (length cell)))
                    (if (< index (length widths))
                        (setf (nth index widths)
                              (max width (nth index widths)))
                        (setf widths
                              (append widths
                                      (make-list (- index (length widths))
                                                 :initial-element 0)
                                      (list width))))))))
    (if widths
        (mapcar (lambda (width) (max 1 width)) widths)
        ;; A table may temporarily contain only a horizontal rule.  Recover
        ;; its column count and approximate widths instead of reducing it to
        ;; an unusable `||'.
        (alexandria:when-let
            ((separator (find-if #'org-table-separator-line-p lines)))
          (let* ((start (position #\| separator))
                 (end (position #\| separator :from-end t))
                 (body (and start end (< start end)
                            (subseq separator (1+ start) end))))
            (when body
              (mapcar (lambda (segment)
                        (max 1 (- (length
                                   (string-trim '(#\Space #\Tab) segment))
                                  2)))
                      (cl-ppcre:split "\\+" body))))))))

(defun org-table-line-indentation (line)
  (let ((pipe (position #\| line)))
    (if pipe (subseq line 0 pipe) "")))

(defun org-format-table-row (line widths)
  (let ((indentation (org-table-line-indentation line)))
    (concatenate
     'string indentation
     (if (org-table-separator-line-p line)
         (format nil "|~{~a~^+~}|"
                 (mapcar
                  (lambda (width)
                    (make-string (+ width 2) :initial-element #\-))
                  widths))
         (let ((cells (org-table-cells line)))
           (with-output-to-string (out)
             (write-char #\| out)
             (loop :for width :in widths
                   :for index :from 0
                   :for cell := (or (nth index cells) "")
                   :do (format out " ~vA |" width cell))))))))

(defun org-table-cell-index (&optional (point (current-point)))
  (count #\| (line-string point) :end (min (point-charpos point)
                                            (length (line-string point)))))

(defun org-table-move-to-cell (point index)
  (line-start point)
  (let ((line (line-string point))
        (seen 0))
    (loop :for column :from 0 :below (length line)
          :when (eql (char line column) #\|)
            :do (incf seen)
                (when (= seen index)
                  (move-to-column point (min (length line) (+ column 2)))
                  (return t)))))

(defun org-table-align ()
  "Align the contiguous Org table at point and preserve the current cell."
  (multiple-value-bind (start end) (org-table-bounds)
    (unless start
      (return-from org-table-align nil))
    (let* ((row (- (line-number-at-point (current-point))
                   (line-number-at-point start)))
           (cell (max 1 (org-table-cell-index)))
           (lines (org-table-row-lines start end))
           (widths (org-table-column-widths lines))
           (formatted (format nil "~{~a~^~%~}"
                              (mapcar (lambda (line)
                                        (org-format-table-row line widths))
                                      lines))))
      (delete-between-points start end)
      (insert-string start formatted)
      (move-point (current-point) start)
      (line-offset (current-point) row)
      (org-table-move-to-cell (current-point) cell)
      t)))

(defun org-table-last-cell-p (point)
  (let* ((line (line-string point))
         (column (point-charpos point))
         (next (position #\| line :start (min (length line) (1+ column)))))
    (or (null next)
        (null (position #\| line :start (1+ next))))))

(defun org-table-next-cell ()
  (org-table-align)
  (let ((index (max 1 (org-table-cell-index))))
    (if (org-table-last-cell-p (current-point))
        (with-point ((next (current-point)))
          (if (and (line-offset next 1) (org-table-line-p next)
                   (not (org-table-separator-line-p (line-string next))))
              (progn
                (move-point (current-point) next)
                (org-table-move-to-cell (current-point) 1))
              (org-table-insert-row nil)))
        (org-table-move-to-cell (current-point) (1+ index)))))

(defun org-table-previous-cell ()
  (org-table-align)
  (let ((index (max 1 (org-table-cell-index))))
    (if (> index 1)
        (org-table-move-to-cell (current-point) (1- index))
        (with-point ((previous (current-point)))
          (loop :while (line-offset previous -1)
                :when (and (org-table-line-p previous)
                           (not (org-table-separator-line-p
                                 (line-string previous))))
                  :do (move-point (current-point) previous)
                      (org-table-move-to-cell
                       (current-point)
                       (length (org-table-cells (line-string previous))))
                      (return))))))

(defun org-table-insert-row (above-p)
  (let* ((columns (max 1 (length (org-table-cells (line-string (current-point))))))
         (row (format nil "|~{ ~a |~}" (make-list columns :initial-element ""))))
    (if above-p
        (progn
          (line-start (current-point))
          (insert-string (current-point)
                         (concatenate 'string row (string #\Newline)))
          (line-offset (current-point) -1))
        (progn
          (line-end (current-point))
          (insert-string (current-point) (concatenate 'string (string #\Newline) row))))
    (org-table-move-to-cell (current-point) 1)))

(define-command lem-yath-org-context-action () ()
  "Align a table; execution of source blocks remains a later safe tranche."
  (if (org-table-line-p (current-point))
      (org-table-align)
      (message "No supported Org context action at point")))

;;; --- Vi state maps --------------------------------------------------------

(defvar *org-vi-normal-keymap* (make-keymap :description '*org-vi-normal-keymap*))
(defvar *org-vi-visual-keymap* (make-keymap :description '*org-vi-visual-keymap*))
(defvar *org-vi-insert-keymap* (make-keymap :description '*org-vi-insert-keymap*))

(defun configure-org-vi-common-map (keymap)
  (define-key keymap "j" 'lem-yath-org-next-visible-line)
  (define-key keymap "k" 'lem-yath-org-previous-visible-line)
  (define-key keymap "Tab" 'lem-yath-org-cycle)
  (define-key keymap "g Tab" 'lem-yath-org-cycle)
  (define-key keymap "Shift-Tab" 'lem-yath-org-shift-tab)
  (define-key keymap "g h" 'lem-yath-org-up-element)
  (define-key keymap "g l" 'lem-yath-org-down-element)
  (define-key keymap "g k" 'lem-yath-org-backward-element)
  (define-key keymap "g j" 'lem-yath-org-forward-element)
  (define-key keymap "g H" 'lem-yath-org-top)
  (define-key keymap "M-h" 'lem-yath-org-metaleft)
  (define-key keymap "M-l" 'lem-yath-org-metaright)
  (define-key keymap "M-k" 'lem-yath-org-metaup)
  (define-key keymap "M-j" 'lem-yath-org-metadown)
  (define-key keymap "<" 'lem-yath-org-metaleft)
  (define-key keymap ">" 'lem-yath-org-metaright))

(configure-org-vi-common-map *org-vi-normal-keymap*)
(configure-org-vi-common-map *org-vi-visual-keymap*)

(define-key *org-vi-normal-keymap* "o" 'lem-yath-org-open-below)
(define-key *org-vi-normal-keymap* "O" 'lem-yath-org-open-above)
(define-key *org-vi-normal-keymap* "C-Return" 'lem-yath-org-insert-heading)
(define-key *org-vi-normal-keymap* "C-Shift-Return"
  'lem-yath-org-insert-todo-heading)

(define-key *org-vi-insert-keymap* "Tab" 'lem-yath-org-cycle)
(define-key *org-vi-insert-keymap* "Shift-Tab" 'lem-yath-org-shift-tab)
(define-key *org-vi-insert-keymap* "C-t" 'lem-yath-org-metaright)
(define-key *org-vi-insert-keymap* "C-d" 'lem-yath-org-metaleft)

(defmethod lem-vi-mode/core:mode-specific-keymaps ((mode org-mode))
  (declare (ignore mode))
  (let ((state (lem-vi-mode/core:current-state)))
    (cond
      ((typep state 'lem-vi-mode/visual:visual)
       (list *org-vi-visual-keymap*))
      ((typep state 'lem-vi-mode/states:insert)
       (list *org-vi-insert-keymap*))
      (t (list *org-vi-normal-keymap*)))))

;;; Stock Org chords that do not conflict with the active Evil state maps.
(define-key *org-mode-keymap* "Tab" 'lem-yath-org-cycle)
(define-key *org-mode-keymap* "Shift-Tab" 'lem-yath-org-shift-tab)
(define-key *org-mode-keymap* "C-c C-t" 'lem-yath-org-todo)
(define-key *org-mode-keymap* "C-c C-x C-b" 'lem-yath-org-toggle-checkbox)
(define-key *org-mode-keymap* "C-c C-l" 'lem-yath-org-insert-link)
(define-key *org-mode-keymap* "C-c C-o" 'lem-yath-org-open-at-point)
(define-key *org-mode-keymap* "C-c C-c" 'lem-yath-org-context-action)
(define-key *org-mode-keymap* "M-Return" 'lem-yath-org-meta-return)
(define-key *org-mode-keymap* "C-Return" 'lem-yath-org-insert-heading)
(define-key *org-mode-keymap* "C-Shift-Return" 'lem-yath-org-insert-todo-heading)
