;;;; Project tools: compile (SPC c c), project buffer switching (SPC SPC),
;;;; duplicate-dwim (M-j).

(in-package :lem-yath)

;; Match `recentf-max-saved-items'.  This runs before command-line files are
;; opened.  Lem's dashboard can instantiate the lazy history before user init,
;; so update and, if necessary, trim that already-created object as well.
(setf lem-core/commands/file:*file-history-limit* 300)
(let* ((history (lem-core/commands/file:file-history))
       (limit lem-core/commands/file:*file-history-limit*)
       (data (lem/common/history::history-data history))
       (count (length data)))
  (setf (lem/common/history::history-limit history) limit)
  (when (> count limit)
    ;; History data is oldest to newest, so discard entries from the front.
    (replace data data
             :start1 0
             :end1 limit
             :start2 (- count limit)
             :end2 count)
    (setf (fill-pointer data) limit
          (lem/common/history::history-index history) limit)
    (lem/common/history:save-file history)))

(define-command lem-yath-compile () ()
  "Prompt for a shell command and stream its output into *compilation*.
Runs from the project root; the worker runs on a background thread."
  (let* ((dir (or (ignore-errors
                    (lem-core/commands/project:find-root
                     (buffer-directory (current-buffer))))
                  (ignore-errors (buffer-directory (current-buffer)))
                  (user-homedir-pathname)))
         (command (prompt-for-string (format nil "Compile [~a]: " dir)
                                     :history-symbol 'lem-yath-compile)))
    (when (plusp (length command))
      (stream-to-buffer (list "sh" "-c" command) "*compilation*"
                        :directory dir))))

(define-command lem-yath-project-buffers () ()
  "Switch among buffers of the current project (consult-project-buffer)."
  (let* ((root (ignore-errors
                 (namestring
                  (lem-core/commands/project:find-root
                   (buffer-directory (current-buffer))))))
         (names (loop :for b :in (buffer-list)
                      :for file := (buffer-filename b)
                      :when (and file root
                                 (alexandria:starts-with-subseq
                                  root (namestring file)))
                        :collect (buffer-name b))))
    (unless names
      (message "No file buffers in this project")
      (return-from lem-yath-project-buffers))
    (let ((choice (prompt-for-string
                   "Project buffer: "
                   :completion-function (lambda (s) (prescient-filter s names))
                   :test-function (lambda (s) (member s names :test #'string=)))))
      (when choice
        (switch-to-buffer (get-buffer choice))))))

(define-command lem-yath-kill-current-buffer () ()
  "Kill the current buffer without prompting (kill-current-buffer)."
  (kill-buffer (current-buffer)))

(defun duplicate-string (string count)
  (str:repeat count string))

(defun duplicate-current-line (count)
  "Duplicate the current line COUNT times while preserving point."
  (let ((text (concatenate 'string
                           (line-string (current-point))
                           (string #\Newline))))
    (with-point ((saved-point (current-point) :right-inserting)
                 (insertion (current-point) :left-inserting))
      (line-end insertion)
      (cond
        ((end-buffer-p insertion)
         ;; Emacs terminates both the source and its final copy at EOF.
         (unless (start-line-p insertion)
           (insert-character insertion #\Newline)))
        (t
         (character-offset insertion 1)))
      (insert-string insertion (duplicate-string text count))
      (move-point (current-point) saved-point))))

(defun duplicate-region-bounds (buffer)
  "Return the active contiguous region in BUFFER, or no values."
  (cond
    ((and (typep (current-global-mode) 'lem-vi-mode:vi-mode)
          (lem-vi-mode/visual:visual-p buffer))
     (when (lem-vi-mode/visual:visual-block-p buffer)
       (editor-error "Visual-block duplication is not implemented"))
     (destructuring-bind (first second)
         (lem-vi-mode/visual:visual-range buffer)
       (values (point-min first second) (point-max first second))))
    ((and (not (typep (current-global-mode) 'lem-vi-mode:vi-mode))
          (buffer-mark-p buffer))
     (let ((start (region-beginning buffer))
           (end (region-end buffer)))
       (unless (point= start end)
         (values start end))))))

(defun duplicate-active-region (start end count)
  "Duplicate START..END COUNT times and preserve point and mark orientation."
  (let* ((buffer (point-buffer start))
         (text (points-to-string start end))
         (visual-state
           (when (lem-vi-mode/visual:visual-p buffer)
             (lem-vi-mode/core:buffer-state buffer)))
         (unterminated-linewise-eof-p
           (and visual-state
                (lem-vi-mode/visual:visual-line-p buffer)
                (end-buffer-p end)
                (or (zerop (length text))
                    (char/= (char text (1- (length text))) #\Newline))))
         (copies
           (if unterminated-linewise-eof-p
               (concatenate 'string
                            (string #\Newline)
                            (duplicate-string
                             (concatenate 'string text (string #\Newline))
                             count))
               (duplicate-string text count))))
    (with-point ((saved-point (buffer-point buffer) :right-inserting)
                 (saved-mark (buffer-mark buffer) :right-inserting)
                 (insertion end :left-inserting))
      (insert-string insertion copies)
      (move-point (buffer-point buffer) saved-point)
      ;; INSERT-STRING deactivates the mark.  SETF reactivates it, and restoring
      ;; the saved Vi state retains VISUAL versus V-LINE as well as orientation.
      (setf (buffer-mark buffer) saved-mark)
      (when visual-state
        (setf (lem-vi-mode/core:buffer-state buffer) visual-state)))))

(define-command lem-yath-duplicate-dwim (&optional (count 1)) (:universal)
  "Duplicate an active contiguous region or the current line COUNT times."
  (let ((count (or count 1)))
    (when (plusp count)
      (multiple-value-bind (start end)
          (duplicate-region-bounds (current-buffer))
        (if start
            (duplicate-active-region start end count)
            (duplicate-current-line count))))))
