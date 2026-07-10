;;;; Persistent find-name-dired-style results for M-s f.

(in-package :lem-yath)

(defparameter *find-name-buffer-name* "*Find*")
(defconstant +find-name-buffer-owner+ 'lem-yath-find-name)
(defvar *find-name-mode-keymap* (make-keymap))

(define-major-mode lem-yath-find-name-mode nil
    (:name "Find Name"
     :keymap *find-name-mode-keymap*)
  (setf (buffer-read-only-p (current-buffer)) t))

;; Pinned Lem's Vi keymap assembly does not include ordinary major-mode maps.
(defmethod lem-vi-mode/core:mode-specific-keymaps
    ((mode lem-yath-find-name-mode))
  (list *find-name-mode-keymap*))

(defun find-name-split-nul (string)
  "Return the nonempty NUL-terminated records in STRING."
  (loop :with start := 0
        :for end := (position #\Null string :start start)
        :while end
        :when (< start end)
          :collect (subseq string start end)
        :do (setf start (1+ end))))

(defun find-name-display-string (string)
  "Escape controls in STRING so one filesystem entry occupies one line."
  (with-output-to-string (stream)
    (loop :for character :across string
          :for code := (char-code character)
          :do (case character
                (#\\ (write-string "\\\\" stream))
                (#\Newline (write-string "\\n" stream))
                (#\Return (write-string "\\r" stream))
                (#\Tab (write-string "\\t" stream))
                (otherwise
                 (if (or (< code 32) (= code 127))
                     (format stream "\\x~2,'0X;" code)
                     (write-char character stream)))))))

(defun find-name-relative-path (record)
  (if (alexandria:starts-with-subseq "./" record)
      (subseq record 2)
      record))

(defun find-name-absolute-path (root record)
  "Return RECORD below ROOT without invoking Common Lisp wildcard parsing."
  (if (string= record ".")
      root
      (uiop:parse-native-namestring
       (concatenate 'string
                    (uiop:native-namestring
                     (uiop:ensure-directory-pathname root))
                    (find-name-relative-path record)))))

(defun find-name-results (root output)
  "Turn GNU find's NUL OUTPUT into sorted (DISPLAY . ABSOLUTE) entries."
  (sort
   (mapcar
    (lambda (record)
      (cons record (find-name-absolute-path root record)))
    (find-name-split-nul output))
   #'string<
   :key #'car))

(defun find-name-owned-buffer-p (buffer)
  (and (eq (buffer-value buffer :find-name-owner)
           +find-name-buffer-owner+)
       (eq (buffer-major-mode buffer) 'lem-yath-find-name-mode)))

(defun ensure-find-name-buffer ()
  "Return our result buffer, refusing to repurpose an unrelated *Find*."
  (let ((buffer (get-buffer *find-name-buffer-name*)))
    (cond
      ((null buffer)
       (setf buffer (make-buffer *find-name-buffer-name* :enable-undo-p nil))
       (change-buffer-mode buffer 'lem-yath-find-name-mode)
       (setf (buffer-value buffer :find-name-owner)
             +find-name-buffer-owner+)
       buffer)
      ((find-name-owned-buffer-p buffer)
       buffer)
      (t
       (editor-error "Buffer ~a already exists and is not a find-name result buffer"
                     *find-name-buffer-name*)))))

(defun find-name-current-generation-p (buffer generation)
  (and (member buffer (buffer-list) :test #'eq)
       (find-name-owned-buffer-p buffer)
       (eql generation (buffer-value buffer :find-name-generation))))

(defun find-name-insert-header (point root pattern status)
  (insert-string point (format nil "Find name results~%"))
  (insert-string point
                 (format nil "Directory: ~a~%"
                         (find-name-display-string
                          (uiop:native-namestring root))))
  (insert-string point
                 (format nil "Pattern:   ~a~%"
                         (find-name-display-string pattern)))
  (insert-string point (format nil "Status:    ~a~%~%" status)))

(defun render-find-name-searching (buffer root pattern)
  (with-buffer-read-only buffer nil
    (erase-buffer buffer)
    (find-name-insert-header (buffer-end-point buffer) root pattern "searching..."))
  (setf (buffer-read-only-p buffer) t)
  (buffer-start (buffer-point buffer))
  (redraw-display))

(defun render-find-name-results (buffer root pattern generation results error)
  "Render RESULTS only if GENERATION is still current for BUFFER."
  (when (find-name-current-generation-p buffer generation)
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (let ((point (buffer-end-point buffer)))
        (find-name-insert-header
         point root pattern
         (if error
             (format nil "failed: ~a" (find-name-display-string error))
             (format nil "~d ~a"
                     (length results)
                     (if (= 1 (length results)) "match" "matches"))))
        (cond
          (error
           (insert-string point "Search failed. Press g to retry.\n"))
          ((null results)
           (insert-string point "(no matches)\n"))
          (t
           (with-point ((first-result point :right-inserting))
             (dolist (result results)
               (with-point ((start point :right-inserting))
                 (insert-string point
                                (format nil "~a~%"
                                        (find-name-display-string (car result))))
                 (put-text-property start point :find-name-path (cdr result))))
             (move-point (buffer-point buffer) first-result))))))
    (setf (buffer-read-only-p buffer) t)
    (redraw-display)))

(defun run-find-name (root pattern)
  (let ((find (executable-find "find")))
    (unless find
      (error "GNU find is not available"))
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program
         (list (namestring find) "." "-name" pattern "-print0")
         :directory root
         :output :string
         :error-output :string
         :ignore-error-status t)
      (if (and (integerp exit-code) (zerop exit-code))
          (values (find-name-results root output) nil)
          (values nil
                  (let ((message
                          (string-trim '(#\Space #\Tab #\Newline #\Return)
                                       (or error-output ""))))
                    (if (plusp (length message))
                        message
                        (format nil "find exited with status ~a" exit-code))))))))

(defun start-find-name-search (buffer root pattern)
  "Start a race-safe background search into persistent BUFFER."
  (unless (find-name-owned-buffer-p buffer)
    (editor-error "Not a find-name result buffer"))
  (let ((generation (1+ (or (buffer-value buffer :find-name-generation) 0))))
    (setf (buffer-value buffer :find-name-generation) generation
          (buffer-value buffer :find-name-root) root
          (buffer-value buffer :find-name-pattern) pattern)
    (render-find-name-searching buffer root pattern)
    (bt2:make-thread
     (lambda ()
       (handler-case
           (multiple-value-bind (results error)
               (run-find-name root pattern)
             (send-event
              (lambda ()
                (render-find-name-results
                 buffer root pattern generation results error))))
         (error (condition)
           (let ((message (princ-to-string condition)))
             (send-event
              (lambda ()
                (render-find-name-results
                 buffer root pattern generation nil message)))))))
     :name "lem-yath/find-name")))

(defun normalize-find-name-root (directory base)
  (uiop:ensure-directory-pathname
   (truename
    (merge-pathnames directory (uiop:ensure-directory-pathname base)))))

(define-command lem-yath-find-name (&optional directory pattern) ()
  "Find names recursively and show persistent results, like find-name-dired."
  (let* ((base (buffer-directory (current-buffer)))
         (directory
           (or directory
               (prompt-for-directory "Find name in directory: "
                                     :directory base
                                     :default base
                                     :existing t)))
         (pattern
           (or pattern
               (prompt-for-string "Name pattern: "
                                  :initial-value "*"
                                  :history-symbol 'lem-yath-find-name)))
         (root (normalize-find-name-root directory base))
         (buffer (ensure-find-name-buffer)))
    (setf (buffer-directory buffer) root)
    (switch-to-buffer buffer)
    (start-find-name-search buffer root pattern)))

(define-command lem-yath-find-name-open () ()
  "Open the exact find result on the current line."
  (with-point ((point (current-point)))
    (line-start point)
    (let ((path (text-property-at point :find-name-path)))
      (unless path
        (editor-error "No find result on this line"))
      (unless (uiop:probe-file* path)
        (editor-error "Find result no longer exists: ~a" path))
      (find-file path))))

(define-command lem-yath-find-name-refresh () ()
  "Repeat the search that produced the current *Find* buffer."
  (let* ((buffer (current-buffer))
         (root (buffer-value buffer :find-name-root))
         (pattern (buffer-value buffer :find-name-pattern)))
    (unless (and root pattern)
      (editor-error "No find-name search to refresh"))
    (start-find-name-search buffer root pattern)))

(define-key *find-name-mode-keymap* "Return" 'lem-yath-find-name-open)
(define-key *find-name-mode-keymap* "g" 'lem-yath-find-name-refresh)
(define-key *find-name-mode-keymap* "q" 'quit-active-window)
