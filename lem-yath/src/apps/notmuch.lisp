;;;; Mail: notmuch -> a focused Lem reader.
;;;;
;;;; The Emacs config used `M-x notmuch` / `notmuch-search` over a
;;;; Proton Bridge -> mbsync (isync) -> notmuch pipeline. This port keeps the
;;;; daily read path: a newest-first thread search list, opening a thread into
;;;; a headers+plain-text view, owner-private PDF attachment preview, and a
;;;; `mbsync -a && notmuch new` fetch.
;;;;
;;;; All notmuch interaction is via the CLI with --format=json, parsed by yason
;;;; (JSON arrays -> lists, objects -> hash-tables with string keys, null -> NIL).
;;;; The `notmuch show` tree nests parts arbitrarily, so the walk is defensive.

(in-package :lem-yath)

(defparameter *notmuch-default-query* "tag:inbox"
  "Initial query offered by `lem-yath-notmuch' (mirrors notmuch's inbox view).")

(defparameter *notmuch-search-limit* 100
  "Maximum number of threads requested from `notmuch search'.")

(defparameter *notmuch-process-timeout* 20)
(defparameter *notmuch-output-limit* (* 4 1024 1024))
(defparameter *notmuch-attachment-output-limit* (* 128 1024 1024))

(defparameter *notmuch-list-buffer-name* "*lem-yath-mail*")
(defparameter *notmuch-fetch-buffer-name* "*lem-yath-fetchmail*")

(defstruct notmuch-attachment
  message-id
  part-id
  filename)

(declaim (special *project-process-timeout*))

;;; --- helpers ---------------------------------------------------------------

(defun notmuch-available-p ()
  "True when the notmuch binary is on PATH."
  (and (executable-find "notmuch") t))

(defun notmuch-run-json (args)
  "Run notmuch with ARGS (a list of strings), parse stdout as JSON.
Returns the parsed value and true on success, or NIL/NIL on failure.  The
second value distinguishes a valid empty JSON array from a failed command."
  (handler-case
      (let ((program (executable-find "notmuch"))
            (*project-process-timeout* *notmuch-process-timeout*))
        (unless program (return-from notmuch-run-json (values nil nil)))
        (multiple-value-bind (out err code)
            (run-project-program
             (cons (uiop:native-namestring program) args)
             :directory (or (ignore-errors (buffer-directory (current-buffer)))
                            (uiop:getcwd))
             :output-limit *notmuch-output-limit*)
          (declare (ignore err))
          (if (and (eql code 0) (plusp (length out)))
              (values (yason:parse out) t)
              (values nil nil))))
    (error () (values nil nil))))

(defun notmuch-string (value)
  "Coerce a JSON-derived VALUE to a display string (NIL -> \"\")."
  (cond ((null value) "")
        ((stringp value) value)
        (t (princ-to-string value))))

(defun notmuch-tags-string (tags)
  "Render a list of tag strings as \"(a b c)\", or \"\" when empty."
  (if (and (listp tags) tags)
      (format nil "(~{~a~^ ~})" (mapcar #'notmuch-string tags))
      ""))

;;; --- thread list buffer ----------------------------------------------------

(defvar *notmuch-search-mode-keymap*
  (make-keymap :description '*notmuch-search-mode-keymap*))
(defvar *notmuch-show-mode-keymap*
  (make-keymap :description '*notmuch-show-mode-keymap*))

(define-major-mode notmuch-search-mode nil
    (:name "Notmuch"
     :keymap *notmuch-search-mode-keymap*)
  ;; Nothing extra; the buffer is filled and made read-only by the caller.
  )

(define-major-mode notmuch-show-mode nil
    (:name "Notmuch-Show"
     :keymap *notmuch-show-mode-keymap*)
  (setf (buffer-read-only-p (current-buffer)) t))

(defmethod lem-vi-mode/core:mode-specific-keymaps ((mode notmuch-search-mode))
  (list *notmuch-search-mode-keymap*))

(defmethod lem-vi-mode/core:mode-specific-keymaps ((mode notmuch-show-mode))
  (list *notmuch-show-mode-keymap*))

(define-key *notmuch-search-mode-keymap* "Return" 'lem-yath-notmuch-open-thread)
(define-key *notmuch-search-mode-keymap* "q" 'quit-active-window)
(define-key *notmuch-search-mode-keymap* "g" 'lem-yath-notmuch-refresh)
(define-key *notmuch-show-mode-keymap* "q" 'quit-active-window)
(define-key *notmuch-show-mode-keymap* "g" 'lem-yath-notmuch-show-refresh)
(define-key *notmuch-show-mode-keymap* "Return" 'lem-yath-notmuch-open-part)

(defun notmuch-render-search (buffer threads query &optional selected-id)
  "Fill BUFFER with one line per thread in THREADS (parsed search JSON).
Stores QUERY and a line-number->thread-id map as buffer-local values, then
makes the buffer read-only and switches it to `notmuch-search-mode'."
  (with-buffer-read-only buffer nil
    (erase-buffer buffer)
    (let ((point (buffer-point buffer))
          (line->id (make-hash-table :test 'eql))
          (selected-line nil))
      (if (null threads)
          (insert-string point (format nil "No threads for query: ~a~%" query))
          (loop :for thread :in threads
                :for line :from 1
                :do (let ((id (notmuch-string (gethash "thread" thread)))
                          (date (notmuch-string (gethash "date_relative" thread)))
                          (authors (notmuch-string (gethash "authors" thread)))
                          (subject (notmuch-string (gethash "subject" thread)))
                          (tags (notmuch-tags-string (gethash "tags" thread))))
                      (setf (gethash line line->id) id)
                      (when (and selected-id (string= selected-id id))
                        (setf selected-line line))
                      (insert-string
                       point
                       (format nil "~13a  ~25a  ~a ~a~%"
                               date authors subject tags)))))
      (setf (buffer-value buffer 'notmuch-line->id) line->id)
      (setf (buffer-value buffer 'notmuch-query) query)
      (buffer-start point)
      (when selected-line (move-to-line point selected-line))))
  (change-buffer-mode buffer 'notmuch-search-mode)
  (setf (buffer-read-only-p buffer) t)
  buffer)

(defun notmuch-search (query &optional selected-id)
  "Run a newest-first `notmuch search' for QUERY and render the result list.
Degrades to a message when notmuch is missing or the query fails."
  (unless (notmuch-available-p)
    (message "notmuch not found on PATH")
    (return-from notmuch-search))
  (multiple-value-bind (result success-p)
      (notmuch-run-json
       (list "search" "--format=json"
             (format nil "--limit=~d" *notmuch-search-limit*)
             "--sort=newest-first" query))
    (cond
      ((not success-p)
       (message "notmuch search failed for: ~a" query))
      ((not (listp result))
       (message "Unexpected notmuch search output"))
      (t
       (let ((buffer (make-buffer *notmuch-list-buffer-name*)))
         (notmuch-render-search buffer result query selected-id)
         (switch-to-window (pop-to-buffer buffer))
         (message "~d thread~:p" (length result)))))))

(define-command lem-yath-notmuch () ()
  "Prompt for a notmuch query and show matching threads (M-x notmuch).
Defaults to \"tag:inbox\"; results are newest-first, one thread per line."
  (unless (notmuch-available-p)
    (message "notmuch not found on PATH")
    (return-from lem-yath-notmuch))
  (let ((query (prompt-for-string "notmuch query: "
                                  :initial-value *notmuch-default-query*
                                  :history-symbol 'lem-yath-notmuch)))
    (when (plusp (length query))
      (notmuch-search query))))

(define-command lem-yath-notmuch-refresh () ()
  "Re-run the current query in the *lem-yath-mail* list buffer (g)."
  (let ((buffer (current-buffer)))
    (let ((query (buffer-value buffer 'notmuch-query))
          (selected-id (notmuch-thread-id-at-point)))
      (if query
          (notmuch-search query selected-id)
          (message "No notmuch query to refresh")))))

;;; --- thread show buffer ----------------------------------------------------

(defun notmuch-thread-id-at-point ()
  "The thread id for the line at point in the *lem-yath-mail* buffer, or NIL."
  (let* ((buffer (current-buffer))
         (map (buffer-value buffer 'notmuch-line->id)))
    (when (hash-table-p map)
      (gethash (line-number-at-point (current-point)) map))))

(defun notmuch-message-id-at-point ()
  "The bare Message-ID for the message at point in a Notmuch show buffer."
  (let ((map (buffer-value (current-buffer) 'notmuch-line->message-id)))
    (when (hash-table-p map)
      (gethash (line-number-at-point (current-point)) map))))

(defun notmuch-attachment-at-point ()
  "The PDF attachment descriptor on the current show-buffer line, or NIL."
  (let ((map (buffer-value (current-buffer) 'notmuch-line->attachment)))
    (when (hash-table-p map)
      (gethash (line-number-at-point (current-point)) map))))

(defun notmuch-collect-text-parts (node acc)
  "Defensively walk a `notmuch show' NODE, pushing text/plain bodies onto ACC.
NODE may be a list (forest / part list / [message replies] pair) or a part
hash-table. Returns the updated accumulator (reversed at the call site)."
  (handler-case
      (cond
        ((null node) acc)
        ((listp node)
         (dolist (child node acc)
           (setf acc (notmuch-collect-text-parts child acc))))
        ((hash-table-p node)
         (let ((content-type (notmuch-string (gethash "content-type" node)))
               (content (gethash "content" node)))
           (cond
             ;; Leaf text/plain part with a string body.
             ((and (string-equal content-type "text/plain")
                   (stringp content))
              (cons content acc))
             ;; A multipart part: content is a list of sub-parts.
             ((listp content)
              (notmuch-collect-text-parts content acc))
             ;; A message object: descend into its body.
             ((gethash "body" node)
              (notmuch-collect-text-parts (gethash "body" node) acc))
             (t acc))))
        (t acc))
    (error () acc)))

(defun notmuch-collect-pdf-parts (node acc)
  "Defensively collect application/pdf leaf parts below NODE."
  (handler-case
      (cond
        ((null node) acc)
        ((listp node)
         (dolist (child node acc)
           (setf acc (notmuch-collect-pdf-parts child acc))))
        ((hash-table-p node)
         (let ((content-type (notmuch-string (gethash "content-type" node)))
               (content (gethash "content" node))
               (part-id (gethash "id" node)))
           (cond
             ((and (string-equal content-type "application/pdf")
                   (integerp part-id)
                   (<= 1 part-id 1000000))
              (cons node acc))
             ((listp content)
              (notmuch-collect-pdf-parts content acc))
             ((gethash "body" node)
              (notmuch-collect-pdf-parts (gethash "body" node) acc))
             (t acc))))
        (t acc))
    (error () acc)))

(defun notmuch-attachment-display-name (value)
  "Return one bounded, control-free basename for a MIME filename."
  (let* ((text (document-safe-display-text (notmuch-string value)))
         (slash (position-if (lambda (character)
                               (or (char= character #\/)
                                   (char= character #\\)))
                             text :from-end t))
         (name (string-trim '(#\Space #\Tab #\Newline #\Return)
                            (if slash (subseq text (1+ slash)) text)))
         (name (substitute #\Space #\Newline name)))
    (cond ((zerop (length name)) "attachment.pdf")
          ((> (length name) 256) (subseq name 0 256))
          (t name))))

(defun notmuch-collect-messages (node acc)
  "Walk the `notmuch show' tree NODE, collecting message hash-tables into ACC.
A message is a hash-table carrying a \"headers\" key."
  (handler-case
      (cond
        ((null node) acc)
        ((hash-table-p node)
         (if (gethash "headers" node)
             (let ((acc (cons node acc)))
               ;; Replies are not under this hash-table; they sit beside it in
               ;; the enclosing pair, so just return.
               acc)
             acc))
        ((listp node)
         (dolist (child node acc)
           (setf acc (notmuch-collect-messages child acc))))
        (t acc))
    (error () acc)))

(defun notmuch-render-message (point message attachment-map)
  "Insert MESSAGE headers, text/plain body, and PDF rows at POINT.

ATTACHMENT-MAP records the exact selectable line for every rendered PDF part."
  (let ((headers (gethash "headers" message)))
    (when (hash-table-p headers)
      (dolist (field '("From" "To" "Date" "Subject"))
        (let ((value (gethash field headers)))
          (when value
            (insert-string point (format nil "~a: ~a~%" field
                                         (notmuch-string value))))))))
  (insert-string point (format nil "~%"))
  (let* ((body (gethash "body" message))
         (parts (nreverse (notmuch-collect-text-parts body '()))))
    (if parts
        (dolist (part parts)
          (insert-string point part)
          (insert-string point (format nil "~%")))
        (insert-string point (format nil "[no text/plain body]~%"))))
  (let ((pdf-parts
          (nreverse (notmuch-collect-pdf-parts (gethash "body" message) '())))
        (message-id (notmuch-string (gethash "id" message))))
    (when pdf-parts
      (insert-string point (format nil "~%PDF attachments:~%"))
      (dolist (part pdf-parts)
        (let* ((line (line-number-at-point point))
               (filename
                 (notmuch-attachment-display-name (gethash "filename" part)))
               (attachment
                 (make-notmuch-attachment
                  :message-id message-id
                  :part-id (gethash "id" part)
                  :filename filename)))
          (setf (gethash line attachment-map) attachment)
          (insert-string point
                         (format nil "  [PDF] ~a  (Return to preview)~%"
                                 filename))))))
  (insert-string point (format nil "~%~a~%~%" (make-string 60 :initial-element #\-))))

(defun notmuch-show (thread-id)
  "Run `notmuch show' for THREAD-ID and render headers + text/plain bodies."
  (unless (notmuch-available-p)
    (message "notmuch not found on PATH")
    (return-from notmuch-show))
  (multiple-value-bind (tree success-p)
      (notmuch-run-json
       (list "show" "--format=json" "--include-html=false" thread-id))
    (unless success-p
      (message "notmuch show failed for ~a" thread-id)
      (return-from notmuch-show))
    (let* ((messages (nreverse (notmuch-collect-messages tree '())))
           (buffer (make-buffer (format nil "*lem-yath-mail: ~a*" thread-id)))
           (line->message-id (make-hash-table :test 'eql))
           (line->attachment (make-hash-table :test 'eql)))
      (with-buffer-read-only buffer nil
        (erase-buffer buffer)
        (let ((point (buffer-point buffer)))
          (if messages
              (dolist (message messages)
                (let ((start-line (line-number-at-point point))
                      (message-id (notmuch-string (gethash "id" message))))
                  (notmuch-render-message point message line->attachment)
                  (when (plusp (length message-id))
                    (loop :for line :from start-line
                          :to (line-number-at-point point)
                          :do (setf (gethash line line->message-id)
                                    message-id)))))
              (insert-string point (format nil "No messages in thread ~a~%" thread-id)))
          (setf (buffer-value buffer 'notmuch-thread-id) thread-id)
          (setf (buffer-value buffer 'notmuch-line->message-id)
                line->message-id)
          (setf (buffer-value buffer 'notmuch-line->attachment)
                line->attachment)
          (buffer-start point)))
      (change-buffer-mode buffer 'notmuch-show-mode)
      (setf (buffer-read-only-p buffer) t)
      (switch-to-window (pop-to-buffer buffer)))))

;;; --- PDF attachment preview ----------------------------------------------

(defun notmuch-private-temp-directory ()
  "Create and return a new owner-private attachment directory."
  #+sbcl
  (loop :repeat 64
        :for pathname :=
          (uiop:ensure-directory-pathname
           (merge-pathnames
            (format nil "lem-yath-notmuch-~d-~16,'0x/"
                    (sb-posix:getpid) (random (ash 1 60)))
            (uiop:temporary-directory)))
        :do (let ((created-p nil))
              (handler-case
                  (progn
                    (sb-posix:mkdir (uiop:native-namestring pathname) #o700)
                    (setf created-p t)
                    (let ((stat (sb-posix:lstat
                                 (uiop:native-namestring pathname))))
                      (unless
                          (and (= (sb-posix:stat-uid stat) (sb-posix:getuid))
                               (= (logand (sb-posix:stat-mode stat)
                                          sb-posix:s-ifmt)
                                  sb-posix:s-ifdir)
                               (zerop (logand (sb-posix:stat-mode stat)
                                             #o077)))
                        (error "Unsafe attachment directory")))
                    (return pathname))
                (error ()
                  (when created-p
                    (ignore-errors
                      (sb-posix:rmdir
                       (uiop:native-namestring pathname)))))))
        :finally (editor-error "Could not create a private attachment directory"))
  #-sbcl
  (editor-error "Secure attachment preview requires the supported SBCL runtime"))

(defun notmuch-message-id-query (message-id)
  "Return an exact, quoted notmuch id: query for MESSAGE-ID."
  (unless (and (stringp message-id)
               (plusp (length message-id))
               (<= (length message-id) 4096)
               (notany (lambda (character)
                         (or (char= character #\Null)
                             (char= character #\Newline)
                             (char= character #\Return)))
                       message-id))
    (editor-error "The attachment Message-ID is invalid"))
  (with-output-to-string (stream)
    (write-string "id:\"" stream)
    (loop :for character :across message-id
          :do (when (or (char= character #\\) (char= character #\"))
                (write-char #\\ stream))
              (write-char character stream))
    (write-char #\" stream)))

(defun notmuch-extract-raw-part (attachment pathname)
  "Extract ATTACHMENT's decoded raw bytes into a new private PATHNAME."
  #+sbcl
  (let ((notmuch (or (executable-find "notmuch")
                     (editor-error "notmuch not found on PATH")))
        (descriptor nil)
        (file-stream nil)
        (process nil)
        (finished-p nil)
        (complete-p nil)
        (*project-process-timeout* *notmuch-process-timeout*))
    (unwind-protect
         (progn
           (setf descriptor
                 (sb-posix:open
                  (uiop:native-namestring pathname)
                  (logior sb-posix:o-creat sb-posix:o-excl
                          sb-posix:o-wronly sb-posix:o-nofollow)
                  #o600))
           (sb-posix:fchmod descriptor #o600)
           (setf file-stream
                 (sb-sys:make-fd-stream
                  descriptor :output t :element-type '(unsigned-byte 8)
                  :buffering :full
                  :name (uiop:native-namestring pathname))
                 descriptor nil)
           (setf process
                 (uiop:launch-program
                  (project-timeout-command
                   (list (uiop:native-namestring notmuch)
                         "show" "--format=raw"
                         (format nil "--part=~d"
                                 (notmuch-attachment-part-id attachment))
                         (notmuch-message-id-query
                          (notmuch-attachment-message-id attachment))))
                  :directory (or (ignore-errors
                                   (buffer-directory (current-buffer)))
                                 (uiop:getcwd))
                  :input nil :output :stream :error-output nil
                  :element-type '(unsigned-byte 8)))
           (let ((octets (make-array 65536 :element-type '(unsigned-byte 8)))
                 (count 0))
             (with-open-stream (stdout (uiop:process-info-output process))
               (loop :for length := (read-sequence octets stdout)
                     :until (zerop length)
                     :do (incf count length)
                         (when (> count *notmuch-attachment-output-limit*)
                           (ignore-errors (uiop:terminate-process process))
                           (editor-error
                            "PDF attachment exceeds the ~d MiB preview limit"
                            (floor *notmuch-attachment-output-limit* 1048576)))
                         (write-sequence octets file-stream :end length))))
           (finish-output file-stream)
           (close file-stream)
           (setf file-stream nil)
           (let ((status (uiop:wait-process process)))
             (setf finished-p t)
             (unless (and (integerp status) (zerop status))
               (editor-error "notmuch could not extract PDF part ~d (exit ~a)"
                             (notmuch-attachment-part-id attachment) status)))
           (unless (with-open-file (stream pathname
                                           :element-type '(unsigned-byte 8))
                     (let ((magic (make-array 5 :element-type '(unsigned-byte 8))))
                       (and (= (read-sequence magic stream) 5)
                            (equalp magic #(37 80 68 70 45)))))
             (editor-error "The selected attachment is not a PDF file"))
           (setf complete-p t)
           pathname)
      (when file-stream (ignore-errors (close file-stream :abort t)))
      (when descriptor (ignore-errors (sb-posix:close descriptor)))
      (when (and process (not finished-p))
        (ignore-errors (uiop:terminate-process process))
        (ignore-errors (uiop:wait-process process)))
      (unless complete-p
        (ignore-errors (delete-file pathname)))))
  #-sbcl
  (declare (ignore attachment pathname))
  #-sbcl
  (editor-error "Secure attachment preview requires the supported SBCL runtime"))

(defun notmuch-remove-temp-attachment (pathname directory)
  (when pathname (ignore-errors (delete-file pathname)))
  #+sbcl
  (when directory
    (ignore-errors (sb-posix:rmdir (uiop:native-namestring directory)))))

(defun notmuch-delete-temp-attachment (&optional (buffer (current-buffer)))
  "Delete the private PDF attachment owned by BUFFER."
  (let ((pathname (buffer-value buffer 'notmuch-temp-attachment))
        (directory (buffer-value buffer 'notmuch-temp-directory)))
    (setf (buffer-value buffer 'notmuch-temp-attachment) nil
          (buffer-value buffer 'notmuch-temp-directory) nil)
    (notmuch-remove-temp-attachment pathname directory)))

(define-command lem-yath-notmuch-open-part () ()
  "Preview the PDF attachment on the current Notmuch show line."
  (alexandria:if-let ((attachment (notmuch-attachment-at-point)))
    (let* ((origin-window (current-window))
           (origin-buffer (current-buffer))
           (origin-pop-state
             (lem-core::window-pop-to-buffer-state origin-window))
           (directory (notmuch-private-temp-directory))
           (pathname (merge-pathnames "attachment.pdf" directory))
           (transferred-p nil))
      (unwind-protect
           (progn
             (notmuch-extract-raw-part attachment pathname)
             (let ((buffer
                     (document-open-buffer pathname :pdf :ephemeral-p t)))
               (setf (buffer-value buffer 'notmuch-temp-attachment) pathname
                     (buffer-value buffer 'notmuch-temp-directory) directory
                     (buffer-value buffer 'document-return-buffer) origin-buffer
                     (buffer-value buffer 'document-return-pop-state)
                     origin-pop-state)
               (add-hook (variable-value 'kill-buffer-hook :buffer buffer)
                         'notmuch-delete-temp-attachment)
               (setf transferred-p t)
               ;; Replace the show buffer in-place.  A nested pop-to split can
               ;; transfer or discard the show's own parent link; preserving
               ;; the same window avoids altering the surrounding tree.
               (switch-to-buffer buffer nil)
               (setf (lem-core::window-pop-to-buffer-state origin-window)
                     origin-pop-state)))
        (unless transferred-p
          (notmuch-remove-temp-attachment pathname directory))))
    (message "No PDF attachment on this line")))

(define-command lem-yath-notmuch-open-thread () ()
  "Open the thread on the current *lem-yath-mail* line in a read-only view (Return)."
  (let ((id (notmuch-thread-id-at-point)))
    (if id
        (notmuch-show id)
        (message "No thread on this line"))))

(define-command lem-yath-notmuch-show-refresh () ()
  "Refresh the currently displayed Notmuch thread (g)."
  (alexandria:if-let ((thread-id
                       (buffer-value (current-buffer) 'notmuch-thread-id)))
    (notmuch-show thread-id)
    (message "No Notmuch thread to refresh")))

;;; --- fetch mail ------------------------------------------------------------

(define-command lem-yath-fetchmail () ()
  "Fetch and index new mail: `mbsync -a && notmuch new' (yath/fetchmail).
Streams progress into *lem-yath-fetchmail*."
  (cond
    ((not (executable-find "mbsync"))
     (message "mbsync not found on PATH"))
    ((not (notmuch-available-p))
     (message "notmuch not found on PATH"))
    (t
     (stream-to-buffer (list "sh" "-c" "mbsync -a && notmuch new")
                       *notmuch-fetch-buffer-name*))))
