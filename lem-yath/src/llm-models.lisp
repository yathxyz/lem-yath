;;;; Cached, asynchronous OpenRouter model discovery.

(in-package :lem-yath)

(defparameter *llm-openrouter-model-fallback*
  '("openrouter/auto" "openrouter/free"))
(defparameter *llm-openrouter-model-count-limit* 10000)
(defparameter *llm-openrouter-model-id-limit* 512)
(defparameter *llm-openrouter-model-response-limit* (* 4 1024 1024))
(defparameter *llm-openrouter-model-cache-limit* (* 4 1024 1024))
(defparameter *llm-openrouter-model-refresh-timeout* 30)
(defparameter *llm-openrouter-model-refresh-idle-delay* 5)

(defvar *llm-openrouter-models* (copy-list *llm-openrouter-model-fallback*))
(defvar *llm-openrouter-model-source* :fallback)
(defvar *llm-openrouter-model-refresh-timer* nil)
(defvar *llm-openrouter-model-refresh-running-p* nil)
(defvar *llm-openrouter-model-refresh-generation* 0)
(defvar *llm-openrouter-model-refresh-lock*
  (bt2:make-lock :name "lem-yath/openrouter-model-refresh"))

(defun llm-openrouter-api-key ()
  "Return the OpenRouter-specific API key, if configured."
  (let ((key (uiop:getenv "OPENROUTER_API_KEY")))
    (and key (plusp (length key)) key)))

(defun llm-openrouter-models-url ()
  (if (llm-openrouter-api-key)
      "https://openrouter.ai/api/v1/models/user"
      "https://openrouter.ai/api/v1/models"))

(defun llm-openrouter-model-headers ()
  (alexandria:when-let ((key (llm-openrouter-api-key)))
    `(("Authorization" . ,(format nil "Bearer ~a" key)))))

(defun llm-openrouter-model-id-valid-p (value)
  (and (stringp value)
       (plusp (length value))
       (<= (length value) *llm-openrouter-model-id-limit*)
       (every (lambda (character)
                (let ((code (char-code character)))
                  (and (>= code 32) (/= code 127))))
              value)))

(defun llm-openrouter-normalize-models (values &key strict)
  "Return bounded, ordered, duplicate-free model ids from VALUES."
  (let ((values (llm-json-elements values)))
    (when (> (length values) *llm-openrouter-model-count-limit*)
      (error "OpenRouter model list exceeds the entry limit"))
    (when (and strict
               (not (every #'llm-openrouter-model-id-valid-p values)))
      (error "OpenRouter model cache contains an invalid model id"))
    (remove-duplicates
     (remove-if-not #'llm-openrouter-model-id-valid-p values)
     :test #'string=
     :from-end t)))

(defun llm-openrouter-model-cache-override ()
  (uiop:getenv "LEM_YATH_OPENROUTER_MODEL_CACHE"))

(defun llm-openrouter-model-cache-pathname ()
  "Return the JSON model cache pathname."
  (alexandria:if-let ((override (llm-openrouter-model-cache-override)))
    (uiop:parse-native-namestring override)
    (let ((cache-home
            (alexandria:if-let ((xdg (uiop:getenv "XDG_CACHE_HOME")))
              (uiop:ensure-directory-pathname
               (uiop:parse-native-namestring xdg))
              (merge-pathnames ".cache/" (user-homedir-pathname)))))
      (merge-pathnames "lem-yath/openrouter/models.json" cache-home))))

(defun llm-openrouter-model-cache-directory-private-p (directory)
  #+sbcl
  (let ((stat (sb-posix:stat (uiop:native-namestring directory))))
    (and (= (sb-posix:stat-uid stat) (sb-posix:getuid))
         (zerop (logand (sb-posix:stat-mode stat) #o077))))
  #-sbcl
  (declare (ignore directory))
  #-sbcl nil)

(defun llm-openrouter-prepare-cache-directory ()
  "Create or validate the private model-cache directory."
  (let* ((pathname (llm-openrouter-model-cache-pathname))
         (directory (uiop:pathname-directory-pathname pathname))
         (existed (uiop:directory-exists-p directory)))
    (ensure-directories-exist pathname)
    #+sbcl
    (if (and (llm-openrouter-model-cache-override) existed)
        (unless (llm-openrouter-model-cache-directory-private-p directory)
          (error "OpenRouter model cache override directory must be private and user-owned"))
        (sb-posix:chmod (uiop:native-namestring directory) #o700))
    #-sbcl
    (error "Safe OpenRouter model caching requires SBCL")
    directory))

(defun llm-openrouter-validate-cache-file (pathname)
  (when (uiop:file-exists-p pathname)
    #+sbcl
    (let ((stat (sb-posix:lstat (uiop:native-namestring pathname))))
      (unless (and (= (logand (sb-posix:stat-mode stat) sb-posix:s-ifmt)
                      sb-posix:s-ifreg)
                   (= (sb-posix:stat-uid stat) (sb-posix:getuid))
                   (zerop (logand (sb-posix:stat-mode stat) #o077)))
        (error "OpenRouter model cache must be a private user-owned regular file")))
    #-sbcl
    (error "Safe OpenRouter model caching requires SBCL")))

(defun llm-openrouter-read-cache-text (pathname)
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((length (file-length stream)))
      (when (> length *llm-openrouter-model-cache-limit*)
        (error "OpenRouter model cache exceeds the size limit"))
      (let ((octets (make-array length :element-type '(unsigned-byte 8))))
        (unless (= length (read-sequence octets stream))
          (error "Could not read the complete OpenRouter model cache"))
        #+sbcl (sb-ext:octets-to-string octets :external-format :utf-8)
        #-sbcl (error "UTF-8 model cache decoding requires SBCL")))))

(defun llm-openrouter-read-model-cache ()
  "Read and validate the model cache without evaluating Lisp data."
  (handler-case
      (let ((pathname (llm-openrouter-model-cache-pathname)))
        (when (uiop:file-exists-p pathname)
          (llm-openrouter-validate-cache-file pathname)
          (let* ((object (yason:parse
                          (llm-openrouter-read-cache-text pathname)))
                 (version (and (hash-table-p object)
                               (gethash "version" object)))
                 (models (and (hash-table-p object)
                              (gethash "models" object))))
            (when (and (eql version 1) models)
              (llm-openrouter-normalize-models models :strict t)))))
    (error () nil)))

(defun llm-openrouter-cache-temporary-pathname (pathname)
  (uiop:parse-native-namestring
   (format nil "~a.tmp.~d.~16,'0x"
           (uiop:native-namestring pathname)
           #+sbcl (sb-posix:getpid)
           #-sbcl 0
           (random (ash 1 60)))))

(defun llm-openrouter-model-cache-json (models)
  (with-output-to-string (stream)
    (yason:encode
     (llm-json-object "version" 1 "models" (coerce models 'vector))
     stream)
    (terpri stream)))

(defun llm-openrouter-write-model-cache (models)
  "Atomically replace the private model cache with MODELS."
  (let* ((pathname (llm-openrouter-model-cache-pathname))
         (temporary (llm-openrouter-cache-temporary-pathname pathname))
         (text (llm-openrouter-model-cache-json models))
         (octets
           #+sbcl (sb-ext:string-to-octets text :external-format :utf-8)
           #-sbcl (error "UTF-8 model cache encoding requires SBCL"))
         (descriptor nil)
         (stream nil))
    (when (> (length octets) *llm-openrouter-model-cache-limit*)
      (error "OpenRouter model cache exceeds the size limit"))
    (llm-openrouter-prepare-cache-directory)
    (llm-openrouter-validate-cache-file pathname)
    (unwind-protect
         (progn
           #+sbcl
           (progn
             (setf descriptor
                   (sb-posix:open
                    (uiop:native-namestring temporary)
                    (logior sb-posix:o-creat sb-posix:o-excl
                            sb-posix:o-wronly sb-posix:o-nofollow)
                    #o600))
             (sb-posix:fchmod descriptor #o600)
             (setf stream
                   (sb-sys:make-fd-stream
                    descriptor :output t :element-type '(unsigned-byte 8)
                    :buffering :full
                    :name (uiop:native-namestring temporary)))
             (write-sequence octets stream)
             (finish-output stream)
             (sb-posix:fsync descriptor)
             (close stream)
             (setf stream nil descriptor nil))
           #-sbcl
           (error "Safe OpenRouter model caching requires SBCL")
           (uiop:rename-file-overwriting-target temporary pathname))
      (when stream (ignore-errors (close stream :abort t)))
      (when descriptor (ignore-errors (sb-posix:close descriptor)))
      (when (uiop:file-exists-p temporary)
        (ignore-errors (delete-file temporary))))))

(defun llm-openrouter-fetch-models ()
  "Synchronously fetch a bounded model list; call only off the editor thread."
  (let ((*project-process-timeout*
          (+ *llm-openrouter-model-refresh-timeout* 2)))
    (multiple-value-bind (stdout stderr status)
        (run-project-program
         (llm-curl-arguments *llm-openrouter-model-refresh-timeout*)
         :input (llm-curl-config
                 "GET" (llm-openrouter-models-url)
                 (llm-openrouter-model-headers))
         :output-limit *llm-openrouter-model-response-limit*)
      (declare (ignore stderr))
      (unless (and (integerp status) (zerop status))
        (error "OpenRouter model refresh failed (exit ~a)" status))
      (let* ((object (handler-case (yason:parse stdout)
                       (error () (error "OpenRouter returned malformed JSON"))))
             (entries (and (hash-table-p object) (gethash "data" object))))
        (unless entries
          (error "OpenRouter model refresh returned no data"))
        (let ((models
                (llm-openrouter-normalize-models
                 (loop :for entry :in (llm-json-elements entries)
                       :when (hash-table-p entry)
                         :collect (gethash "id" entry)))))
          (unless models
            (error "OpenRouter model refresh returned no models"))
          models)))))

(defun llm-openrouter-apply-models (models source &key quiet)
  (let ((models (llm-openrouter-normalize-models models :strict t)))
    (unless models (error "Cannot apply an empty OpenRouter model list"))
    (setf *llm-openrouter-models* models
          *llm-openrouter-model-source* source)
    (when (and (eq *llm-backend* :openrouter)
               (not (member *llm-model* models :test #'string=)))
      (setf *llm-model* (first models)))
    (unless quiet
      (message "OpenRouter: loaded ~d models from ~(~a~)"
               (length models) source))))

(defun llm-openrouter-finish-refresh (generation models condition quiet)
  (when (and models (= generation *llm-openrouter-model-refresh-generation*))
    (llm-openrouter-apply-models models :network :quiet quiet))
  (when (and condition (not quiet))
    (message "OpenRouter: model refresh failed (~a)" condition)))

(defun llm-openrouter-start-model-refresh (&key quiet)
  "Start one asynchronous model refresh; return NIL if one already runs."
  (let ((claimed nil)
        (generation *llm-openrouter-model-refresh-generation*))
    (bt2:with-lock-held (*llm-openrouter-model-refresh-lock*)
      (unless *llm-openrouter-model-refresh-running-p*
        (setf *llm-openrouter-model-refresh-running-p* t
              claimed t)))
    (when claimed
      (handler-case
          (bt2:make-thread
           (lambda ()
             (let ((models nil)
                   (condition nil))
               (unwind-protect
                    (handler-case
                        (let ((fetched (llm-openrouter-fetch-models)))
                          (llm-openrouter-write-model-cache fetched)
                          (setf models fetched))
                      (error (error) (setf condition error)))
                 (bt2:with-lock-held (*llm-openrouter-model-refresh-lock*)
                   (setf *llm-openrouter-model-refresh-running-p* nil)))
               (send-event
                (lambda ()
                  (llm-openrouter-finish-refresh
                   generation models condition quiet)))))
           :name "lem-yath/openrouter-model-refresh")
        (error (condition)
          (bt2:with-lock-held (*llm-openrouter-model-refresh-lock*)
            (setf *llm-openrouter-model-refresh-running-p* nil))
          (unless quiet
            (message "OpenRouter: could not start model refresh (~a)"
                     condition))
          (setf claimed nil)))
    claimed)))

(defun llm-openrouter-stop-model-refresh-timer ()
  (alexandria:when-let ((timer *llm-openrouter-model-refresh-timer*))
    (setf *llm-openrouter-model-refresh-timer* nil)
    (ignore-errors (stop-timer timer))))

(defun llm-openrouter-automatic-refresh-p ()
  (let ((setting (uiop:getenv "LEM_YATH_OPENROUTER_MODEL_REFRESH")))
    (not (member (string-downcase (or setting "1"))
                 '("0" "false" "no") :test #'string=))))

(defun llm-openrouter-schedule-model-refresh ()
  (llm-openrouter-stop-model-refresh-timer)
  (when (llm-openrouter-automatic-refresh-p)
    (let (timer)
      (setf timer
            (make-idle-timer
             (lambda ()
               (when (eq timer *llm-openrouter-model-refresh-timer*)
                 (setf *llm-openrouter-model-refresh-timer* nil)
                 (llm-openrouter-start-model-refresh :quiet t)))
             :name "lem-yath OpenRouter model refresh"))
      (setf *llm-openrouter-model-refresh-timer*
            (start-timer
             timer (* 1000 *llm-openrouter-model-refresh-idle-delay*)
             :repeat nil)))))

(defun initialize-llm-openrouter-models ()
  (incf *llm-openrouter-model-refresh-generation*)
  (alexandria:when-let ((cached (llm-openrouter-read-model-cache)))
    (llm-openrouter-apply-models cached :cache :quiet t))
  (llm-openrouter-schedule-model-refresh))

(define-command lem-yath-openrouter-refresh-models () ()
  "Refresh the OpenRouter model catalog asynchronously."
  (if (llm-openrouter-start-model-refresh)
      (message "OpenRouter: refreshing models")
      (message "OpenRouter: model refresh already running")))

(define-command lem-yath-llm-set-model () ()
  "Select a discovered OpenRouter model or set another backend's model."
  (let* ((openrouter-p (eq *llm-backend* :openrouter))
         (models (and openrouter-p *llm-openrouter-models*))
         (choice
           (prompt-for-string
            "Model: "
            :completion-function
            (and models
                 (lambda (string) (prescient-filter string models)))
            :initial-value *llm-model*
            :history-symbol 'lem-yath-llm-model)))
    (cond
      ((zerop (length choice)) nil)
      ((and openrouter-p (not (member choice models :test #'string=)))
       (message "Unknown OpenRouter model: ~a" choice))
      (t
       (setf *llm-model* choice)
       (message "LLM model: ~a" choice)))))

(initialize-editor-feature 'initialize-llm-openrouter-models)
