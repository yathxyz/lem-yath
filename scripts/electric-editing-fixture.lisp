(in-package :lem-yath)

(defvar *electric-editing-report*
  (uiop:getenv "LEM_YATH_ELECTRIC_EDITING_REPORT"))

(define-major-mode lem-yath-electric-hook-test-mode ()
    (:name "ElectricHook"))

(defvar *electric-editing-before-count* 0)
(defvar *electric-editing-after-count* 0)

(defmethod execute :before
    ((mode lem-yath-electric-hook-test-mode)
     (command lem-core/commands/edit:self-insert)
     argument)
  (declare (ignore mode command argument))
  (incf *electric-editing-before-count*))

(defmethod execute :after
    ((mode lem-yath-electric-hook-test-mode)
     (command lem-core/commands/edit:self-insert)
     argument)
  (declare (ignore mode command argument))
  (incf *electric-editing-after-count*))

(defun electric-editing-log (control &rest arguments)
  (with-open-file (stream *electric-editing-report*
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (apply #'format stream control arguments)
    (terpri stream)))

(defun electric-editing-buffer-text (&optional (buffer (current-buffer)))
  (points-to-string (buffer-start-point buffer) (buffer-end-point buffer)))

(defun electric-editing-hex (string)
  (with-output-to-string (stream)
    (loop :for character :across string
          :do (format stream "~2,'0x" (char-code character)))))

(defun electric-editing-global-mode-name ()
  (cond
    ((typep (current-global-mode) 'lem-vi-mode:vi-mode) "vi")
    ((typep (current-global-mode) 'lem-core::emacs-mode) "emacs")
    (t "other")))

(defun electric-editing-record (&optional label)
  (let* ((buffer (current-buffer))
         (point (current-point))
         (mark (cursor-mark point))
         (mark-point (mark-point mark)))
    (electric-editing-log
     (concatenate
      'string
      "RESULT label=~a text-hex=~a point=~d mark=~a mark-point=~a "
      "readonly=~a global=~a paredit=~a")
     (or label
         (buffer-value buffer :electric-editing-label)
         (buffer-name buffer))
     (electric-editing-hex (electric-editing-buffer-text buffer))
     (position-at-point point)
     (if (mark-active-p mark) "yes" "no")
     (if mark-point (position-at-point mark-point) "none")
     (if (buffer-read-only-p buffer) "yes" "no")
     (electric-editing-global-mode-name)
     (if (mode-active-p buffer 'lem-paredit-mode:paredit-mode)
         "yes"
         "no"))))

(defun electric-editing-setup-region
    (label point-position mark-position &key read-only)
  ;; Vi turns every active mark into VISUAL.  Switch through the real global
  ;; mode command so this fixture exercises the Emacs-style region contract.
  (lem-core::emacs-mode)
  (let ((buffer (current-buffer)))
    (setf (buffer-read-only-p buffer) nil)
    (buffer-mark-cancel buffer)
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (insert-string (buffer-end-point buffer) "abcdef"))
    (buffer-start (buffer-point buffer))
    (character-offset (buffer-point buffer) point-position)
    (with-point ((mark (buffer-start-point buffer)))
      (character-offset mark mark-position)
      (setf (buffer-mark buffer) mark))
    (clear-buffer-edit-history buffer)
    (setf (buffer-value buffer :electric-editing-label) label
          (buffer-read-only-p buffer) (not (null read-only)))
    (electric-editing-log "SETUP label=~a" label)))

(defun electric-editing-setup-text-region
    (label text point-position mark-position)
  (lem-core::emacs-mode)
  (let ((buffer (current-buffer)))
    (setf (buffer-read-only-p buffer) nil)
    (buffer-mark-cancel buffer)
    (erase-buffer buffer)
    (insert-string (buffer-end-point buffer) text)
    (buffer-start (buffer-point buffer))
    (character-offset (buffer-point buffer) point-position)
    (with-point ((mark (buffer-start-point buffer)))
      (character-offset mark mark-position)
      (setf (buffer-mark buffer) mark))
    (clear-buffer-edit-history buffer)
    (setf (buffer-value buffer :electric-editing-label) label)
    (electric-editing-log "SETUP label=~a" label)))

(define-command lem-yath-test-electric-record () ()
  (electric-editing-record))

(define-command lem-yath-test-electric-forward-replace () ()
  (electric-editing-setup-region "forward-replace" 1 4))

(define-command lem-yath-test-electric-reverse-replace () ()
  (electric-editing-setup-region "reverse-replace" 4 1))

(define-command lem-yath-test-electric-forward-wrap () ()
  (electric-editing-setup-region "forward-wrap" 1 4))

(define-command lem-yath-test-electric-reverse-wrap () ()
  (electric-editing-setup-region "reverse-wrap" 4 1))

(define-command lem-yath-test-electric-quote-wrap () ()
  (electric-editing-setup-region "quote-wrap" 1 4))

(define-command lem-yath-test-electric-reverse-quote-wrap () ()
  (electric-editing-setup-region "reverse-quote-wrap" 4 1))

(define-command lem-yath-test-electric-zero-mark () ()
  (electric-editing-setup-region "zero-mark" 3 3))

(define-command lem-yath-test-electric-read-only () ()
  (electric-editing-setup-region "read-only" 1 4 :read-only t))

(define-command lem-yath-test-electric-wrap-undo () ()
  (electric-editing-setup-region "wrap-undo" 1 4))

(define-command lem-yath-test-electric-lisp-wrap () ()
  (electric-editing-setup-region "lisp-wrap" 1 4))

(define-command lem-yath-test-electric-lisp-reverse-wrap () ()
  (electric-editing-setup-region "lisp-reverse-wrap" 4 1))

(define-command lem-yath-test-electric-lisp-quote-wrap () ()
  (electric-editing-setup-region "lisp-quote-wrap" 1 4))

(define-command lem-yath-test-electric-lisp-reverse-quote-wrap () ()
  (electric-editing-setup-region "lisp-reverse-quote-wrap" 4 1))

(define-command lem-yath-test-electric-lisp-quote-escape-backslash () ()
  (electric-editing-setup-text-region
   "lisp-quote-escape-backslash" "ab\\cz" 1 4))

(define-command lem-yath-test-electric-lisp-quote-escape-quote () ()
  (electric-editing-setup-text-region
   "lisp-quote-escape-quote" "ab\"q\"cz" 1 6))

(define-command lem-yath-test-electric-lisp-replace () ()
  (electric-editing-setup-region "lisp-replace" 1 4))

(define-command lem-yath-test-electric-empty-emacs () ()
  (lem-core::emacs-mode)
  (let ((buffer (current-buffer)))
    (setf (buffer-read-only-p buffer) nil)
    (buffer-mark-cancel buffer)
    (erase-buffer buffer)
    (clear-buffer-edit-history buffer)
    (setf (buffer-value buffer :electric-editing-label) "empty-emacs")
    (electric-editing-log "SETUP label=empty-emacs")))

(define-command lem-yath-test-electric-count-existing () ()
  (lem-core::emacs-mode)
  (let ((buffer (current-buffer)))
    (setf (buffer-read-only-p buffer) nil)
    (buffer-mark-cancel buffer)
    (erase-buffer buffer)
    (insert-string (buffer-point buffer) "  )")
    (buffer-start (buffer-point buffer))
    (clear-buffer-edit-history buffer)
    (setf (buffer-value buffer :electric-editing-label) "count-existing")
    (electric-editing-log "SETUP label=count-existing")))

(defun electric-editing-setup-count-seed (label text offset)
  (lem-core::emacs-mode)
  (let ((buffer (current-buffer)))
    (setf (buffer-read-only-p buffer) nil)
    (buffer-mark-cancel buffer)
    (erase-buffer buffer)
    (insert-string (buffer-point buffer) text)
    (buffer-start (buffer-point buffer))
    (character-offset (buffer-point buffer) offset)
    (clear-buffer-edit-history buffer)
    (setf (buffer-value buffer :electric-editing-label) label)
    (electric-editing-log "SETUP label=~a" label)))

(define-command lem-yath-test-electric-count-quote-existing () ()
  (electric-editing-setup-count-seed "count-quote-existing" "\"" 0))

(define-command lem-yath-test-electric-count-odd-escape () ()
  (electric-editing-setup-count-seed "count-odd-escape" "\\z" 1))

(define-command lem-yath-test-electric-count-even-escape () ()
  (electric-editing-setup-count-seed "count-even-escape" "\\\\z" 2))

(define-command lem-yath-test-electric-lisp-completion-setup () ()
  (lem/completion-mode:completion-end)
  (auto-completion-cancel-timer)
  (let* ((buffer (current-buffer))
         (mode (buffer-major-mode buffer))
         (source (or (get-buffer "*electric-pair-source*")
                     (make-buffer "*electric-pair-source*"))))
    (erase-buffer buffer)
    (change-buffer-mode source mode)
    (with-current-buffer source
      (erase-buffer source)
      (insert-string (buffer-point source) "alphaCandidate\n"))
    (setf (buffer-value buffer :electric-editing-label) "lisp-completion")
    (electric-editing-log "SETUP label=lisp-completion")))

(define-command lem-yath-test-electric-hook-setup () ()
  (lem/completion-mode:completion-end)
  (lem-core::emacs-mode)
  (change-buffer-mode (current-buffer) 'lem-yath-electric-hook-test-mode)
  (erase-buffer (current-buffer))
  (setf *electric-editing-before-count* 0
        *electric-editing-after-count* 0)
  (electric-editing-log "SETUP label=mode-hooks"))

(define-command lem-yath-test-electric-hook-record () ()
  (electric-editing-log
   "HOOKS before=~d after=~d text-hex=~a"
   *electric-editing-before-count*
   *electric-editing-after-count*
   (electric-editing-hex (electric-editing-buffer-text))))

(dolist (keymap (list *global-keymap*
                      lem-vi-mode:*normal-keymap*
                      lem-vi-mode:*insert-keymap*
                      lem-vi-mode:*visual-keymap*))
  (define-key keymap "F12" 'lem-yath-test-electric-record)
  (define-key keymap "F10" 'lem-yath-test-electric-hook-record))

(electric-editing-log "READY")
