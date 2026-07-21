;;;; Magit-compatible action dispatches in Legit log buffers.

(in-package :lem-yath)

(defun legit-log-action-repository-root (&optional (directory (uiop:getcwd)))
  "Return the current Git top-level without presenting command failures."
  (let ((git (or (executable-find "git")
                 (editor-error "Git is unavailable.")))
        (*project-process-timeout* *legit-log-timeout*))
    (multiple-value-bind (output error-output status)
        (run-project-program
         (list (uiop:native-namestring git) "rev-parse" "--show-toplevel")
         :directory directory
         :environment
         (legit-rebase-child-environment
          "GIT_PAGER" "cat" "LC_ALL" "C")
         :output-limit *legit-log-output-limit*)
      (declare (ignore error-output))
      (and (eql status 0)
           (str:non-blank-string-p output)
           (str:trim output)))))

(defun legit-log-action-status-buffer-p ()
  (and (lem/legit::legit-status-active-p)
       (eq (current-window) lem/legit::*peek-window*)
       (string= (buffer-name (window-buffer lem/legit::*peek-window*))
                "*peek-legit*")))

(defun legit-log-action-status-directory ()
  (and (lem/legit::legit-status-active-p)
       (buffer-directory (window-buffer lem/legit::*peek-window*))))

(defun legit-log-find-commit (buffer hash)
  "Return HASH's heading position in BUFFER, or NIL."
  (when hash
    (with-point ((point (buffer-start-point buffer)))
      (loop
        (when (string= hash (or (text-property-at point :commit-hash) ""))
          (return (position-at-point point)))
        (unless (line-offset point 1)
          (return nil))))))

(defun legit-log-fallback-position (buffer line)
  "Return the position at bounded one-based LINE in BUFFER."
  (let ((point (buffer-start-point buffer)))
    (loop :repeat (max 0 (1- line))
          :while (line-offset point 1))
    (position-at-point point)))

(defun legit-log-restore-action-origin (state hash line)
  "Redisplay log STATE and restore HASH or its prior LINE."
  (legit-log-display state)
  (let* ((buffer (current-buffer))
         (target (or (legit-log-find-commit buffer hash)
                     (legit-log-fallback-position buffer line))))
    (move-to-position (buffer-point buffer) target)
    (lem/legit::show-matched-line)))

(defun legit-log-call-action (action)
  "Call ACTION and retain the originating log after ordinary status refresh."
  (lem/legit::with-current-project (vcs)
    (legit-log-require-git vcs)
    (let ((state (copy-legit-log-state (legit-log-buffer-state)))
          (hash (text-property-at (current-point) :commit-hash))
          (line (line-number-at-point (current-point)))
          (root (legit-log-action-repository-root)))
      (call-command action nil)
      ;; Do not override an intentional message, preview, list, or changed-root
      ;; transition.  Mutating actions in this port otherwise finish by showing
      ;; the ordinary status buffer, which Magit would refresh behind the log.
      (when (and root (legit-log-action-status-buffer-p)
                 (string=
                  root
                  (or (legit-log-action-repository-root
                       (legit-log-action-status-directory))
                      "")))
        (handler-case
            (legit-log-restore-action-origin state hash line)
          (error (condition)
            (message "History changed; staying in status: ~a" condition)))))))

(defmacro define-legit-log-action-wrapper (name action)
  `(define-command ,name () ()
     (legit-log-call-action ',action)))

(define-legit-log-action-wrapper lem-yath-legit-log-cherry-pick
  lem-yath-legit-cherry-pick)
(define-legit-log-action-wrapper lem-yath-legit-log-bisect
  lem-yath-legit-bisect)
(define-legit-log-action-wrapper lem-yath-legit-log-fetch
  lem-yath-legit-fetch)
(define-legit-log-action-wrapper lem-yath-legit-log-pull
  lem-yath-legit-pull)
(define-legit-log-action-wrapper lem-yath-legit-log-branch
  lem-yath-legit-branch)
(define-legit-log-action-wrapper lem-yath-legit-log-merge
  lem-yath-legit-merge)
(define-legit-log-action-wrapper lem-yath-legit-log-revert-no-commit
  lem-yath-legit-revert-no-commit)
(define-legit-log-action-wrapper lem-yath-legit-log-revert
  lem-yath-legit-revert)
(define-legit-log-action-wrapper lem-yath-legit-log-reset
  lem-yath-legit-reset)
(define-legit-log-action-wrapper lem-yath-legit-log-push
  lem-yath-legit-push)
(define-legit-log-action-wrapper lem-yath-legit-log-worktree
  lem-yath-legit-worktree)
(define-legit-log-action-wrapper lem-yath-legit-log-stash
  lem-yath-legit-stash)
(define-legit-log-action-wrapper lem-yath-legit-log-remote
  lem-yath-legit-remote)
(define-legit-log-action-wrapper lem-yath-legit-log-submodule
  lem-yath-legit-submodule)
(define-legit-log-action-wrapper lem-yath-legit-log-subtree
  lem-yath-legit-subtree)

(defparameter *legit-log-action-bindings*
  '(("A" . lem-yath-legit-log-cherry-pick)
    ("B" . lem-yath-legit-log-bisect)
    ("f" . lem-yath-legit-log-fetch)
    ("F" . lem-yath-legit-log-pull)
    ("b" . lem-yath-legit-log-branch)
    ("m" . lem-yath-legit-log-merge)
    ("-" . lem-yath-legit-log-revert-no-commit)
    ("_" . lem-yath-legit-log-revert)
    ("O" . lem-yath-legit-log-reset)
    ("p" . lem-yath-legit-log-push)
    ("Z" . lem-yath-legit-log-worktree)
    ("z" . lem-yath-legit-log-stash)
    ("M" . lem-yath-legit-log-remote)
    ("'" . lem-yath-legit-log-submodule)
    ("\"" . lem-yath-legit-log-subtree))
  "Effective Evil Collection Magit dispatches shared by log buffers.")

(defun install-legit-log-action-bindings ()
  "Install the implemented Magit action surface in Legit's log map."
  (dolist (binding *legit-log-action-bindings*)
    (define-key lem/legit::*legit-commits-log-keymap*
      (car binding) (cdr binding)))
  (define-key lem/legit::*legit-commits-log-keymap*
    "c" *legit-commit-dispatch-keymap*))

(install-legit-log-action-bindings)
