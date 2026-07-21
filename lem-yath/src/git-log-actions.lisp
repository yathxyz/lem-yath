;;;; Magit-compatible action dispatches in Legit log buffers.

(in-package :lem-yath)

(defparameter *legit-log-action-bindings*
  '(("A" . lem-yath-legit-cherry-pick)
    ("B" . lem-yath-legit-bisect)
    ("f" . lem-yath-legit-fetch)
    ("F" . lem-yath-legit-pull)
    ("b" . lem-yath-legit-branch)
    ("m" . lem-yath-legit-merge)
    ("-" . lem-yath-legit-revert-no-commit)
    ("_" . lem-yath-legit-revert)
    ("O" . lem-yath-legit-reset)
    ("p" . lem-yath-legit-push)
    ("Z" . lem-yath-legit-worktree)
    ("z" . lem-yath-legit-stash)
    ("M" . lem-yath-legit-remote)
    ("'" . lem-yath-legit-submodule)
    ("\"" . lem-yath-legit-subtree))
  "Effective Evil Collection Magit dispatches shared by log buffers.")

(defun install-legit-log-action-bindings ()
  "Install the implemented Magit action surface in Legit's log map."
  (dolist (binding *legit-log-action-bindings*)
    (define-key lem/legit::*legit-commits-log-keymap*
      (car binding) (cdr binding)))
  (define-key lem/legit::*legit-commits-log-keymap*
    "c" *legit-commit-dispatch-keymap*))

(install-legit-log-action-bindings)
