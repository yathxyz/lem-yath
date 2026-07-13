;;;; Buffer-local centered document view, matching yath/centered-view-mode.

(in-package :lem-yath)

(defvar *centered-view-width* 100
  "Preferred text width, in columns, for `centered-view-mode'.")

(defun centered-view-mark-visible-windows (buffer)
  "Request redisplay for every ordinary window currently showing BUFFER."
  (dolist (window (window-list))
    (when (eq buffer (window-buffer window))
      (lem-core::need-to-redraw window))))

(defun centered-view-enable ()
  (let ((buffer (current-buffer)))
    ;; The Emacs mode enables visual wrapping and intentionally leaves it
    ;; enabled after the centered presentation is turned off.
    (setf (variable-value 'line-wrap :buffer buffer) t)
    (centered-view-mark-visible-windows buffer)))

(defun centered-view-disable ()
  (centered-view-mark-visible-windows (current-buffer)))

(define-minor-mode centered-view-mode
    (:name "Center"
     :enable-hook 'centered-view-enable
     :disable-hook 'centered-view-disable))

(defmethod compute-window-content-width
    ((mode centered-view-mode) buffer window)
  (declare (ignore mode buffer window))
  (check-type *centered-view-width* (integer 1))
  *centered-view-width*)
