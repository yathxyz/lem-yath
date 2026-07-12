(in-package :lem-yath)

(defun org-test-report-path ()
  (or (uiop:getenv "LEM_YATH_ORG_REPORT")
      (error "LEM_YATH_ORG_REPORT is unset")))

(defun org-test-log (format-control &rest arguments)
  (with-open-file (stream (org-test-report-path)
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :append)
    (apply #'format stream format-control arguments)
    (terpri stream)
    (finish-output stream)))

(defun org-test-find (text &key line-start-p)
  (let ((point (current-point)))
    (buffer-start point)
    (unless (search-forward-regexp point (cl-ppcre:quote-meta-chars text))
      (error "Org test text not found: ~s" text))
    (when line-start-p
      (line-start point))
    point))

(defun org-test-attribute-name (text)
  (with-point ((point (org-test-find text)))
    (character-offset point (- (length text)))
    (let ((attribute (text-property-at point :attribute)))
      (cond ((symbolp attribute) (symbol-name attribute))
            (attribute (princ-to-string attribute))
            (t "NONE")))))

(defun org-test-binding (keys)
  (let ((command (find-keybind (lem-core::parse-keyspec keys))))
    (if (symbolp command) (symbol-name command) (princ-to-string command))))

(define-command lem-yath-test-org-static-report () ()
  (lem-core::syntax-scan-buffer (current-buffer))
  (org-test-log
   "STATIC mode=~a programming=~a heading=~a todo=~a drawer=~a timestamp=~a table=~a link=~a source=~a"
   (symbol-name (buffer-major-mode (current-buffer)))
   (if (programming-buffer-p (current-buffer)) "yes" "no")
   (org-test-attribute-name "Parent")
   (org-test-attribute-name "TODO")
   (org-test-attribute-name ":PROPERTIES:")
   (org-test-attribute-name "2026-07-12")
   (org-test-attribute-name "alpha")
   (org-test-attribute-name "target.org")
   (org-test-attribute-name "print('hello')"))
  (let ((tab (org-test-binding "Tab"))
        (little-t (org-test-binding "t"))
        (big-t (org-test-binding "T"))
        (return (org-test-binding "Return"))
        (control-return (org-test-binding "C-Return"))
        (control-shift-return (org-test-binding "C-Shift-Return"))
        (meta-o (org-test-binding "M-o")))
    (org-test-log
     (concatenate
      'string
      "KEYS tab-org=~a t-todo=~a T-todo=~a return-org=~a "
      "c-return-org=~a cs-return-org=~a m-o-other=~a")
     (if (member tab '("LEM-YATH-ORG-CYCLE" "LEM-YATH-SNIPPET-TAB")
                 :test #'string=)
         "yes" "no")
     (if (string= little-t "LEM-YATH-ORG-TODO") "yes" "no")
     (if (string= big-t "LEM-YATH-ORG-TODO") "yes" "no")
     (if (member return '("LEM-YATH-ORG-CYCLE" "LEM-YATH-ORG-META-RETURN")
                 :test #'string=)
         "yes" "no")
     (if (string= control-return "LEM-YATH-ORG-INSERT-HEADING")
         "yes" "no")
     (if (string= control-shift-return "LEM-YATH-ORG-INSERT-TODO-HEADING")
         "yes" "no")
     (if (member meta-o '("OTHER-WINDOW" "NEXT-WINDOW") :test #'string=)
         "yes" "no"))))

(define-command lem-yath-test-org-point-report () ()
  (org-test-log "POINT line=~d column=~d text=~s hidden=~a modified=~a folds=~d"
                (line-number-at-point (current-point))
                (point-charpos (current-point))
                (line-string (current-point))
                (if (org-line-hidden-p (current-point)) "yes" "no")
                (if (buffer-modified-p (current-buffer)) "yes" "no")
                (length (org-buffer-folds (current-buffer)))))

(defmacro define-org-test-goto-command (name text &optional line-start-p)
  `(define-command ,name () ()
     (move-point (current-point)
                 (org-test-find ,text :line-start-p ,line-start-p))))

(define-org-test-goto-command lem-yath-test-org-goto-parent "* TODO Parent" t)
(define-org-test-goto-command lem-yath-test-org-goto-parent-body
  "Parent body sentinel" t)
(define-org-test-goto-command lem-yath-test-org-goto-grand-body
  "Grand body sentinel" t)
(define-org-test-goto-command lem-yath-test-org-goto-list "- [ ] first" t)
(define-org-test-goto-command lem-yath-test-org-goto-table "alpha" nil)
(define-org-test-goto-command lem-yath-test-org-goto-indented-table
  "nested" nil)
(define-org-test-goto-command lem-yath-test-org-goto-hline-only
  "|---+-----|" t)
(define-org-test-goto-command lem-yath-test-org-goto-link "target.org" nil)
(define-org-test-goto-command lem-yath-test-org-goto-sibling "* Sibling" t)

(defun org-test-hook-count (function hooks)
  (count function hooks :key #'car :test #'eq))

(define-command lem-yath-test-org-reload-report () ()
  (handler-case
      (let ((source (asdf:system-source-directory "lem-yath")))
        (dotimes (_ 2)
          (declare (ignore _))
          (dolist (relative '("src/org/parser.lisp"
                              "src/org/folding.lisp"
                              "src/org/mode.lisp"
                              "src/org/commands.lisp"))
            (load (merge-pathnames relative source))))
        (org-test-log
         (concatenate
          'string
          "RELOAD post=~d change=~d kill=~d association=~d folds=~d "
          "tab=~a")
         (org-test-hook-count 'org-reveal-point-after-command
                              *post-command-hook*)
         (org-test-hook-count
          'org-clear-folds-after-change
          (variable-value 'after-change-functions
                          :buffer (current-buffer)))
         (org-test-hook-count
          'org-mode-kill-buffer-cleanup
          (variable-value 'kill-buffer-hook :buffer (current-buffer)))
         (count '("org" . org-mode)
                lem-core::*file-type-relationals*
                :test #'equal)
         (length (org-buffer-folds (current-buffer)))
         (if (member (org-test-binding "Tab")
                     '("LEM-YATH-ORG-CYCLE" "LEM-YATH-SNIPPET-TAB")
                     :test #'string=)
             "yes" "no")))
    (error (condition)
      (org-test-log "RELOAD error=~a" condition))))

(define-command lem-yath-test-org-eof-heading-report () ()
  (let* ((fixture (current-buffer))
         (path (merge-pathnames "eof.org" (workdir))))
    (find-file path)
    (buffer-start (current-point))
    (org-insert-heading-after-subtree)
    (insert-string (current-point) "After")
    (let ((contents (points-to-string (buffer-start-point (current-buffer))
                                      (buffer-end-point (current-buffer)))))
      (org-test-log "EOF text=~s"
                    (substitute #\| #\Newline contents)))
    (save-buffer (current-buffer))
    (kill-buffer (current-buffer))
    (switch-to-buffer fixture)))

(define-command lem-yath-test-org-kill-cleanup-report () ()
  (let* ((victim (current-buffer))
         (other-path (merge-pathnames "other.org" (workdir))))
    (find-file other-path)
    (let ((survivor (current-buffer)))
      (buffer-start (current-point))
      (org-fold-subtree (current-point))
      (kill-buffer victim)
      (org-test-log
       "KILL current=~a survivor-folds=~d victim-live=~a"
       (if (eq (current-buffer) survivor) "yes" "no")
       (length (org-buffer-folds survivor))
       (if (member victim (buffer-list)) "yes" "no")))))

(define-command lem-yath-test-org-return-fixture () ()
  (find-file (merge-pathnames "fixture.org" (workdir))))
