;;;; Stacked GNU Org and Evil-Org agenda filters for the bounded agenda view.

(in-package :lem-yath)

(defparameter *agenda-filter-effort-values*
  '("0" "0:10" "0:30" "1:00" "2:00" "3:00" "4:00" "5:00"
    "6:00" "7:00")
  "The pinned Org default Effort choices, addressed by 1..9 and 0.")

(defparameter *agenda-filter-duration-unit-minutes*
  '(("min" . 1d0) ("h" . 60d0) ("d" . 1440d0)
    ("w" . 10080d0) ("m" . 43200d0) ("y" . 525960d0)))

(defparameter *agenda-filter-duration-token-scanner*
  (ppcre:create-scanner
   "([0-9]+(?:\\.[0-9]*)?)\\s*(min|h|d|w|m|y)"))

(defstruct (agenda-filter-condition
            (:constructor make-agenda-filter-condition))
  value negative-p scanner operator minutes)

(defstruct (agenda-filter-state (:constructor make-agenda-filter-state))
  category tags regexps efforts top-headline limit limit-generation)

(defun agenda-filter-state (buffer)
  (or (buffer-value buffer 'lem-yath-agenda-filter-state)
      (setf (buffer-value buffer 'lem-yath-agenda-filter-state)
            (make-agenda-filter-state))))

(defun agenda-filter-prefix-magnitude (argument)
  (if argument (org-prefix-magnitude argument) 0))

(defun agenda-filter-decimal (value)
  "Parse a non-negative decimal VALUE without invoking the Lisp reader."
  (let ((dot (position #\. value)))
    (if dot
        (let* ((whole (if (zerop dot) 0 (parse-integer value :end dot)))
               (fraction-text (subseq value (1+ dot)))
               (scale (expt 10 (length fraction-text)))
               (fraction (if (zerop (length fraction-text))
                             0
                             (parse-integer fraction-text))))
          (+ (coerce whole 'double-float)
             (/ (coerce fraction 'double-float) scale)))
        (coerce (parse-integer value) 'double-float))))

(defun agenda-filter-hms-minutes (value)
  (let ((parts (uiop:split-string value :separator '(#\:))))
    (when (member (length parts) '(2 3))
      (let ((hours (parse-integer (first parts)))
            (minutes (parse-integer (second parts)))
            (seconds (and (third parts) (parse-integer (third parts)))))
        (+ (* 60d0 hours) minutes (/ (or seconds 0) 60d0))))))

(defun agenda-filter-unit-minutes (value)
  (let ((total 0d0))
    (ppcre:do-register-groups (number unit)
        (*agenda-filter-duration-token-scanner* value)
      (incf total
            (* (agenda-filter-decimal number)
               (or (cdr (assoc unit *agenda-filter-duration-unit-minutes*
                               :test #'string=))
                   (error "Unknown duration unit: ~a" unit)))))
    total))

(defun agenda-filter-duration-minutes (value)
  "Translate Org duration VALUE to minutes using the pinned default units."
  (when value
    (let ((trimmed (string-trim '(#\Space #\Tab #\Return) value)))
      (unless (agenda-duration-p trimmed)
        (error "Invalid duration format: ~s" value))
      (cond
        ((string= trimmed "") 0d0)
        ((ppcre:scan "^[0-9]+(?::[0-9]{2}){1,2}$" trimmed)
         (agenda-filter-hms-minutes trimmed))
        ((ppcre:scan "^[0-9]+(?:\\.[0-9]*)?$" trimmed)
         (agenda-filter-decimal trimmed))
        (t
         (multiple-value-bind (start end registers register-ends)
             (ppcre:scan "([0-9]+(?::[0-9]{2}){1,2})\\s*$" trimmed)
           (declare (ignore end))
           (if (and start registers (plusp start))
               (+ (agenda-filter-unit-minutes (subseq trimmed 0 start))
                  (agenda-filter-hms-minutes
                   (subseq trimmed (aref registers 0)
                           (aref register-ends 0))))
               (agenda-filter-unit-minutes trimmed))))))))

(defun agenda-filter-condition-match-p (condition matched-p)
  (if (agenda-filter-condition-negative-p condition)
      (not matched-p)
      matched-p))

(defun agenda-filter-category-match-p (condition item)
  (agenda-filter-condition-match-p
   condition
   (string= (agenda-filter-condition-value condition)
            (or (agenda-item-category item) ""))))

(defun agenda-filter-top-headline-match-p (condition item)
  (agenda-filter-condition-match-p
   condition
   (string= (agenda-filter-condition-value condition)
            (or (agenda-item-top-headline item) ""))))

(defun agenda-filter-tag-match-p (condition item)
  (let ((tag (agenda-filter-condition-value condition))
        (tags (agenda-item-tags item)))
    (agenda-filter-condition-match-p
     condition
     (if (string= tag "")
         (not (null tags))
         (not (null (member tag tags :test #'string=)))))))

(defun agenda-filter-regexp-match-p (condition item)
  (agenda-filter-condition-match-p
   condition
   (not (null
         (ppcre:scan (agenda-filter-condition-scanner condition)
                     (agenda-display-line item))))))

(defun agenda-filter-effort-match-p (condition item)
  (let ((effort
          (if (agenda-item-effort item)
              (agenda-filter-duration-minutes (agenda-item-effort item))
              most-positive-fixnum))
        (threshold (agenda-filter-condition-minutes condition)))
    (agenda-filter-condition-match-p
     condition
     (ecase (agenda-filter-condition-operator condition)
       (#\< (<= effort threshold))
       (#\> (>= effort threshold))
       (#\= (= effort threshold))))))

(defun agenda-filter-item-visible-p (buffer item)
  "Whether ITEM satisfies every active agenda filter in BUFFER."
  (let ((state (agenda-filter-state buffer)))
    (and
     (or (null (agenda-filter-state-category state))
         (agenda-filter-category-match-p
          (agenda-filter-state-category state) item))
     (or (null (agenda-filter-state-top-headline state))
         (agenda-filter-top-headline-match-p
          (agenda-filter-state-top-headline state) item))
     (every (lambda (condition)
              (agenda-filter-tag-match-p condition item))
            (agenda-filter-state-tags state))
     (every (lambda (condition)
              (agenda-filter-regexp-match-p condition item))
            (agenda-filter-state-regexps state))
     (every (lambda (condition)
              (agenda-filter-effort-match-p condition item))
            (agenda-filter-state-efforts state)))))

(defun agenda-filter-effective-limit (buffer)
  (let ((state (agenda-filter-state buffer)))
    (when (and (agenda-filter-state-limit state)
               (= (or (agenda-filter-state-limit-generation state) -1)
                  (agenda-buffer-generation buffer)))
      (agenda-filter-state-limit state))))

(defun agenda-filter-limit-value (item kind)
  (ecase kind
    (:entries 1)
    (:todos (and (agenda-item-keyword item) 1))
    (:tags (and (agenda-item-tags item) 1))
    (:effort
     (if (agenda-item-effort item)
         (agenda-filter-duration-minutes (agenda-item-effort item))
         most-positive-fixnum))))

(defun agenda-filter-limit-items (items kind maximum)
  "Apply GNU Org's cumulative MAXIMUM limiter to one sorted section."
  (let ((include-unqualified-p (minusp maximum))
        (limit (abs maximum))
        (total 0d0)
        (result '()))
    (dolist (item items (nreverse result))
      (let ((value (agenda-filter-limit-value item kind)))
        (when value (incf total value))
        (when (or (and value (<= total limit))
                  (and include-unqualified-p (null value)))
          (push item result))))))

(defun agenda-filter-transform-section (buffer section items)
  (declare (ignore section))
  (alexandria:if-let ((limit (agenda-filter-effective-limit buffer)))
    (agenda-filter-limit-items items (first limit) (second limit))
    items))

(defun agenda-filter-condition-label (condition prefix)
  (format nil "~a~a~a"
          prefix
          (if (agenda-filter-condition-negative-p condition) "-" "+")
          (agenda-filter-condition-value condition)))

(defun agenda-filter-status (buffer)
  "Return a compact, visible summary of BUFFER's active filters and limit."
  (let* ((state (agenda-filter-state buffer))
         (parts
           (append
            (when (agenda-filter-state-category state)
              (list (agenda-filter-condition-label
                     (agenda-filter-state-category state) "Cat:")))
            (mapcar (lambda (condition)
                      (agenda-filter-condition-label condition "Tag:"))
                    (agenda-filter-state-tags state))
            (mapcar (lambda (condition)
                      (agenda-filter-condition-label condition "Re:"))
                    (agenda-filter-state-regexps state))
            (mapcar
             (lambda (condition)
               (format nil "Eff:~a~c~a"
                       (if (agenda-filter-condition-negative-p condition)
                           "-" "+")
                       (agenda-filter-condition-operator condition)
                       (agenda-filter-condition-value condition)))
             (agenda-filter-state-efforts state))
            (when (agenda-filter-state-top-headline state)
              (list (agenda-filter-condition-label
                     (agenda-filter-state-top-headline state) "Top:")))
            (alexandria:when-let ((limit (agenda-filter-effective-limit buffer)))
              (list (format nil "Max-~(~a~):~d" (first limit) (second limit)))))))
    (if parts (format nil "  [~{~a~^ ~}]" parts) "")))

(defun agenda-filter-rerender (&optional (buffer (current-buffer)))
  "Re-render BUFFER from its last unfiltered scan without source I/O."
  (let ((items (buffer-value buffer 'lem-yath-agenda-cached-items)))
    (if (null (buffer-value buffer 'lem-yath-agenda-cache-ready))
        (message "Agenda data is not ready yet")
        (progn
          (setf (buffer-value buffer 'lem-yath-agenda-restore-entry)
                (agenda-entry-key-at-point (buffer-point buffer)))
          (render-agenda
           buffer items
           (buffer-value buffer 'lem-yath-agenda-cached-failures)
           (buffer-value buffer 'lem-yath-agenda-cached-clock-report))
          t))))

(defun agenda-filter-current-value (property description)
  (or (text-property-at (current-point) property)
      (progn
        (message "No ~a on this agenda line" description)
        nil)))

(define-command lem-yath-agenda-filter-by-category (&optional argument)
    (:universal-nil)
  "Toggle a positive or prefix-negative filter for the category at point."
  (let* ((buffer (current-buffer))
         (state (agenda-filter-state buffer)))
    (if (agenda-filter-state-category state)
        (progn
          (setf (agenda-filter-state-category state) nil)
          (agenda-filter-rerender buffer)
          (message "Category filter removed"))
        (alexandria:when-let
            ((category (agenda-filter-current-value
                        :agenda-category "category")))
          (setf (agenda-filter-state-category state)
                (make-agenda-filter-condition
                 :value category :negative-p (not (null argument))))
          (agenda-filter-rerender buffer)
          (message "Category filter: ~a~a"
                   (if argument "exclude " "") category)))))

(define-command lem-yath-agenda-filter-by-top-headline (&optional argument)
    (:universal-nil)
  "Toggle a positive or prefix-negative filter for point's top headline."
  (let* ((buffer (current-buffer))
         (state (agenda-filter-state buffer)))
    (if (agenda-filter-state-top-headline state)
        (progn
          (setf (agenda-filter-state-top-headline state) nil)
          (agenda-filter-rerender buffer)
          (message "Top-headline filter removed"))
        (alexandria:when-let
            ((headline (agenda-filter-current-value
                        :agenda-top-headline "top-level headline")))
          (setf (agenda-filter-state-top-headline state)
                (make-agenda-filter-condition
                 :value headline :negative-p (not (null argument))))
          (agenda-filter-rerender buffer)
          (message "Top-headline filter: ~a~a"
                   (if argument "exclude " "") headline)))))

(defun agenda-filter-read-regexp (negative-p)
  (let* ((pattern
           (prompt-for-string
            (if negative-p
                "Hide entries matching regexp: "
                "Narrow to entries matching regexp: ")))
         (scanner
           (handler-case
               (ppcre:create-scanner
                (project-regexp-to-extended pattern)
                :case-insensitive-mode t)
             (error () (editor-error "Invalid agenda regexp")))))
    (make-agenda-filter-condition
     :value pattern :negative-p negative-p :scanner scanner)))

(define-command lem-yath-agenda-filter-by-regexp (&optional argument)
    (:universal-nil)
  "Toggle, negate, or double-prefix-accumulate an agenda regexp filter."
  (let* ((buffer (current-buffer))
         (state (agenda-filter-state buffer))
         (magnitude (agenda-filter-prefix-magnitude argument))
         (accumulate-p (= magnitude 16)))
    (if (and (agenda-filter-state-regexps state) (not accumulate-p))
        (progn
          (setf (agenda-filter-state-regexps state) nil)
          (agenda-filter-rerender buffer)
          (message "Regexp filter removed"))
        (let ((condition (agenda-filter-read-regexp (= magnitude 4))))
          (setf (agenda-filter-state-regexps state)
                (append (if accumulate-p
                            (agenda-filter-state-regexps state)
                            nil)
                        (list condition)))
          (agenda-filter-rerender buffer)
          (message "Regexp filter applied")))))

(defun agenda-filter-read-effort-condition (negative-p)
  (loop :for operator := (prompt-for-character
                          "Effort operator? (> = or <), or _ to remove: ")
        :do
           (cond
             ((null operator) (return nil))
             ((char= operator #\_) (return :remove))
             ((member operator '(#\< #\> #\=) :test #'char=)
              (let ((choice
                      (loop :for character :=
                              (prompt-for-character
                               "Effort [1]0 [2]0:10 [3]0:30 [4]1:00 [5]2:00 [6]3:00 [7]4:00 [8]5:00 [9]6:00 [0]7:00: ")
                            :for index :=
                              (and character
                                   (digit-char-p character))
                            :when index
                              :return (nth (mod (1- index) 10)
                                           *agenda-filter-effort-values*))))
                (return
                  (make-agenda-filter-condition
                   :value choice
                   :negative-p negative-p
                   :operator operator
                   :minutes (agenda-filter-duration-minutes choice))))))))

(define-command lem-yath-agenda-filter-by-effort (&optional argument)
    (:universal-nil)
  "Apply, negate, accumulate, or explicitly remove an Effort filter."
  (let* ((buffer (current-buffer))
         (state (agenda-filter-state buffer))
         (magnitude (agenda-filter-prefix-magnitude argument))
         (accumulate-p (= magnitude 16))
         (condition (agenda-filter-read-effort-condition (= magnitude 4))))
    (cond
      ((null condition))
      ((eq condition :remove)
       (setf (agenda-filter-state-efforts state) nil)
       (agenda-filter-rerender buffer)
       (message "Effort filter removed"))
      (t
       (setf (agenda-filter-state-efforts state)
             (append (if accumulate-p
                         (agenda-filter-state-efforts state)
                         nil)
                     (list condition)))
       (agenda-filter-rerender buffer)
       (message "Effort filter applied")))))

(defun agenda-filter-known-tags (buffer)
  (sort
   (agenda-unique-strings
    (loop :for item :in (buffer-value buffer 'lem-yath-agenda-cached-items)
          :nconc (copy-list (agenda-item-tags item))))
   #'string-lessp))

(defun agenda-filter-read-tag-name (buffer)
  (let ((tags (agenda-filter-known-tags buffer)))
    (prompt-for-string
     "Tag: "
     :completion-function
     (lambda (input) (prescient-filter input tags :category :symbol))
     :test-function (lambda (input) (member input tags :test #'string=))
     :history-symbol 'lem-yath-agenda-filter-tags)))

(defun agenda-filter-read-tag-selection (buffer exclude-p)
  "Return tag list, exclusion mode, and action from Org's tag dispatcher."
  (loop
    :for character :=
      (prompt-for-character
       (format nil "~a by tag: [SPC]tagged [TAB]tag [.]at point [\\]off [q]uit: "
               (if exclude-p "Exclude[+]" "Filter[-]")))
    :do
       (cond
         ((or (null character) (member character '(#\q #\Q #\Escape)
                                       :test #'char=))
          (return (values nil exclude-p :cancel)))
         ((char= character #\-)
          (setf exclude-p t))
         ((char= character #\+)
          (setf exclude-p nil))
         ((char= character #\\)
          (return (values nil exclude-p :remove)))
         ((or (char= character #\Return) (char= character #\Newline))
          (return (values nil exclude-p :remove)))
         ((char= character #\Space)
          (return (values (list "") exclude-p :apply)))
         ((char= character #\Tab)
          (return (values (list (agenda-filter-read-tag-name buffer))
                          exclude-p :apply)))
         ((char= character #\.)
          (alexandria:if-let
              ((tags (text-property-at (current-point) :agenda-tags)))
            (return (values (copy-list tags) exclude-p :apply))
            (message "No tags on this agenda line"))))))

(define-command lem-yath-agenda-filter-by-tag (&optional argument)
    (:universal-nil)
  "Filter by tags with Org's prefix-negation and accumulation behavior."
  (let* ((buffer (current-buffer))
         (state (agenda-filter-state buffer))
         (magnitude (agenda-filter-prefix-magnitude argument))
         (accumulate-p (= magnitude 16)))
    (multiple-value-bind (tags exclude-p action)
        (agenda-filter-read-tag-selection buffer (= magnitude 4))
      (ecase action
        (:cancel nil)
        (:remove
         (setf (agenda-filter-state-tags state) nil)
         (agenda-filter-rerender buffer)
         (message "Tag filter removed"))
        (:apply
         (setf (agenda-filter-state-tags state)
               (append
                (if accumulate-p (agenda-filter-state-tags state) nil)
                (mapcar
                 (lambda (tag)
                   (make-agenda-filter-condition
                    :value tag :negative-p exclude-p))
                 tags)))
         (agenda-filter-rerender buffer)
         (message "Tag filter applied"))))))

(define-command lem-yath-agenda-limit-interactively (&optional argument)
    (:universal-nil)
  "Temporarily limit entries, TODOs, tagged rows, or cumulative Effort."
  (let* ((buffer (current-buffer))
         (state (agenda-filter-state buffer)))
    (if argument
        (progn
          (setf (agenda-filter-state-limit state) nil
                (agenda-filter-state-limit-generation state) nil)
          (agenda-filter-rerender buffer)
          (message "Agenda limits removed"))
        (let* ((character
                 (prompt-for-character
                  "Number of [e]ntries [t]odos [T]ags [E]ffort? "))
               (kind (case character
                       (#\e :entries) (#\t :todos)
                       (#\T :tags) (#\E :effort))))
          (if (null kind)
              (message "Wrong agenda limit input")
              (let ((number
                      (prompt-for-integer
                       (if (eq kind :effort)
                           "How many minutes? "
                           (format nil "How many ~(~a~)? " kind)))))
                (setf (agenda-filter-state-limit state) (list kind number)
                      (agenda-filter-state-limit-generation state)
                      (agenda-buffer-generation buffer))
                (agenda-filter-rerender buffer)
                (message "Agenda limit applied")))))))

(define-command lem-yath-agenda-filter-remove-all () ()
  "Remove all stacked filters; generation-local `ss' limits are unchanged."
  (let* ((buffer (current-buffer))
         (state (agenda-filter-state buffer)))
    (setf (agenda-filter-state-category state) nil
          (agenda-filter-state-tags state) nil
          (agenda-filter-state-regexps state) nil
          (agenda-filter-state-efforts state) nil
          (agenda-filter-state-top-headline state) nil)
    (agenda-filter-rerender buffer)
    (message "All agenda filters removed")))

(setf *agenda-item-filter-function* 'agenda-filter-item-visible-p
      *agenda-section-transform-function* 'agenda-filter-transform-section
      *agenda-status-function* 'agenda-filter-status)

;; Effective Evil-Org filter bindings.
(define-key *lem-yath-agenda-vi-keymap* "C-u"
  'lem/universal-argument:universal-argument)
(define-key *lem-yath-agenda-vi-keymap* "s c"
  'lem-yath-agenda-filter-by-category)
(define-key *lem-yath-agenda-vi-keymap* "s r"
  'lem-yath-agenda-filter-by-regexp)
(define-key *lem-yath-agenda-vi-keymap* "s e"
  'lem-yath-agenda-filter-by-effort)
(define-key *lem-yath-agenda-vi-keymap* "s t"
  'lem-yath-agenda-filter-by-tag)
(define-key *lem-yath-agenda-vi-keymap* "s ^"
  'lem-yath-agenda-filter-by-top-headline)
(define-key *lem-yath-agenda-vi-keymap* "s s"
  'lem-yath-agenda-limit-interactively)
(define-key *lem-yath-agenda-vi-keymap* "S"
  'lem-yath-agenda-filter-remove-all)

;; GNU Org's base agenda aliases remain available in Emacs state.
(define-key *lem-yath-agenda-mode-keymap* "\\"
  'lem-yath-agenda-filter-by-tag)
(define-key *lem-yath-agenda-mode-keymap* "_"
  'lem-yath-agenda-filter-by-effort)
(define-key *lem-yath-agenda-mode-keymap* "="
  'lem-yath-agenda-filter-by-regexp)
(define-key *lem-yath-agenda-mode-keymap* "|"
  'lem-yath-agenda-filter-remove-all)
(define-key *lem-yath-agenda-mode-keymap* "~"
  'lem-yath-agenda-limit-interactively)
(define-key *lem-yath-agenda-mode-keymap* "<"
  'lem-yath-agenda-filter-by-category)
(define-key *lem-yath-agenda-mode-keymap* "^"
  'lem-yath-agenda-filter-by-top-headline)
