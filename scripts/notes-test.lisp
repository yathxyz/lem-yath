(with-open-file (out (uiop:getenv "LEM_YATH_NOTES_REPORT")
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (let ((failures 0)
        (stamp "[2026-07-10 Fri 09:30]"))
    (labels ((check (condition label)
               (format out "~a ~a~%" (if condition "PASS" "FAIL") label)
               (unless condition (incf failures)))
             (contents (path)
               (alexandria:read-file-into-string path))
             (rejected-date-p (date)
               (handler-case
                   (progn (lem-yath::daily-note-path date) nil)
                 (error () t))))
      (handler-case
          (progn
            (check (lem-yath::valid-iso-date-p "2024-02-29")
                   "leap-day-valid")
            (check (every #'rejected-date-p
                          '("2023-02-29" "2026-02-30" "2026-13-01"
                            "2026-7-10" " 2026-07-10" "../../etc/x"
                            "2026-07-10/evil"))
                   "invalid-and-unsafe-dates-rejected")
            (let ((daily (lem-yath::daily-note-path "2024-02-29")))
              (check (string= (namestring daily)
                              (namestring
                               (merge-pathnames "roam/2024-02-29.org"
                                                (lem-yath::workdir))))
                     "daily-directly-under-roam")
              (check (null (search "/roam/daily/" (namestring daily)))
                     "no-daily-subdirectory"))
            (check (cl-ppcre:scan
                    "^\\[[0-9]{4}-[0-9]{2}-[0-9]{2} (Mon|Tue|Wed|Thu|Fri|Sat|Sun) [0-9]{2}:[0-9]{2}\\]$"
                    (lem-yath::inactive-org-timestamp))
                   "inactive-timestamp-has-weekday-and-time")

            (let ((inbox (merge-pathnames "inbox.org" (lem-yath::workdir))))
              (alexandria:write-string-into-file
               (format nil "#+title: existing~%* Inbox~%intro~%** Existing child~%body~%* Later~%later body~%")
               inbox :if-exists :supersede)
              (lem-yath::write-capture "i" "Inbox item" :timestamp stamp)
              (let* ((text (contents inbox))
                     (entry (search "** Inbox item" text))
                     (later (search "* Later" text)))
                (check (and entry later (< entry later))
                       "inbox-entry-before-next-top-level-heading")
                (check (search ":CREATED: [2026-07-10 Fri 09:30]" text)
                       "inbox-created-timestamp")))

            (let ((todo (merge-pathnames "todo.org" (lem-yath::workdir))))
              (alexandria:write-string-into-file
               (format nil "* Inboxish~%wrong~%* Inbox :tag:~%also wrong~%")
               todo :if-exists :supersede)
              (lem-yath::write-capture "t" "Task item" :timestamp stamp)
              (let ((text (contents todo)))
                (check (search (format nil "* Inbox~%~%** TODO Task item") text)
                       "missing-exact-inbox-created")
                (check (= 1 (cl-ppcre:count-matches "(?m)^\\* Inbox$" text))
                       "exact-inbox-created-once")))

            (let ((reading (merge-pathnames "readlist.org"
                                            (lem-yath::workdir))))
              (alexandria:write-string-into-file
               (format nil "* Inbox~%reading notes~%* Archive~%old~%")
               reading :if-exists :supersede)
              (lem-yath::write-capture "r" "Book item" :timestamp stamp)
              (let* ((text (contents reading))
                     (entry (search "** TODO Book item" text))
                     (archive (search "* Archive" text)))
                (check (and entry archive (< entry archive))
                       "reading-entry-inside-inbox")))

            (let ((public (lem-yath::write-capture
                           "p" "Public item" :timestamp stamp)))
              (let ((text (contents public)))
                (check (string= (namestring public)
                                (namestring
                                 (merge-pathnames "inbox.org"
                                                  (lem-yath::public-org-directory))))
                       "public-target-directory")
                (check (cl-ppcre:scan "(?m)^\\* TODO Public item$" text)
                       "public-entry-is-top-level-todo")
                (check (cl-ppcre:scan
                        "(?m)^:ID: [0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-4[0-9A-Fa-f]{3}-[89ABab][0-9A-Fa-f]{3}-[0-9A-Fa-f]{12}$"
                        text)
                       "public-entry-has-generated-uuid")
                (check (search ":CREATED: [2026-07-10 Fri 09:30]" text)
                       "public-entry-has-created")))

            (check (not (uiop:directory-exists-p
                         (merge-pathnames "roam/daily/"
                                          (lem-yath::workdir))))
                   "tests-created-no-legacy-daily-directory"))
        (error (condition)
          (format out "FAIL unhandled-error: ~a~%" condition)
          (incf failures)))
      (format out "SUMMARY ~a (~d failure~:p)~%"
              (if (zerop failures) "PASS" "FAIL") failures))))
