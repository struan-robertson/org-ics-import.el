;;; org-ics-import.el --- Import iCalendar files to org agenda -*- lexical-binding: t -*-

;; Author: Struan Robertson contact@struan.tech
;; Version 1.0
;; Package-Requires: ((emacs "29.0"))

;;; Commentary
;; A pure Emacs Lisp package for the importing of remote iCalendar files to org agenda
;; The idea of this package is to have an org-agenda file for each calendar, which is overwritten at each update.

;;; Code

(require 'icalendar)
(require 'calendar)

;;;; Custom Variables

(defcustom org-ics-import-exclude-strings '()
  "If the summary of an event contains one of these strings it is excluded."
  :type '(repeat string)
  :group 'org-ics-import)

(defcustom org-ics-import-exclude-passed-events t
  "Whether to exclude events that have passed.
This can speed up org agenda on large iCalendar files."
  :type 'boolean
  :group 'org-ics-import)

(defcustom org-ics-import-recurring-future-count 10
  "Number of future occurrences to generate for each recurring event."
  :type 'integer
  :group 'org-ics-import)

(defcustom org-ics-import-todo-keyword nil
  "String used as the state for importing upcoming events.
If set to `nil' plain events are created."
  :type '(choice (const :tag "Use no todo keyword" nil)
                 (string :tag "String to be used as the todo keyword"))
  :group 'org-ics-import)

(defcustom org-ics-import-done-keyword nil
  "String used as the state for importing past events.
If `org-ics-import-todo-keyword' is nil, this value has no effect."
  :type '(choice (const :tag "Use no done keyword" nil)
                 (string :tag "String to be used as the done keyword"))
  :group 'org-ics-import)

(defcustom org-ics-import-calendars-alist '()
  "Association list specifying the mappings between calendars and org files.
Of the form ((https://full/url/to/calendar.ics . path/to/calendar_org_file.org))

DO NOT INCLUDE YOUR OWN ORG AGENDA FILES HERE!
They will be overwritten with every calendar refresh."
  :type '(alist (string string))
  :group 'org-ics-import)

(defcustom org-ics-import-confirmed-overwrite '()
  "List of org files that are safe to overwrite when updating calendars."
  :type '(repeat string)
  :group 'org-ics-import)

(defcustom org-ics-import-update-interval nil
  "Interval between updating remote calendars. `nil' disables automatic updates."
  :type '(choice (integer :tag "Interval (seconds)")
		 (const :tag "Disabled" nil))
  :group 'org-ics-import
  :set (lambda (sym val)
	 (let ((val (cond
		     ((null val) nil) ; Preserve explicit disabling
		     ((integerp val) (max 1 (abs val))) ; Ensure positive integers
		     (t (default-value sym))))) ; Fallback to current value
	   (set-default sym val))
	 (when (boundp 'org-ics-import--update-timer)
	   (org-ics-import--stop-update-timer)
	   (when val
	     (org-ics-import--start-update-timer)))))

(defvar org-ics-import--update-timer nil
  "Storage for update timer object.")

;;;; Automatic Updating

(defun org-ics-import--start-update-timer ()
  "Start/restart update timer `org-ics-import--update-timer'.
Timer interval is specified by `org-ics-import-update-interval'."
  (org-ics-import--stop-update-timer)
  (when org-ics-import-update-interval
    (setq org-ics-import--update-timer (run-at-time nil org-ics-import-update-interval #'org-ics-import-refresh-calendars))))

(defun org-ics-import--stop-update-timer ()
  "Stop current update timer `org-ics-import--update-timer'."
  (when org-ics-import--update-timer
    (cancel-timer org-ics-import--update-timer)
    (setq org-ics-import--update-timer nil)))

(defun org-ics-import-unload-function ()
  "Stop update timer on package unload"
  (org-ics-import--stop-update-timer))

(with-eval-after-load 'org-ics-import
  (when org-ics-import-update-interval
    (org-ics-import--start-update-timer)))


;;;; Repeat Events

(defmacro org-ics-import--create-plist-key (property)
  "Downcase and prepend ':' to `property' for use as a plist-key"
  `(intern (concat ":" (downcase ,property))))

(defun org-ics-import--rrule-parse (rrule-str dtstart)
  "Parse `rrule-str' and `dtstart' into a plist representation."
  (let* ((rrule-str (if (string= (seq-subseq rrule-str 0 6) "RRULE:")
			(seq-subseq rrule-str 6)
		      rrule-str))
	 (plist `(:interval 1 :dtstart ,dtstart :by-parts nil))
	 (parts (split-string rrule-str ";")))
    (dolist (part parts plist)
      (let* ((kv (split-string part "="))
	     (key (car kv)) 
	     (val (cadr kv)))
	(cond
	 ((string= key "FREQ")     (plist-put plist (org-ics-import--create-plist-key key) (intern (downcase val))))
	 ((string= key "INTERVAL") (plist-put plist (org-ics-import--create-plist-key key) (string-to-number val)))
	 ((string= key "COUNT")    (plist-put plist (org-ics-import--create-plist-key key) (string-to-number val)))
	 ((string= key "WKST")     (plist-put plist (org-ics-import--create-plist-key key) val))
	 ((string= key "UNTIL")    (plist-put plist (org-ics-import--create-plist-key key) (date-to-time val)))
	 (t
	  ;; Collect complex BY-rules into an alist under by-parts
	  (let ((existing (plist-get plist :by-parts)))
	    (plist-put plist :by-parts (cons (cons key val) existing)))))))))

(defun org-ics-import--add-interval (time freq interval)
  "Add INTERVAL units of FREQ to TIME. Returns an encoded time."
  (let ((dt (decode-time time)))
    (encode-time
     (decoded-time-add dt
		       (pcase freq
			 ('yearly  (make-decoded-time :year interval))
			 ('monthly (make-decoded-time :month interval))
			 ('weekly  (make-decoded-time :day (* 7 interval)))
			 ('daily   (make-decoded-time :day interval))
			 ('hourly  (make-decoded-time :hour interval))
			 ('minutely (make-decoded-time :minute interval))
			 ('secondly (make-decoded-time :second interval))
			 (_        (make-decoded-time)))))))

(defun org-ics-import--set-weekday (time target-day-str &optional wkst-str)
  "Return TIME set to TARGET-DAY-STR within the week starting on WKST-STR.
WKST-STR defaults to \"MO\" (Monday) if nil."
  (let* ((dow-map '(("SU" . 0) ("MO" . 1) ("TU" . 2) ("WE" . 3)
                    ("TH" . 4) ("FR" . 5) ("SA" . 6)))
         ;; 1. Get integer values for days (0=Sun ... 6=Sat)
         (target-dow (cdr (assoc target-day-str dow-map)))
         (wkst-dow (cdr (assoc (or wkst-str "MO") dow-map)))
         (decoded (decode-time time))
         (current-dow (decoded-time-weekday decoded))
         
         ;; 2. Calculate how far back the Week Start is from current day
         ;;    modulo 7 ensures we handle wrapping correctly
         (days-from-start (mod (- current-dow wkst-dow) 7))
         
         ;; 3. Calculate how far forward the Target is from Week Start
         (target-offset (mod (- target-dow wkst-dow) 7))
         
         ;; 4. Net movement: Rewind to start, then add target offset
         (delta (- target-offset days-from-start)))

    (encode-time 
     (decoded-time-add decoded (make-decoded-time :day delta)))))


;; BY rules that specify a unit larger than or equal to FREQ filter/limit the set.
;; BY rules that specify a smaller unit than FREQ expand the set.
;; E.g. FREQ=YEARLY;BYMONTH=1 generates one occurrence per year, in January.
;; E.g. FREQ=WEEKLY;BYDAY=MO,WE generates two occurrences per week.
;; BYSETPOS filters the final expanded set by position (can be negative).
;; Applied in order: BYMONTH -> BYMONTHDAY -> BYDAY -> BYSETPOS.

(defun org-ics-import--weekdays-in-month (base target-dow)
  "Return all dates in BASE's month that fall on TARGET-DOW (0=Sun..6=Sat).
Each result preserves the time-of-day from BASE."
  (let* ((dt (decode-time base))
         (year (decoded-time-year dt))
         (month (decoded-time-month dt))
         (last-day (calendar-last-day-of-month month year))
         results
         (first-dt (copy-sequence dt)))
    (setf (decoded-time-day first-dt) 1)
    (let* ((first-dow (decoded-time-weekday (decode-time (encode-time first-dt))))
           (d (1+ (mod (- target-dow first-dow) 7))))
      (while (<= d last-day)
        (let ((day-dt (copy-sequence dt)))
          (setf (decoded-time-day day-dt) d)
          (push (encode-time day-dt) results))
        (setq d (+ d 7))))
    (nreverse results)))

(defun org-ics-import--parse-byday-spec (spec)
  "Parse a BYDAY spec like \"2MO\", \"-1FR\", or \"MO\" into (ordinal . day-name).
Ordinal is nil when not specified."
  (when (string-match "^\\(-?[0-9]+\\)?\\([A-Z][A-Z]\\)$" spec)
    (cons (when (match-string 1 spec)
            (string-to-number (match-string 1 spec)))
          (match-string 2 spec))))

(defun org-ics-import--expand-byday-in-month (base day-specs)
  "Expand BYDAY day-specs list in the monthly context of BASE.
Returns a list of encoded times. Handles ordinals like \"2MO\" and \"-1FR\"."
  (let ((dow-map '(("SU" . 0) ("MO" . 1) ("TU" . 2) ("WE" . 3)
                   ("TH" . 4) ("FR" . 5) ("SA" . 6))))
    (apply #'append
           (mapcar
            (lambda (spec)
              (let* ((parsed (org-ics-import--parse-byday-spec spec))
                     (ordinal (car parsed))
                     (day-name (cdr parsed))
                     (dow (cdr (assoc day-name dow-map)))
                     (all-days (org-ics-import--weekdays-in-month base dow)))
                (if ordinal
                    (let* ((len (length all-days))
                           (idx (if (< ordinal 0) (+ len ordinal) (1- ordinal))))
                      (when (and (>= idx 0) (< idx len))
                        (list (nth idx all-days))))
                  all-days)))
            day-specs))))

(defun org-ics-import--expand-candidates (base freq by-parts)
  "Apply BY* rules to BASE to generate a list of candidate occurrence times."
  (let* ((candidates (list base))
         (by-mo     (alist-get "BYMONTH"    by-parts nil nil #'string=))
         (by-mday   (alist-get "BYMONTHDAY" by-parts nil nil #'string=))
         (by-day    (alist-get "BYDAY"      by-parts nil nil #'string=))
         (by-setpos (alist-get "BYSETPOS"   by-parts nil nil #'string=)))

    (when by-mo
      (let ((months (mapcar #'string-to-number (split-string by-mo ","))))
        (if (eq freq 'yearly)
            ;; Expand: one candidate per specified month
            (setq candidates
                  (apply #'append
                         (mapcar (lambda (cand)
                                   (mapcar (lambda (m)
                                             (let ((dt (decode-time cand)))
                                               (setf (decoded-time-month dt) m)
                                               (encode-time dt)))
                                           months))
                                 candidates)))
          ;; Filter: only keep candidates in the specified months
          (setq candidates
                (seq-filter (lambda (cand)
                              (memq (decoded-time-month (decode-time cand)) months))
                            candidates)))))

    (when by-mday
      (let ((days (mapcar #'string-to-number (split-string by-mday ","))))
        (setq candidates
              (apply #'append
                     (mapcar (lambda (cand)
                               (let* ((dt (decode-time cand))
                                      (last (calendar-last-day-of-month
                                             (decoded-time-month dt)
                                             (decoded-time-year dt))))
                                 (mapcar (lambda (d)
                                           (let ((dt2 (copy-sequence dt))
                                                 (actual-day (if (< d 0) (+ last d 1) d)))
                                             (setf (decoded-time-day dt2) actual-day)
                                             (encode-time dt2)))
                                         days)))
                             candidates)))))

    (when by-day
      (let ((day-specs (split-string by-day ",")))
        (if (eq freq 'weekly)
            ;; Move to specified days within the same week
            (setq candidates
                  (apply #'append
                         (mapcar (lambda (cand)
                                   (mapcar (lambda (d) (org-ics-import--set-weekday cand d))
                                           day-specs))
                                 candidates)))
          ;; Monthly/Yearly: expand to n-th or all weekdays of each type in the month
          (setq candidates
                (apply #'append
                       (mapcar (lambda (cand)
                                 (org-ics-import--expand-byday-in-month cand day-specs))
                               candidates))))))

    (when by-setpos
      (let* ((positions (mapcar #'string-to-number (split-string by-setpos ",")))
             (n (length candidates)))
        (setq candidates
              (delq nil
                    (mapcar (lambda (pos)
                              (let ((idx (if (< pos 0) (+ n pos) (1- pos))))
                                (when (and (>= idx 0) (< idx n))
                                  (nth idx candidates))))
                            positions)))))

    candidates))

(defun org-ics-import--rrule-occurrences (rrule duration)
  "Generate a list of (start . end) time pairs for RRULE.
DURATION is the event length in seconds. Respects COUNT, UNTIL, and
`org-ics-import-exclude-passed-events'. Collects up to
`org-ics-import-recurring-future-count' future occurrences, with a 2-year
safety cap to prevent runaway iteration on unbounded rules."
  (let* ((start        (plist-get rrule :dtstart))
         (freq         (plist-get rrule :freq))
         (interval     (plist-get rrule :interval))
         (until        (plist-get rrule :until))
         (count        (plist-get rrule :count))
         (by-parts     (plist-get rrule :by-parts))
         (now          (current-time))
         (safety-limit (time-add now (* 2 365 24 3600)))
         (future-limit org-ics-import-recurring-future-count)
         (current      start)
         (total-count  0)
         (future-count 0)
         occurrences)
    (catch 'done
      (while t
        (when (time-less-p safety-limit current)
          (throw 'done nil))
        (when (and until (time-less-p until current))
          (throw 'done nil))
        (let ((candidates (sort (copy-sequence
                                 (org-ics-import--expand-candidates current freq by-parts))
                                #'time-less-p)))
          (dolist (cand candidates)
            ;; Skip candidates before DTSTART (can occur on first iteration with BYDAY)
            (unless (time-less-p cand start)
              (when (and until (time-less-p until cand))
                (throw 'done nil))
              (when (and count (>= total-count count))
                (throw 'done nil))
              (setq total-count (1+ total-count))
              (let ((is-future (not (time-less-p cand now))))
                (when (or (not org-ics-import-exclude-passed-events) is-future)
                  (when is-future
                    (when (>= future-count future-limit)
                      (throw 'done nil))
                    (setq future-count (1+ future-count)))
                  (push (cons cand (time-add cand duration)) occurrences))))))
        (setq current (org-ics-import--add-interval current freq interval))))
    (nreverse occurrences)))

;;;; Core Functionality

(defun org-ics-import--create-property-list (event)
  "Create a list of properties from `event' for the org :PROPERTIES: drawer.
This list excludes SUMMARY, DTSTART, LOCATION, DESCRIPTION and UID properties
as they are represented elsewhere in the TODO.
\\='ICAL_\\=' is added to the start of the property name."
  (let ((properties (caddr event)))
    (string-join
     (remove nil
	     (mapcar (lambda (x)
		       (if (memq (car x) '(SUMMARY DTSTART DESCRIPTION UID))
			   nil
			 (format ":ICAL_%s: %s\n" (car x) (caddr x))))
		     properties)))))

(defun org-ics-import--read-buffer-to-list (buffer)
  (let ((unfolded-buffer (icalendar--get-unfolded-buffer buffer)))
    (with-current-buffer unfolded-buffer
      (goto-char (point-min))
      (icalendar--read-element nil nil))))

(defun org-ics-import--ics-url-to-org (ics-url org-file)
  "Download ics file from `ics-url' asynchronously.
Events are then parsed and then `org-file' overwritten."
  (url-retrieve
   ics-url
   (let ((target-file org-file))
     (lambda (status)
       (when-let ((err (plist-get status :error)))
	 (error "Download error: %S" err))
       (goto-char (point-min))
       (re-search-forward "\r\n\r\n\\|\n\n") 
       (delete-region (point-min) (point))
       (let* ((icalendar (org-ics-import--read-buffer-to-list (current-buffer)))
	      (timezones (icalendar--convert-all-timezones icalendar))
	      (events (icalendar--all-events icalendar))
	      (todos (string-join
		      (remove nil
			      (mapcar (apply-partially #'org-ics-import--event-to-org-todo timezones) events))
		      "\n")))
	 (with-temp-file target-file
	   (set-buffer-file-coding-system 'raw-text t) ;; Handle buffer encoding
	   (insert todos)
	   (message (format "Successfully parsed %s" target-file))))))))

(defun org-ics-import--convert-time-to-local (time timezone)
  "Convert time `time' from a specified timezone `timezone' to local time"
  (let* ((encoded-time (encode-time (nconc
				     (butlast time 3)
				     `(nil -1 ,timezone))))
	 (local-time (decode-time encoded-time nil)))
    (encode-time local-time)))

(defun org-ics-import--format-org-entry (summary start end uid description extra-properties)
  "Format a single org entry string for an event occurrence.
START and END are encoded times; END may be nil for a point-in-time event."
  (let* ((start-str (format-time-string "%Y-%m-%d %a %H:%M" start))
         (end-str   (when end (format-time-string "%Y-%m-%d %a %H:%M" end)))
         (event-future (time-less-p (current-time) start))
         (todo-kw (if-let ((kw (if (and org-ics-import-todo-keyword event-future)
                                   org-ics-import-todo-keyword
                                 org-ics-import-done-keyword)))
                      (concat kw " ")
                    "")))
    (format "* %s%s\n%s\n:PROPERTIES:\n:CUSTOM_ID: %s\n%s:END:\n%s"
            todo-kw summary
            (if end-str
                (format "<%s>--<%s>" start-str end-str)
              (format "<%s>" start-str))
            uid extra-properties description)))

(defun org-ics-import--event-to-org-todo (timezones event)
  "Convert an icalendar EVENT to one or more org TODO items.
Returns a string (possibly containing multiple entries) or nil."
  (let* ((summary (icalendar--get-event-property event 'SUMMARY))
         (dtstart (icalendar--get-event-property event 'DTSTART))
         (dtstart-attrs (icalendar--get-event-property-attributes event 'DTSTART))
         (dtend (icalendar--get-event-property event 'DTEND))
         (dtend-attrs (icalendar--get-event-property-attributes event 'DTEND))
         (description (icalendar--get-event-property event 'DESCRIPTION))
         (description (if description (replace-regexp-in-string "\\\\n" "\n" description) ""))
         (uid (icalendar--get-event-property event 'UID))
         (rrule-str (icalendar--get-event-property event 'RRULE))
         (dtstart-encoded
          (when dtstart
            (let ((timestr (parse-time-string
                            (if (> (length dtstart) 9) dtstart (concat dtstart "T00")))))
              (if (plist-member dtstart-attrs 'TZID)
                  (org-ics-import--convert-time-to-local
                   timestr (icalendar--find-time-zone dtstart-attrs timezones))
                (encode-time timestr)))))
         (dtend-encoded
          (when dtend
            (let ((timestr (parse-time-string
                            (if (> (length dtend) 9) dtend (concat dtend "T00")))))
              (if (plist-member dtend-attrs 'TZID)
                  (org-ics-import--convert-time-to-local
                   timestr (icalendar--find-time-zone dtend-attrs timezones))
                (encode-time timestr)))))
         (extra-properties (org-ics-import--create-property-list event)))
    (when (and summary
               (not (string-match (regexp-opt org-ics-import-exclude-strings) summary)))
      (if rrule-str
          ;; Recurring event: generate one org entry per future occurrence
          (let* ((rrule (org-ics-import--rrule-parse rrule-str dtstart-encoded))
                 (duration (if (and dtstart-encoded dtend-encoded)
                               (- (float-time dtend-encoded) (float-time dtstart-encoded))
                             3600))
                 (occurrences (org-ics-import--rrule-occurrences rrule duration)))
            (when occurrences
              (string-join
               (mapcar (lambda (occ)
                         (let ((occ-uid (format "%s-%s" uid
                                                (format-time-string "%Y%m%dT%H%M%S" (car occ)))))
                           (org-ics-import--format-org-entry
                            summary (car occ) (cdr occ) occ-uid description extra-properties)))
                       occurrences)
               "\n")))
        ;; Single event
        (let ((event-future (when dtstart-encoded
                              (time-less-p (current-time) dtstart-encoded))))
          (when (if org-ics-import-exclude-passed-events event-future t)
            (org-ics-import--format-org-entry
             summary dtstart-encoded dtend-encoded uid description extra-properties)))))))

;;;###autoload
(defun org-ics-import-refresh-calendars ()
  "Refresh all calendars specified in `org-ics-import-calendars-alist'"
  (interactive)
  (message "Refreshing remote iCalendars")
  (dolist (calendar-mapping org-ics-import-calendars-alist)
    (let* ((ics-url (car calendar-mapping))
	   (org-file (cdr calendar-mapping))
	   (safe-to-write (cond
			   ;; If the file doesn't exist then it will be safe to overwrite in future
			   ((not (file-exists-p org-file))
			    (customize-save-variable 'org-ics-import-confirmed-overwrite
						     (add-to-list 'org-ics-import-confirmed-overwrite org-file))
			    t)
			   ;; If the file exists and has not been marked as safe, we should check that it can be overwritten
			   ((not (member org-file org-ics-import-confirmed-overwrite))
			    (when (yes-or-no-p
				   (format "File %s exists. Overwrite and mark as safe in future?" org-file))
			      (customize-save-variable 'org-ics-import-confirmed-overwrite
						       (add-to-list 'org-ics-import-confirmed-overwrite org-file))
			      t))
			   (t t))))
      (when safe-to-write
	(org-ics-import--ics-url-to-org ics-url org-file)))))


(provide 'org-ics-import)

;;; org-ics-import ends here
