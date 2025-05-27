;;; org-ics-import.el --- Import iCalendar files to org agenda -*- lexical-binding: t -*-

;; Author: Struan Robertson contact@struan.tech
;; Version 1.0
;; Package-Requires: ((emacs "29.0") (icalendar))

;;; Commentary
;; A pure Emacs Lisp package for the importing of remote iCalendar files to org agenda
;; The idea of this package is to have an org-agenda file for each calendar, which is overwritten at each update.

;;; Code

(require 'icalendar)

;;;; Custom Variables

(defcustom org-ics-import-exclude-strings '()
  "If the summary of an event contains one of these strings it is excluded."
  :type '(repeat string))

(defcustom org-ics-import-exclude-passed-events t
  "Whether to exclude events that have passed.
Note that some ical files can contain many of these dates, so which can slow down org agenda."
  :type 'boolean)

(defcustom org-ics-import-calendars-alist '()
  "Association list specifying the mappings between calendars and org files.
Of the form ((https://full/url/to/calendar.ics . path/to/calendar_org_file.org))

DO NOT INCLUDE YOUR OWN ORG AGENDA FILES HERE!
They will be overwritten with every calendar refresh."
  :type '(alist (string string)))

(defcustom org-ics-import-confirmed-overwrite '()
  "List of org files that are safe to overwrite when updating calendars.")

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
  "Start/restart update timer `org-ics-import--update-timer' using interval from `org-ics-import-update-interval'."
  (org-ics-import--stop-update-timer)
  (when org-ics-import-update-interval
    (setq org-ics-import--update-timer (run-at-time nil org-ics-import-update-interval #'org-ics-refresh-calendars))))

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

;;;; Core Functionality

(defun org-ics-import--create-property-list (event)
  "Create a list of properties from `event' for the org :PROPERTIES: drawer.
This list excludes the SUMMARY, DTSTART, LCOATION, DESCRIPTION and UID properties as they are represented elsewhere in the TODO.
'ICAL_' will be added to the start of the property name to protect it from future org built in property changes."
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
    (save-excursion
      (set-buffer unfolded-buffer)
      (goto-char (point-min))
      (icalendar--read-element nil nil))))

(defun org-ics-import--ics-url-to-org (ics-url org-file)
  "Download ics file from `ics-url' asynchronously, parsing events and then updating `org-file' by overwriting."
  (url-retrieve
   ics-url
   (let ((target-file org-file))
     (lambda (status)
       (when-let ((err (plist-get status :error)))
	 (error "Download error: %S" err))
       (goto-char (point-min))
       (re-search-forward "\r\n\r\n\\|\n\n")
       (delete-region (point-min) (point))
       (let* ((events (icalendar--all-events
		       (org-ics-import--read-buffer-to-list
			(current-buffer))))
	      (todos (string-join
		      (remove nil
			      (mapcar #'org-ics-import--event-to-org-todo events))
		      "\n")))
	 (with-temp-file target-file
	   (set-buffer-file-coding-system 'raw-text t) ;; Handle buffer encoding
	   (insert todos)
	   (message (format "Successfully parsed %s" target-file))))))))

(defun org-ics-import--event-to-org-todo (event)
  "Convert an icalendar EVENT `event' to an org TODO item."
  (let* ((summary (icalendar--get-event-property event 'SUMMARY))
         (dtstart (icalendar--get-event-property event 'DTSTART))
	 (description (icalendar--get-event-property event 'DESCRIPTION))
	 (description (if description (replace-regexp-in-string "\\\\n" "\n" description) ""))
	 (uid (icalendar--get-event-property event 'UID))
	 (dtstart-encoded (if dtstart
			      (encode-time
			       (parse-time-string
				(if (> (length dtstart) 9)
				    dtstart
				  (concat dtstart "T00"))))
			    nil))
         (time (if dtstart-encoded (format-time-string "%Y-%m-%d %a %H:%M" dtstart-encoded) ""))
	 (extra-properties (org-ics-import--create-property-list event))
	 (event-passed (if dtstart-encoded (time-less-p (current-time) dtstart-encoded) nil))
         (todo-line (format "* %s %s\nSCHEDULED: <%s>\n:PROPERTIES:\n:CUSTOM_ID: %s\n%s:END:\n%s"
			    (if event-passed "TODO" "DONE")
			    summary
			    time
			    uid
			    extra-properties
			    description)))
    (when (and (if org-ics-import-exclude-passed-events event-passed t)
	       (not (string-match (regexp-opt org-ics-import-exclude-strings) summary)))
      todo-line)))

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
