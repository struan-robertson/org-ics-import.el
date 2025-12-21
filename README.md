# org-ics-import.el

A pure Emacs Lisp package to allow importing remote iCalendar files to Org Agenda.

The idea is to have an Org file for each calendar, with org headings for each event.
This file will be overwritten at each update, **PLEASE DO NOT USE YOUR EXISTING ORG AGENDA FILES.**

## Installation 

Currently this package is only available from git.

My setup using the [Elpaca](https://github.com/progfolio/elpaca) package manager looks like this:
``` emacs-lisp
(use-package org-ics-import
  :ensure
  (:repo "https://git.sr.ht/~struanr/org-ics-import.el")
  :custom
  (org-ics-import-update-interval 3600)
  (org-ics-import-calendars-alist '(("https://outlook.office365.com/owa/calendar/_/_/calendar.ics" . "~/Sync/Notes/Tasks/ical/university.org")))
  (org-ics-import-exclude-strings '("Cancelled"))
  (org-ics-import-exclude-passed-events t))
```

Any solution that allows installing packages from git repositories should work, or cloning the git repository and loading it locally.

### Not Yet Implemented

  * Repeating events
  
### Features

  * Scheduled calendar refresh
  * Automatic time zone conversion

## Configuration

The alist `org-ics-import-calendars-alist` stores the mappings between remote iCalendar files and the org files that parsed events will be written to.
It is of the form `(("https://ical/file/url/calendar.ics" . "~/path/to/org/file"))`.

The command `org-ics-import-refresh-calendars` asynchronously downloads each iCalendar file and parses the result to its mapped org file.

To automatically run this command on a timer, set `org-ics-import-update-interval` to the number of seconds between calendar refreshes.

Custom keywords for future and passed events can be set using the `org-ics-import-todo-keyword` and `org-ics-import-done-keyword`, respectively.

### Filtering

Microsoft Outlook (and probably others) includes passed events and cancelled events in their iCalendar files.
The boolean variable `org-ics-import-exclude-passed-events` controls whether to exclude passed events from the resulting org file.

If the summary of an event (the title you would see in a calendar) contains one of the strings found in `org-ics-import-exclude-strings` it will be excluded.
This can be useful to filter out cancelled events.

## Contributing 

Please send any patches, issues, or questions to my [mailing list](https://lists.sr.ht/~struanr/emacs-package-development). If you would prefer to create a pull request or issue, I maintain a Codeberg [mirror of this repository.](https://codeberg.org/struan/org-ics-import.el)


