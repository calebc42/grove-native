;;; bloom-agenda.el --- The Android app's Agenda screen -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Bloom's Agenda as the Android app draws and fills it: the day headline
;; with its levers button, the Today/Upcoming switch, the optional levers
;; panel, a red overdue card, and day sections of task rows.
;;
;; The rows follow the app's own rules rather than `org-agenda':  a heading
;; lands on exactly one day, its SCHEDULED date or else its DEADLINE, and a
;; bare active timestamp in a heading's body is an event shown on each day it
;; covers.  Repeaters are not expanded; a past plan is overdue until it is
;; done or moved.  The headings come from Bloom's notebook outlines, so Org's
;; own TODO keywords, priorities and tags decide each row.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'calendar)
(require 'org)
(require 'ebp-org)
(require 'jetpacs-widgets)
(require 'jetpacs-design)
(require 'bloom-core)
(require 'bloom-design)
(require 'bloom-library)
(require 'bloom-org)

(declare-function bloom-document-open-action "bloom-document" (notebook-id heading-id))
(declare-function bloom-document--after-mutation "bloom-document" (notebook-id heading-id))
(declare-function jetpacs-app-defer-refresh "jetpacs-apps" (&optional params))
(declare-function jetpacs-shell-notify "jetpacs-shell")

;;;; Levers

(defcustom bloom-agenda-grouping-today "date"
  "How the Today tab groups its rows: date, priority, tag or file."
  :type '(choice (const "date") (const "priority") (const "tag") (const "file"))
  :group 'bloom)

(defcustom bloom-agenda-grouping-upcoming "date"
  "How the Upcoming tab groups its rows: date, priority, tag or file."
  :type '(choice (const "date") (const "priority") (const "tag") (const "file"))
  :group 'bloom)

(defcustom bloom-agenda-filter-today "open"
  "Which states the Today tab shows: open, all, or one TODO keyword."
  :type 'string :group 'bloom)

(defcustom bloom-agenda-filter-upcoming "open"
  "Which states the Upcoming tab shows: open, all, or one TODO keyword."
  :type 'string :group 'bloom)

(defcustom bloom-agenda-show-tags t
  "Non-nil shows each row's tags, inherited ones included."
  :type 'boolean :group 'bloom)

(defcustom bloom-agenda-show-file nil
  "Non-nil shows each row's source notebook."
  :type 'boolean :group 'bloom)

(defcustom bloom-agenda-show-timestamps nil
  "Non-nil shows each row's scheduled date even under a day section."
  :type 'boolean :group 'bloom)

(defconst bloom-agenda-window-days 14
  "Days, starting today, the Upcoming tab looks ahead.")

(defvar bloom-agenda--tab 'today
  "The visible tab, `today' or `upcoming'.  Session state, as in the app.")

(defvar bloom-agenda--levers-open nil
  "Non-nil while the levers panel is open.")

(defvar bloom-agenda--overdue-open nil
  "Non-nil while the overdue card is expanded.")

(defun bloom-agenda--grouping ()
  "Return the visible tab's grouping."
  (if (eq bloom-agenda--tab 'today) bloom-agenda-grouping-today
    bloom-agenda-grouping-upcoming))

(defun bloom-agenda--filter (keywords)
  "Return the visible tab's state filter, valid against active KEYWORDS."
  (let ((filter (if (eq bloom-agenda--tab 'today) bloom-agenda-filter-today
                  bloom-agenda-filter-upcoming)))
    (if (or (member filter '("open" "all")) (member filter keywords)) filter
      "open")))

;;;; Timestamps and days

(defun bloom-agenda-parse-timestamp (string)
  "Return Org timestamp STRING as a plist, or nil.
The plist has `:date' (ISO), `:time' and `:end' (HH:MM or nil),
`:repeater' (as written, such as \"+1w\"), and `:range-end' (ISO, for a
`<a>--<b>' range)."
  (when (and (stringp string)
             (string-match
              "\\`[<[]\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\([^]>]*\\)[]>]\\(?:--[<[]\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)[^]>]*[]>]\\)?"
              (string-trim string)))
    (let ((date (match-string 1 (string-trim string)))
          (rest (match-string 2 (string-trim string)))
          (range-end (match-string 3 (string-trim string)))
          time end repeater)
      (dolist (token (split-string rest " " t))
        (cond
         ((string-match "\\`\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\(?:-\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\)?\\'" token)
          (let ((start (match-string 1 token)) (stop (match-string 2 token)))
            (setq time (bloom-agenda--clock start)
                  end (and stop (bloom-agenda--clock stop)))))
         ((string-match-p "\\`\\(?:\\.\\+\\|\\+\\+\\|\\+\\)[0-9]+[hdwmy]\\'" token)
          (setq repeater token))))
      (and (bloom-org-date-valid-p date)
           (list :date date :time time :end end :repeater repeater
                 :range-end (and (bloom-org-date-valid-p range-end) range-end))))))

(defun bloom-agenda--clock (string)
  "Return clock STRING zero-padded to HH:MM."
  (if (string-match "\\`\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\'" string)
      (format "%02d:%s" (string-to-number (match-string 1 string))
              (match-string 2 string))
    string))

(defun bloom-agenda--today ()
  "Return today's ISO date."
  (format-time-string "%F"))

(defun bloom-agenda--weekday (date)
  "Return ISO DATE's English weekday name."
  (aref ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"]
        (calendar-day-of-week (bloom-org--date-parts date))))

(defun bloom-agenda--short-date (date)
  "Return ISO DATE as the app's short form, such as \"Oct 21\"."
  (pcase-let ((`(,month ,day ,_) (bloom-org--date-parts date)))
    (format "%s %d" (aref ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
                           "Sep" "Oct" "Nov" "Dec"]
                          (1- month))
            day)))

(defun bloom-agenda--long-date (date)
  "Return ISO DATE as the header's form, such as \"October 23\"."
  (pcase-let ((`(,month ,day ,_) (bloom-org--date-parts date)))
    (format "%s %d" (aref ["January" "February" "March" "April" "May" "June"
                           "July" "August" "September" "October" "November"
                           "December"]
                          (1- month))
            day)))

(defun bloom-agenda-day-label (date today)
  "Return the app's label for ISO DATE seen from TODAY.
Today, Tomorrow, Yesterday, the weekday within a week, and the weekday
with its short date beyond."
  (let ((delta (bloom-org--date-delta date today)))
    (pcase delta
      (0 "Today")
      (1 "Tomorrow")
      (-1 "Yesterday")
      (_ (if (> (abs delta) 6)
             (format "%s, %s" (bloom-agenda--weekday date)
                     (bloom-agenda--short-date date))
           (bloom-agenda--weekday date))))))

;;;; Notes

(defun bloom-agenda--notebook-notes (notebook)
  "Return planned or dated heading plists of NOTEBOOK's outline."
  (let* ((notebook-id (plist-get notebook :id))
         (snapshot (condition-case nil
                       (bloom-library-outline-snapshot notebook-id)
                     (error nil)))
         (records (plist-get snapshot :records))
         (file-tags (plist-get snapshot :file-tags))
         (todo (plist-get snapshot :todo-keywords))
         (done (plist-get snapshot :done-keywords))
         (by-id (make-hash-table :test #'equal))
         notes)
    (dolist (record records)
      (puthash (plist-get record :id) record by-id))
    (dolist (record records)
      (let ((scheduled (bloom-agenda-parse-timestamp (plist-get record :scheduled)))
            (deadline (bloom-agenda-parse-timestamp (plist-get record :deadline)))
            (active (delq nil (mapcar #'bloom-agenda-parse-timestamp
                                      (plist-get record :active)))))
        (when (or scheduled deadline active)
          (let ((tags (copy-sequence (plist-get record :tags)))
                (parent (gethash (plist-get record :parent-id) by-id)))
            (while parent
              (dolist (tag (plist-get parent :tags))
                (unless (member tag tags) (setq tags (append tags (list tag)))))
              (setq parent (gethash (plist-get parent :parent-id) by-id)))
            (dolist (tag file-tags)
              (unless (member tag tags) (setq tags (append tags (list tag)))))
            (push (list :notebook-id notebook-id
                        :heading-id (plist-get record :id)
                        :file (or (plist-get notebook :label) "")
                        :title (or (plist-get record :title) "")
                        :keyword (plist-get record :todo)
                        :done (and (plist-get record :done) t)
                        :priority (plist-get record :priority)
                        :tags tags
                        :scheduled scheduled :deadline deadline :active active
                        :active-keywords (cl-set-difference todo done
                                                            :test #'equal))
                  notes)))))
    (nreverse notes)))

(defun bloom-agenda-notes ()
  "Return every planned or dated heading across Bloom's notebooks."
  (cl-mapcan #'bloom-agenda--notebook-notes
             (bloom-library-notebooks-in-folder nil t)))

(defun bloom-agenda--when (note)
  "Return the timestamp that places NOTE: SCHEDULED, else DEADLINE."
  (or (plist-get note :scheduled) (plist-get note :deadline)))

(defun bloom-agenda--when-date (note)
  "Return NOTE's agenda day, or nil."
  (plist-get (bloom-agenda--when note) :date))

(defun bloom-agenda--keep-p (note filter)
  "Non-nil when FILTER admits NOTE: open, all, or one keyword."
  (pcase filter
    ("all" t)
    ("open" (not (plist-get note :done)))
    (_ (equal (plist-get note :keyword) filter))))

(defun bloom-agenda--sort-time (note &optional stamp)
  "Return NOTE's sort clock, or STAMP's; untimed sorts last."
  (or (plist-get (or stamp (bloom-agenda--when note)) :time) "99:99"))

(defun bloom-agenda--by-time (a b)
  "Non-nil when entry A sorts before entry B by time, then title."
  (let ((ta (bloom-agenda--sort-time (car a) (cdr a)))
        (tb (bloom-agenda--sort-time (car b) (cdr b))))
    (if (string= ta tb)
        (string< (downcase (plist-get (car a) :title))
                 (downcase (plist-get (car b) :title)))
      (string< ta tb))))

(defun bloom-agenda--priority-key (note)
  "Return NOTE's priority sort key; unprioritised sorts last."
  (or (plist-get note :priority) "Z"))

(defun bloom-agenda--by-priority (a b)
  "Non-nil when entry A sorts before entry B by priority, time, then title."
  (let ((pa (bloom-agenda--priority-key (car a)))
        (pb (bloom-agenda--priority-key (car b))))
    (if (string= pa pb) (bloom-agenda--by-time a b) (string< pa pb))))

(defun bloom-agenda--overdue (notes today)
  "Return NOTES planned before TODAY, oldest first, then priority and title."
  (sort (seq-filter (lambda (note)
                      (let ((date (bloom-agenda--when-date note)))
                        (and date (< (bloom-org--date-delta date today) 0))))
                    notes)
        (lambda (a b)
          (let ((da (bloom-agenda--when-date a)) (db (bloom-agenda--when-date b)))
            (cond ((not (string= da db)) (string< da db))
                  ((not (string= (bloom-agenda--priority-key a)
                                 (bloom-agenda--priority-key b)))
                   (string< (bloom-agenda--priority-key a)
                            (bloom-agenda--priority-key b)))
                  (t (string< (downcase (plist-get a :title))
                              (downcase (plist-get b :title)))))))))

(defun bloom-agenda--events-on (notes day)
  "Return (NOTE . STAMP) for each bare active stamp of NOTES covering DAY."
  (cl-loop for note in notes
           nconc (cl-loop for stamp in (plist-get note :active)
                          when (and (<= (bloom-org--date-delta (plist-get stamp :date) day) 0)
                                    (>= (bloom-org--date-delta
                                         (or (plist-get stamp :range-end)
                                             (plist-get stamp :date))
                                         day)
                                        0))
                          collect (cons note stamp))))

(defun bloom-agenda--same-note-p (a b)
  "Non-nil when notes A and B are the same heading."
  (and (equal (plist-get a :notebook-id) (plist-get b :notebook-id))
       (equal (plist-get a :heading-id) (plist-get b :heading-id))))

(defun bloom-agenda--day-entries (visible planned day)
  "Return DAY's (NOTE . STAMP) entries: PLANNED, then VISIBLE events.
An event whose heading is already planned for DAY is dropped."
  (append (mapcar (lambda (note) (cons note nil)) planned)
          (seq-remove (lambda (event)
                        (seq-find (lambda (note)
                                    (bloom-agenda--same-note-p note (car event)))
                                  planned))
                      (bloom-agenda--events-on visible day))))

;;;; Rows

(defun bloom-agenda--meta (note today show-date &optional stamp event-day)
  "Return NOTE's meta strip as (TEXT TONE ICON) items, seen from TODAY.
SHOW-DATE adds the day chip.  STAMP makes the row an event on that bare
timestamp, shown under EVENT-DAY's section when that is non-nil."
  (let ((items nil))
    (cl-flet ((add (text tone &optional icon)
                (push (list text tone icon) items))
              (range (stamp)
                (concat (plist-get stamp :time)
                        (if (plist-get stamp :end)
                            (concat "–" (plist-get stamp :end)) ""))))
      (if stamp
          (progn
            (when (or (null event-day) bloom-agenda-show-timestamps)
              (add (bloom-agenda-day-label (or event-day (plist-get stamp :date)) today)
                   'event 'dot))
            (when (plist-get stamp :time) (add (range stamp) 'normal))
            (when (plist-get stamp :repeater)
              (add (concat "↻ " (plist-get stamp :repeater)) 'muted)))
        (let* ((scheduled (plist-get note :scheduled))
               (deadline (plist-get note :deadline))
               (anchor (or scheduled deadline))
               (date (plist-get anchor :date))
               (late (and date (- (bloom-org--date-delta date today))))
               (overdue (and late (> late 0)))
               (deadline-only (and deadline (not scheduled))))
          (cond
           ((and date (or show-date (and bloom-agenda-show-timestamps scheduled)))
            (add (concat (if deadline-only "⚑ " "")
                         (if overdue
                             (format "%s · %dd late" (bloom-agenda--short-date date) late)
                           (bloom-agenda-day-label date today)))
                 (if (or overdue deadline-only) 'danger 'normal)
                 (unless (or overdue deadline-only) 'calendar)))
           (deadline-only (add "⚑ due" 'danger)))
          (when (plist-get anchor :time) (add (range anchor) 'normal))
          (when (and deadline scheduled)
            (add (concat "⚑ " (bloom-agenda--short-date (plist-get deadline :date)))
                 'danger))
          (when-let* ((repeater (plist-get (or scheduled deadline) :repeater)))
            (add (concat "↻ " repeater) 'muted))))
      (when (and bloom-agenda-show-tags (plist-get note :tags))
        (add (concat ":" (string-join (plist-get note :tags) ":") ":") 'tag))
      (when bloom-agenda-show-file (add (plist-get note :file) 'muted)))
    (nreverse items)))

(defun bloom-agenda--tone-ink (tone)
  "Return the ink of meta TONE."
  (pcase tone
    ('normal "muted") ('muted "faint") ('danger "error")
    ('tag "syn-tag") ('event "violet")))

(defun bloom-agenda--priority-ink (priority)
  "Return the agenda's ink for PRIORITY: red, amber, blue, then faint."
  (pcase (upcase (or priority ""))
    ("A" "error") ("B" "warning") ("C" "info") (_ "faint")))

(defun bloom-agenda--text (role ink text &rest attrs)
  "Return TEXT in text ROLE and INK, with universal ATTRS."
  (apply #'jetpacs-with-attrs
         (jetpacs-design-styled
          (delq nil (list (concat "bloom.t." role)
                          (and ink (concat "bloom.fg." ink))))
          (list (jetpacs-text text)))
         attrs))

(defun bloom-agenda--meta-node (item)
  "Return one meta strip ITEM as a node."
  (pcase-let ((`(,text ,tone ,icon) item))
    (pcase icon
      ('calendar
       (jetpacs-row (jetpacs-icon "calendar_month" :size 11.5 :color
                                  (bloom-design-color "info"))
                    (bloom-agenda--text "agenda-meta" "info" text)
                    :spacing 3 :align "center"))
      ('dot
       (jetpacs-row (jetpacs-design-styled '("bloom.agenda.dot")
                                         (list (jetpacs-spacer :width 7 :height 7)))
                    (bloom-agenda--text "agenda-meta" "violet" text)
                    :spacing 3 :align "center"))
      (_ (bloom-agenda--text "agenda-meta" (bloom-agenda--tone-ink tone) text)))))

(defun bloom-agenda--affordance-p (note)
  "Non-nil when NOTE's row has a done checkbox: a keyword or a repeater."
  (or (plist-get note :keyword)
      (plist-get (bloom-agenda--when note) :repeater)))

(defun bloom-agenda--checkbox (note)
  "Return NOTE's priority-tinted ring, filled with a check once done."
  (let* ((done (plist-get note :done))
         (ring (cond (done "success")
                     ((plist-get note :priority)
                      (bloom-agenda--priority-ink (plist-get note :priority)))
                     (t "outline"))))
    (jetpacs-with-attrs
     (jetpacs-design-styled
      (list "bloom.agenda.check" (concat "bloom.agenda.check-" ring))
      (list (jetpacs-box
             (if done (jetpacs-icon "check" :size 12 :color (bloom-design-color "surface"))
               (jetpacs-spacer :width 1))
             :alignment "center"
             :on-tap (bloom-agenda--action "bloom.agenda.toggle" note))))
     :pad '(:top 1))))

(defun bloom-agenda--state-chip (keyword done)
  "Return the agenda's TODO-state chip for KEYWORD, DONE when finished."
  (let ((tone (cond (done "success") ((equal keyword "NEXT") "success")
                    ((equal keyword "WAITING") "raised") (t "warning")))
        (ink (cond (done "success") ((equal keyword "NEXT") "success")
                   ((equal keyword "WAITING") "faint") (t "syn-todo"))))
    (jetpacs-with-attrs
     (jetpacs-design-styled
      (list (concat "bloom.agenda.state-" tone))
      (list (bloom-agenda--text "agenda-state" ink keyword)))
     :pad '(:top 2 :end 7))))

(defun bloom-agenda--action (name note)
  "Return action NAME addressed to NOTE's heading."
  (jetpacs-action name :args (list :notebook-id (plist-get note :notebook-id)
                                   :heading-id (plist-get note :heading-id))))

(defun bloom-agenda--row-content (note meta)
  "Return NOTE's row content with META strip items."
  (let ((done (plist-get note :done)))
    (jetpacs-with-attrs
     (apply
      #'jetpacs-row
      (delq
       nil
       (list
        (if (bloom-agenda--affordance-p note)
            (bloom-agenda--checkbox note)
          (jetpacs-spacer :width 20))
        (jetpacs-spacer :width 10)
        (jetpacs-with-attrs
         (apply
          #'jetpacs-column
          (delq
           nil
           (list
            (jetpacs-row
             (and (plist-get note :keyword)
                  (bloom-agenda--state-chip (plist-get note :keyword) done))
             (jetpacs-with-attrs
              (jetpacs-design-styled
               (list (if done "bloom.t.agenda-title-done" "bloom.t.agenda-title"))
               (list (jetpacs-text (plist-get note :title))))
              :weight 1)
             :align "top" :fill t)
            (and meta
                 (jetpacs-with-attrs
                  (apply #'jetpacs-flow-row
                         (append (mapcar #'bloom-agenda--meta-node meta)
                                 (list :spacing 9 :run-spacing 2)))
                  :pad '(:top 4))))))
         :weight 1)
        (and (plist-get note :priority)
             (jetpacs-spacer :width 10))
        (and (plist-get note :priority)
             (jetpacs-design-styled
              '("bloom.agenda.priority")
              (list (jetpacs-box
                     (bloom-agenda--text "agenda-priority"
                                         (bloom-agenda--priority-ink
                                          (plist-get note :priority))
                                         (plist-get note :priority))
                     :alignment "center"))))
        :align "top" :fill t)))
     :pad '(:horizontal 12 :vertical 10)
     :alpha (if done 0.55 1))))

(defun bloom-agenda--swipes (note)
  "Return (START . END) swipe actions for NOTE's row.
Like the app's defaults: the start side schedules or reveals the heading,
the end side marks it done, or offers only Reveal on a plain event."
  (cons
   (jetpacs-swipe
    (list (jetpacs-swipe-action
           "Sched" :icon "calendar_month" :color "primary"
           :on-trigger (jetpacs-action
                        "bloom.document.planning"
                        :args (list :notebook-id (plist-get note :notebook-id)
                                    :heading-id (plist-get note :heading-id)
                                    :focus "scheduled")))
          (jetpacs-swipe-action
           "Reveal" :icon "menu_book" :color "secondary"
           :on-trigger (bloom-agenda--action "bloom.agenda.reveal" note))))
   (and (bloom-agenda--affordance-p note)
        (jetpacs-swipe
         (list (jetpacs-swipe-action
                "Done" :icon "check" :color "primary"
                :on-trigger (bloom-agenda--action "bloom.agenda.toggle" note)))
         :commit t))))

(defun bloom-agenda--row (note meta key &optional plain)
  "Return NOTE's tappable row with META, keyed KEY.
PLAIN rows, inside the overdue card, carry no swipes."
  (let ((swipes (unless plain (bloom-agenda--swipes note))))
    (jetpacs-with-attrs
     (apply #'jetpacs-card
            (bloom-agenda--row-content note meta)
            (append
             (list :variant "filled"
                   :on-tap (bloom-document-open-action
                            (plist-get note :notebook-id)
                            (plist-get note :heading-id)))
             (and (car swipes) (list :swipe-start (car swipes)))
             (and (cdr swipes) (list :swipe-end (cdr swipes)))))
     :key key)))

;;;; Sections

(defun bloom-agenda--group-header (label count)
  "Return a section header: LABEL, its COUNT, and a hairline."
  (jetpacs-with-attrs
   (jetpacs-row
    (bloom-agenda--text "agenda-group" nil (upcase label))
    (jetpacs-spacer :width 8)
    (bloom-agenda--text "agenda-group-count" nil (number-to-string count))
    (jetpacs-spacer :width 8)
    (jetpacs-with-attrs (jetpacs-design-styled
                         '("bloom.agenda.hairline")
                         (list (jetpacs-spacer :height 1)))
                        :weight 1)
    :align "center" :fill t)
   :pad '(:start 2 :end 2 :bottom 4)))

(defun bloom-agenda--sections (visible list today grouping today-tab)
  "Return (LABEL . ENTRIES) sections of LIST seen from TODAY.
VISIBLE supplies the events woven into date sections; GROUPING and
TODAY-TAB choose the buckets."
  (pcase grouping
    ("date"
     (if today-tab
         (let ((entries (bloom-agenda--day-entries visible list today)))
           (and entries
                (list (cons "Scheduled today"
                            (sort entries #'bloom-agenda--by-time)))))
       (let ((days nil))
         (dolist (note list)
           (cl-pushnew (bloom-agenda--when-date note) days :test #'equal))
         (cl-loop for offset from 1 below bloom-agenda-window-days
                  for day = (bloom-org-date-shift today offset)
                  when (bloom-agenda--events-on visible day)
                  do (cl-pushnew day days :test #'equal))
         (mapcar
          (lambda (day)
            (cons (cons day (bloom-agenda-day-label day today))
                  (sort (bloom-agenda--day-entries
                         visible
                         (seq-filter (lambda (note)
                                       (equal (bloom-agenda--when-date note) day))
                                     list)
                         day)
                        #'bloom-agenda--by-time)))
          (sort days #'string<)))))
    ("priority"
     (delq nil
           (mapcar (pcase-lambda (`(,key . ,label))
                     (let ((entries (mapcar #'list
                                            (seq-filter
                                             (lambda (note)
                                               (equal (plist-get note :priority) key))
                                             list))))
                       (and entries
                            (cons label (sort entries #'bloom-agenda--by-time)))))
                   '(("A" . "Priority A") ("B" . "Priority B")
                     ("C" . "Priority C") (nil . "No priority")))))
    ("tag"
     (let ((tags nil))
       (dolist (note list)
         (dolist (tag (or (plist-get note :tags) '("untagged")))
           (unless (member tag tags) (setq tags (append tags (list tag))))))
       (mapcar (lambda (tag)
                 (cons (concat ":" tag ":")
                       (sort (mapcar #'list
                                     (seq-filter
                                      (lambda (note)
                                        (member tag (or (plist-get note :tags)
                                                        '("untagged"))))
                                      list))
                             #'bloom-agenda--by-priority)))
               tags)))
    (_
     (let ((files nil))
       (dolist (note list)
         (unless (member (plist-get note :file) files)
           (setq files (append files (list (plist-get note :file))))))
       (mapcar (lambda (file)
                 (cons file
                       (sort (mapcar #'list
                                     (seq-filter
                                      (lambda (note)
                                        (equal (plist-get note :file) file))
                                      list))
                             #'bloom-agenda--by-priority)))
               files)))))

(defun bloom-agenda-state (&optional notes today)
  "Return the Agenda's visible state over NOTES seen from TODAY.
NOTES default to `bloom-agenda-notes' and TODAY to the current date."
  (let* ((notes (or notes (bloom-agenda-notes)))
         (today (or today (bloom-agenda--today)))
         (keywords (delete-dups
                    (cl-mapcan (lambda (note)
                                 (copy-sequence (plist-get note :active-keywords)))
                               notes)))
         (today-tab (eq bloom-agenda--tab 'today))
         (grouping (bloom-agenda--grouping))
         (filter (bloom-agenda--filter keywords))
         (visible (seq-filter (lambda (note) (bloom-agenda--keep-p note filter)) notes))
         (on-today (seq-filter (lambda (note)
                                 (equal (bloom-agenda--when-date note) today))
                               visible))
         (horizon (bloom-org-date-shift today (1- bloom-agenda-window-days)))
         (future (seq-filter (lambda (note)
                               (let ((date (bloom-agenda--when-date note)))
                                 (and date (> (bloom-org--date-delta date today) 0)
                                      (<= (bloom-org--date-delta date horizon) 0))))
                             visible))
         (list (if today-tab on-today future))
         (show-date (and (not (equal grouping "date")) (not today-tab)))
         (sections (bloom-agenda--sections visible list today grouping today-tab)))
    (list :day (bloom-agenda--weekday today)
          :date (bloom-agenda--long-date today)
          :today-count (+ (length on-today)
                          (length (bloom-agenda--events-on visible today)))
          :keywords keywords :grouping grouping :filter filter
          :overdue (bloom-agenda--overdue visible today)
          :sections
          (mapcar
           (lambda (section)
             (let* ((head (car section))
                    (day (and (consp head) (car head)))
                    (label (if (consp head) (cdr head) head)))
               (list :label label
                     :rows (mapcar
                            (pcase-lambda (`(,note . ,stamp))
                              (list :note note
                                    :meta (bloom-agenda--meta
                                           note today show-date stamp
                                           (and stamp (or day today)))
                                    :key (jetpacs-wire-id
                                          "agenda-row"
                                          (format "%s|%s|%s" label
                                                  (plist-get note :heading-id)
                                                  (plist-get stamp :date)))))
                            (cdr section)))))
           sections)
          :today today)))

;;;; Screen

(defun bloom-agenda--header (state back)
  "Return the day headline over STATE's date and count, BACK and levers."
  (jetpacs-with-attrs
   (jetpacs-row
    ;; System Back reads this leading `arrow_back' button; its 48 dp target
    ;; centres where the app's 36 dp glyph circle centres.
    (jetpacs-icon-button "arrow_back" back :content-description "Back")
    (jetpacs-spacer :width 2)
    (jetpacs-with-attrs
     (jetpacs-column
      (bloom-agenda--text "agenda-day" nil (plist-get state :day))
      (bloom-agenda--text "agenda-date" nil
                          (format "%s · %d scheduled" (plist-get state :date)
                                  (plist-get state :today-count))
                          :pad '(:top 1)))
     ;; The app rounds each of the two line boxes up to whole pixels.
     :weight 1 :pad '(:top 6 :bottom 0.7))
    (jetpacs-spacer :width 8)
    (jetpacs-with-attrs
     (jetpacs-design-styled
      (list "bloom.agenda.levers"
            (if bloom-agenda--levers-open "bloom.agenda.levers-on" "bloom.agenda.levers-off"))
      (list (jetpacs-with-semantics
             (jetpacs-box (bloom-agenda--text
                           "agenda-levers"
                           (if bloom-agenda--levers-open "primary" "muted") "⇅")
                          :alignment "center"
                          :on-tap (jetpacs-action "bloom.agenda.levers"))
             :name "Agenda options")))
     :pad '(:top 6))
    :align "top" :fill t)
   :pad '(:top 4 :end 14)))

(defun bloom-agenda--tabs ()
  "Return the Today/Upcoming switch."
  (jetpacs-with-attrs
   (jetpacs-design-styled
    '("bloom.agenda.tabs")
    (list
     (jetpacs-row
      (bloom-agenda--tab-option 'today "Today")
      (bloom-agenda--tab-option 'upcoming "Upcoming")
      :spacing 3 :fill t)))
   :pad '(:start 14 :top 11 :end 14 :bottom 14)))

(defun bloom-agenda--tab-option (tab label)
  "Return TAB's option of the switch, named LABEL."
  (let ((on (eq bloom-agenda--tab tab)))
    (jetpacs-with-attrs
     (jetpacs-design-styled
      (list "bloom.agenda.tab" (if on "bloom.agenda.tab-on" "bloom.agenda.tab-off"))
      (list (jetpacs-box (bloom-agenda--text "agenda-tab"
                                             (if on "on-background" "faint") label)
                         :alignment "center"
                         :on-tap (jetpacs-action "bloom.agenda.tab"
                                                 :args (list :tab (symbol-name tab))))))
     :weight 1)))

(defun bloom-agenda--lever-chip (label on action)
  "Return a levers-panel chip named LABEL, lit when ON, dispatching ACTION."
  (jetpacs-design-styled
   (list "bloom.agenda.chip" (if on "bloom.agenda.chip-on" "bloom.agenda.chip-off"))
   (list (jetpacs-box (bloom-agenda--text "agenda-lever-chip"
                                          (if on "on-primary" "muted") label)
                      :on-tap action))))

(defun bloom-agenda--lever-toggle (label on field)
  "Return a levers-panel switch row named LABEL, ON or off, for FIELD."
  (jetpacs-row
   (jetpacs-with-attrs (bloom-agenda--text "agenda-toggle" nil label) :weight 1)
   (jetpacs-spacer :width 10)
   (jetpacs-design-styled
    (list "bloom.agenda.track" (if on "bloom.agenda.track-on" "bloom.agenda.track-off"))
    (list (jetpacs-with-semantics
           (jetpacs-box
            (jetpacs-with-attrs
             (jetpacs-design-styled '("bloom.agenda.knob")
                                    (list (jetpacs-spacer :width 16 :height 16)))
             :pad (list :start (if on 17.5 2.5) :top 2.5))
            :on-tap (jetpacs-action "bloom.agenda.lever"
                                    :args (list :field field
                                                :value (if on "off" "on"))))
           :name label)))
   :align "center" :fill t))

(defun bloom-agenda--levers (state)
  "Return the levers panel for STATE."
  (let ((grouping (plist-get state :grouping))
        (filter (plist-get state :filter)))
    (jetpacs-with-attrs
     (jetpacs-design-styled
      '("bloom.agenda.panel")
      (list
       (jetpacs-column
        (bloom-agenda--text "agenda-lever-label" nil "GROUP BY" :pad '(:bottom 8))
        (apply #'jetpacs-flow-row
               (append
                (mapcar (pcase-lambda (`(,key . ,label))
                          (bloom-agenda--lever-chip
                           label (equal grouping key)
                           (jetpacs-action "bloom.agenda.lever"
                                           :args (list :field "grouping" :value key))))
                        '(("date" . "Date") ("priority" . "Priority")
                          ("tag" . "Tag") ("file" . "File")))
                (list :spacing 6 :run-spacing 6)))
        (bloom-agenda--text "agenda-lever-label" nil "SHOW" :pad '(:top 14 :bottom 8))
        (apply #'jetpacs-flow-row
               (append
                (mapcar (pcase-lambda (`(,key . ,label))
                          (bloom-agenda--lever-chip
                           label (equal filter key)
                           (jetpacs-action "bloom.agenda.lever"
                                           :args (list :field "filter" :value key))))
                        (append '(("open" . "Open"))
                                (mapcar (lambda (k) (cons k k)) (plist-get state :keywords))
                                '(("all" . "Everything"))))
                (list :spacing 6 :run-spacing 6)))
        (jetpacs-with-attrs
         (bloom-agenda--lever-toggle "Tags on rows" bloom-agenda-show-tags "tags")
         :pad '(:top 14))
        (jetpacs-with-attrs
         (bloom-agenda--lever-toggle "Source file on rows" bloom-agenda-show-file "file")
         :pad '(:top 11))
        (jetpacs-with-attrs
         (bloom-agenda--lever-toggle "Timestamp chips on rows"
                                     bloom-agenda-show-timestamps "timestamps")
         :pad '(:top 11)))))
     :pad '(:start 14 :top 10 :end 14))))

(defun bloom-agenda--overdue-card (state)
  "Return the red overdue card for STATE's overdue rows."
  (let* ((rows (plist-get state :overdue))
         (today (plist-get state :today)))
    (jetpacs-with-attrs
     (jetpacs-design-styled
      '("bloom.agenda.overdue")
      (list
       (apply
        #'jetpacs-column
        (append
         (list
          (jetpacs-row
           (jetpacs-with-attrs
            (jetpacs-box
             (jetpacs-row
              (bloom-agenda--text "agenda-overdue-caret" nil
                                  (if bloom-agenda--overdue-open "▾" "▸"))
              (jetpacs-spacer :width 9)
              (bloom-agenda--text "agenda-overdue-count" nil
                                  (format "%d overdue" (length rows)))
              :align "center")
             :on-tap (jetpacs-action "bloom.agenda.overdue"))
            :weight 1)
           (jetpacs-spacer :width 9)
           (jetpacs-design-styled
            '("bloom.agenda.move")
            (list (jetpacs-box (bloom-agenda--text "agenda-move" nil "Move to today")
                               :on-tap (jetpacs-action "bloom.agenda.move-overdue"))))
           :align "center" :fill t))
         (and bloom-agenda--overdue-open
              (cons (jetpacs-spacer :height 4)
                    (mapcar (lambda (note)
                              (bloom-agenda--row
                               note (bloom-agenda--meta note today t)
                               (jetpacs-wire-id "agenda-overdue"
                                                (plist-get note :heading-id))
                               t))
                            rows)))))))
     :pad '(:bottom 14))))

(defun bloom-agenda--empty ()
  "Return the empty-list message for the visible tab."
  (jetpacs-with-attrs
   (jetpacs-column
    (bloom-agenda--text "empty-title" nil "Nothing scheduled")
    (bloom-agenda--text "agenda-empty-caption" nil
                        (if (eq bloom-agenda--tab 'today) "Nothing on today's plate."
                          "Nothing coming up.")
                        :pad '(:top 7))
    :align "center" :fill t)
   :pad '(:top 52)))

(defun bloom-agenda--list (state)
  "Return the agenda list's children for STATE."
  (append
   (and (null (plist-get state :sections)) (list (bloom-agenda--empty)))
   (and (plist-get state :overdue) (list (bloom-agenda--overdue-card state)))
   (cl-mapcan
    (lambda (section)
      (append
       (list (bloom-agenda--group-header (plist-get section :label)
                                         (length (plist-get section :rows))))
       (mapcar (lambda (row)
                 (bloom-agenda--row (plist-get row :note) (plist-get row :meta)
                                    (plist-get row :key)))
               (plist-get section :rows))
       (list (jetpacs-spacer :height 14))))
    (plist-get state :sections))))

(defun bloom-agenda-screen (back)
  "Return the parity Agenda screen with BACK."
  (let ((state (bloom-agenda-state)))
    (jetpacs-design-profile-scope
     (jetpacs-design-active-profile bloom-design-profile-id)
     (list
      (jetpacs-with-semantics
       ;; The app's Agenda has no floating button.  An explicit, empty
       ;; `:fab' keeps the app-default capture button out of the slot.
       (append
        (jetpacs-scaffold
         :top-bar
        (apply #'jetpacs-column
               (delq nil
                     (list (bloom-agenda--header state back)
                           (bloom-agenda--tabs)
                           (and bloom-agenda--levers-open
                                (bloom-agenda--levers state)))))
        :body
        (jetpacs-design-scope
         nil nil
         (list
          (jetpacs-with-attrs
           (apply #'jetpacs-lazy-column
                  (append (list (jetpacs-spacer :height 12))
                          (bloom-agenda--list state)
                          (list (jetpacs-spacer :height 20))))
           :pad '(:horizontal 14)))
         :component-styles
         (list (jetpacs-design-component-style "card.filled" '("bloom.agenda.row")))))
        (list :fab nil))
       :pane-title "Agenda")))))

;;;; Actions

(defun bloom-agenda--persist (symbol value)
  "Set lever SYMBOL to VALUE and save it, as Customize does."
  (if (fboundp 'customize-save-variable)
      (ignore-errors (customize-save-variable symbol value))
    (set-default symbol value)))

(defun bloom-agenda-on-tab (args params)
  "Show ARGS' tab and refresh PARAMS' surface."
  (pcase (plist-get args :tab)
    ((and tab (or "today" "upcoming"))
     (setq bloom-agenda--tab (intern tab))
     (jetpacs-app-defer-refresh params)
     'accepted)
    (_ 'rejected)))

(defun bloom-agenda-on-levers (_args params)
  "Open or close the levers panel and refresh PARAMS' surface."
  (setq bloom-agenda--levers-open (not bloom-agenda--levers-open))
  (jetpacs-app-defer-refresh params)
  'accepted)

(defun bloom-agenda-on-overdue (_args params)
  "Expand or collapse the overdue card and refresh PARAMS' surface."
  (setq bloom-agenda--overdue-open (not bloom-agenda--overdue-open))
  (jetpacs-app-defer-refresh params)
  'accepted)

(defun bloom-agenda-on-lever (args params)
  "Set ARGS' lever field to its value for the visible tab; refresh PARAMS."
  (let ((field (plist-get args :field))
        (value (plist-get args :value))
        (today (eq bloom-agenda--tab 'today)))
    (if (not (and (stringp field) (stringp value))) 'rejected
      (pcase field
        ("grouping"
         (when (member value '("date" "priority" "tag" "file"))
           (bloom-agenda--persist (if today 'bloom-agenda-grouping-today
                                    'bloom-agenda-grouping-upcoming)
                                  value)))
        ("filter"
         (bloom-agenda--persist (if today 'bloom-agenda-filter-today
                                  'bloom-agenda-filter-upcoming)
                                value))
        ("tags" (bloom-agenda--persist 'bloom-agenda-show-tags (equal value "on")))
        ("file" (bloom-agenda--persist 'bloom-agenda-show-file (equal value "on")))
        ("timestamps"
         (bloom-agenda--persist 'bloom-agenda-show-timestamps (equal value "on"))))
      (jetpacs-app-defer-refresh params)
      'accepted)))

(defun bloom-agenda--with-heading (args fn)
  "Call FN at ARGS' heading in its Org buffer; return its value or `stale'."
  (let* ((notebook-id (plist-get args :notebook-id))
         (heading-id (plist-get args :heading-id))
         (ref (and (stringp notebook-id) (stringp heading-id)
                   (bloom-library-resolve-heading notebook-id heading-id))))
    (if (null ref) 'stale
      (let ((marker (ebp-org-resolve-ref ref)))
        (unwind-protect
            (with-current-buffer (marker-buffer marker)
              (org-with-wide-buffer
               (goto-char marker)
               (org-back-to-heading t)
               (prog1 (funcall fn)
                 (save-buffer)
                 (bloom-document--after-mutation notebook-id heading-id))))
          (set-marker marker nil))))))

(defun bloom-agenda--advance-repeater ()
  "Move the heading at point's repeating plan to its next occurrence.
A keyword-less heading has nothing to mark done, so its SCHEDULED, or else
DEADLINE, stamp steps forward by its repeater, leaving keywords alone."
  (let ((today (bloom-agenda--today)))
    (save-excursion
      (when (re-search-forward
             (concat "\\(?:" org-scheduled-string "\\|" org-deadline-string "\\) *\\(<[^>]+>\\)")
             (save-excursion (outline-next-heading) (point)) t)
        (let* ((start (match-beginning 1)) (end (match-end 1))
               (stamp (bloom-agenda-parse-timestamp (match-string 1)))
               (repeater (plist-get stamp :repeater)))
          (when (and repeater
                     (string-match "\\([.+]?\\+\\)\\([0-9]+\\)\\([hdwmy]\\)" repeater))
            (let* ((kind (match-string 1 repeater))
                   (n (string-to-number (match-string 2 repeater)))
                   (unit (pcase (match-string 3 repeater)
                           ("h" 'hour) ("d" 'day) ("w" 'week) ("m" 'month) ("y" 'year)))
                   (text (buffer-substring start end)))
              (with-temp-buffer
                (org-mode)
                (insert text)
                (goto-char 2)
                (let ((org-display-custom-times nil))
                  (cl-loop do (org-timestamp-change
                               (if (eq unit 'week) (* 7 n) n)
                               (if (eq unit 'week) 'day unit))
                           while (and (not (equal kind "+"))
                                      (<= (bloom-org--date-delta
                                           (plist-get (bloom-agenda-parse-timestamp
                                                       (buffer-string))
                                                      :date)
                                           today)
                                          0))))
                (setq text (buffer-string)))
              (let ((inhibit-read-only t))
                (delete-region start end)
                (goto-char start)
                (insert text))
              t)))))))

(defun bloom-agenda-on-toggle (args params)
  "Mark ARGS' heading done, reopen it, or advance its repeater; refresh PARAMS."
  (let ((result
         (condition-case err
             (bloom-agenda--with-heading
              args
              (lambda ()
                (let ((keyword (org-get-todo-state)))
                  (cond
                   ((null keyword) (bloom-agenda--advance-repeater))
                   ((member keyword org-done-keywords)
                    (org-todo (or (car (cl-set-difference org-todo-keywords-1
                                                          org-done-keywords
                                                          :test #'equal))
                                  'none)))
                   (t (org-todo (car org-done-keywords)))))
                t))
           (error (message "bloom: agenda toggle failed (%s)" (car err)) 'failed))))
    (pcase result
      ('stale 'stale)
      ('failed 'rejected)
      (_ (jetpacs-app-defer-refresh params) 'accepted))))

(defun bloom-agenda--move-to-today (note today)
  "Rewrite NOTE's overdue SCHEDULED, or lone DEADLINE, to TODAY.
Time of day, repeater and warning all survive the move."
  (bloom-agenda--with-heading
   (list :notebook-id (plist-get note :notebook-id)
         :heading-id (plist-get note :heading-id))
   (lambda ()
     (let ((word (if (plist-get note :scheduled) org-scheduled-string
                   org-deadline-string)))
       (save-excursion
         (when (re-search-forward
                (concat (regexp-quote word)
                        " *<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?: [[:alpha:]]+\\)?\\)")
                (save-excursion (outline-next-heading) (point)) t)
           (replace-match (concat today " " (substring (bloom-agenda--weekday today) 0 3))
                          t t nil 1)
           t))))))

(defun bloom-agenda-on-move-overdue (_args params)
  "Move every overdue row to today and refresh PARAMS' surface."
  (let* ((state (bloom-agenda-state))
         (today (plist-get state :today))
         (moved 0))
    (dolist (note (plist-get state :overdue))
      (when (eq t (condition-case nil (bloom-agenda--move-to-today note today)
                    (error nil)))
        (setq moved (1+ moved))))
    (setq bloom-agenda--overdue-open nil)
    (jetpacs-shell-notify (format "Moved %d to today" moved) (plist-get params :surface))
    (jetpacs-app-defer-refresh params)
    'accepted))

(defun bloom-agenda-on-reveal (args params)
  "Show ARGS' heading in its notebook's outline on PARAMS' surface."
  (let ((notebook-id (plist-get args :notebook-id)))
    (if (not (stringp notebook-id)) 'rejected
      (if (fboundp 'bloom-ui-open-outline)
          (progn (bloom-ui-open-outline notebook-id)
                 (ignore params)
                 'accepted)
        'rejected))))

(defconst bloom-agenda-action-names
  '("bloom.agenda.tab" "bloom.agenda.levers" "bloom.agenda.overdue"
    "bloom.agenda.lever" "bloom.agenda.toggle" "bloom.agenda.move-overdue"
    "bloom.agenda.reveal")
  "Action names owned by Bloom's Agenda module.")

(defun bloom-agenda-register ()
  "Register the Agenda's actions."
  (with-jetpacs-owner bloom-owner
    (jetpacs-defaction "bloom.agenda.tab" #'bloom-agenda-on-tab
                       :args '((:name tab :type "enum" :required t))
                       :doc "Show the Agenda's Today or Upcoming tab")
    (jetpacs-defaction "bloom.agenda.levers" #'bloom-agenda-on-levers
                       :doc "Open or close the Agenda's options panel")
    (jetpacs-defaction "bloom.agenda.overdue" #'bloom-agenda-on-overdue
                       :doc "Expand or collapse the Agenda's overdue card")
    (jetpacs-defaction "bloom.agenda.lever" #'bloom-agenda-on-lever
                       :args '((:name field :type "enum" :required t)
                               (:name value :type "text" :required t))
                       :doc "Set one Agenda grouping, filter or row option")
    (jetpacs-defaction "bloom.agenda.toggle" #'bloom-agenda-on-toggle
                       :args '((:name notebook-id :type "ref" :required t)
                               (:name heading-id :type "ref" :required t))
                       :doc "Mark an agenda heading done or reopen it")
    (jetpacs-defaction "bloom.agenda.move-overdue" #'bloom-agenda-on-move-overdue
                       :doc "Move every overdue agenda heading to today")
    (jetpacs-defaction "bloom.agenda.reveal" #'bloom-agenda-on-reveal
                       :args '((:name notebook-id :type "ref" :required t)
                               (:name heading-id :type "ref" :required t))
                       :doc "Show an agenda heading in its notebook outline")))

(provide 'bloom-agenda)
;;; bloom-agenda.el ends here
