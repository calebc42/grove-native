;;; grove-org.el --- Org adapter for the Grove applet -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Grove is a skin over Emacs Org, not a second Org implementation.  This
;; module is its one package adapter: it asks Jetpacs' canonical Org agenda
;; extractor for data, normalizes that data into an explicit deterministic
;; snapshot, and keeps the private Org references on the Emacs side.
;;
;; Builders never call Org or mint capabilities.  A producer refreshes the
;; snapshot after relevant Org changes; app and widget builders only read it.
;; Rows carry a stable opaque item id on the wire.  After process death Grove
;; rebuilds the snapshot before EBP queue replay, so an offline occurrence can
;; resolve that id again without exposing a path or persisting a second store.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'calendar)
(require 'ebp-org)
(require 'jetpacs-org-reminders)
(require 'grove-core)
(require 'grove-library)

(defcustom grove-org-agenda-days 8
  "Number of calendar days represented in Grove's agenda snapshot."
  :type '(integer 1 31)
  :group 'grove)

(defcustom grove-org-agenda-item-limit 80
  "Maximum number of Org agenda rows retained in one Grove snapshot."
  :type '(integer 1 512)
  :group 'grove)

(defcustom grove-org-planning-mark-limit 512
  "Maximum number of agenda occurrences scanned for planning-calendar marks.
The resulting projection contains only ISO dates and capped occurrence counts;
headlines, paths, and Org references never enter planning screen state."
  :type '(integer 1 2048)
  :group 'grove)

(defconst grove-org--weekdays
  ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"]
  "Locale-independent weekday labels used by Grove's visual language.")

(defconst grove-org--months
  ["January" "February" "March" "April" "May" "June"
   "July" "August" "September" "October" "November" "December"]
  "Locale-independent month labels used by Grove's visual language.")

(defvar grove-org-after-refresh-hook nil
  "Hook run after Grove replaces or marks its explicit agenda snapshot.")

(defvar grove-org--refresh-timer nil
  "Pending idle refresh after an Org state change, or nil.")

(defvar grove-org--snapshot
  '(:day-label "Today" :date-label "" :updated-label "Not refreshed"
    :today-count 0 :total-count 0 :items nil :error nil)
  "Current explicit, presentation-independent Grove agenda snapshot.")

(defun grove-org-snapshot ()
  "Return a defensive copy of Grove's current agenda snapshot."
  (copy-tree grove-org--snapshot))

(defun grove-org--date-parts (date)
  "Return Gregorian (MONTH DAY YEAR) for ISO DATE, or nil."
  (when (and (stringp date)
             (string-match
              "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'"
              date))
    (list (string-to-number (match-string 2 date))
          (string-to-number (match-string 3 date))
          (string-to-number (match-string 1 date)))))

(defun grove-org-date-valid-p (date)
  "Return non-nil when DATE is a real ISO Gregorian calendar date."
  (when-let* ((parts (grove-org--date-parts date)))
    (calendar-date-is-valid-p parts)))

(defun grove-org-month-valid-p (month)
  "Return non-nil when MONTH has the canonical YYYY-MM form."
  (and (stringp month)
       (string-match-p
        "\\`[0-9]\\{4\\}-\\(?:0[1-9]\\|1[0-2]\\)\\'" month)))

(defun grove-org--date-delta (date today)
  "Return DATE's signed day offset from TODAY, or nil.
DATE and TODAY are ISO calendar dates."
  (when-let* (((grove-org-date-valid-p date))
              ((grove-org-date-valid-p today))
              (date-parts (grove-org--date-parts date))
              (today-parts (grove-org--date-parts today)))
    (- (calendar-absolute-from-gregorian date-parts)
       (calendar-absolute-from-gregorian today-parts))))

(defun grove-org-date-shift (date days)
  "Return ISO DATE shifted by integer DAYS, or nil for invalid input."
  (when (and (grove-org-date-valid-p date) (integerp days))
    (pcase-let* ((absolute
                  (+ (calendar-absolute-from-gregorian
                      (grove-org--date-parts date))
                     days))
                 (`(,month ,day ,year)
                  (calendar-gregorian-from-absolute absolute)))
      (format "%04d-%02d-%02d" year month day))))

(defun grove-org--agenda-item-date (item)
  "Return ITEM's canonical ISO occurrence date, or nil."
  (let ((date (alist-get 'date item))
        (absolute (alist-get 'ts-date item)))
    (cond
     ((grove-org-date-valid-p date) date)
     ((numberp absolute)
      (pcase-let ((`(,month ,day ,year)
                    (calendar-gregorian-from-absolute absolute)))
        (format "%04d-%02d-%02d" year month day))))))

(defun grove-org-planning-marks (month)
  "Return bounded agenda occurrence counts for ISO MONTH.
The result is a date-sorted alist of (YYYY-MM-DD . COUNT), with COUNT capped
at three to match the neutral month-grid mark grammar.  This producer performs
the Org agenda read; planning builders only consume its explicit result."
  (unless (grove-org-month-valid-p month)
    (error "Grove planning marks: invalid month %S" month))
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (item
             (seq-take (jetpacs-org-mode-agenda-items
                        'month (concat month "-01"))
                       grove-org-planning-mark-limit))
      (when-let* ((date (grove-org--agenda-item-date item))
                  ((string-prefix-p month date)))
        (puthash date (min 3 (1+ (gethash date counts 0))) counts)))
    (let (marks)
      (maphash (lambda (date count) (push (cons date count) marks)) counts)
      (sort marks (lambda (left right) (string< (car left) (car right)))))))

(defun grove-org--section-label (date today type)
  "Return Grove's ledger section for agenda DATE, TODAY, and TYPE."
  (let ((delta (grove-org--date-delta date today)))
    (cond
     ((or (and (stringp type) (string-match-p "past" type))
          (and delta (< delta 0)))
      "OVERDUE")
     ((equal delta 0) "TODAY")
     ((equal delta 1) "TOMORROW")
     ((and delta (> delta 1))
      (upcase
       (aref grove-org--weekdays
             (calendar-day-of-week (grove-org--date-parts date)))))
     (t "AGENDA"))))

(defun grove-org--item-id (ref)
  "Return a stable opaque wire identifier for private Org REF."
  (let* ((file (or (plist-get ref :file) ""))
         (org-id (plist-get ref :id))
         (identity
          (if (and (stringp org-id) (not (string-empty-p org-id)))
              (format "%s|id:%s" file org-id)
            (format "%s|pos:%s|heading:%s" file
                    (or (plist-get ref :pos) 0)
                    (or (plist-get ref :headline) "")))))
    (concat "grove-item-" (substring (secure-hash 'sha256 identity) 0 32))))

(defalias 'grove-org--item-time #'jetpacs-org-reminders-item-hm
  "Normalize agenda time-grid RAW to HH:MM, or nil (the foundation's).")

(defun grove-org--item-identity (item)
  "Return ITEM's stable deduplication identity."
  (grove-org--item-id (alist-get 'ref item)))

(defun grove-org--deduplicate (items)
  "Return ITEMS with repeated agenda occurrences removed.
Org may emit one heading twice when its scheduled and deadline timestamps
coincide.  Grove renders one row and lets Org keep the underlying semantics."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (item items (nreverse result))
      (let ((identity (grove-org--item-identity item)))
        (unless (gethash identity seen)
          (puthash identity t seen)
          (push item result))))))

(defun grove-org--normalize-item (item today)
  "Normalize one canonical agenda ITEM relative to TODAY."
  (let* ((ref (copy-tree (alist-get 'ref item)))
         (date (alist-get 'date item))
         (type (alist-get 'type item))
         (tags (alist-get 'tags item))
         (file (alist-get 'file item)))
    (list :id (grove-org--item-id ref)
          :ref ref
          :headline (or (alist-get 'headline item) "Untitled")
          :todo (alist-get 'todo item)
          :priority (alist-get 'priority item)
          :tags (cond ((vectorp tags) (append tags nil))
                      ((listp tags) (copy-sequence tags))
                      (t nil))
          :file-label (and (stringp file) (file-name-nondirectory file))
          :time (grove-org--item-time (alist-get 'time item))
          :date date
          :type type
          :section (grove-org--section-label date today type))))

(defun grove-org--row-sort-key (row)
  "Return a deterministic lexical ordering key for normalized ROW."
  (format "%s|%s|%s|%s|%s"
          (if (equal (plist-get row :section) "OVERDUE") "0" "1")
          (or (plist-get row :date) "9999-99-99")
          (or (plist-get row :time) "99:99")
          (downcase (or (plist-get row :headline) ""))
          (plist-get row :id)))

(defun grove-org--today-metadata (now)
  "Return Grove day/date/update labels for Emacs time value NOW."
  (let* ((today (format-time-string "%Y-%m-%d" now))
         (parts (grove-org--date-parts today))
         (month (nth 0 parts))
         (day (nth 1 parts)))
    (list :today today
          :day-label
          (aref grove-org--weekdays (calendar-day-of-week parts))
          :date-label (format "%s %d" (aref grove-org--months (1- month)) day)
          :updated-label (format-time-string "Updated %H:%M" now))))

(defun grove-org-refresh (&optional now)
  "Refresh Grove's explicit agenda snapshot from Emacs Org.
NOW is an Emacs time value used only to make date grouping explicit and
testable.  Return non-nil on success.  On failure, retain the last safe rows,
mark the snapshot unavailable without recording sensitive error text, and
return nil."
  (interactive)
  (let* ((now (or now (current-time)))
         (metadata (grove-org--today-metadata now))
         (today (plist-get metadata :today)))
    (condition-case err
        (let* ((raw (jetpacs-org-mode-agenda-items
                     grove-org-agenda-days nil))
               (bounded (seq-take (grove-org--deduplicate raw)
                                  grove-org-agenda-item-limit))
               (rows (mapcar (lambda (item)
                               (grove-org--normalize-item item today))
                             bounded))
               (rows (sort rows
                           (lambda (left right)
                             (string< (grove-org--row-sort-key left)
                                      (grove-org--row-sort-key right)))))
               (today-count
                (cl-count "TODAY" rows :key (lambda (row)
                                               (plist-get row :section))
                          :test #'equal)))
          (setq grove-org--snapshot
                (list :day-label (plist-get metadata :day-label)
                      :date-label (plist-get metadata :date-label)
                      :updated-label (plist-get metadata :updated-label)
                      :today-count today-count
                      :total-count (length rows)
                      :items rows
                      :error nil))
          (run-hooks 'grove-org-after-refresh-hook)
          t)
      (error
       ;; Do not log ERR's message: Org/file errors may carry private paths or
       ;; heading text.  The condition symbol is sufficient local evidence.
       (message "grove-org: agenda refresh failed (%s)" (car err))
       (setq grove-org--snapshot
             (plist-put (copy-tree grove-org--snapshot)
                        :error "Agenda unavailable"))
       (run-hooks 'grove-org-after-refresh-hook)
       nil))))

(defun grove-org-ref-for-id (item-id)
  "Return the private Org reference for opaque ITEM-ID, or nil.
The returned plist is a copy and never belongs in a Jetpacs node tree."
  (when (stringp item-id)
    (when-let* ((row (cl-find item-id (plist-get grove-org--snapshot :items)
                             :key (lambda (candidate)
                                    (plist-get candidate :id))
                             :test #'equal)))
      (copy-tree (plist-get row :ref)))))

(defun grove-org--run-scheduled-refresh ()
  "Run and clear Grove's coalesced library and agenda refresh."
  (setq grove-org--refresh-timer nil)
  (grove-library-reset-projections)
  (grove-library-refresh)
  (grove-org-refresh))

(defun grove-org-schedule-refresh (&rest _ignored)
  "Coalesce a future Grove agenda refresh after an Org state change."
  (unless (timerp grove-org--refresh-timer)
    (setq grove-org--refresh-timer
          (run-with-idle-timer 0.2 nil #'grove-org--run-scheduled-refresh))))

(defun grove-org--after-save ()
  "Schedule a snapshot refresh after saving an Org buffer."
  (when (derived-mode-p 'org-mode)
    (grove-org-schedule-refresh)))

(defun grove-org-register ()
  "Install Grove's Org invalidation hooks and produce its first snapshot.
The hooks are global Org hooks, ledgered under the app's owner so its
teardown detaches them."
  (with-jetpacs-owner grove-owner
    (jetpacs-add-hook 'after-save-hook #'grove-org--after-save)
    (jetpacs-add-hook 'org-capture-after-finalize-hook
                      #'grove-org-schedule-refresh)
    (jetpacs-add-hook 'org-after-todo-state-change-hook
                      #'grove-org-schedule-refresh))
  (grove-org-refresh))

(defun grove-org-unregister ()
  "Remove Grove's Org hooks, timer, and private snapshot state."
  (jetpacs-remove-hook 'after-save-hook #'grove-org--after-save)
  (jetpacs-remove-hook 'org-capture-after-finalize-hook
                       #'grove-org-schedule-refresh)
  (jetpacs-remove-hook 'org-after-todo-state-change-hook
                       #'grove-org-schedule-refresh)
  (when (timerp grove-org--refresh-timer)
    (cancel-timer grove-org--refresh-timer))
  (setq grove-org--refresh-timer nil
        grove-org--snapshot
        '(:day-label "Today" :date-label "" :updated-label "Not refreshed"
          :today-count 0 :total-count 0 :items nil :error nil))
  t)

(provide 'grove-org)
;;; grove-org.el ends here
