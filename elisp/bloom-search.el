;;; bloom-search.el --- The Android app's Search screen over native Org search -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Search as the Android app draws it, answered by Emacs: the search field
;; row, a result count and Advanced toggle, the Quick Start cards and saved
;; searches while the query is blank, results grouped by notebook, and the
;; Filters bar.
;;
;; The app has its own query language; Bloom does not copy it.  A query is
;; native Emacs search, chosen by its shape:
;;
;; - a Lisp form, such as (and (todo) (tags "work")), runs through org-ql when
;;   org-ql is installed;
;; - Org's tags/property/TODO match syntax, such as +work/!TODO or
;;   SCHEDULED<"<today>", runs through Org's own matcher;
;; - anything else is Org's agenda text search, with its Boolean words and
;;   {regexp} forms.
;;
;; The four Quick Start cards are Org match queries, so tapping one puts an
;; ordinary, editable query in the field.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jetpacs-widgets)
(require 'jetpacs-design)
(require 'bloom-core)
(require 'bloom-design)
(require 'bloom-library)
(require 'bloom-views)
(require 'bloom-agenda)

(defvar bloom-navigation--line-rounding)

(declare-function bloom-document-open-action "bloom-document" (notebook-id heading-id))
(declare-function jetpacs-app-defer-refresh "jetpacs-apps" (&optional params))

(defvar bloom-search--advanced-open nil
  "Non-nil while the operators panel is open.")

(defvar bloom-search--collapsed nil
  "Notebook ids whose result groups are folded.")

;;;; Queries

(defun bloom-search-mode-for (query)
  "Return the native search mode that QUERY's shape asks for.
\"ql\" for a Lisp form when org-ql is available, \"match\" for Org's
tags/property/TODO syntax, and \"text\" for everything else."
  (let ((query (string-trim (or query ""))))
    (cond
     ((and (string-prefix-p "(" query) (bloom-library-org-ql-available-p)) "ql")
     ((or (string-match-p "[=<>]" query)
          (string-match-p "\\`[-+/]" query)
          (string-match-p "/!" query)
          (string-match-p "\\`[[:alnum:]_@#%]+\\(?:[-+&|][[:alnum:]_@#%]+\\)+\\'" query))
      "match")
     (t "text"))))

(defun bloom-search--done-regexp ()
  "Return a regexp matching every finished TODO keyword in Bloom's notebooks."
  (let ((done (delete-dups
               (cl-mapcan (lambda (notebook)
                            (copy-sequence
                             (plist-get (ignore-errors
                                          (bloom-library-outline-snapshot
                                           (plist-get notebook :id)))
                                        :done-keywords)))
                          (bloom-library-notebooks-in-folder nil t)))))
    (concat "^\\(" (mapconcat #'regexp-quote (or done '("DONE")) "\\|") "\\)$")))

(defun bloom-search-quick-cards ()
  "Return the Quick Start cards as plists of label, query, icon and tone."
  (let ((not-done (format "-TODO={%s}" (bloom-search--done-regexp))))
    (list
     (list :key "overdue" :label "Overdue" :icon "priority_high" :tone "error"
           :query (format "SCHEDULED<\"<today>\"%s|DEADLINE<\"<today>\"%s"
                          not-done not-done)
           :meta "%d past their date")
     (list :key "today" :label "Today" :icon "schedule" :tone "warning"
           :query "SCHEDULED=\"<today>\"|DEADLINE=\"<today>\""
           :meta "%d scheduled or due")
     (list :key "open" :label "Open tasks" :icon "check_box_outline_blank" :tone "info"
           :query "/!" :meta "%d TODO items" :meta-one "%d TODO item")
     (list :key "unscheduled" :label "Unscheduled" :icon "event_busy" :tone "syn-tag"
           :query "SCHEDULED=\"\"+DEADLINE=\"\"/!" :meta "%d without a date"))))

(defun bloom-search--count (query)
  "Return how many headings native QUERY finds, or 0 when it fails."
  (condition-case nil
      (length (bloom-library-search-rows query (bloom-search-mode-for query) 512))
    (error 0)))

(defun bloom-search-run (query &optional view)
  "Run QUERY natively and store the result, as saved VIEW when non-nil."
  (bloom-library-search query (bloom-search-mode-for query) view))

;;;; Results

(defun bloom-search--records ()
  "Return a hash of every outline record by heading id."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (notebook (bloom-library-notebooks-in-folder nil t))
      (dolist (record (plist-get (ignore-errors
                                   (bloom-library-outline-snapshot
                                    (plist-get notebook :id)))
                                 :records))
        (puthash (plist-get record :id) record table)))
    table))

(defun bloom-search--date-label (stamp today)
  "Return STAMP's pill label seen from TODAY and whether it is overdue."
  (when stamp
    (let* ((date (plist-get stamp :date))
           (delta (bloom-org--date-delta date today))
           (clock (if (plist-get stamp :time)
                      (concat " " (plist-get stamp :time)
                              (if (plist-get stamp :end)
                                  (concat "-" (plist-get stamp :end)) ""))
                    "")))
      (cond
       ((< delta 0) (cons (format "%dd overdue" (- delta)) t))
       ((= delta 0) (cons (concat "today" clock) nil))
       ((= delta 1) (cons (concat "tomorrow" clock) nil))
       (t (cons (concat (substring (bloom-agenda--weekday date) 0 3) " "
                        (number-to-string (nth 1 (bloom-org--date-parts date))) " "
                        (substring (bloom-agenda--short-date date) 0 3)
                        clock)
                nil))))))

(defun bloom-search--groups (results records)
  "Return RESULTS grouped by notebook, enriched from outline RECORDS.
Each group is (NOTEBOOK-ID LABEL FOLDER ROWS); rows keep result order."
  (let (groups)
    (dolist (result results)
      (let* ((id (plist-get result :notebook-id))
             (record (gethash (plist-get result :heading-id) records))
             (row (append result (list :record record)))
             (group (assoc id groups)))
        (if group
            (setcdr (last group) (list row))
          (push (list id row) groups))))
    (mapcar (lambda (group)
              (let ((notebook (bloom-library--notebook-record (car group))))
                (list (car group)
                      (or (plist-get notebook :file-label)
                          (plist-get (cadr group) :notebook-label) "")
                      ;; A notebook at the Org root has no folder to name.
                      (and (plist-get notebook :folder-id)
                           (plist-get notebook :folder-label))
                      (cdr group))))
            (nreverse groups))))

;;;; Nodes

(defun bloom-search--text (role ink text &rest attrs)
  "Return TEXT in ROLE and INK with universal ATTRS."
  (apply #'bloom-agenda--text role ink text attrs))

(defun bloom-search--field-row (query back)
  "Return the BACK button, the search field holding QUERY, and the save star."
  (jetpacs-with-attrs
   (apply
    #'jetpacs-row
    (delq
     nil
     (list
      (jetpacs-icon-button
       "arrow_back"
       (if (string-empty-p query)
           (or back (jetpacs-action "bloom.notebooks.open"))
         (jetpacs-action "bloom.search.clear"))
       :content-description "Back")
      (jetpacs-with-attrs
       (jetpacs-design-styled
        '("bloom.search.field")
        (list
         (jetpacs-row
          (jetpacs-icon "search" :size 16 :color (bloom-design-color "faint"))
          (jetpacs-spacer :width 8)
          (jetpacs-with-attrs
           (jetpacs-text-input
            "bloom-search-query" :value query :single-line t :max-length 512
            :hide-keyboard-on-submit t
            :on-submit (jetpacs-action "bloom.search.query"))
           :weight 1)
          (and (not (string-empty-p query))
               (jetpacs-with-semantics
                (jetpacs-box (bloom-search--text "search-clear" nil "×")
                             :alignment "center"
                             :on-tap (jetpacs-action "bloom.search.clear"))
                :name "Clear search"))
          :align "center" :fill t)))
       :weight 1)
      (and (not (string-empty-p query))
           (jetpacs-with-semantics
            (jetpacs-with-attrs
             (jetpacs-box (bloom-search--text "search-star" nil "☆")
                          :alignment "center"
                          :on-tap (bloom-views-save-action (bloom-search-mode-for query)))
             :width 44 :height 44)
            :name "Save search"))
      :align "center" :fill t)))
   ;; The app's row is 6 dp around a 44 dp glyph box; the receiver's back
   ;; button is 48 dp, so 4 dp around it keeps the row 56 dp tall, and a
   ;; 6 dp start keeps the field where the app's begins.
   :pad '(:start 6 :end 10 :vertical 4)))

(defun bloom-search--meta-row (snapshot results groups)
  "Return the result count over SNAPSHOT's RESULTS and GROUPS, and Advanced."
  (let ((blank (bloom-search--blank-p snapshot)))
    (jetpacs-with-attrs
     (jetpacs-row
      (jetpacs-with-attrs
       (if blank (jetpacs-spacer :width 1)
         (bloom-search--text
          "search-count" nil
          (format "%s · %s"
                  (bloom-library--plural (length results) "result")
                  (bloom-library--plural (length groups) "notebook"))))
       :weight 1)
      (jetpacs-design-styled
       (list "bloom.search.advanced"
             (if bloom-search--advanced-open "bloom.search.advanced-on"
               "bloom.search.advanced-off"))
       (list (jetpacs-box (bloom-search--text
                           "search-advanced"
                           (if bloom-search--advanced-open "on-primary" "muted")
                           "⚑ Advanced")
                          :on-tap (jetpacs-action "bloom.search.advanced"))))
      :align "center" :fill t)
     :pad '(:horizontal 14 :vertical 5))))

(defconst bloom-search--operators
  '("+TAG" "-TAG" "/!" "TODO=\"TODO\"" "PRIORITY=\"A\""
    "SCHEDULED<=\"<today>\"" "DEADLINE<=\"<+7d>\"" "LEVEL=1" "{regexp}")
  "Org match and text-search forms the operators panel inserts.")

(defun bloom-search--advanced-panel ()
  "Return the operators panel: native Org forms that tap into the query."
  (jetpacs-with-attrs
   (jetpacs-design-styled
    '("bloom.search.ops")
    (list
     (jetpacs-column
      (bloom-search--text "search-ops-label" nil
                          (if (bloom-library-org-ql-available-p)
                              "Org match · text · org-ql forms"
                            "Org match · text search"))
      (jetpacs-spacer :height 8)
      (apply #'jetpacs-flow-row
             (append
              (mapcar (lambda (op)
                        (jetpacs-design-styled
                         '("bloom.search.op")
                         (list (jetpacs-box (bloom-search--text "search-op" nil op)
                                            :on-tap (jetpacs-action
                                                     "bloom.search.insert"
                                                     :args (list :token op))))))
                      (append bloom-search--operators
                              (and (bloom-library-org-ql-available-p)
                                   '("(and (todo) (tags \"TAG\"))"))))
              (list :spacing 8 :run-spacing 8))))))
   :pad '(:horizontal 14 :vertical 6)))

(defun bloom-search--section (label &optional top)
  "Return a Quick Start or Saved Searches section LABEL, TOP dp below."
  (bloom-search--text "search-section" nil label
                      :pad (list :top (or top 4) :bottom (if top 6 10))))

(defun bloom-search--card (card)
  "Return one Quick Start CARD."
  (let* ((count (bloom-search--count (plist-get card :query)))
         (meta (format (if (and (= count 1) (plist-get card :meta-one))
                           (plist-get card :meta-one)
                         (plist-get card :meta))
                       count))
         (tone (plist-get card :tone)))
    (jetpacs-with-attrs
     (jetpacs-design-styled
      '("bloom.search.card")
      (list
       (jetpacs-box
        (jetpacs-column
         (jetpacs-design-styled
          (list (concat "bloom.search.card-icon-" tone))
          (list (jetpacs-box (jetpacs-icon (plist-get card :icon) :size 16
                                           :color (bloom-design-color tone))
                             :alignment "center")))
         (bloom-search--text "search-card-title" nil (plist-get card :label)
                             :pad '(:top 10))
         (bloom-search--text "search-card-meta" nil meta :pad '(:top 3))
         :fill t)
        :on-tap (jetpacs-action "bloom.search.quick"
                                :args (list :card (plist-get card :key))))))
     :weight 1)))

(defun bloom-search--saved-row (view)
  "Return saved search VIEW's row: its star, name and query."
  (jetpacs-design-styled
   '("bloom.search.saved")
   (list
    (jetpacs-box
     (jetpacs-row
      (bloom-search--text "search-saved-star" nil "★")
      (jetpacs-spacer :width 11)
      (jetpacs-with-attrs
       (jetpacs-column
        (bloom-search--text "search-saved-name" nil (plist-get view :label))
        (bloom-search--text "search-saved-query" nil
                            (or (plist-get view :query)
                                (format "Emacs agenda view · %s" (plist-get view :kind)))
                            ;; The app's two line boxes each round up to
                            ;; whole pixels: 0.4 dp more per row.
                            :pad (list :top 2 :bottom bloom-navigation--line-rounding)))
       :weight 1)
      :align "top" :fill t)
     :on-tap (bloom-views-run-action view)))))

(defun bloom-search--blank ()
  "Return the blank state: the Quick Start cards and saved searches."
  (let* ((cards (bloom-search-quick-cards))
         (views (plist-get (bloom-views-snapshot) :views)))
    (jetpacs-with-attrs
     (apply
      #'jetpacs-column
      (append
       (list (bloom-search--section "QUICK START")
             (jetpacs-row (bloom-search--card (nth 0 cards))
                          (bloom-search--card (nth 1 cards))
                          :spacing 8 :fill t)
             (jetpacs-spacer :height 8)
             (jetpacs-row (bloom-search--card (nth 2 cards))
                          (bloom-search--card (nth 3 cards))
                          :spacing 8 :fill t))
       (and views
            (cons (bloom-search--section "SAVED SEARCHES" 18)
                  (mapcar #'bloom-search--saved-row views)))
       (list (jetpacs-spacer :height 16) :fill t)))
     :pad '(:horizontal 14 :vertical 4))))

(defun bloom-search--terms (snapshot)
  "Return the words SNAPSHOT's text query looks for, for highlighting.
Only a text search has plain words; Org match and org-ql queries name
fields instead."
  (when (equal (plist-get snapshot :mode) "text")
    (seq-remove (lambda (word)
                  (or (member (upcase word) '("AND" "OR" "NOT"))
                      (string-match-p "\\`[{+-]" word)))
                (split-string (or (plist-get snapshot :query) "") "[ \t\"]+" t))))

(defun bloom-search--highlighted (role text terms)
  "Return TEXT in text ROLE with every occurrence of TERMS highlighted.
A match reads amber and semibold on the soft amber fill, as in the app."
  (let ((case-fold-search t) ranges)
    (dolist (term terms)
      (let ((start 0))
        (while (string-match (regexp-quote term) text start)
          (push (cons (match-beginning 0) (match-end 0)) ranges)
          (setq start (match-end 0)))))
    (if (null ranges)
        (bloom-search--text role nil text)
      (setq ranges (sort ranges (lambda (a b) (< (car a) (car b)))))
      (let ((pos 0) spans)
        (dolist (range ranges)
          (when (>= (car range) pos)
            (when (> (car range) pos)
              (push (jetpacs-span (substring text pos (car range))) spans))
            (push (jetpacs-span (substring text (car range) (cdr range))
                                :color (bloom-design-color "warning")
                                :bg (bloom-design-opaque-color "warning-soft")
                                :font-weight 600)
                  spans)
            (setq pos (cdr range))))
        (when (< pos (length text))
          (push (jetpacs-span (substring text pos)) spans))
        (jetpacs-design-styled (list (concat "bloom.t." role))
                               (list (jetpacs-rich-text (nreverse spans))))))))

(defun bloom-search--date-pill (label overdue kind)
  "Return a date pill showing LABEL for planning KIND, red when OVERDUE."
  (let ((tone (cond (overdue "error") ((eq kind 'deadline) "warning") (t "info"))))
    (jetpacs-design-styled
     (list (concat "bloom.search.date-" tone))
     (list (jetpacs-row
            (jetpacs-icon (if (eq kind 'deadline) "flag_filled" "calendar_month")
                          :size 10.5 :color (bloom-design-color tone))
            (bloom-search--text "search-date" tone label)
            :spacing 3 :align "center")))))

(defun bloom-search--row (row today &optional terms)
  "Return result ROW's tappable heading row seen from TODAY.
TERMS are the query words to highlight in the title."
  (let* ((record (plist-get row :record))
         (done (plist-get record :done))
         (keyword (plist-get row :todo))
         (priority (plist-get row :priority))
         (scheduled (bloom-agenda-parse-timestamp (plist-get record :scheduled)))
         (deadline (bloom-agenda-parse-timestamp (plist-get record :deadline)))
         (tags (plist-get row :tags))
         (pills (unless done
                  (delq nil
                        (list (when-let* ((d (bloom-search--date-label deadline today)))
                                (bloom-search--date-pill (car d) (cdr d) 'deadline))
                              (when-let* ((s (bloom-search--date-label scheduled today)))
                                (bloom-search--date-pill (car s) (cdr s) 'scheduled))))))
         (meta (append pills
                       (and tags
                            (list (bloom-search--text
                                   "search-tags" nil
                                   (mapconcat (lambda (tag) (format ":%s:" tag)) tags " ")))))))
    (jetpacs-with-attrs
     (jetpacs-card
      (jetpacs-with-attrs
       (apply
        #'jetpacs-column
        (delq
         nil
         (list
          (apply #'jetpacs-row
                 (append
                  (delq nil
                        (list
                         (and keyword
                              (jetpacs-with-attrs
                               (jetpacs-design-styled
                                (list (if done "bloom.search.pill-success"
                                        "bloom.search.pill-warning"))
                                (list (bloom-search--text "search-pill"
                                                          (if done "success" "warning")
                                                          keyword)))
                               :pad '(:end 7)))
                         (and priority
                              (bloom-search--text
                               "search-priority"
                               (pcase priority ("A" "error") ("B" "warning") (_ "success"))
                               (format "[#%s]" priority) :pad '(:end 7)))
                         (jetpacs-with-attrs
                          (bloom-search--highlighted
                           (if done "search-title-done" "search-title")
                           (or (plist-get record :title) (plist-get row :headline))
                           terms)
                          :weight 1)))
                  (list :align "center" :fill t)))
          (and meta
               (jetpacs-with-attrs
                (apply #'jetpacs-flow-row (append meta (list :spacing 7 :run-spacing 4)))
                :pad '(:top 6))))))
       :pad '(:start 16 :top 9 :end 11 :bottom 11))
      :variant "filled"
      :on-tap (bloom-document-open-action (plist-get row :notebook-id)
                                          (plist-get row :heading-id))
      :swipe-start
      (jetpacs-swipe
       (list (jetpacs-swipe-action
              "Schedule" :icon "calendar_month" :color "primary"
              :on-trigger (jetpacs-action
                           "bloom.document.planning"
                           :args (list :notebook-id (plist-get row :notebook-id)
                                       :heading-id (plist-get row :heading-id)
                                       :focus "scheduled")))
             (jetpacs-swipe-action
              "Reveal" :icon "menu_book" :color "secondary"
              :on-trigger (jetpacs-action
                           "bloom.agenda.reveal"
                           :args (list :notebook-id (plist-get row :notebook-id)
                                       :heading-id (plist-get row :heading-id))))))
      :swipe-end
      (and keyword (not done)
           (jetpacs-swipe
            (list (jetpacs-swipe-action
                   "Done" :icon "check" :color "primary"
                   :on-trigger (jetpacs-action
                                "bloom.agenda.toggle"
                                :args (list :notebook-id (plist-get row :notebook-id)
                                            :heading-id (plist-get row :heading-id)))))
            :commit t)))
     :key (jetpacs-wire-id "search-row" (plist-get row :heading-id)))))

(defun bloom-search--group (group today &optional terms)
  "Return GROUP's header and, unless folded, its rows seen from TODAY.
TERMS are the query words to highlight."
  (pcase-let ((`(,id ,label ,folder ,rows) group))
    (let ((folded (member id bloom-search--collapsed))
          (count (length rows)))
      (append
       (list
        (jetpacs-with-attrs
         (jetpacs-design-styled
          '("bloom.search.group")
          (list
           (jetpacs-box
            (jetpacs-row
             (bloom-search--text "search-caret" nil (if folded "▸" "▾") :pad '(:top 1))
             (jetpacs-spacer :width 9)
             (jetpacs-with-attrs
              (jetpacs-column
               (bloom-search--text "search-file" nil label)
               (bloom-search--text
                "search-file-meta" nil
                (let ((matches (if (= count 1) "1 match" (format "%d matches" count))))
                  (if (and folder (not (string-empty-p folder)))
                      (format "%s/ · %s" (string-remove-suffix "/" folder) matches)
                    matches))
                :pad '(:top 1)))
              :weight 1)
             :align "top" :fill t)
            :on-tap (jetpacs-action "bloom.search.group" :args (list :notebook-id id)))))
         :key (jetpacs-wire-id "search-group" id)))
       (unless folded
         (cl-loop for row in rows
                  for index from 0
                  when (> index 0)
                  collect (jetpacs-design-styled '("bloom.agenda.hairline")
                                                 (list (jetpacs-spacer :height 1)))
                  collect (bloom-search--row row today terms)))))))

(defun bloom-search--nothing (snapshot)
  "Return the no-results state for SNAPSHOT."
  (jetpacs-with-attrs
   (jetpacs-column
    (bloom-search--text "empty-title" nil "Nothing matches")
    (bloom-search--text "agenda-empty-caption" nil
                        (or (plist-get snapshot :error)
                            "Try another Org search, or open the operators.")
                        :pad '(:top 7))
    (jetpacs-with-attrs
     (jetpacs-design-styled
      '("bloom.search.loosen")
      (list (jetpacs-box (bloom-search--text "search-loosen" nil "Open filters")
                         :on-tap (jetpacs-action "bloom.search.advanced"))))
     :pad '(:top 16))
    :align "center" :fill t)
   :pad '(:horizontal 24 :vertical 52)))

(defun bloom-search--filters-bar ()
  "Return the Filters bar.  It opens the operators panel for now."
  (jetpacs-column
   (jetpacs-design-styled '("bloom.agenda.hairline") (list (jetpacs-spacer :height 1)))
   (jetpacs-design-styled
    '("bloom.search.filters-bar")
    (list
     (jetpacs-design-styled
      '("bloom.search.filters")
      (list
       (jetpacs-box
        (jetpacs-row
         (jetpacs-icon "filter_list" :size 16 :color (bloom-design-color "on-primary"))
         (jetpacs-spacer :width 9)
         (bloom-search--text "search-filters" nil "Filters")
         :align "center")
        :alignment "center"
        :on-tap (jetpacs-action "bloom.search.advanced"))))))))

(defun bloom-search--blank-p (snapshot)
  "Non-nil when SNAPSHOT holds no query and no saved view."
  (and (string-empty-p (or (plist-get snapshot :query) ""))
       (null (plist-get snapshot :view-id))))

(defun bloom-search-screen (back)
  "Return the parity Search screen with BACK."
  (let* ((snapshot (bloom-library-search-snapshot))
         (query (or (plist-get snapshot :query) ""))
         (results (plist-get snapshot :results))
         (groups (bloom-search--groups results (bloom-search--records)))
         (today (bloom-agenda--today)))
    (jetpacs-design-profile-scope
     (jetpacs-design-active-profile bloom-design-profile-id)
     (list
      (bloom-design-screen-scope
       ;; Only the styles this screen uses: the receiver caps the styles in
       ;; effect, the profile's included, at 256.
       (append (bloom-design-search-styles)
               (seq-filter (lambda (style)
                             (member (car style) '("bloom.agenda.hairline"
                                                   "bloom.agenda.row")))
                           (bloom-design-agenda-styles)))
       (list
      (jetpacs-with-semantics
       (append
        (jetpacs-scaffold
         :top-bar
         (jetpacs-design-scope
          nil nil
          (list
           (apply #'jetpacs-column
                  (delq nil
                        (list (bloom-search--field-row query back)
                              (bloom-search--meta-row snapshot results groups)
                              (and bloom-search--advanced-open
                                   (bloom-search--advanced-panel))))))
          :component-styles
          (list (jetpacs-design-component-style "text-field.outlined"
                                                '("bloom.search.input"))
                (jetpacs-design-component-style "text-field.text"
                                                '("bloom.t.search-input"))
                (jetpacs-design-component-style "text-field.placeholder"
                                                '("bloom.t.search-input" "bloom.fg.faint"))))
         :body
         (jetpacs-design-scope
          nil nil
          (list
           ;; The Filters bar ends the body rather than taking the scaffold's
           ;; bottom bar, which the receiver wraps in its own padded band.
           (jetpacs-column
            (jetpacs-with-attrs
             (cond
              ((bloom-search--blank-p snapshot)
               (jetpacs-lazy-column (bloom-search--blank)))
              ((null groups)
               (jetpacs-lazy-column (bloom-search--nothing snapshot)))
              (t
               (jetpacs-with-attrs
                (apply #'jetpacs-lazy-column
                       (let ((terms (bloom-search--terms snapshot)))
                         (cl-mapcan (lambda (group)
                                      (bloom-search--group group today terms))
                                    groups)))
                :pad '(:horizontal 12 :vertical 4))))
             :weight 1)
            (bloom-search--filters-bar)
            :fill t))
          :component-styles
          (list (jetpacs-design-component-style "card.filled" '("bloom.agenda.row")))))
        (list :fab nil))
       :pane-title "Search")))))))

;;;; Actions

(defun bloom-search-on-query (args params)
  "Run ARGS' submitted query natively and refresh PARAMS' surface."
  (let ((query (string-trim (or (plist-get args :value) ""))))
    (if (> (length query) 512) 'rejected
      (if (string-empty-p query)
          (bloom-library-search "" "text")
        (bloom-search-run query))
      (jetpacs-app-defer-refresh params)
      'accepted)))

(defun bloom-search-on-clear (_args params)
  "Return Search to its blank state and refresh PARAMS' surface."
  (bloom-library-search "" "text")
  (jetpacs-app-defer-refresh params)
  'accepted)

(defun bloom-search-on-quick (args params)
  "Run ARGS' Quick Start card and refresh PARAMS' surface."
  (if-let* ((card (cl-find (plist-get args :card) (bloom-search-quick-cards)
                           :key (lambda (c) (plist-get c :key)) :test #'equal)))
      (progn (bloom-search-run (plist-get card :query))
             (jetpacs-app-defer-refresh params)
             'accepted)
    'rejected))

(defun bloom-search-on-advanced (_args params)
  "Open or close the operators panel and refresh PARAMS' surface."
  (setq bloom-search--advanced-open (not bloom-search--advanced-open))
  (jetpacs-app-defer-refresh params)
  'accepted)

(defun bloom-search-on-insert (args params)
  "Append ARGS' operator to the query, without running it; refresh PARAMS."
  (let ((token (plist-get args :token))
        (query (or (plist-get (bloom-library-search-snapshot) :query) "")))
    (if (not (member token (append bloom-search--operators
                                   '("(and (todo) (tags \"TAG\"))"))))
        'rejected
      (setq bloom-library--search-snapshot
            (plist-put (bloom-library-search-snapshot) :query
                       (string-trim (concat query (if (string-empty-p query) "" " ")
                                            token))))
      (jetpacs-app-defer-refresh params)
      'accepted)))

(defun bloom-search-on-group (args params)
  "Fold or unfold ARGS' notebook group and refresh PARAMS' surface."
  (let ((id (plist-get args :notebook-id)))
    (if (not (stringp id)) 'rejected
      (setq bloom-search--collapsed
            (if (member id bloom-search--collapsed)
                (remove id bloom-search--collapsed)
              (cons id bloom-search--collapsed)))
      (jetpacs-app-defer-refresh params)
      'accepted)))

(defun bloom-search-register ()
  "Register the Search screen's actions."
  (with-jetpacs-owner bloom-owner
    (jetpacs-defaction "bloom.search.query" #'bloom-search-on-query
                       :doc "Run a native Org search for the submitted text")
    (jetpacs-defaction "bloom.search.clear" #'bloom-search-on-clear
                       :doc "Clear the search back to Quick Start")
    (jetpacs-defaction "bloom.search.quick" #'bloom-search-on-quick
                       :args '((:name card :type "enum" :required t))
                       :doc "Run one Quick Start search")
    (jetpacs-defaction "bloom.search.advanced" #'bloom-search-on-advanced
                       :doc "Open or close the search operators")
    (jetpacs-defaction "bloom.search.insert" #'bloom-search-on-insert
                       :args '((:name token :type "text" :required t))
                       :doc "Add a search operator to the query")
    (jetpacs-defaction "bloom.search.group" #'bloom-search-on-group
                       :args '((:name notebook-id :type "ref" :required t))
                       :doc "Fold or unfold one notebook's search results")))

(provide 'bloom-search)
;;; bloom-search.el ends here
