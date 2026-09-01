;;; grove-ui.el --- Grove Agenda presentation for Jetpacs -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Grove's native presentation over the explicit `grove-org' snapshot.  The
;; sectioned ledger, compact metadata, and reveal-first/deep-commit swipe are
;; adapted from Grove's Android UI.  Every semantic effect remains a named
;; Emacs action; this module contains no Org parsing or persistence behavior.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'jetpacs-chrome)
(require 'jetpacs-shell)
(require 'jetpacs-widgets)
(require 'grove-core)
(require 'grove-design)
(require 'grove-org)

(defvar grove-agenda-range "today"
  "Visible Grove agenda range, either `today' or `upcoming'.")

(defun grove-ui-capture-action (&optional offline)
  "Return the existing Org capture action.
When OFFLINE is non-nil, retain the action for one day.  Wake Emacs when the
session granted that capability; otherwise queue it for the next connection."
  (if offline
      (jetpacs-action "org.capture.show"
                      :when-offline (jetpacs-durable-offline-policy)
                      :ttl-s 86400)
    (jetpacs-action "org.capture.show")))

(defun grove-ui--item-open-action (item-id)
  "Return a durable action opening opaque ITEM-ID on Grove's app surface."
  (jetpacs-shell-action-opening-surface
   "grove.agenda.item.open" grove-app-surface
   :args (list :item-id item-id)
   :when-offline (jetpacs-durable-offline-policy) :ttl-s 86400))

(defun grove-ui--item-done-action (item-id)
  "Return a durable, deduplicated mark-done action for ITEM-ID."
  (jetpacs-action
   "grove.agenda.done"
   :args (list :item-id item-id)
   :when-offline (jetpacs-durable-offline-policy) :ttl-s 86400
   :dedupe (concat "grove-done-" item-id)))

(defun grove-ui--item-meta (item)
  "Return Grove's compact metadata line for normalized agenda ITEM."
  (string-join
   (delq nil
         (list (plist-get item :time)
               (plist-get item :todo)
               (plist-get item :file-label)
               (when-let* ((tags (plist-get item :tags))
                           ((not (null tags))))
                 (mapconcat (lambda (tag) (format ":%s:" tag)) tags " "))))
   " · "))

(defun grove-ui--item-leading (item interactive)
  "Build ITEM's task-ring affordance.
INTERACTIVE non-nil makes the ring dispatch the same durable done action as
the deep swipe commit."
  (let ((icon (jetpacs-icon
               (if (plist-get item :todo)
                   "radio_button_unchecked" "event")
               :size 18
               :color (if (equal (plist-get item :section) "OVERDUE")
                          "error" "primary")
               :content-description
               (if (plist-get item :todo) "Mark done" "Agenda item"))))
    (if (and interactive (plist-get item :todo))
        (jetpacs-box
         icon :on-tap (grove-ui--item-done-action (plist-get item :id)))
      icon)))

(defun grove-ui-agenda-row (item &optional compact interactive rich-swipe)
  "Build one Grove agenda ITEM row.
COMPACT omits tags and clamps text for widgets.  INTERACTIVE enables row
opening.  RICH-SWIPE enables the app-only reveal-first swipe actions."
  (let* ((item-id (plist-get item :id))
         (headline (or (plist-get item :headline) "Untitled"))
         (meta (grove-ui--item-meta
                (if compact
                    (plist-put (copy-sequence item) :tags nil)
                  item)))
         (middle
          (jetpacs-with-attrs
           (jetpacs-column
            (jetpacs-text headline :max-lines (if compact 2 3))
            (jetpacs-text meta :style "caption" :max-lines 1)
            :spacing 2)
           :weight 1))
         (trailing
          (when-let* ((priority (plist-get item :priority)))
            (jetpacs-badge (format "#%s" priority)
                           :color (if (equal (plist-get item :section)
                                            "OVERDUE")
                                      "error" "primary"))))
         (open (and interactive (grove-ui--item-open-action item-id)))
         (swipe
          (and rich-swipe interactive (plist-get item :todo)
               (jetpacs-swipe
                (list
                 (jetpacs-swipe-action
                  "Done" :icon "done" :color "primary"
                  :on-trigger (grove-ui--item-done-action item-id))
                 (jetpacs-swipe-action
                  "Open" :icon "arrow_forward" :color "secondary"
                  :on-trigger open))
                :commit t)))
         (row
          (apply #'jetpacs-row
                 (append
                  (list (grove-ui--item-leading item interactive) middle)
                  (when trailing (list trailing))
                  (list :align "center" :spacing 10)))))
    (jetpacs-with-attrs
     (apply #'jetpacs-card
            (append (list row)
                    (when open (list :on-tap open))
                    (when swipe (list :swipe-end swipe))
                    (list :variant "outlined")))
     :key item-id)))

(defun grove-ui--items-for-range (snapshot range)
  "Return SNAPSHOT items admitted by Grove agenda RANGE."
  (seq-filter
   (lambda (item)
     (let ((section (plist-get item :section)))
       (if (equal range "today")
           (member section '("OVERDUE" "TODAY" "AGENDA"))
         (not (member section '("OVERDUE" "TODAY" "AGENDA"))))))
   (plist-get snapshot :items)))

(defun grove-ui-group-items (items)
  "Group ordered ITEMS into an ordered alist by their section label."
  (let (groups order)
    (dolist (item items)
      (let ((section (plist-get item :section)))
        (unless (assoc section groups)
          (push section order)
          (push (cons section nil) groups))
        (setcdr (assoc section groups)
                (append (cdr (assoc section groups)) (list item)))))
    (mapcar (lambda (section) (assoc section groups)) (nreverse order))))

(defun grove-ui--range-button (value label)
  "Build a Grove range button selecting VALUE and showing LABEL."
  (jetpacs-with-attrs
   (jetpacs-button
    label
    (jetpacs-action "grove.agenda.select" :args (list :range value))
    :variant (if (equal grove-agenda-range value) "filled" "text"))
   :weight 1))

(defun grove-ui--empty (snapshot range)
  "Build Grove's empty or unavailable state for SNAPSHOT and RANGE."
  (if (plist-get snapshot :error)
      (jetpacs-empty-state
       :icon "sync_problem"
       :title "Agenda unavailable"
       :caption "Refresh after checking your Org agenda files."
       :action-label "Refresh"
       :on-tap (jetpacs-action "grove.agenda.refresh"))
    (jetpacs-empty-state
     :icon "event_available"
     :title (if (equal range "today") "Nothing scheduled today"
              "Nothing upcoming")
     :caption "Capture something when it is ready to grow."
     :action-label "Capture"
     :on-tap (grove-ui-capture-action))))

(defun grove-ui-agenda-body ()
  "Build Grove's deterministic Agenda body from its explicit snapshot."
  (let* ((snapshot (grove-org-snapshot))
         (items (grove-ui--items-for-range snapshot grove-agenda-range))
         (children
          (list
           (jetpacs-text
            (format "%s · %d scheduled"
                    (plist-get snapshot :date-label)
                    (plist-get snapshot :today-count))
            :style "caption")
           (jetpacs-row
            (grove-ui--range-button "today" "Today")
            (grove-ui--range-button "upcoming" "Upcoming")
            :spacing 6))))
    (if (null items)
        (setq children (append children
                               (list (grove-ui--empty snapshot
                                                     grove-agenda-range))))
      (dolist (group (grove-ui-group-items items))
        (setq children
              (append children
                      (list
                       (jetpacs-section-header
                        (car group)
                        :trailing
                        (jetpacs-badge (number-to-string (length (cdr group))))))
                      (mapcar (lambda (item)
                                (grove-ui-agenda-row item nil t t))
                              (cdr group))))))
    (apply #'jetpacs-lazy-column
           (append children (list :spacing 9 :content-padding 12)))))

(defun grove-ui-screen (back)
  "Build Grove's Agenda-first root screen with BACK navigation."
  (let ((snapshot (grove-org-snapshot)))
    (jetpacs-chrome-screen
     (plist-get snapshot :day-label)
     (grove-ui-agenda-body)
     :back back
     :actions
     (list
      (jetpacs-icon-button
       "refresh" (jetpacs-action "grove.agenda.refresh")
       :content-description "Refresh agenda"))
     :fab
     (jetpacs-button
      "Capture" (grove-ui-capture-action)
      :icon "add" :variant "filled" :expanded "auto")
     :on-refresh (jetpacs-action "grove.agenda.refresh"))))

(defun grove-ui-presented-screen (back)
  "Build Agenda with BACK for the advertised app design extension."
  (let ((screen (grove-ui-screen back)))
    (if (jetpacs-extension-advertised-p "jetpacs.design" :app)
        (grove-design-present-screen screen 'agenda)
      screen)))

(provide 'grove-ui)
;;; grove-ui.el ends here
