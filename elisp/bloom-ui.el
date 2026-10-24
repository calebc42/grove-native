;;; bloom-ui.el --- Bloom Agenda presentation for Jetpacs -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Bloom's native presentation over the explicit `bloom-org' snapshot.  The
;; sectioned ledger, compact metadata, and reveal-first/deep-commit swipe are
;; adapted from Bloom's Android UI.  Every semantic effect remains a named
;; Emacs action; this module contains no Org parsing or persistence behavior.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'jetpacs-chrome)
(require 'jetpacs-shell)
(require 'jetpacs-components)
(require 'jetpacs-widgets)
(require 'bloom-core)
(require 'bloom-design)
(require 'bloom-org)

(defvar bloom-agenda-range "today"
  "Visible Bloom agenda range, either `today' or `upcoming'.")

(defun bloom-ui-capture-action (&optional offline)
  "Return the existing Org capture action.
When OFFLINE is non-nil, retain the action for one day.  Wake Emacs when the
session granted that capability; otherwise queue it for the next connection."
  (if offline
      (jetpacs-action "org.capture.show"
                      :when-offline (jetpacs-durable-offline-policy)
                      :ttl-s 86400)
    (jetpacs-action "org.capture.show")))

(defun bloom-ui--item-open-action (item-id)
  "Return a durable action opening opaque ITEM-ID on Bloom's app surface."
  (jetpacs-shell-action-opening-surface
   "bloom.agenda.item.open" bloom-app-surface
   :args (list :item-id item-id)
   :when-offline (jetpacs-durable-offline-policy) :ttl-s 86400))

(defun bloom-ui--item-done-action (item-id)
  "Return a durable, deduplicated mark-done action for ITEM-ID."
  (jetpacs-action
   "bloom.agenda.done"
   :args (list :item-id item-id)
   :when-offline (jetpacs-durable-offline-policy) :ttl-s 86400
   :dedupe (concat "bloom-done-" item-id)))

(defun bloom-ui--item-meta (item)
  "Return Bloom's compact metadata line for normalized agenda ITEM."
  (string-join
   (delq nil
         (list (plist-get item :time)
               (plist-get item :todo)
               (plist-get item :file-label)
               (when-let* ((tags (plist-get item :tags))
                           ((not (null tags))))
                 (mapconcat (lambda (tag) (format ":%s:" tag)) tags " "))))
   " · "))

(defun bloom-ui--item-leading (item interactive)
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
         icon :on-tap (bloom-ui--item-done-action (plist-get item :id)))
      icon)))

(defun bloom-ui-agenda-row (item &optional compact interactive rich-swipe
                                 target)
  "Build one Bloom agenda ITEM row.
COMPACT omits tags and clamps text for widgets.  INTERACTIVE enables row
opening.  RICH-SWIPE enables the app-only reveal-first swipe actions.
TARGET is the render target the row is bound for, defaulting to `:app';
the widget profile advertises fewer nodes, so its caller passes `:widget'
and the row degrades to the canonical composition."
  (let* ((item-id (plist-get item :id))
         (headline (or (plist-get item :headline) "Untitled"))
         (meta (bloom-ui--item-meta
                (if compact
                    (plist-put (copy-sequence item) :tags nil)
                  item)))
         (trailing
          (when-let* ((priority (plist-get item :priority)))
            (jetpacs-badge (format "#%s" priority)
                           :color (if (equal (plist-get item :section)
                                            "OVERDUE")
                                      "error" "primary"))))
         (open (and interactive (bloom-ui--item-open-action item-id)))
         (swipe
          (and rich-swipe interactive (plist-get item :todo)
               (jetpacs-swipe
                (list
                 (jetpacs-swipe-action
                  "Done" :icon "done" :color "primary"
                  :on-trigger (bloom-ui--item-done-action item-id))
                 (jetpacs-swipe-action
                  "Open" :icon "arrow_forward" :color "secondary"
                  :on-trigger open))
                :commit t))))
    (jetpacs-components-list-item
     headline
     :subtitle meta
     :title-max-lines (if compact 2 3) :subtitle-max-lines 1
     :leading (bloom-ui--item-leading item interactive)
     :trailing trailing
     :on-tap open
     :swipe-end swipe
     :key item-id
     :target (or target :app))))

(defun bloom-ui--items-for-range (snapshot range)
  "Return SNAPSHOT items admitted by Bloom agenda RANGE."
  (seq-filter
   (lambda (item)
     (let ((section (plist-get item :section)))
       (if (equal range "today")
           (member section '("OVERDUE" "TODAY" "AGENDA"))
         (not (member section '("OVERDUE" "TODAY" "AGENDA"))))))
   (plist-get snapshot :items)))

(defun bloom-ui-group-items (items)
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

(defun bloom-ui--range-button (value label)
  "Build a Bloom range button selecting VALUE and showing LABEL."
  (jetpacs-with-attrs
   (jetpacs-button
    label
    (jetpacs-action "bloom.agenda.select" :args (list :range value))
    :variant (if (equal bloom-agenda-range value) "filled" "text"))
   :weight 1))

(defun bloom-ui--empty (snapshot range)
  "Build Bloom's empty or unavailable state for SNAPSHOT and RANGE."
  (if (plist-get snapshot :error)
      (jetpacs-empty-state
       :icon "sync_problem"
       :title "Agenda unavailable"
       :caption "Refresh after checking your Org agenda files."
       :action-label "Refresh"
       :on-tap (jetpacs-action "bloom.agenda.refresh"))
    (jetpacs-empty-state
     :icon "event_available"
     :title (if (equal range "today") "Nothing scheduled today"
              "Nothing upcoming")
     :caption "Capture something when it is ready to grow."
     :action-label "Capture"
     :on-tap (bloom-ui-capture-action))))

(defun bloom-ui-agenda-body ()
  "Build Bloom's deterministic Agenda body from its explicit snapshot."
  (let* ((snapshot (bloom-org-snapshot))
         (items (bloom-ui--items-for-range snapshot bloom-agenda-range))
         (children
          (list
           (jetpacs-text
            (format "%s · %d scheduled"
                    (plist-get snapshot :date-label)
                    (plist-get snapshot :today-count))
            :style "caption")
           (jetpacs-row
            (bloom-ui--range-button "today" "Today")
            (bloom-ui--range-button "upcoming" "Upcoming")
            :spacing 6))))
    (if (null items)
        (setq children (append children
                               (list (bloom-ui--empty snapshot
                                                     bloom-agenda-range))))
      (dolist (group (bloom-ui-group-items items))
        (setq children
              (append children
                      (list
                       (jetpacs-section-header
                        (car group)
                        :trailing
                        (jetpacs-badge (number-to-string (length (cdr group))))))
                      (mapcar (lambda (item)
                                (bloom-ui-agenda-row item nil t t))
                              (cdr group))))))
    (apply #'jetpacs-lazy-column
           (append children (list :spacing 9 :content-padding 12)))))

(defun bloom-ui-screen (back)
  "Build Bloom's Agenda-first root screen with BACK navigation."
  (let ((snapshot (bloom-org-snapshot)))
    (jetpacs-chrome-screen
     (plist-get snapshot :day-label)
     (bloom-ui-agenda-body)
     :back back
     :actions
     (list
      (jetpacs-icon-button
       "refresh" (jetpacs-action "bloom.agenda.refresh")
       :content-description "Refresh agenda"))
     :fab
     (jetpacs-button
      "Capture" (bloom-ui-capture-action)
      :icon "add" :variant "filled" :expanded "auto")
     :on-refresh (jetpacs-action "bloom.agenda.refresh"))))

(declare-function bloom-agenda-screen "bloom-agenda" (back))

(defun bloom-ui-presented-screen (back)
  "Build Agenda with BACK for the advertised app design extension.
Under the design runtime with Bloom's parity faces it is the Android app's
own Agenda."
  (if (and (fboundp 'bloom-agenda-screen)
           (jetpacs-extension-advertised-p "jetpacs.design" :app)
           (bloom-design-parity-available-p))
      (bloom-agenda-screen back)
    (bloom-design-present-if-advertised (bloom-ui-screen back) 'agenda)))

(provide 'bloom-ui)
;;; bloom-ui.el ends here
