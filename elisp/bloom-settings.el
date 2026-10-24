;;; bloom-settings.el --- The Android app's Settings pages for Bloom's options -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Settings as the Android app draws them: a root page of grouped rows,
;; each opening a page of toggles and choices, with a Help-style section
;; below.  The rows are Bloom's own Customize options; nothing here copies
;; the app's settings that belong to Android, such as sync or widgets.
;;
;; Bloom registers in the same two places Jetpacs, Emacs, Theme and
;; Glasspane do: a settings hub entry, so Bloom appears beside them in the
;; Settings screen, and a settings section, which authorizes exactly these
;; options for the generic `settings.set' and lists them under Emacs
;; Settings.  Every change goes through `jetpacs-settings-apply', so it is
;; validated against the option's Customize type and saved.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jetpacs-widgets)
(require 'jetpacs-design)
(require 'jetpacs-settings)
(require 'bloom-core)
(require 'bloom-design)
(require 'bloom-library)
(require 'bloom-agenda)

(declare-function jetpacs-app-defer-refresh "jetpacs-apps" (&optional params))
(declare-function jetpacs-chrome-push-screen "jetpacs-chrome" (surface-or-owner id builder))
(declare-function jetpacs-flow-continue "jetpacs-surfaces" (fn))
(declare-function bloom-reminders-schedule-sync "bloom-reminders" ())
(declare-function bloom-org-refresh "bloom-org" ())
(defvar bloom-reminders-enabled)
(defvar bloom-reminder-lead-minutes)
(defvar bloom-reminder-horizon-hours)
(defvar bloom-org-agenda-days)

(defconst bloom-settings-section "Bloom"
  "The title of Bloom's section in the Jetpacs settings registry.")

;;;; The pages

(defun bloom-settings--theme-choices ()
  "Return the design presets a user can pick, as (VALUE . LABEL)."
  '((nil . "Follow Emacs") ("bloom.light" . "Bloom Light")
    ("bloom.dark" . "Bloom Dark") ("bloom.grove-light" . "Grove Light")
    ("bloom.grove-dark" . "Grove Dark")))

(defun bloom-settings-pages ()
  "Return Bloom's settings pages.
Each page is a plist of `:key', `:title', `:description' and `:rows'.  A
row is (toggle SYMBOL LABEL DESCRIPTION) or (choice SYMBOL LABEL
DESCRIPTION CHOICES), where CHOICES are (VALUE . LABEL)."
  `((:key "look" :title "Look and Feel"
     :description "Theme for Bloom's screens"
     :rows ((choice jetpacs-design-active-profile-id "Theme"
                    "Bloom's palettes, or your Emacs theme's colors"
                    ,(bloom-settings--theme-choices))))
    (:key "notebooks" :title "Notebooks"
     :description "Folder tree and how many files Bloom lists"
     :rows ((toggle bloom-library-tree-view "Folder tree"
                    "Show notebooks inside their folders")
            (choice bloom-library-notebook-limit "Notebook limit"
                    "The most Org files Bloom lists"
                    ((100 . "100") (300 . "300") (1000 . "1000")))))
    (:key "notes" :title "Notes"
     :description "How a note opens, drawers"
     :rows ((choice bloom-reader-default-mode "Open notes in"
                    "The mode a note starts in"
                    ((read . "Read") (edit . "Edit")))
            (toggle bloom-reader-show-drawers "Show drawers"
                    "Open PROPERTIES and LOGBOOK drawers in read mode")))
    (:key "agenda" :title "Agenda"
     :description "Row details on the Agenda"
     :rows ((toggle bloom-agenda-show-tags "Tags on rows"
                    "Each row's tags, inherited ones included")
            (toggle bloom-agenda-show-file "Source file on rows"
                    "The notebook a row comes from")
            (toggle bloom-agenda-show-timestamps "Timestamp chips on rows"
                    "The scheduled day even under a day section")))
    (:key "search" :title "Search"
     :description "How many results a search keeps"
     :rows ((choice bloom-search-result-limit "Result limit"
                    "The most headings one search returns"
                    ((50 . "50") (100 . "100") (200 . "200") (512 . "512")))))
    (:key "reminders" :title "Reminders"
     :description "Notifications for timed SCHEDULED and DEADLINE"
     :rows ((toggle bloom-reminders-enabled "Reminders"
                    "Notify at timed Org entries")
            (choice bloom-reminder-lead-minutes "Remind me"
                    "How long before the time"
                    ((0 . "At time") (15 . "15 min") (60 . "1 hour") (1440 . "1 day")))
            (choice bloom-reminder-horizon-hours "Look ahead"
                    "How far ahead reminders are scheduled"
                    ((24 . "1 day") (72 . "3 days") (168 . "1 week") (336 . "2 weeks")))))))

(defun bloom-settings--symbols ()
  "Return every option the settings pages expose, theme excluded."
  (cl-loop for page in (bloom-settings-pages)
           nconc (cl-loop for row in (plist-get page :rows)
                          unless (eq (nth 1 row) 'jetpacs-design-active-profile-id)
                          collect (nth 1 row))))

(defun bloom-settings--after-set (symbol _value)
  "Bring Bloom's state up to date after SYMBOL changed."
  (pcase symbol
    ((or 'bloom-reminders-enabled 'bloom-reminder-lead-minutes
         'bloom-reminder-horizon-hours)
     (when (fboundp 'bloom-reminders-schedule-sync) (bloom-reminders-schedule-sync)))
    ('bloom-library-notebook-limit (bloom-library-refresh))
    ('bloom-org-agenda-days (when (fboundp 'bloom-org-refresh) (bloom-org-refresh)))))

;;;; Nodes

(defun bloom-settings--text (role ink text &rest attrs)
  "Return TEXT in ROLE and INK with universal ATTRS."
  (apply #'bloom-agenda--text role ink text attrs))

(defun bloom-settings--top-bar (title back)
  "Return the app's settings bar: BACK and TITLE."
  (jetpacs-with-attrs
   (jetpacs-row
    (jetpacs-icon-button "arrow_back" (or back (jetpacs-action "bloom.notebooks.open"))
                         :content-description "Back")
    (jetpacs-with-attrs (bloom-settings--text "top-title" nil title)
                        :weight 1 :pad '(:start 2))
    :align "center" :fill t)
   :height 56 :pad '(:start 6 :end 16)))

(defun bloom-settings--group (rows)
  "Return the app's rounded card holding ROWS, hairlines between them.
Each row is a function of its top and bottom trim in dp.  The receiver
insets a bordered box's content by the 1 dp border, which the app's
border does not take, so the first row starts 1 dp higher and the last
ends 1 dp lower than their padding alone."
  (let ((count (length rows)))
    (jetpacs-design-styled
     '("bloom.settings.group")
     (list
      (apply #'jetpacs-column
             (append
              (cl-loop for row in rows
                       for index from 0
                       when (> index 0)
                       collect (jetpacs-with-attrs
                                (jetpacs-design-styled
                                 '("bloom.agenda.hairline")
                                 (list (jetpacs-spacer :height 1)))
                                :pad '(:horizontal 14))
                       collect (funcall row (if (= index 0) 1 0)
                                        (if (= index (1- count)) 1 0)))
              (list :fill t)))))))

(defun bloom-settings--label-column (label description)
  "Return a row's LABEL over its DESCRIPTION."
  (apply #'jetpacs-column
         (delq nil
               (list (bloom-settings--text "settings-label" nil label)
                     (and description
                          (bloom-settings--text "settings-description" nil description
                                                :pad '(:top 2)))))))

(defun bloom-settings--link-row (label description action &optional trailing)
  "Return a group row for LABEL and DESCRIPTION dispatching ACTION.
TRAILING replaces the › chevron.  The row is a function of its edge trim;
see `bloom-settings--group'."
  (lambda (top bottom)
    (let ((row (jetpacs-with-attrs
                (jetpacs-row
                 (jetpacs-with-attrs (bloom-settings--label-column label description)
                                     :weight 1)
                 (jetpacs-spacer :width 12)
                 (or trailing (bloom-settings--text "settings-chevron" nil "›"))
                 :align "center" :fill t)
                ;; 0.2 dp is what the app's whole-pixel line boxes add to a
                ;; two-line row here; measured on the device.
                :pad (list :horizontal 14 :top (- 14 top)
                           :bottom (- 14.2 bottom)))))
      (if action (jetpacs-box row :on-tap action) row))))

(defun bloom-settings--change-action (symbol value)
  "Return the action setting SYMBOL to VALUE."
  (jetpacs-action "bloom.settings.change"
                  :args (list :name (symbol-name symbol)
                              :value (format "%S" value))))

(defun bloom-settings--switch (symbol label)
  "Return the app's Material switch for boolean SYMBOL, named LABEL.
Drawn from boxes so its geometry is Material's exactly.  The thumb sits
inside the track's 2 dp outline while off, so its 8 dp offset is 6 dp of
padding there; on, the track has no outline."
  (let ((on (and (symbol-value symbol) t)))
    (jetpacs-with-semantics
     (jetpacs-design-styled
      (list "bloom.settings.track"
            (if on "bloom.settings.track-on" "bloom.settings.track-off"))
      (list
       (jetpacs-box
        (jetpacs-with-attrs
         (jetpacs-design-styled
          (list (if on "bloom.settings.thumb-on" "bloom.settings.thumb-off"))
          (list (jetpacs-spacer :width 1)))
         :pad (list :start (if on 24 6)))
        :alignment "center_start"
        :on-tap (bloom-settings--change-action symbol (not on)))))
     :name label
     :state-description (if on "On" "Off"))))

(defun bloom-settings--toggle-row (symbol label description)
  "Return a switch row for boolean SYMBOL with LABEL and DESCRIPTION.
The row is a function of its edge trim; see `bloom-settings--group'."
  (lambda (top bottom)
  (jetpacs-with-attrs
   (jetpacs-row
    (jetpacs-with-attrs (bloom-settings--label-column label description) :weight 1)
    (jetpacs-spacer :width 12)
    ;; The switch injects its new state as `value' (SPEC 14.3).
    (bloom-settings--switch symbol label)
    :align "center" :fill t)
   :pad (list :horizontal 14 :top (- 12 top) :bottom (- 12 bottom)))))

(defun bloom-settings--segments (symbol choices)
  "Return the app's segmented control picking SYMBOL's value from CHOICES."
  (let ((current (symbol-value symbol)))
    (jetpacs-design-styled
     '("bloom.settings.segments")
     (list
      (apply #'jetpacs-row
             (append
              (mapcar (pcase-lambda (`(,value . ,name))
                        (let ((on (equal value current)))
                          (jetpacs-with-attrs
                           (jetpacs-design-styled
                            (list "bloom.settings.segment"
                                  (if on "bloom.settings.segment-on"
                                    "bloom.settings.segment-off"))
                            (list (jetpacs-box
                                   (bloom-settings--text "settings-segment"
                                                         (if on "on-primary" "muted")
                                                         name)
                                   :alignment "center"
                                   :on-tap (bloom-settings--change-action symbol value))))
                           :weight 1)))
                      choices)
              (list :fill t)))))))

(defun bloom-settings--choice-row (symbol label description choices)
  "Return a row picking SYMBOL's value from CHOICES, with LABEL and DESCRIPTION.
Two choices sit beside the label in a 160 dp segmented control, as the
app's do; more span the row below it.  The row is a function of its edge
trim; see `bloom-settings--group'."
  (lambda (top bottom)
    (let ((pad (list :horizontal 14 :top (- 14 top) :bottom (- 14 bottom))))
      (if (<= (length choices) 2)
          (jetpacs-with-attrs
           (jetpacs-row
            (jetpacs-with-attrs (bloom-settings--label-column label description)
                                :weight 1)
            (jetpacs-spacer :width 12)
            (jetpacs-with-attrs (bloom-settings--segments symbol choices) :width 160)
            :align "center" :fill t)
           :pad pad)
        (jetpacs-with-attrs
         (jetpacs-column
          (bloom-settings--label-column label description)
          (jetpacs-with-attrs (bloom-settings--segments symbol choices)
                              :pad '(:top 10))
          :fill t)
         :pad pad)))))

(defun bloom-settings--row (row)
  "Return the node for one page ROW."
  (pcase row
    (`(toggle ,symbol ,label ,description)
     (bloom-settings--toggle-row symbol label description))
    (`(choice ,symbol ,label ,description ,choices)
     (bloom-settings--choice-row symbol label description choices))))

(defun bloom-settings--section-label (text)
  "Return a section label TEXT in the accent color."
  (bloom-settings--text "settings-section" nil text
                        :pad '(:start 4 :top 4 :bottom 10)))

(defun bloom-settings--screen (title back body)
  "Return a settings screen titled TITLE with BACK around BODY nodes."
  (jetpacs-design-profile-scope
   (jetpacs-design-active-profile bloom-design-profile-id)
   (list
    (bloom-design-screen-scope
     (append (bloom-design-settings-styles)
             (seq-filter (lambda (style)
                           (equal (car style) "bloom.agenda.hairline"))
                         (bloom-design-agenda-styles)))
     (list
      (jetpacs-with-semantics
       (append
        (jetpacs-scaffold
         :top-bar (bloom-settings--top-bar title back)
         :body
         (jetpacs-design-scope
          nil nil
          (list
           (jetpacs-with-attrs
            (apply #'jetpacs-lazy-column
                   (append body (list (jetpacs-spacer :height 24))))
            :pad '(:horizontal 16 :top 12)))
))
        (list :fab nil))
       :pane-title title))))))

(defun bloom-settings-root-screen (back)
  "Return Bloom's settings root with BACK."
  (bloom-settings--screen
   "Settings" back
   (list
    (bloom-settings--group
     (mapcar (lambda (page)
               (bloom-settings--link-row
                (plist-get page :title) (plist-get page :description)
                (jetpacs-action "bloom.settings.page"
                                :args (list :page (plist-get page :key)))))
             (bloom-settings-pages)))
    (jetpacs-spacer :height 10)
    (bloom-settings--section-label "MORE")
    (bloom-settings--group
     (list (bloom-settings--link-row
            "Jetpacs Settings" "Jetpacs, Emacs, theme, and other apps"
            (jetpacs-shell-open-surface-action "app:jetpacs.settings"))))
    (jetpacs-spacer :height 10)
    (bloom-settings--section-label "ABOUT")
    (bloom-settings--group
     (list (bloom-settings--link-row
            "Bloom" "Bloom Native, Org notebooks in Emacs" nil
            (bloom-settings--text "settings-id" nil "app:bloom")))))))

(defun bloom-settings-page-screen (key back)
  "Return Bloom's settings page KEY with BACK."
  (let ((page (cl-find key (bloom-settings-pages)
                       :key (lambda (p) (plist-get p :key)) :test #'equal)))
    (bloom-settings--screen
     (plist-get page :title) back
     (list (bloom-settings--group (mapcar #'bloom-settings--row
                                          (plist-get page :rows)))))))

;;;; Actions

(defun bloom-settings--surface (params)
  "Return PARAMS' surface, or Bloom's app surface."
  (or (plist-get params :surface) bloom-app-surface))

(defun bloom-settings--push (params id builder)
  "Push screen ID built by BUILDER onto PARAMS' surface, later."
  (let ((surface (bloom-settings--surface params)))
    (jetpacs-flow-continue
     (lambda ()
       (condition-case err
           (jetpacs-chrome-push-screen surface id builder)
         (error (message "bloom: settings push failed (%s)" (car err))))))
    'accepted))

(defun bloom-settings-on-open (_args params)
  "Open Bloom's settings root on PARAMS' surface, such as the settings hub."
  (bloom-settings--push params "bloom-settings" #'bloom-settings-root-screen))

(defun bloom-settings-on-page (args params)
  "Open ARGS' settings page on PARAMS' surface."
  (let ((key (plist-get args :page)))
    (if (not (cl-find key (bloom-settings-pages)
                      :key (lambda (p) (plist-get p :key)) :test #'equal))
        'rejected
      (bloom-settings--push params (concat "bloom-settings-" key)
                            (lambda (back) (bloom-settings-page-screen key back))))))

(defun bloom-settings-on-change (args params)
  "Set ARGS' option to its value when it is one of Bloom's; refresh PARAMS."
  (let* ((symbol (intern-soft (or (plist-get args :name) "")))
         (raw (plist-get args :value))
         (value (cond
                 ;; A switch injects its new state as a JSON boolean.
                 ((eq raw t) t)
                 ((eq raw :json-false) nil)
                 ((stringp raw)
                  (condition-case nil (car (read-from-string raw))
                    (error 'bloom-settings--invalid)))
                 (t 'bloom-settings--invalid))))
    (cond
     ((eq value 'bloom-settings--invalid) 'rejected)
     ((eq symbol 'jetpacs-design-active-profile-id)
      (if (not (assoc value (bloom-settings--theme-choices))) 'rejected
        (jetpacs-design-set-active-profile value t)
        (jetpacs-app-defer-refresh params)
        'accepted))
     ((not (memq symbol (bloom-settings--symbols))) 'rejected)
     ((jetpacs-settings-apply symbol value #'bloom-settings--after-set)
      (jetpacs-app-defer-refresh params)
      'accepted)
     (t 'rejected))))

;;;; Registration

(defun bloom-settings-register ()
  "Register Bloom's settings beside Jetpacs', Emacs', Theme and Glasspane's."
  (jetpacs-settings-register-section
   bloom-settings-section
   (mapcar (lambda (symbol)
             (list symbol :after-set #'bloom-settings--after-set))
           (bloom-settings--symbols)))
  (jetpacs-settings-remove-hub-entry "bloom")
  (jetpacs-settings-register-hub-entry
   "bloom" :order 45 :icon "local_florist"
   :title "Bloom"
   :subtitle "Notebooks, notes, agenda, search, and reminders"
   :on-tap (jetpacs-action "bloom.settings.hub"))
  (with-jetpacs-owner bloom-owner
    ;; Bloom's settings also live on the Jetpacs settings hub, another
    ;; owner's surface, so these verbs are dispatched from any surface.
    ;; Each still accepts only Bloom's own pages and options.
    (jetpacs-defaction "bloom.settings.hub" #'bloom-settings-on-open
                       :any-surface t
                       :doc "Open Bloom's settings from the Settings hub")
    (jetpacs-defaction "bloom.settings.page" #'bloom-settings-on-page
                       :any-surface t
                       :args '((:name page :type "enum" :required t))
                       :doc "Open one page of Bloom's settings")
    (jetpacs-defaction "bloom.settings.change" #'bloom-settings-on-change
                       :any-surface t
                       :args '((:name name :type "text" :required t)
                               (:name value))
                       :doc "Change one of Bloom's settings")))

(defun bloom-settings-unregister ()
  "Remove Bloom's settings section and hub entry."
  (jetpacs-settings-remove-section bloom-settings-section)
  (jetpacs-settings-remove-hub-entry "bloom"))

(provide 'bloom-settings)
;;; bloom-settings.el ends here
