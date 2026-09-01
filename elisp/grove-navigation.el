;;; grove-navigation.el --- Grove app shell and notebook workflows -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Notebooks-first Grove information architecture.  This module owns only
;; presentation and navigation state; `grove-library' owns projections and
;; Emacs/Org or the reusable Jetpacs file layer owns every effect.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ebp-org)
(require 'jetpacs-apps)
(require 'jetpacs-chrome)
(require 'jetpacs-dialog)
(require 'jetpacs-settings)
(require 'jetpacs-shell)
(require 'jetpacs-widgets)
(require 'grove-core)
(require 'grove-design)
(require 'grove-document)
(require 'grove-library)
(require 'grove-org)
(require 'grove-reminders)
(require 'grove-structure)
(require 'grove-ui)
(require 'grove-views)

(defconst grove-navigation-destinations
  '((:key "notebooks" :label "Notebooks" :icon "folder"
          :subtitle "Your configured Org notebook library"
          :verb "grove.notebooks.open" :bar t)
    (:key "agenda" :label "Agenda" :icon "event"
          :subtitle "Today and upcoming Org work"
          :verb "grove.agenda.open" :bar t)
    (:key "search" :label "Search" :icon "search"
          :subtitle "Org text search and standard match syntax"
          :verb "grove.search.open" :bar t)
    (:key "settings" :label "Settings" :icon "settings"
          :subtitle "Grove presentation preferences"
          :verb "grove.settings.open" :bar t))
  "Grove's authoritative app destinations.")

(defvar grove-navigation--notebook-id nil
  "Notebook shown by the current Outline screen.")

(defun grove-navigation-capture-fab (&optional _surface)
  "Return Grove's app-default capture affordance."
  (jetpacs-icon-button
   "add" (grove-ui-capture-action)
   :content-description "Capture" :variant "filled" :size "large"))

(defun grove-navigation--surface (params)
  "Return PARAMS' source surface or Grove's app surface."
  (or (plist-get params :surface) grove-app-surface))

(defun grove-navigation-open-destination (route id builder &optional params)
  "Open ROUTE as screen ID through BUILDER on PARAMS' surface."
  (let ((surface (grove-navigation--surface params)))
    (jetpacs-apps-note-route grove-owner route)
    (jetpacs-flow-continue
     (lambda ()
       (condition-case err
           (progn
             (unless (equal route "notebooks")
               (jetpacs-chrome-reset-screens surface))
             (if (equal route "notebooks")
                 (jetpacs-chrome-reset-screens surface)
               (jetpacs-chrome-push-screen surface id builder)))
         (error
          (message "grove: destination %s failed (%s)" id (car err))))))
    'accepted))

(defun grove-navigation--folder-wire-id (folder-id)
  "Return non-nil wire value for optional FOLDER-ID."
  (or folder-id "root"))

(defun grove-navigation--folder-from-wire (value)
  "Return optional internal folder id from wire VALUE."
  (and (stringp value) (not (equal value "root")) value))

(defun grove-navigation--notebook-action (notebook-id)
  "Return action opening NOTEBOOK-ID's Grove outline."
  (jetpacs-action
   "grove.notebook.open" :args (list :notebook-id notebook-id)
   :when-offline (jetpacs-durable-offline-policy) :ttl-s 86400
   :open-surface grove-app-surface))

(defun grove-navigation--notebook-menu (record)
  "Return safe management menu for notebook RECORD."
  (let ((id (plist-get record :id)))
    (jetpacs-menu
     (list
      (jetpacs-menu-item
       (if (plist-get record :favorite) "Unfavorite" "Favorite")
       (jetpacs-action "grove.library.favorite"
                       :args (list :notebook-id id))
       :icon (if (plist-get record :favorite) "star" "star_outline"))
      (jetpacs-menu-item
       "Rename"
       (jetpacs-action "grove.library.rename"
                       :args (list :notebook-id id))
       :icon "edit")
      (jetpacs-menu-item
       "Move"
       (jetpacs-action "grove.library.move"
                       :args (list :notebook-id id))
       :icon "drive_file_move")
      (jetpacs-menu-item
       "Move to trash"
       (jetpacs-action
        "grove.library.trash" :args (list :notebook-id id)
        :confirm "Move this notebook to the recoverable trash?")
       :icon "delete"))
     :icon "more_vert")))

(defun grove-navigation-notebook-row (record)
  "Build one Grove notebook RECORD with reveal-first actions."
  (let* ((id (plist-get record :id))
         (favorite (plist-get record :favorite))
         (open (grove-navigation--notebook-action id))
         (favorite-action
          (jetpacs-action "grove.library.favorite"
                          :args (list :notebook-id id)))
         (rename-action
          (jetpacs-action "grove.library.rename"
                          :args (list :notebook-id id)))
         (swipe
          (jetpacs-swipe
           (list
            (jetpacs-swipe-action
             (if favorite "Unfavorite" "Favorite")
             :icon (if favorite "star" "star_outline")
             :color "primary" :on-trigger favorite-action)
            (jetpacs-swipe-action
             "Rename" :icon "edit" :color "secondary"
             :on-trigger rename-action)))))
    (jetpacs-with-attrs
     (jetpacs-card
      (jetpacs-row
       (jetpacs-icon (if favorite "star" "description")
                     :color (if favorite "primary" nil)
                     :content-description
                     (if favorite "Favorite notebook" "Notebook"))
       (jetpacs-with-attrs
        (jetpacs-column
         (jetpacs-text (plist-get record :label) :max-lines 2)
         (jetpacs-text (plist-get record :folder-label)
                       :style "caption" :max-lines 1)
         :spacing 2)
        :weight 1)
       (grove-navigation--notebook-menu record)
       :align "center" :spacing 10)
      :on-tap open :swipe-end swipe :variant "outlined")
     :key id)))

(defun grove-navigation--folder-row (record)
  "Build one folder RECORD for Notebooks drill-down."
  (jetpacs-chrome-row
   (plist-get record :label)
   :subtitle (plist-get record :path-label)
   :icon "folder"
   :key (plist-get record :id)
   :on-tap (jetpacs-action
            "grove.library.folder.open"
            :args (list :folder-id (plist-get record :id)))))

(defun grove-navigation--folder-title (folder-id)
  "Return FOLDER-ID's safe screen title."
  (if (null folder-id) "Notebooks"
    (or (plist-get
         (cl-find folder-id
                  (plist-get (grove-library-snapshot) :folders)
                  :key (lambda (record) (plist-get record :id))
                  :test #'equal)
         :label)
        "Notebooks")))

(defun grove-navigation--notebooks-body (folder-id)
  "Build FOLDER-ID's explicit notebook/folder snapshot."
  (let* ((snapshot (grove-library-snapshot))
         (flat (not grove-library-tree-view))
         (folders (unless flat
                    (grove-library-folders-in-folder folder-id)))
         (notebooks (grove-library-notebooks-in-folder
                     folder-id flat))
         (children
          (list
           (jetpacs-row
            (jetpacs-button
             (if flat "Flat" "Folders")
             (jetpacs-action "grove.library.view.toggle")
             :icon (if flat "view_list" "account_tree")
             :variant "tonal")
            (jetpacs-spacer :weight 1)
            (jetpacs-text
             (format "%d notebook%s"
                     (plist-get snapshot :total-count)
                     (if (= (plist-get snapshot :total-count) 1) "" "s"))
             :style "caption")))))
    (cond
     ((plist-get snapshot :error)
      (setq children
            (append children
                    (list
                     (jetpacs-empty-state
                      :icon "sync_problem" :title "Notebooks unavailable"
                      :caption "Check the Org agenda scope in Emacs."
                      :action-label "Refresh"
                      :on-tap (jetpacs-action "grove.library.refresh"))))))
     ((and (null folders) (null notebooks))
      (setq children
            (append children
                    (list
                     (jetpacs-empty-state
                      :icon "create_new_folder" :title "No notebooks here"
                      :caption "Create an Org notebook or change your agenda scope."
                      :action-label "New notebook"
                      :on-tap
                      (jetpacs-action
                       "grove.library.create"
                       :args (list :folder-id
                                   (grove-navigation--folder-wire-id
                                    folder-id))))))))
     (t
      (when folders
        (setq children
              (append children
                      (list (jetpacs-section-header "Folders"))
                      (mapcar #'grove-navigation--folder-row folders))))
      (when notebooks
        (setq children
              (append children
                      (list (jetpacs-section-header
                             (if flat "All notebooks" "Notebooks")))
                      (mapcar #'grove-navigation-notebook-row notebooks))))
      (when (plist-get snapshot :truncated)
        (setq children
              (append children
                      (list (jetpacs-text
                             "More notebooks exist; raise the Grove library limit in Settings."
                             :style "caption")))))))
    (apply #'jetpacs-lazy-column
           (append children (list :spacing 8 :content-padding 12)))))

(defun grove-navigation--notebooks-screen-for-folder (folder-id back)
  "Build FOLDER-ID's Notebooks screen with BACK."
  (jetpacs-chrome-screen
   (grove-navigation--folder-title folder-id)
   (grove-navigation--notebooks-body folder-id)
   :back back
   :actions
   (list
    (jetpacs-icon-button
     "refresh" (jetpacs-action "grove.library.refresh")
     :content-description "Refresh notebooks")
    (jetpacs-icon-button
     "create_new_folder"
     (jetpacs-action
      "grove.library.create"
      :args (list :folder-id
                  (grove-navigation--folder-wire-id
                   folder-id)))
     :content-description "New notebook"))
   :on-refresh (jetpacs-action "grove.library.refresh")))

(defun grove-navigation-notebooks-screen (back)
  "Build Grove's Notebooks-first root screen with BACK."
  (grove-navigation--notebooks-screen-for-folder nil back))

(defun grove-navigation--present-notebooks-screen-for-folder (folder-id back)
  "Build FOLDER-ID's profile-selected Notebooks screen with BACK."
  (let ((screen (grove-navigation--notebooks-screen-for-folder folder-id back)))
    (if (jetpacs-extension-advertised-p "jetpacs.design" :app)
        (grove-design-present-screen screen 'notebooks)
      screen)))

(defun grove-navigation-presented-notebooks-screen (back)
  "Build the profile-selected root Notebooks screen with BACK."
  (grove-navigation--present-notebooks-screen-for-folder nil back))

(defun grove-navigation--outline-children (records parent-id)
  "Build RECORDS whose parent is PARENT-ID recursively."
  (mapcar
   (lambda (record)
     (let* ((id (plist-get record :id))
            (children (grove-navigation--outline-children records id))
            (open (grove-document-open-action
                   grove-navigation--notebook-id id))
            (done (and (plist-get record :todo)
                       (jetpacs-action
                        "grove.document.todo"
                        :args (list :notebook-id grove-navigation--notebook-id
                                    :heading-id id :state "done")
                        :when-offline (jetpacs-durable-offline-policy)
                        :ttl-s 86400
                        :dedupe (jetpacs-wire-id "grove-outline-done" id))))
            (plan
             (jetpacs-action
              "grove.document.planning"
              :args (list :notebook-id grove-navigation--notebook-id
                          :heading-id id)))
            (swipe-end
             (if done
                 (jetpacs-swipe
                  (list
                   (jetpacs-swipe-action
                    "Done" :icon "done" :color "primary"
                    :on-trigger done)
                   (jetpacs-swipe-action
                    "Plan" :icon "event" :color "secondary"
                    :on-trigger plan))
                  :commit t)
               (jetpacs-swipe
                (list
                 (jetpacs-swipe-action
                  "Plan" :icon "event" :color "secondary"
                  :on-trigger plan)
                 (jetpacs-swipe-action
                  "Open" :icon "arrow_forward" :color "primary"
                  :on-trigger open)))))
            (row
             (jetpacs-card
              (jetpacs-row
               (jetpacs-icon
                (if (plist-get record :done) "check_circle"
                  (if children "account_tree" "chevron_right"))
                :content-description "Org heading")
               (jetpacs-with-attrs
                (jetpacs-column
                 (jetpacs-text (plist-get record :title) :max-lines 3)
                 (when-let* ((preview (plist-get record :preview)))
                   (jetpacs-text preview :style "caption" :max-lines 2))
                 :spacing 2)
                :weight 1)
               (when-let* ((todo (plist-get record :todo)))
                 (jetpacs-badge todo))
               (grove-structure-menu grove-navigation--notebook-id record)
               :spacing 8 :align "center")
              :on-tap open
              :swipe-start
              (grove-structure-swipe-start
               grove-navigation--notebook-id record)
              :swipe-end swipe-end
              :variant "outlined")))
       (if children
           (jetpacs-collapsible
            (jetpacs-wire-id "grove-outline" id) row children
            :collapsed
            (grove-structure-collapsed-value
             grove-navigation--notebook-id))
         row)))
   (seq-filter (lambda (record)
                 (equal (plist-get record :parent-id) parent-id))
               records)))

(defun grove-navigation-outline-screen (back)
  "Build the selected notebook's native outline screen with BACK."
  (condition-case err
      (let* ((snapshot
              (grove-library-outline-snapshot
               grove-navigation--notebook-id))
             (slice
              (grove-structure-outline-slice
               grove-navigation--notebook-id
               (plist-get snapshot :records)))
             (records (plist-get slice :records))
             (focused (plist-get slice :focused))
             (expanded
              (grove-structure-expanded-p
               grove-navigation--notebook-id))
             (children nil))
        (when-let* (((not focused))
                    (preface (plist-get snapshot :preface))
                    ((not (string-blank-p preface))))
          (push (jetpacs-card
                 (jetpacs-column
                  (jetpacs-text "Preface" :style "label")
                  (jetpacs-text preface :syntax "org" :max-lines 12)
                  :spacing 4)
                 :variant "outlined")
                children))
        (setq children
              (append (nreverse children)
                      (if records
                          (grove-navigation--outline-children records nil)
                        (list
                         (jetpacs-empty-state
                          :icon "format_list_bulleted"
                          :title "No headings"
                          :caption "The notebook currently contains only preface text.")))))
        (when (plist-get snapshot :truncated)
          (setq children
                (append children
                        (list (jetpacs-text
                               "Outline truncated at the configured safety limit."
                               :style "caption")))))
        (jetpacs-chrome-screen
         (concat (or (plist-get snapshot :label) "Outline")
                 (if focused " · Focus" ""))
         (apply #'jetpacs-lazy-column
                (append children (list :spacing 8 :content-padding 12)))
         :back back
         :actions
         (delq
          nil
          (list
           (when (jetpacs-org-structure-undo-summary grove-owner)
             (jetpacs-icon-button
              "undo" (grove-structure-undo-action)
              :content-description "Undo last outline change"))
           (when focused
             (jetpacs-icon-button
              "filter_center_focus"
              (grove-structure-widen-action
               grove-navigation--notebook-id)
              :content-description "Widen outline"))
           (jetpacs-icon-button
            (if expanded "unfold_less" "unfold_more")
            (grove-structure-expansion-action
             grove-navigation--notebook-id (not expanded))
            :content-description
            (if expanded "Collapse all headings" "Expand all headings"))
           (jetpacs-icon-button
            "refresh" (jetpacs-action "grove.outline.refresh")
            :content-description "Refresh outline")))))
    ((ebp-org-unresolved ebp-org-refused)
     (jetpacs-chrome-screen
      "Outline"
      (jetpacs-empty-state :icon "search_off" :title "Notebook moved or gone"
                           :caption "Return to Notebooks and refresh.")
      :back back))
    (error
     (message "grove: outline build failed (%s)" (car err))
     (jetpacs-chrome-screen
      "Outline"
      (jetpacs-empty-state :icon "error" :title "Outline unavailable"
                           :caption "Refresh the notebook and try again.")
      :back back))))

(defun grove-ui-open-outline (notebook-id &optional params)
  "Open NOTEBOOK-ID's Grove outline on PARAMS' surface."
  (cond
   ((not (stringp notebook-id)) 'rejected)
   ((jetpacs-event-stale-p params) 'stale)
   ((not (grove-library-resolve-notebook notebook-id)) 'stale)
   (t
    (setq grove-navigation--notebook-id notebook-id)
    (condition-case err
        (progn
          (grove-library-outline-refresh notebook-id)
          (jetpacs-flow-continue
           (lambda ()
             (jetpacs-chrome-push-screen
              (grove-navigation--surface params) "outline"
              #'grove-navigation-outline-screen)))
          'accepted)
      (ebp-org-unresolved 'stale)
      (error
       (message "grove: outline open failed (%s)" (car err))
       'rejected)))))

(defun grove-navigation--search-result-card (record)
  "Build one canonical Org search result RECORD."
  (jetpacs-card
   (jetpacs-column
    (jetpacs-text (plist-get record :headline) :max-lines 3)
    (jetpacs-text
     (string-join
      (delq nil
            (list (plist-get record :todo)
                  (plist-get record :notebook-label)
                  (when-let* ((tags (plist-get record :tags)))
                    (mapconcat (lambda (tag) (format ":%s:" tag)) tags " "))))
      " · ")
     :style "caption" :max-lines 2)
    :spacing 3)
   :on-tap
   (grove-document-open-action
    (plist-get record :notebook-id) (plist-get record :heading-id))
   :variant "outlined"))

(defun grove-navigation--search-body ()
  "Build Grove's search screen from its explicit snapshot."
  (let* ((snapshot (grove-library-search-snapshot))
         (query (plist-get snapshot :query))
         (mode (if (member (plist-get snapshot :mode) '("text" "match"))
                   (plist-get snapshot :mode)
                 "text"))
         (results (plist-get snapshot :results))
         (children
          (append
           (grove-views-nodes)
           (list
            (jetpacs-section-header "Search")
            (when-let* ((label (plist-get snapshot :view-label)))
              (jetpacs-card
               (jetpacs-row
                (jetpacs-icon "bookmark" :content-description "Active saved view")
                (jetpacs-column
                 (jetpacs-text label :max-lines 2)
                 (jetpacs-text
                  (if (equal (plist-get snapshot :view-source) "emacs")
                      "Read-only Emacs custom agenda"
                    "Grove saved view")
                  :style "caption" :max-lines 1)
                 :spacing 2)
                :spacing 8 :align "center")
               :variant "filled"))
            (jetpacs-text-input
             "grove-search-query" :value query
             :label (if (equal mode "match") "Org match expression"
                      "Text, Boolean, or {regexp} search")
             :leading-icon "search" :single-line t :max-length 512
             :on-submit
             (jetpacs-action "grove.search.run"
                             :args (list :mode mode)))
            (jetpacs-row
             (jetpacs-button
              "Text / regexp"
              (jetpacs-action "grove.search.mode"
                              :args (list :mode "text"))
              :variant (if (equal mode "text") "filled" "text"))
             (jetpacs-button
              "Org match"
              (jetpacs-action "grove.search.mode"
                              :args (list :mode "match"))
              :variant (if (equal mode "match") "filled" "text"))
             :spacing 6)
            (jetpacs-row
             (jetpacs-button
              "Search"
              (jetpacs-action "grove.search.run"
                              :args (list :mode mode)
                              :capture-fields '("grove-search-query"))
              :icon "search" :variant "tonal")
             (jetpacs-button
              "Save view" (grove-views-save-action mode)
              :icon "bookmark_add" :variant "text")
             :spacing 6)))))
    (setq children (delq nil children))
    (when-let* ((error (plist-get snapshot :error)))
      (setq children
            (append children
                    (list (jetpacs-empty-state
                           :icon "error" :title "Search expression error"
                           :caption error)))))
    (cond
     (results
      (let ((last-label nil))
        (dolist (record results)
          (let ((label (plist-get record :notebook-label)))
            (unless (equal label last-label)
              (setq children
                    (append children (list (jetpacs-section-header label)))
                    last-label label))
            (setq children
                  (append children
                          (list (grove-navigation--search-result-card record))))))))
     ((or (plist-get snapshot :view-id) (not (string-blank-p query)))
      (setq children
            (append children
                    (list (jetpacs-empty-state
                           :icon "search_off" :title "No matches"
                           :caption
                           (if (plist-get snapshot :view-id)
                               "This saved view currently has no matching headings."
                             "Try another text or Org match expression."))))))
     (t
      (setq children
            (append children
                    (list (jetpacs-empty-state
                           :icon "manage_search" :title "Search your notebooks"
                           :caption "Use Emacs Org text search or standard Org match syntax."))))))
    (apply #'jetpacs-lazy-column
           (append children (list :spacing 8 :content-padding 12)))))

(defun grove-navigation-search-screen (back)
  "Build Grove's native search destination with BACK."
  (jetpacs-chrome-screen "Search" (grove-navigation--search-body) :back back))

(defun grove-navigation-settings-screen (back)
  "Build Grove's app-specific settings destination with BACK."
  (jetpacs-chrome-screen
   "Grove Settings"
   (jetpacs-lazy-column
    (jetpacs-section-header "Library")
    (jetpacs-switch
     "grove-setting-tree" :checked (jetpacs-bool grove-library-tree-view)
     :label "Folder tree"
     :on-change (jetpacs-action "grove.settings.set"
                                :args (list :field "tree")))
    (jetpacs-section-header "Reader")
    (jetpacs-switch
     "grove-setting-drawers" :checked (jetpacs-bool grove-reader-show-drawers)
     :label "Show drawers initially"
     :on-change (jetpacs-action "grove.settings.set"
                                :args (list :field "drawers")))
    (jetpacs-row
     (jetpacs-button
      "Read first" (jetpacs-action "grove.settings.set"
                                   :args (list :field "mode" :value "read"))
      :variant (if (eq grove-reader-default-mode 'read) "filled" "text"))
     (jetpacs-button
      "Edit first" (jetpacs-action "grove.settings.set"
                                   :args (list :field "mode" :value "edit"))
      :variant (if (eq grove-reader-default-mode 'edit) "filled" "text"))
     :spacing 6)
    (jetpacs-section-header "Reminders")
    (jetpacs-switch
     "grove-setting-reminders"
     :checked (jetpacs-bool grove-reminders-enabled)
     :label "Timed Org reminders"
     :on-change (jetpacs-action "grove.settings.set"
                                :args (list :field "reminders-enabled")))
    (jetpacs-text
     (if (zerop grove-reminder-lead-minutes)
         "Alert at the scheduled time"
       (format "Alert %d minutes early" grove-reminder-lead-minutes))
     :style "caption")
    (jetpacs-flow-row
     (jetpacs-button
      "At time" (jetpacs-action "grove.settings.set"
                                 :args (list :field "reminder-lead" :value 0))
      :variant (if (zerop grove-reminder-lead-minutes) "filled" "outlined"))
     (jetpacs-button
      "15 min" (jetpacs-action "grove.settings.set"
                                :args (list :field "reminder-lead" :value 15))
      :variant (if (= grove-reminder-lead-minutes 15) "filled" "outlined"))
     (jetpacs-button
      "1 hour" (jetpacs-action "grove.settings.set"
                                :args (list :field "reminder-lead" :value 60))
      :variant (if (= grove-reminder-lead-minutes 60) "filled" "outlined"))
     (jetpacs-button
      "1 day" (jetpacs-action "grove.settings.set"
                               :args (list :field "reminder-lead" :value 1440))
      :variant (if (= grove-reminder-lead-minutes 1440) "filled" "outlined")))
    (jetpacs-section-header "Bounds")
    (jetpacs-text
     (format "Agenda: %d days · Search: %d results · Library: %d notebooks"
             grove-org-agenda-days grove-search-result-limit
             grove-library-notebook-limit)
     :style "caption")
    (jetpacs-flow-row
     (jetpacs-button "Agenda 7d"
                     (jetpacs-action "grove.settings.set"
                                     :args (list :field "agenda-days" :value 7))
                     :variant "outlined")
     (jetpacs-button "Agenda 14d"
                     (jetpacs-action "grove.settings.set"
                                     :args (list :field "agenda-days" :value 14))
                     :variant "outlined")
     (jetpacs-button "Search 50"
                     (jetpacs-action "grove.settings.set"
                                     :args (list :field "search-limit" :value 50))
                     :variant "outlined")
     (jetpacs-button "Search 200"
                     (jetpacs-action "grove.settings.set"
                                     :args (list :field "search-limit" :value 200))
                     :variant "outlined"))
    (jetpacs-divider)
    (jetpacs-text
     "Org roots, synchronization, backups, connection policy, and Android permissions remain owned by Emacs and the Jetpacs Companion."
     :style "caption")
    :spacing 10 :content-padding 12)
   :back back))

(defun grove-navigation--on-library-refresh (_args params)
  "Refresh library and outline projections for event PARAMS."
  (if (grove-library-refresh)
      (progn
        (when grove-navigation--notebook-id
          (ignore-errors
            (grove-library-outline-refresh grove-navigation--notebook-id)))
        (jetpacs-app-defer-refresh params)
        'accepted)
    'rejected))

(defun grove-navigation--on-folder-open (args params)
  "Open opaque folder ARGS on PARAMS' surface."
  (let ((folder-id (plist-get args :folder-id)))
    (if (not (and (stringp folder-id)
                  (grove-library-resolve-folder folder-id)))
        'stale
      (jetpacs-flow-continue
       (lambda ()
         (jetpacs-chrome-push-screen
          (grove-navigation--surface params)
          (jetpacs-wire-id "grove-folder" folder-id)
          (apply-partially
           #'grove-navigation--present-notebooks-screen-for-folder folder-id))))
      'accepted)))

(defun grove-navigation--on-view-toggle (_args params)
  "Toggle the library view and refresh PARAMS' surface."
  (grove-navigation--save-setting
   'grove-library-tree-view (not grove-library-tree-view))
  (jetpacs-app-defer-refresh params)
  'accepted)

(defun grove-navigation--prompt-name (prompt initial)
  "Read one trimmed notebook name using PROMPT and INITIAL."
  (string-trim
   (condition-case nil (read-string prompt initial) (quit ""))))

(defun grove-navigation--on-create (args params)
  "Prompt for a notebook inside ARGS' folder for event PARAMS."
  (let* ((wire-folder-id (plist-get args :folder-id))
         (folder-id (grove-navigation--folder-from-wire wire-folder-id))
         (surface (grove-navigation--surface params)))
    (cond
     ((not (stringp wire-folder-id)) 'rejected)
     ((and folder-id (not (grove-library-resolve-folder folder-id))) 'stale)
     (t
      (jetpacs-flow-continue
       (lambda ()
         (if (not (jetpacs-dialog-can-bridge-p))
             (jetpacs-shell-notify "Notebook dialog is unavailable" surface)
           (let ((name (grove-navigation--prompt-name "New notebook: " "")))
             (unless (string-empty-p name)
               (condition-case err
                   (progn
                     (grove-library-create-notebook folder-id name)
                     (jetpacs-shell-notify "Notebook created" surface)
                     (jetpacs-shell-push surface))
                 (error
                  (message "grove: create failed (%s)" (car err))
                  (jetpacs-shell-notify
                   "Notebook could not be created" surface))))))))
      'accepted))))

(defun grove-navigation--on-rename (args params)
  "Prompt for and rename ARGS' notebook for event PARAMS."
  (let* ((notebook-id (plist-get args :notebook-id))
         (record (grove-library--notebook-record notebook-id))
         (surface (grove-navigation--surface params)))
    (if (null record) 'stale
      (jetpacs-flow-continue
       (lambda ()
         (if (not (jetpacs-dialog-can-bridge-p))
             (jetpacs-shell-notify "Rename dialog is unavailable" surface)
           (let ((name (grove-navigation--prompt-name
                        "Rename notebook: " (plist-get record :file-label))))
             (unless (string-empty-p name)
               (condition-case err
                   (progn
                     (grove-library-rename-notebook notebook-id name)
                     (jetpacs-shell-notify "Notebook renamed" surface)
                     (jetpacs-shell-push surface))
                 (error
                  (message "grove: rename failed (%s)" (car err))
                  (jetpacs-shell-notify "Notebook could not be renamed"
                                        surface))))))))
      'accepted)))

(defun grove-navigation--folder-choices ()
  "Return unique display-name to opaque-folder-id choices."
  (cons (cons "Notebooks" "root")
        (mapcar (lambda (record)
                  (cons (plist-get record :path-label)
                        (plist-get record :id)))
                (plist-get (grove-library-snapshot) :folders))))

(defun grove-navigation--on-move (args params)
  "Prompt for and move ARGS' notebook for event PARAMS."
  (let ((notebook-id (plist-get args :notebook-id))
        (surface (grove-navigation--surface params)))
    (if (null (grove-library--notebook-record notebook-id)) 'stale
      (jetpacs-flow-continue
       (lambda ()
         (if (not (jetpacs-dialog-can-bridge-p))
             (jetpacs-shell-notify "Move dialog is unavailable" surface)
           (let* ((choices (grove-navigation--folder-choices))
                  (selected
                   (condition-case nil
                       (completing-read "Move notebook to: " choices nil t)
                     (quit "")))
                  (folder-id (cdr (assoc selected choices))))
             (when folder-id
               (condition-case err
                   (progn
                     (grove-library-move-notebook
                      notebook-id
                      (grove-navigation--folder-from-wire folder-id))
                     (jetpacs-shell-notify "Notebook moved" surface)
                     (jetpacs-shell-push surface))
                 (error
                  (message "grove: move failed (%s)" (car err))
                  (jetpacs-shell-notify "Notebook could not be moved"
                                        surface))))))))
      'accepted)))

(defun grove-navigation--on-trash (args params)
  "Move ARGS' notebook to trash and update PARAMS' surface."
  (condition-case err
      (progn
        (grove-library-trash-notebook (plist-get args :notebook-id))
        (jetpacs-shell-notify "Notebook moved to trash"
                              (grove-navigation--surface params))
        (jetpacs-app-defer-refresh params)
        'accepted)
    (ebp-org-unresolved 'stale)
    (error
     (message "grove: trash failed (%s)" (car err))
     (jetpacs-shell-notify "Recoverable trash is unavailable"
                           (grove-navigation--surface params))
     'rejected)))

(defun grove-navigation--on-favorite (args params)
  "Toggle ARGS' notebook favorite and refresh PARAMS' surface."
  (condition-case nil
      (progn
        (grove-library-toggle-favorite (plist-get args :notebook-id))
        (jetpacs-app-defer-refresh params)
        'accepted)
    (ebp-org-unresolved 'stale)
    (error 'rejected)))

(defun grove-navigation--on-outline-refresh (_args params)
  "Refresh the current outline for event PARAMS."
  (if (null grove-navigation--notebook-id) 'stale
    (condition-case nil
        (progn
          (grove-library-outline-refresh grove-navigation--notebook-id)
          (jetpacs-app-defer-refresh params)
          'accepted)
      (ebp-org-unresolved 'stale)
      (error 'rejected))))

(defun grove-navigation--on-search-run (args params)
  "Run native Org search using ARGS and captured PARAMS."
  (let* ((captured (plist-get (plist-get params :fields)
                              :grove-search-query))
         (query (or (plist-get args :value) captured ""))
         (mode (or (plist-get args :mode)
                   (plist-get (grove-library-search-snapshot) :mode)
                   "text")))
    (if (not (and (stringp query) (<= (length query) 512)
                  (member mode '("text" "match"))))
        'rejected
      (grove-library-search query mode)
      (jetpacs-app-defer-refresh params)
      'accepted)))

(defun grove-navigation--on-search-mode (args params)
  "Switch to ARGS' search mode and refresh PARAMS' surface."
  (let ((mode (plist-get args :mode)))
    (if (not (member mode '("text" "match"))) 'rejected
      (grove-library-search-set-mode mode)
      (jetpacs-app-defer-refresh params)
      'accepted)))

(defun grove-navigation--save-setting (symbol value)
  "Set Customize SYMBOL to VALUE and persist it."
  (let ((old (symbol-value symbol)))
    (condition-case err
        (unless (jetpacs-settings-apply symbol value)
          (user-error "Grove setting was not accepted"))
      (error
       (set symbol old)
       (signal (car err) (cdr err))))))

(defun grove-navigation--on-setting (args params)
  "Apply whitelisted setting ARGS and refresh PARAMS' surface."
  (let ((field (plist-get args :field))
        (value (plist-get args :value)))
    ;; Stateful controls inject their current value into ARGS.
    (if
        (pcase field
          ((or "tree" "drawers")
           (when (memq value '(t :json-false))
             (grove-navigation--save-setting
              (if (equal field "tree")
                  'grove-library-tree-view
                'grove-reader-show-drawers)
              (eq value t))
             t))
          ("mode"
           (when (member value '("read" "edit"))
             (grove-navigation--save-setting
              'grove-reader-default-mode (intern value))
             t))
          ("agenda-days"
           (when (and (integerp value) (<= 1 value 31))
             (grove-navigation--save-setting 'grove-org-agenda-days value)
             (grove-org-refresh)
             t))
          ("search-limit"
           (when (and (integerp value) (<= 1 value 512))
             (grove-navigation--save-setting
              'grove-search-result-limit value)
             t))
          ("reminders-enabled"
           (when (memq value '(t :json-false))
             (grove-navigation--save-setting
              'grove-reminders-enabled (eq value t))
             (grove-reminders-schedule-sync)
             t))
          ("reminder-lead"
           (when (memq value '(0 5 10 15 30 60 1440))
             (grove-navigation--save-setting
              'grove-reminder-lead-minutes value)
             (grove-reminders-schedule-sync)
             t)))
        (progn
          (jetpacs-app-defer-refresh params)
          'accepted)
      'rejected)))

(defconst grove-navigation--verbs
  '("grove.notebooks.open" "grove.agenda.open" "grove.search.open"
    "grove.settings.open" "grove.library.refresh"
    "grove.library.folder.open" "grove.library.view.toggle"
    "grove.library.create" "grove.library.rename" "grove.library.move"
    "grove.library.trash" "grove.library.favorite" "grove.notebook.open"
    "grove.outline.refresh" "grove.search.run" "grove.search.mode"
    "grove.settings.set")
  "Action names owned by Grove navigation.")

(defun grove-navigation-register ()
  "Register Grove's destinations and library actions idempotently."
  (with-jetpacs-owner grove-owner
                      (jetpacs-defaction
                       "grove.notebooks.open"
                       (lambda (_args params)
                         (grove-navigation-open-destination
                          "notebooks" "notebooks"
                          #'grove-navigation-presented-notebooks-screen params)))
                      (jetpacs-defaction
                       "grove.agenda.open"
                       (lambda (_args params)
                         (grove-navigation-open-destination
                          "agenda" "agenda" #'grove-ui-presented-screen params)))
                      (jetpacs-defaction
                       "grove.search.open"
                       (lambda (_args params)
                         (grove-navigation-open-destination
                          "search" "search" #'grove-navigation-search-screen params)))
                      (jetpacs-defaction
                       "grove.settings.open"
                       (lambda (_args params)
                         (grove-navigation-open-destination
                          "settings" "settings" #'grove-navigation-settings-screen params)))
                      (jetpacs-defaction "grove.library.refresh"
                                         #'grove-navigation--on-library-refresh)
                      (jetpacs-defaction "grove.library.folder.open"
                                         #'grove-navigation--on-folder-open)
                      (jetpacs-defaction "grove.library.view.toggle"
                                         #'grove-navigation--on-view-toggle)
                      (jetpacs-defaction "grove.library.create"
                                         #'grove-navigation--on-create)
                      (jetpacs-defaction "grove.library.rename"
                                         #'grove-navigation--on-rename)
                      (jetpacs-defaction "grove.library.move"
                                         #'grove-navigation--on-move)
                      (jetpacs-defaction "grove.library.trash"
                                         #'grove-navigation--on-trash)
                      (jetpacs-defaction "grove.library.favorite"
                                         #'grove-navigation--on-favorite)
                      (jetpacs-defaction
                       "grove.notebook.open"
                       (lambda (args params)
                         (grove-ui-open-outline (plist-get args :notebook-id) params)))
                      (jetpacs-defaction "grove.outline.refresh"
                                         #'grove-navigation--on-outline-refresh)
                      (jetpacs-defaction "grove.search.run"
                                         #'grove-navigation--on-search-run)
                      (jetpacs-defaction "grove.search.mode"
                                         #'grove-navigation--on-search-mode)
                      (jetpacs-defaction "grove.settings.set"
                                         #'grove-navigation--on-setting))
  t)

(defun grove-navigation-unregister ()
  "Unregister Grove navigation and clear its transient selections."
  (dolist (verb grove-navigation--verbs) (jetpacs-undefaction verb))
  (setq grove-navigation--notebook-id nil)
  t)

(provide 'grove-navigation)
;;; grove-navigation.el ends here
