;;; bloom-navigation.el --- Bloom app shell and notebook workflows -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Notebooks-first Bloom information architecture.  This module owns only
;; presentation and navigation state; `bloom-library' owns projections and
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
(require 'jetpacs-components)
(require 'jetpacs-widgets)
(require 'bloom-core)
(require 'bloom-design)
(require 'bloom-document)
(require 'bloom-library)
(require 'bloom-org)
(require 'bloom-reminders)
(require 'bloom-structure)
(require 'bloom-ui)
(require 'bloom-views)

(defconst bloom-navigation-destinations
  '((:key "notebooks" :label "Notebooks" :icon "folder"
          :subtitle "Your configured Org notebook library"
          :verb "bloom.notebooks.open" :bar t)
    (:key "agenda" :label "Agenda" :icon "event"
          :subtitle "Today and upcoming Org work"
          :verb "bloom.agenda.open" :bar t)
    (:key "search" :label "Search" :icon "search"
          :subtitle "Org text search and standard match syntax"
          :verb "bloom.search.open" :bar t)
    (:key "settings" :label "Settings" :icon "settings"
          :subtitle "Bloom presentation preferences"
          :verb "bloom.settings.open" :bar t))
  "Bloom's authoritative app destinations.")

(defvar bloom-navigation--notebook-id nil
  "Notebook shown by the current Outline screen.")

(defun bloom-navigation-capture-fab (&optional _surface)
  "Return Bloom's app-default capture affordance."
  (jetpacs-icon-button
   "add" (bloom-ui-capture-action)
   :content-description "Capture" :variant "filled" :size "large"))

(defun bloom-navigation--surface (params)
  "Return PARAMS' source surface or Bloom's app surface."
  (or (plist-get params :surface) bloom-app-surface))

(defun bloom-navigation-open-destination (route id builder &optional params)
  "Open ROUTE as screen ID through BUILDER on PARAMS' surface."
  (let ((surface (bloom-navigation--surface params)))
    (jetpacs-apps-note-route bloom-owner route)
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
          (message "bloom: destination %s failed (%s)" id (car err))))))
    'accepted))

(defun bloom-navigation--folder-wire-id (folder-id)
  "Return non-nil wire value for optional FOLDER-ID."
  (or folder-id "root"))

(defun bloom-navigation--folder-from-wire (value)
  "Return optional internal folder id from wire VALUE."
  (and (stringp value) (not (equal value "root")) value))

(defun bloom-navigation--notebook-action (notebook-id)
  "Return action opening NOTEBOOK-ID's Bloom outline."
  (jetpacs-action
   "bloom.notebook.open" :args (list :notebook-id notebook-id)
   :when-offline (jetpacs-durable-offline-policy) :ttl-s 86400
   :open-surface bloom-app-surface))

(defun bloom-navigation--notebook-menu (record)
  "Return safe management menu for notebook RECORD."
  (let ((id (plist-get record :id)))
    (jetpacs-with-semantics
     (jetpacs-menu
      (list
       (jetpacs-menu-item
        (if (plist-get record :favorite) "Unfavorite" "Favorite")
        (jetpacs-action "bloom.library.favorite"
                        :args (list :notebook-id id))
        :icon (if (plist-get record :favorite) "star" "star_outline"))
       (jetpacs-menu-item
        "Rename"
        (jetpacs-action "bloom.library.rename"
                        :args (list :notebook-id id))
        :icon "edit")
       (jetpacs-menu-item
        "Move"
        (jetpacs-action "bloom.library.move"
                        :args (list :notebook-id id))
        :icon "drive_file_move")
       (jetpacs-menu-item
        "Move to trash"
        (jetpacs-action
         "bloom.library.trash" :args (list :notebook-id id)
         :confirm "Move this notebook to the recoverable trash?")
        :icon "delete"))
      :icon "more_vert")
     :name "Notebook actions")))

(defun bloom-navigation-notebook-row (record)
  "Build one Bloom notebook RECORD with reveal-first actions."
  (let* ((id (plist-get record :id))
         (favorite (plist-get record :favorite))
         (open (bloom-navigation--notebook-action id))
         (favorite-action
          (jetpacs-action "bloom.library.favorite"
                          :args (list :notebook-id id)))
         (rename-action
          (jetpacs-action "bloom.library.rename"
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
             :on-trigger rename-action))))
         (leading (jetpacs-icon (if favorite "star" "description")
                                :color (if favorite "primary" nil)
                                :content-description
                                (if favorite "Favorite notebook" "Notebook")))
         (trailing (bloom-navigation--notebook-menu record)))
    (jetpacs-components-list-item
     (plist-get record :label)
     :subtitle (plist-get record :folder-label)
     :title-max-lines 2 :subtitle-max-lines 1
     :leading leading
     :trailing trailing
     :on-tap open
     :swipe-end swipe
     :key id)))

(defun bloom-navigation--folder-row (record)
  "Build one folder RECORD for Notebooks drill-down."
  (jetpacs-chrome-row
   (plist-get record :label)
   :subtitle (plist-get record :path-label)
   :icon "folder"
   :key (plist-get record :id)
   :on-tap (jetpacs-action
            "bloom.library.folder.open"
            :args (list :folder-id (plist-get record :id)))))

(defun bloom-navigation--folder-title (folder-id)
  "Return FOLDER-ID's safe screen title."
  (if (null folder-id) "Notebooks"
    (or (plist-get
         (cl-find folder-id
                  (plist-get (bloom-library-snapshot) :folders)
                  :key (lambda (record) (plist-get record :id))
                  :test #'equal)
         :label)
        "Notebooks")))

(defun bloom-navigation--notebooks-body (folder-id)
  "Build FOLDER-ID's explicit notebook/folder snapshot."
  (let* ((snapshot (bloom-library-snapshot))
         (flat (not bloom-library-tree-view))
         (folders (unless flat
                    (bloom-library-folders-in-folder folder-id)))
         (notebooks (bloom-library-notebooks-in-folder
                     folder-id flat))
         (children
          (list
           (jetpacs-row
            (jetpacs-button
             (if flat "Flat" "Folders")
             (jetpacs-action "bloom.library.view.toggle")
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
                      :on-tap (jetpacs-action "bloom.library.refresh"))))))
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
                       "bloom.library.create"
                       :args (list :folder-id
                                   (bloom-navigation--folder-wire-id
                                    folder-id))))))))
     (t
      (when folders
        (setq children
              (append children
                      (list (jetpacs-section-header "Folders"))
                      (mapcar #'bloom-navigation--folder-row folders))))
      (when notebooks
        (setq children
              (append children
                      (list (jetpacs-section-header
                             (if flat "All notebooks" "Notebooks")))
                      (mapcar #'bloom-navigation-notebook-row notebooks))))
      (when (plist-get snapshot :truncated)
        (setq children
              (append children
                      (list (jetpacs-text
                             "More notebooks exist; raise the Bloom library limit in Settings."
                             :style "caption")))))))
    (apply #'jetpacs-lazy-column
           (append children (list :spacing 8 :content-padding 12)))))

(defun bloom-navigation--material-drawer ()
  "Return Bloom's drawer for receivers without the design runtime.
Bloom is a standalone Jetpacs app, so it authors its own navigation."
  (jetpacs-lazy-column
   (jetpacs-chrome-row "Search" :icon "search"
                       :on-tap (jetpacs-action "bloom.search.open"))
   (jetpacs-chrome-row "Notebooks" :icon "book"
                       :on-tap (jetpacs-action "bloom.notebooks.open"))
   (jetpacs-chrome-row "Agenda" :icon "calendar_view_day"
                       :on-tap (jetpacs-action "bloom.agenda.open"))
   (jetpacs-chrome-row "Settings" :icon "settings"
                       :on-tap (jetpacs-action "bloom.settings.open"))
   (jetpacs-chrome-row "Jetpacs apps" :icon "apps"
                       :on-tap (jetpacs-shell-open-surface-action
                                "app:jetpacs.app-store"))
   :spacing 4))

(defun bloom-navigation--notebooks-screen-for-folder (folder-id back)
  "Build FOLDER-ID's Notebooks screen with BACK."
  (jetpacs-chrome-screen
   (bloom-navigation--folder-title folder-id)
   (bloom-navigation--notebooks-body folder-id)
   :back back
   :drawer (unless (or back folder-id) (bloom-navigation--material-drawer))
   :actions
   (list
    (jetpacs-icon-button
     "refresh" (jetpacs-action "bloom.library.refresh")
     :content-description "Refresh notebooks")
    (jetpacs-icon-button
     "create_new_folder"
     (jetpacs-action
      "bloom.library.create"
      :args (list :folder-id
                  (bloom-navigation--folder-wire-id
                   folder-id)))
     :content-description "New notebook"))
   :on-refresh (jetpacs-action "bloom.library.refresh")))

(defun bloom-navigation-notebooks-screen (back)
  "Build Bloom's Notebooks-first root screen with BACK."
  (bloom-navigation--notebooks-screen-for-folder nil back))

(defun bloom-navigation--present-notebooks-screen-for-folder (folder-id back)
  "Build FOLDER-ID's profile-selected Notebooks screen with BACK."
  (bloom-design-present-if-advertised
   (bloom-navigation--notebooks-screen-for-folder folder-id back)
   'notebooks))

(defun bloom-navigation-presented-notebooks-screen (back)
  "Build the profile-selected root Notebooks screen with BACK.
With the design runtime and Bloom's parity faces this is the Android app's
own Notebooks screen; otherwise the Material screen, scoped when possible."
  (if (bloom-navigation--parity-p)
      (bloom-navigation--parity-notebooks-screen back)
    (bloom-navigation--present-notebooks-screen-for-folder nil back)))

;;;; Parity Notebooks: the Android app's tree, rows, dock, and drawer
;;
;; Built only when the receiver advertises `jetpacs.design' and the active
;; profile declares Bloom's parity faces (`bloom-design-parity-available-p');
;; otherwise the Material screen above stays the authority.  Every color is a
;; profile token or EBP role, never a literal, and every measurement is the
;; app's: 14 dp gutters, 8x10 dp row insets, 42 dp monogram tiles, a 20 dp
;; indent with a 1.5 dp rail, and the floating Search/Capture/New dock.

(defconst bloom-navigation--row-height
  (+ 20 (* 15 1.65) (* 12.5 1.65) 0.4)
  "Height in dp of a notebook row: 10 dp insets around its two text lines.
The title (15 sp) and age line (12.5 sp) both carry the app's 1.65 em
line height, which outgrows the 42 dp tile.")

(defconst bloom-navigation--dock-pill-width 121
  "Width in dp of the dock's Capture pill, which the capsule reserves.")

(defun bloom-navigation--styled-text (role text)
  "Return TEXT set in Bloom's exact text ROLE."
  (jetpacs-design-styled (list (concat "bloom.t." role))
                         (list (jetpacs-text text :max-lines 1))))

(defun bloom-navigation--tile (styles content)
  "Return a 42 dp tile carrying STYLES around centered CONTENT text."
  (jetpacs-design-styled
   (cons "bloom.tile" styles)
   (list (jetpacs-box (jetpacs-text content) :alignment "center"))))

(defconst bloom-navigation--line-rounding 0.4
  "Slack in dp the app's whole-pixel line boxes add to a two-line row.
Compose rounds each text line box up to device pixels; two lines add about
this much on a 2.8 density phone, so rows keep the app's exact pitch.")

(defun bloom-navigation--row-box (children &rest keys)
  "Return one flat Notebooks row of CHILDREN with tap KEYS.
KEYS are `:on-tap', `:on-long-tap', `:key', and `:content-height', the dp
height of the row's text lines.  The face is the app's: a 14 dp rounded
press area, 8 dp side and 10 dp vertical insets."
  (jetpacs-with-attrs
   (jetpacs-box
    (jetpacs-with-attrs
     (apply #'jetpacs-row (append children (list :align "center" :fill t)))
     :pad '(:horizontal 8 :vertical 10)
     :min_height (when-let* ((height (plist-get keys :content-height)))
                   (+ height bloom-navigation--line-rounding)))
    :on-tap (plist-get keys :on-tap)
    :on-long-tap (plist-get keys :on-long-tap))
   :corner 14 :clip t :fill_fraction 1
   :key (plist-get keys :key)))

(defun bloom-navigation--indented (depth row)
  "Return ROW indented for tree DEPTH with the app's guide rail."
  (if (<= depth 0) row
    (let ((indent (* 20 (min depth 2))))
      (jetpacs-row
       (jetpacs-spacer :width (- indent 2))
       (jetpacs-design-styled
        '("bloom.rail")
        (list (jetpacs-spacer :width 1.5
                              :height bloom-navigation--row-height)))
       (jetpacs-spacer :width 0.5)
       (jetpacs-with-attrs row :weight 1)
       :fill t))))

(defun bloom-navigation--parity-file-row (record depth &optional pinned)
  "Return notebook RECORD's parity row at tree DEPTH; PINNED shows its path."
  (let* ((id (plist-get record :id))
         (count (plist-get record :note-count))
         (age (or (plist-get record :modified-label) ""))
         (meta (if count
                   (format "%s \u00b7 %s" (bloom-library--plural count "note") age)
                 age))
         (label (or (plist-get record :file-label) ""))
         (palette (or (plist-get record :palette)
                      (bloom-library-palette-key label)))
         (directory (and pinned (plist-get record :directory-label))))
    (bloom-navigation--indented
     depth
     (bloom-navigation--row-box
      (delq nil
            (list
             (bloom-navigation--tile
              (list (concat "bloom.tile." palette) "bloom.t.monogram")
              (or (plist-get record :monogram) (bloom-library--monogram label)))
             (jetpacs-spacer :width 12)
             (jetpacs-with-attrs
              (apply #'jetpacs-column
                     (delq nil
                           (list
                            (bloom-navigation--styled-text "row-title" label)
                            (and directory
                                 (jetpacs-with-attrs
                                  (bloom-navigation--styled-text
                                   "row-path" directory)
                                  :pad '(:top 1)))
                            (bloom-navigation--styled-text "row-meta" meta))))
              :weight 1)
             (and (plist-get record :favorite)
                  (jetpacs-with-attrs
                   (jetpacs-icon "push_pin" :size 14 :color "primary"
                                 :content-description "Pinned")
                   :pad '(:end 6)))))
      :on-tap (bloom-navigation--notebook-action id)
      :on-long-tap (jetpacs-action "bloom.library.notebook.menu"
                                   :args (list :notebook-id id))
      :content-height (+ (* 15 1.65) (* 12.5 1.65)
                         (if directory (+ 1 (* 11 1.65)) 0))
      :key id))))

(defun bloom-navigation--parity-folder-row (record depth)
  "Return folder RECORD's parity row at tree DEPTH."
  (let* ((id (plist-get record :id))
         (palette (or (plist-get record :palette)
                      (bloom-library-palette-key
                       (or (plist-get record :path-label) ""))))
         (files (or (plist-get record :file-count) 0))
         (folders (or (plist-get record :folder-count) 0))
         (drill (> files bloom-library-drill-threshold))
         (meta (if (> folders 0)
                   (format "%s / %s" (bloom-library--plural files "file")
                           (bloom-library--plural folders "folder"))
                 (bloom-library--plural files "file"))))
    (bloom-navigation--indented
     depth
     (bloom-navigation--row-box
      (list
       (bloom-navigation--tile
        (list (concat "bloom.tile." palette) (concat "bloom.tile-edge." palette)
              "bloom.t.folder-glyph")
        "▪")
       (jetpacs-spacer :width 12)
       (jetpacs-with-attrs
        (jetpacs-column
         (bloom-navigation--styled-text "folder-title" (plist-get record :label))
         (jetpacs-with-attrs
          (bloom-navigation--styled-text "folder-meta" meta)
          :pad '(:top 2)))
        :weight 1)
       (jetpacs-spacer :width 8)
       (jetpacs-with-attrs
        (bloom-navigation--styled-text
         (if drill "chevron-flat" "chevron")
         (cond (drill "›")
               ((plist-get record :expanded) "▴")
               (t "▾")))
        :pad '(:end 6)))
      :on-tap (if drill
                  (jetpacs-action "bloom.library.folder.open"
                                  :args (list :folder-id id))
                (jetpacs-action "bloom.library.folder.toggle"
                                :args (list :folder-id id)))
      :content-height (+ (* 14.5 1.65) 2 (* 12 1.65))
      :key id))))

(defun bloom-navigation--parity-tree (folder-id depth)
  "Return parity rows below FOLDER-ID at DEPTH, folders first.
Pinned notebooks are omitted here; they lead the list in their own strip."
  (append
   (cl-mapcan
    (lambda (folder)
      (cons (bloom-navigation--parity-folder-row folder depth)
            (when (plist-get folder :expanded)
              (bloom-navigation--parity-tree
               (plist-get folder :id) (1+ depth)))))
    (bloom-library-folders-in-folder folder-id))
   (mapcar (lambda (record)
             (bloom-navigation--parity-file-row record depth))
           (sort (seq-remove (lambda (record) (plist-get record :favorite))
                             (bloom-library-notebooks-in-folder folder-id))
                 (lambda (left right)
                   (string-lessp (downcase (plist-get left :file-label))
                                 (downcase (plist-get right :file-label))))))))

(defun bloom-navigation--parity-empty (title caption)
  "Return the app's centered empty message with TITLE and CAPTION."
  (jetpacs-with-attrs
   (jetpacs-column
    (bloom-navigation--styled-text "empty-glyph" "✦")
    (bloom-navigation--styled-text "empty-title" title)
    (bloom-navigation--styled-text "empty-caption" caption)
    :align "center" :spacing 6)
   :pad '(:top 120) :fill_fraction 1))

(defun bloom-navigation--parity-body ()
  "Return the parity Notebooks list: pinned strip, then the tree."
  (let* ((snapshot (bloom-library-snapshot))
         (pinned (seq-filter (lambda (record) (plist-get record :favorite))
                             (plist-get snapshot :notebooks)))
         (rows
          (cond
           ((plist-get snapshot :error)
            (list (bloom-navigation--parity-empty
                   "Notebooks unavailable"
                   "Check the Org agenda scope in Emacs")))
           ((null (plist-get snapshot :notebooks))
            (list (bloom-navigation--parity-empty
                   "No .org files here yet"
                   "Capture a note or make a notebook from the bar below")))
           (t
            (append
             (mapcar (lambda (record)
                       (bloom-navigation--parity-file-row record 0 t))
                     pinned)
             (bloom-navigation--parity-tree nil 0))))))
    (jetpacs-with-attrs
     (apply #'jetpacs-lazy-column
            (append (list (jetpacs-spacer :height 4))
                    rows
                    ;; Clearance for the dock the list scrolls under.
                    (list (jetpacs-spacer :height 96))))
     :pad '(:horizontal 14))))

(defun bloom-navigation--glyph-button (icon size box on-tap description
                                            &optional color corner)
  "Return a BOX dp square tap target drawing ICON at SIZE dp.
ON-TAP is its action and DESCRIPTION its accessible name; COLOR defaults to
the ink role and CORNER to half the box, a circle."
  (jetpacs-with-attrs
   (jetpacs-box (jetpacs-icon icon :size size
                              :color (or color "on_background")
                              :content-description description)
                :alignment "center" :on-tap on-tap)
   :width box :height box :corner (or corner (/ box 2.0)) :clip t))

(defun bloom-navigation--parity-top-bar (snapshot)
  "Return the parity top bar row for SNAPSHOT: title, unfold, and sync."
  (let* ((folders (plist-get snapshot :folders))
         (collapsed (not (cl-some (lambda (folder) (plist-get folder :expanded))
                                  folders))))
    (jetpacs-with-attrs
     (apply #'jetpacs-row
            (append
             (list (jetpacs-with-attrs
                    (bloom-navigation--styled-text "top-title" "Notebooks")
                    :weight 1 :pad '(:start 8)))
             (when folders
               (list (bloom-navigation--glyph-button
                      (if collapsed "unfold_more_filled" "unfold_less_filled")
                      22 44
                      (jetpacs-action "bloom.library.folders.expand"
                                      :args (list :expand (if collapsed t :json-false)))
                      (if collapsed "Expand all folders" "Collapse all folders")
                      nil 10)))
             (list (bloom-navigation--glyph-button
                    "check_filled" 24 48
                    (jetpacs-action "bloom.library.refresh")
                    "Synced" "success"))
             (list :align "center" :fill t)))
     :height 56 :pad '(:end 16))))

(defun bloom-navigation--parity-dock ()
  "Return the app's floating dock: Search, a raised Capture pill, New notebook."
  (jetpacs-box
   (jetpacs-with-attrs
   (jetpacs-surface
    (jetpacs-design-styled
     '("bloom.dock")
     (list
      (jetpacs-row
       (bloom-navigation--glyph-button
        "search" 20 48 (jetpacs-action "bloom.search.open") "Search")
       (jetpacs-spacer :width bloom-navigation--dock-pill-width :height 48)
       (bloom-navigation--glyph-button
        "book" 20 48
        (jetpacs-action "bloom.library.create" :args (list :folder-id "root"))
        "New notebook")
       :spacing 4 :align "center")))
    :shadow-elevation 10)
   :corner 30)
   (jetpacs-with-attrs
   (jetpacs-surface
    (jetpacs-design-styled
     '("bloom.dock-pill")
     (list
      (jetpacs-box
       (jetpacs-row
        (bloom-navigation--styled-text "dock-plus" "+")
        (jetpacs-spacer :width 9)
        (bloom-navigation--styled-text "dock-label" "Capture")
        :align "center")
       :alignment "center" :on-tap (bloom-ui-capture-action))))
    :shadow-elevation 8 :color "primary")
   :corner 36)
   :alignment "center"))

(defun bloom-navigation--drawer-item (icon label on-tap &optional active)
  "Return one parity drawer row: ICON, LABEL, ON-TAP; ACTIVE highlights it."
  (let ((row (jetpacs-with-attrs
              (jetpacs-row
               (jetpacs-with-attrs
                (jetpacs-icon icon :size 24
                              :color (if active "primary" "secondary"))
                :width 30)
               (bloom-navigation--styled-text
                (if active "drawer-item-active" "drawer-item") label)
               :align "center" :fill t)
              :pad '(:horizontal 12 :vertical 11))))
    (jetpacs-with-attrs
     (jetpacs-box
      (if active (jetpacs-design-styled '("bloom.drawer-active") (list row)) row)
      :on-tap on-tap)
     :pad '(:horizontal 12 :vertical 2) :corner 11 :clip t :fill_fraction 1)))

(defun bloom-navigation--drawer-section (label)
  "Return a parity drawer section LABEL."
  (jetpacs-with-attrs
   (bloom-navigation--styled-text "drawer-section" label)
   :pad '(:start 22 :top 16 :bottom 6)))

(defun bloom-navigation--hairline ()
  "Return the app's one dp line-colored divider."
  (jetpacs-design-styled '("bloom.hairline")
                         (list (jetpacs-with-attrs (jetpacs-spacer :height 1)
                                                   :fill_fraction 1))))

(defun bloom-navigation--brand-mark ()
  "Return the Bloom asterisk on its 42 dp soft-accent tile."
  (let* ((size 22.26) (half (/ size 2.0)) (width (* size (/ 7.0 32)))
         ops)
    (dolist (angle '(36 108 180 252 324))
      (let* ((radians (degrees-to-radians (+ angle 90)))
             (reach (- half (/ width 2.0)))
             (x (+ half (* reach (cos radians))))
             (y (+ half (* reach (sin radians)))))
        (push (jetpacs-canvas-line half half x y :color "primary" :width width)
              ops)
        (push (jetpacs-canvas-circle x y (/ width 2.0) :fill "primary") ops)))
    (jetpacs-design-styled
     '("bloom.tile" "bloom.tile.accent")
     (list (jetpacs-box (jetpacs-canvas size size (nreverse ops))
                        :alignment "center")))))

(defun bloom-navigation--parity-drawer (active)
  "Return Bloom's own navigation drawer with ACTIVE route highlighted."
  (let ((views (plist-get (bloom-views-snapshot) :views)))
    (apply
     #'jetpacs-column
     (append
      (list
       (jetpacs-with-attrs
        (jetpacs-column
         (bloom-navigation--brand-mark)
         (jetpacs-spacer :height 10)
         (bloom-navigation--styled-text "drawer-app" "Bloom")
         ;; The app names its vault folder home-relative, as `~/NAME'.
         (bloom-navigation--styled-text
          "drawer-path"
          (concat "~/" (file-name-nondirectory
                        (directory-file-name
                         (or (bloom-library--org-root) "~/org"))))))
        :padding 22)
       (bloom-navigation--hairline)
       (jetpacs-spacer :height 8)
       (bloom-navigation--drawer-item
        "search" "Search" (jetpacs-action "bloom.search.open"))
       (bloom-navigation--drawer-item
        "book" "Notebooks" (jetpacs-action "bloom.notebooks.open")
        (equal active "notebooks")))
      (when views
        (cons (bloom-navigation--drawer-section "SEARCHES")
              (mapcar (lambda (view)
                        (bloom-navigation--drawer-item
                         "filter_center_focus" (plist-get view :label)
                         (bloom-views-run-action view)))
                      views)))
      (list
       (jetpacs-with-attrs (bloom-navigation--hairline)
                           :pad '(:vertical 8))
       (bloom-navigation--drawer-item
        "calendar_view_day" "Agenda" (jetpacs-action "bloom.agenda.open")
        (equal active "agenda"))
       (bloom-navigation--drawer-item
        "settings" "Settings" (jetpacs-action "bloom.settings.open")
        (equal active "settings"))
       ;; Bloom runs as a standalone Jetpacs app, so the host drawer is
       ;; withdrawn; this row is the way back to the other apps.
       (bloom-navigation--drawer-item
        "apps" "Jetpacs apps"
        (jetpacs-shell-open-surface-action "app:jetpacs.app-store"))
       (jetpacs-spacer :height 8))))))

(defun bloom-navigation--parity-notebooks-screen (_back)
  "Return the parity Notebooks root screen inside the active profile."
  (let ((snapshot (bloom-library-snapshot)))
    (jetpacs-design-profile-scope
     (jetpacs-design-active-profile bloom-design-profile-id)
     (list
      (jetpacs-with-semantics
       (jetpacs-scaffold
        :top-bar (bloom-navigation--parity-top-bar snapshot)
        :body (bloom-navigation--parity-body)
        :drawer (bloom-navigation--parity-drawer "notebooks")
        :fab (bloom-navigation--parity-dock)
        :fab-position "center"
        :on-refresh (jetpacs-action "bloom.library.refresh"))
       :pane-title "Notebooks")))))

(defun bloom-navigation--parity-p ()
  "Return non-nil when Bloom can draw its parity screens on this receiver."
  (and (jetpacs-extension-advertised-p "jetpacs.design" :app)
       (bloom-design-parity-available-p)))

(defun bloom-navigation--outline-children (records parent-id)
  "Build RECORDS whose parent is PARENT-ID recursively."
  (mapcar
   (lambda (record)
     (let* ((id (plist-get record :id))
            (children (bloom-navigation--outline-children records id))
            (open (bloom-document-open-action
                   bloom-navigation--notebook-id id))
            (done (and (plist-get record :todo)
                       (jetpacs-action
                        "bloom.document.todo"
                        :args (list :notebook-id bloom-navigation--notebook-id
                                    :heading-id id :state "done")
                        :when-offline (jetpacs-durable-offline-policy)
                        :ttl-s 86400
                        :dedupe (jetpacs-wire-id "bloom-outline-done" id))))
            (plan
             (jetpacs-action
              "bloom.document.planning"
              :args (list :notebook-id bloom-navigation--notebook-id
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
            (menu (bloom-structure-menu bloom-navigation--notebook-id record))
            (todo (plist-get record :todo))
            (row
             (jetpacs-components-list-item
              (plist-get record :title)
              :subtitle (plist-get record :preview)
              :title-max-lines 3 :subtitle-max-lines 2
              :overline (when (or todo children)
                          (string-join
                           (delq nil
                                 (list todo
                                       (when children
                                         (format "%d section%s" (length children)
                                                 (if (= (length children) 1)
                                                     "" "s")))))
                           " · "))
              ;; The enclosing disclosure owns expansion.  Its header has
              ;; no competing tap target; reading is an explicit action.
              :trailing (jetpacs-row
                         (when children
                           (jetpacs-button "Read" open
                                           :variant "text" :size "small"))
                         menu :spacing 4 :align "center")
              :on-tap (unless children open)
              :swipe-start
              (bloom-structure-swipe-start
               bloom-navigation--notebook-id record)
              :swipe-end swipe-end)))
       (if children
           (jetpacs-collapsible
            (jetpacs-wire-id "bloom-outline" id) row children
            :collapsed
            (bloom-structure-collapsed-value
             bloom-navigation--notebook-id))
         row)))
   (seq-filter (lambda (record)
                 (equal (plist-get record :parent-id) parent-id))
               records)))

(defun bloom-navigation-outline-screen (back)
  "Build the selected notebook's native outline screen with BACK."
  (condition-case err
      (let* ((snapshot
              (bloom-library-outline-snapshot
               bloom-navigation--notebook-id))
             (slice
              (bloom-structure-outline-slice
               bloom-navigation--notebook-id
               (plist-get snapshot :records)))
             (records (plist-get slice :records))
             (focused (plist-get slice :focused))
             (expanded
              (bloom-structure-expanded-p
               bloom-navigation--notebook-id))
             (children nil))
        (when-let* (((not focused))
                    (preface (plist-get snapshot :preface))
                    ((not (string-blank-p preface))))
          (push (jetpacs-card
                 (jetpacs-column
                  (jetpacs-text "Introduction" :style "label")
                  (jetpacs-text preface :syntax "org" :max-lines 12)
                  :spacing 4)
                 :variant "outlined")
                children))
        (setq children
              (append
               (list
                (jetpacs-column
                 (jetpacs-text (if focused "Focused outline" "In this notebook")
                               :style "title")
                 (jetpacs-text
                  "Expand sections to browse. Choose Read to open a parent heading."
                  :style "caption")
                 :spacing 4))
               (nreverse children)
                      (if records
                          (bloom-navigation--outline-children records nil)
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
                (append children (list :spacing 12 :content-padding 16)))
         :back back
         :actions
         (delq
          nil
          (list
           (when (jetpacs-org-structure-undo-summary bloom-owner)
             (jetpacs-icon-button
              "undo" (bloom-structure-undo-action)
              :content-description "Undo last outline change"))
           (when focused
             (jetpacs-icon-button
              "filter_center_focus"
              (bloom-structure-widen-action
               bloom-navigation--notebook-id)
              :content-description "Widen outline"))
           (jetpacs-icon-button
            (if expanded "unfold_less" "unfold_more")
            (bloom-structure-expansion-action
             bloom-navigation--notebook-id (not expanded))
            :content-description
            (if expanded "Collapse all headings" "Expand all headings"))
           (jetpacs-icon-button
            "refresh" (jetpacs-action "bloom.outline.refresh")
            :content-description "Refresh outline")))))
    ((ebp-org-unresolved ebp-org-refused)
     (jetpacs-chrome-screen
      "Outline"
      (jetpacs-empty-state :icon "search_off" :title "Notebook moved or gone"
                           :caption "Return to Notebooks and refresh.")
      :back back))
    (error
     (message "bloom: outline build failed (%s)" (car err))
     (jetpacs-chrome-screen
      "Outline"
      (jetpacs-empty-state :icon "error" :title "Outline unavailable"
                           :caption "Refresh the notebook and try again.")
      :back back))))

;;;; Parity Outline: the Android app's heading tree

(defvar bloom-navigation--outline-expanded (make-hash-table :test #'equal)
  "Notebook id to the heading ids its parity outline shows expanded.
Like the app, an opened notebook starts with every heading folded.")

(defvar bloom-navigation--preface-open (make-hash-table :test #'equal)
  "Notebook ids whose parity outline shows the PREFACE band expanded.")

(defconst bloom-navigation--star-inks
  '("success" "info" "warning" "error" "violet" "primary")
  "Heading-star inks cycled by outline depth, as the app's `starColor'.")

(defun bloom-navigation--ink-text (role ink text &rest attrs)
  "Return TEXT in text ROLE and color INK, with universal ATTRS."
  (apply #'jetpacs-with-attrs
         (jetpacs-design-styled
          (delq nil (list (concat "bloom.t." role)
                          (and ink (concat "bloom.fg." ink))))
          (list (jetpacs-text text)))
         attrs))

(defun bloom-navigation--outline-caret (children collapsed)
  "Return the 9 dp toggle triangle, or a 7 dp leaf ring without CHILDREN.
COLLAPSED points the triangle right; otherwise it points down.  Drawn on a
canvas, as the app does, so every state centers on the same point."
  (let ((ink (bloom-design-color "faint")))
    (if (null children)
        (jetpacs-canvas 7 7 (list (jetpacs-canvas-circle
                                   3.5 3.5 2.9 :color ink :stroke-width 1.2)))
      (jetpacs-canvas
       9 9 (list (jetpacs-canvas-path
                  (if collapsed
                      (list (jetpacs-canvas-point 0 0) (jetpacs-canvas-point 0 9)
                            (jetpacs-canvas-point 9 4.5))
                    (list (jetpacs-canvas-point 0 0) (jetpacs-canvas-point 9 0)
                          (jetpacs-canvas-point 4.5 9)))
                  :fill ink :closed t))))))

(defun bloom-navigation--keyword-ink (todo done)
  "Return the chip ink for TODO keyword, DONE when it is a done state."
  (cond (done "success")
        ((equal todo "IN-PROGRESS") "info")
        (t "warning")))

(defun bloom-navigation--priority-ink (priority)
  "Return the app's urgency ink for PRIORITY letter."
  (pcase (and priority (upcase priority))
    ("A" "error") ("B" "warning") ("C" "success") (_ "muted")))

(defun bloom-navigation--planning-chip (icon ink label)
  "Return a soft planning chip with ICON, INK color, and LABEL."
  (jetpacs-design-styled
   (list (concat "bloom.chip." ink))
   (list (jetpacs-row
          (jetpacs-icon icon :size 11 :color (if (equal ink "error") "error" nil))
          (bloom-navigation--ink-text "outline-meta" ink label)
          :spacing 3 :align "center"))))

(defun bloom-navigation--outline-heading (notebook-id record children collapsed)
  "Return parity heading RECORD of NOTEBOOK-ID with direct CHILDREN.
COLLAPSED folds it.  Tapping the row reads the heading; the caret folds it;
the swipes and long-press menu keep Bloom's structure operations."
  (let* ((id (plist-get record :id))
         (level (or (plist-get record :level) 1))
         (todo (plist-get record :todo))
         (done (plist-get record :done))
         (priority (plist-get record :priority))
         (tags (plist-get record :tags))
         (lines (plist-get record :lines))
         (scheduled (plist-get record :scheduled-label))
         (deadline (plist-get record :deadline-label))
         (count (length children))
         (open (bloom-document-open-action notebook-id id))
         (title-role (concat (if (= level 1) "outline-title" "outline-title-sub")
                             (if done "-done" "")))
         ;; Mono 11 sits 4.2 dp lower than the 14.5 title to share its
         ;; baseline; a chip's own 1 dp inset takes one of those.
         (title-row
          (apply
           #'jetpacs-row
           (append
            (delq
             nil
             (list
              (and todo
                   (jetpacs-with-attrs
                    (jetpacs-design-styled
                     (list (concat "bloom.chip."
                                   (bloom-navigation--keyword-ink todo done)))
                     (list (bloom-navigation--ink-text
                            "outline-chip"
                            (bloom-navigation--keyword-ink todo done) todo)))
                    :pad '(:top 3.2 :end 6)))
              (and priority
                   (bloom-navigation--ink-text
                    "outline-chip" (bloom-navigation--priority-ink priority)
                    (format "[#%s]" priority) :pad '(:top 4.2 :end 6)))
              (jetpacs-with-attrs
               (jetpacs-design-styled
                (list (concat "bloom.t." title-role))
                (list (jetpacs-text (or (plist-get record :title) ""))))
               :weight 1)
              (and collapsed (> count 0)
                   (bloom-navigation--ink-text
                    "outline-meta" "faint" (format "… %d" count)
                    :pad '(:top 4.2 :start 6)))
              (and tags
                   (bloom-navigation--ink-text
                    "outline-meta" "syn-tag"
                    (concat ":" (string-join tags ":") ":")
                    :pad '(:top 4.2 :start 6)))))
            ;; Top-aligned: a wrapped title keeps its chips and tags on its
            ;; first line, as the app's baseline alignment does.
            (list :fill t :align "top"))))
         (column
          (apply
           #'jetpacs-column
           (delq
            nil
            (list
             title-row
             (and lines
                  (jetpacs-with-attrs
                   (jetpacs-design-styled
                    '("bloom.t.outline-preview")
                    (list (jetpacs-text lines :max-lines 2)))
                   :pad '(:top 2)))
             (and (or scheduled deadline)
                  (jetpacs-with-attrs
                   (apply #'jetpacs-flow-row
                          (append
                           (delq nil
                                 (list
                                  (and scheduled
                                       (bloom-navigation--planning-chip
                                        "calendar_month" "info" scheduled))
                                  (and deadline
                                       (bloom-navigation--planning-chip
                                        "flag_filled" "error" deadline))))
                           (list :spacing 5 :run-spacing 3)))
                   :pad '(:top 3)))))))
         (row
          (jetpacs-with-attrs
           (jetpacs-row
            (jetpacs-row
             (jetpacs-with-attrs
              (jetpacs-box (bloom-navigation--outline-caret children collapsed)
                           :alignment "center"
                           :on-tap (and children
                                        (jetpacs-action
                                         "bloom.outline.toggle"
                                         :args (list :heading-id id))))
              :width 22 :height 22 :corner 6 :clip t)
             (jetpacs-spacer :width 4)
             (bloom-navigation--ink-text
              "outline-stars"
              (nth (% (1- level) 6) bloom-navigation--star-inks)
              (make-string level ?*))
             :align "center")
            (jetpacs-spacer :width 7)
            (jetpacs-with-attrs column :weight 1)
            :align "top" :fill t)
           :pad (list :start (* 22 (1- level)) :top 9 :bottom 9 :end 6))))
    (jetpacs-with-attrs
     (jetpacs-card
      row
      :variant "filled"
      :on-tap open
      :on-long-tap (jetpacs-action "bloom.outline.menu"
                                   :args (list :heading-id id))
      :swipe-start (bloom-structure-swipe-start notebook-id record))
     :key id)))

(defun bloom-navigation--outline-rows (notebook-id records parent-id expanded)
  "Return NOTEBOOK-ID's parity rows for RECORDS under PARENT-ID.
EXPANDED lists the heading ids shown unfolded."
  (cl-mapcan
   (lambda (record)
     (let* ((id (plist-get record :id))
            (children (seq-filter (lambda (other)
                                    (equal (plist-get other :parent-id) id))
                                  records))
            (collapsed (not (member id expanded))))
       (cons (bloom-navigation--outline-heading
              notebook-id record children collapsed)
             (unless collapsed
               (bloom-navigation--outline-rows
                notebook-id records id expanded)))))
   (seq-filter (lambda (record) (equal (plist-get record :parent-id) parent-id))
               records)))

(defun bloom-navigation--preface-band (notebook-id keywords)
  "Return NOTEBOOK-ID's faded PREFACE band over its KEYWORDS pairs."
  (let ((open (gethash notebook-id bloom-navigation--preface-open)))
    (jetpacs-with-attrs
     (jetpacs-design-styled
      '("bloom.preface")
      (list
       (apply
        #'jetpacs-column
        (append
         (list
          (jetpacs-with-attrs
           (jetpacs-box
            (jetpacs-row
             (jetpacs-with-attrs
              (bloom-navigation--styled-text "kv-caret" (if open "▾" "▸"))
              :width 10)
             (jetpacs-spacer :width 8)
             (jetpacs-with-attrs (bloom-navigation--styled-text "kv-label" "PREFACE")
                                 :weight 1)
             (bloom-navigation--styled-text "kv-count"
                                            (number-to-string (length keywords)))
             :align "center" :fill t)
            :on-tap (jetpacs-action "bloom.outline.preface"))
           :pad '(:horizontal 12 :vertical 8) :fill_fraction 1))
         (when open
           (list
            (jetpacs-with-attrs
             (apply #'jetpacs-column
                    (append
                     (mapcar (lambda (pair)
                               (jetpacs-row
                                (bloom-navigation--ink-text
                                 "kv" "syn-keyword" (concat (car pair) " "))
                                (bloom-navigation--ink-text "kv" "muted" (cdr pair))))
                             keywords)
                     (list :spacing 3)))
             :pad '(:start 30 :end 12 :bottom 10))))))))
     :alpha 0.66 :pad '(:top 8 :bottom 4) :fill_fraction 1)))

(defun bloom-navigation--outline-top-bar (title count back &optional folded)
  "Return the parity outline bar: BACK, TITLE over COUNT notes, and actions.
FOLDED non-nil, when every heading is folded, offers to unfold them all;
otherwise the bar offers to fold them, as the app's chevrons do."
  (jetpacs-design-scope
   nil nil
   (list
    (jetpacs-with-attrs
     (jetpacs-row
      ;; System Back reads this leading `arrow_back' button, so it stays a
      ;; real icon button; the receiver keeps its 48 dp target, centred where
      ;; the app's 44 dp glyph box centres (30 dp in).
      (jetpacs-icon-button "arrow_back" back :content-description "Back")
      (jetpacs-with-attrs
       (jetpacs-column
        (bloom-navigation--styled-text "outline-file" title)
        (bloom-navigation--styled-text "outline-count"
                                       (bloom-library--plural count "note")))
       :weight 1 :pad '(:start 2))
      (bloom-navigation--glyph-button
       (if folded "unfold_more_filled" "unfold_less_filled")
       24 44 (jetpacs-action "bloom.outline.expand")
       (if folded "Expand all headings" "Collapse all headings") nil 10)
      (bloom-navigation--glyph-button
       "search" 24 44 (jetpacs-action "bloom.search.open")
       "Search in this notebook" nil 10)
      (bloom-navigation--glyph-button
       "more_vert" 24 44 (jetpacs-action "bloom.outline.refresh")
       "Refresh outline" nil 10)
      :align "center" :fill t)
     :height 56 :pad '(:start 6 :end 16)))
   :component-styles
   (list (jetpacs-design-component-style "icon-button.container"
                                         '("bloom.glyph-button")))))

(defun bloom-navigation--parity-outline-screen (back)
  "Return the parity Outline screen for the current notebook with BACK."
  (let* ((notebook-id bloom-navigation--notebook-id)
         (snapshot (bloom-library-outline-snapshot notebook-id))
         (records (plist-get (bloom-structure-outline-slice
                              notebook-id (plist-get snapshot :records))
                             :records))
         (record (bloom-library--notebook-record notebook-id))
         (expanded (gethash notebook-id bloom-navigation--outline-expanded))
         (keywords (plist-get snapshot :keywords))
         (intro (plist-get snapshot :intro))
         (count (+ (cl-count 1 records :key (lambda (r) (plist-get r :level)))
                   (if intro 1 0))))
    (jetpacs-design-profile-scope
     (jetpacs-design-active-profile bloom-design-profile-id)
     (list
      (jetpacs-with-semantics
       (jetpacs-scaffold
        :top-bar (bloom-navigation--outline-top-bar
                  (or (plist-get record :file-label)
                      (plist-get snapshot :label) "Outline")
                  count back (null expanded))
        :body
        (jetpacs-design-scope
         nil nil
         (list
          (jetpacs-with-attrs
           (apply #'jetpacs-lazy-column
                  (append
                   (delq nil
                         (list (and keywords
                                    (bloom-navigation--preface-band
                                     notebook-id keywords))
                               (and intro
                                    (jetpacs-with-attrs
                                     (jetpacs-design-styled
                                      '("bloom.t.intro")
                                      (list (jetpacs-text intro)))
                                     :pad '(:vertical 10 :horizontal 11)))))
                   (bloom-navigation--outline-rows
                    notebook-id records nil expanded)
                   (list (jetpacs-spacer :height 86))))
           :pad '(:horizontal 10)))
         :component-styles
         (list (jetpacs-design-component-style "card.filled" '("bloom.flat-row"))))
        :fab (jetpacs-design-styled
              '("bloom.fab")
              (list (jetpacs-box (bloom-navigation--styled-text "fab-plus" "+")
                                 :alignment "center"
                                 :on-tap (bloom-ui-capture-action))))
        :on-refresh (jetpacs-action "bloom.outline.refresh"))
       :pane-title "Outline")))))

(defun bloom-navigation--on-outline-toggle (args params)
  "Fold or unfold ARGS' heading in the current outline; refresh PARAMS."
  (let* ((id (plist-get args :heading-id))
         (notebook-id bloom-navigation--notebook-id)
         (open (gethash notebook-id bloom-navigation--outline-expanded)))
    (if (not (stringp id)) 'rejected
      (puthash notebook-id (if (member id open) (remove id open) (cons id open))
               bloom-navigation--outline-expanded)
      (jetpacs-app-defer-refresh params)
      'accepted)))

(defun bloom-navigation--on-outline-expand (_args params)
  "Unfold every heading of the current outline and refresh PARAMS' surface.
When every heading is already open, fold them all instead."
  (let* ((notebook-id bloom-navigation--notebook-id)
         (records (plist-get (bloom-library-outline-snapshot notebook-id) :records))
         (parents (delete-dups (delq nil (mapcar (lambda (r) (plist-get r :parent-id))
                                                 records))))
         (open (gethash notebook-id bloom-navigation--outline-expanded)))
    (puthash notebook-id
             (if (cl-every (lambda (id) (member id open)) parents) nil parents)
             bloom-navigation--outline-expanded)
    (jetpacs-app-defer-refresh params)
    'accepted))

(defun bloom-navigation--on-preface-toggle (_args params)
  "Expand or collapse the current outline's PREFACE band; refresh PARAMS."
  (let ((notebook-id bloom-navigation--notebook-id))
    (puthash notebook-id (not (gethash notebook-id bloom-navigation--preface-open))
             bloom-navigation--preface-open)
    (jetpacs-app-defer-refresh params)
    'accepted))

(defconst bloom-navigation--outline-menu-items
  '(("Insert above" "insert-before") ("Insert below" "insert-after")
    ("Insert child" "insert-child") ("Refile…" "refile")
    ("Move up" "move-up") ("Move down" "move-down") ("Promote" "promote")
    ("Demote" "demote") ("Duplicate" "duplicate") ("Copy" "copy") ("Cut" "cut")
    ("Paste under" "paste") ("Narrow to subtree" "narrow")
    ("Archive" "archive" "Archive this heading and its complete subtree?")
    ("Delete" "delete" "Delete this heading and its complete subtree?"))
  "Long-press outline operations: label, structure operation, confirmation.")

(defun bloom-navigation--on-outline-menu (args params)
  "Offer Bloom's structure operations for ARGS' heading on PARAMS' surface."
  (let* ((notebook-id bloom-navigation--notebook-id)
         (id (plist-get args :heading-id))
         (record (cl-find id (plist-get (bloom-library-outline-snapshot notebook-id)
                                        :records)
                          :key (lambda (r) (plist-get r :id)) :test #'equal))
         (surface (bloom-navigation--surface params)))
    (if (null record) 'stale
      (jetpacs-flow-continue
       (lambda ()
         (if (not (jetpacs-dialog-can-bridge-p))
             (jetpacs-shell-notify "Heading menu is unavailable" surface)
           (let* ((choice (condition-case nil
                              (completing-read
                               (format "%s: " (plist-get record :title))
                               (mapcar #'car bloom-navigation--outline-menu-items)
                               nil t)
                            (quit "")))
                  (item (assoc choice bloom-navigation--outline-menu-items)))
             (when (and item
                        (or (null (nth 2 item))
                            (condition-case nil (y-or-n-p (concat (nth 2 item) " "))
                              (quit nil))))
               (bloom-structure--on-mutate
                (plist-get (bloom-structure-action notebook-id record (nth 1 item))
                           :args)
                params))))))
      'accepted)))

(defun bloom-navigation-presented-outline-screen (back)
  "Build the profile-selected native outline screen with BACK.
With the design runtime and Bloom's parity faces this is the Android app's
own outline; otherwise the Material outline, scoped when possible."
  (if (and (bloom-navigation--parity-p)
           (condition-case nil
               (bloom-library-outline-snapshot bloom-navigation--notebook-id)
             (error nil)))
      (bloom-navigation--parity-outline-screen back)
    (bloom-design-present-if-advertised
     (bloom-navigation-outline-screen back) 'outline)))

(defun bloom-ui-open-outline (notebook-id &optional params)
  "Open NOTEBOOK-ID's Bloom outline on PARAMS' surface."
  (cond
   ((not (stringp notebook-id)) 'rejected)
   ((jetpacs-event-stale-p params) 'stale)
   ((not (bloom-library-resolve-notebook notebook-id)) 'stale)
   (t
    (setq bloom-navigation--notebook-id notebook-id)
    (condition-case err
        (progn
          (bloom-library-outline-refresh notebook-id)
          (jetpacs-flow-continue
           (lambda ()
             (jetpacs-chrome-push-screen
              (bloom-navigation--surface params) "outline"
              #'bloom-navigation-presented-outline-screen)))
          'accepted)
      (ebp-org-unresolved 'stale)
      (error
       (message "bloom: outline open failed (%s)" (car err))
       'rejected)))))

(defun bloom-navigation--search-result-card (record)
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
   (bloom-document-open-action
    (plist-get record :notebook-id) (plist-get record :heading-id))
   :variant "outlined"))

(defun bloom-navigation--search-body ()
  "Build Bloom's search screen from its explicit snapshot."
  (let* ((snapshot (bloom-library-search-snapshot))
         (query (plist-get snapshot :query))
         (mode (if (member (plist-get snapshot :mode) '("text" "match"))
                   (plist-get snapshot :mode)
                 "text"))
         (results (plist-get snapshot :results))
         (children
          (append
           (bloom-views-nodes)
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
                    "Bloom saved view")
                  :style "caption" :max-lines 1)
                 :spacing 2)
                :spacing 8 :align "center")
               :variant "filled"))
            (jetpacs-text-input
             "bloom-search-query" :value query
             :label (if (equal mode "match") "Org match expression"
                      "Text, Boolean, or {regexp} search")
             :leading-icon "search" :single-line t :max-length 512
             :on-submit
             (jetpacs-action "bloom.search.run"
                             :args (list :mode mode)))
            (jetpacs-row
             (jetpacs-button
              "Text / regexp"
              (jetpacs-action "bloom.search.mode"
                              :args (list :mode "text"))
              :variant (if (equal mode "text") "filled" "text"))
             (jetpacs-button
              "Org match"
              (jetpacs-action "bloom.search.mode"
                              :args (list :mode "match"))
              :variant (if (equal mode "match") "filled" "text"))
             :spacing 6)
            (jetpacs-row
             (jetpacs-button
              "Search"
              (jetpacs-action "bloom.search.run"
                              :args (list :mode mode)
                              :capture-fields '("bloom-search-query"))
              :icon "search" :variant "tonal")
             (jetpacs-button
              "Save view" (bloom-views-save-action mode)
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
                          (list (bloom-navigation--search-result-card record))))))))
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

(defun bloom-navigation-search-screen (back)
  "Build Bloom's native search destination with BACK."
  (jetpacs-chrome-screen "Search" (bloom-navigation--search-body) :back back))

(declare-function bloom-search-screen "bloom-search" (back))

(defun bloom-navigation-presented-search-screen (back)
  "Build the profile-selected Search destination with BACK.
Under the design runtime with Bloom's parity faces it is the Android app's
own Search screen, answered by native Emacs search."
  (if (and (fboundp 'bloom-search-screen)
           (jetpacs-extension-advertised-p "jetpacs.design" :app)
           (bloom-design-parity-available-p))
      (bloom-search-screen back)
    (bloom-design-present-if-advertised
     (bloom-navigation-search-screen back) 'search)))

(defun bloom-navigation-settings-screen (back)
  "Build Bloom's app-specific settings destination with BACK."
  (jetpacs-chrome-screen
   "Bloom Settings"
   (jetpacs-lazy-column
    (jetpacs-section-header "Library")
    (jetpacs-switch
     "bloom-setting-tree" :checked (jetpacs-bool bloom-library-tree-view)
     :label "Folder tree"
     :on-change (jetpacs-action "bloom.settings.set"
                                :args (list :field "tree")))
    (jetpacs-section-header "Reader")
    (jetpacs-switch
     "bloom-setting-drawers" :checked (jetpacs-bool bloom-reader-show-drawers)
     :label "Show drawers initially"
     :on-change (jetpacs-action "bloom.settings.set"
                                :args (list :field "drawers")))
    (jetpacs-row
     (jetpacs-button
      "Read first" (jetpacs-action "bloom.settings.set"
                                   :args (list :field "mode" :value "read"))
      :variant (if (eq bloom-reader-default-mode 'read) "filled" "text"))
     (jetpacs-button
      "Edit first" (jetpacs-action "bloom.settings.set"
                                   :args (list :field "mode" :value "edit"))
      :variant (if (eq bloom-reader-default-mode 'edit) "filled" "text"))
     :spacing 6)
    (jetpacs-section-header "Reminders")
    (jetpacs-switch
     "bloom-setting-reminders"
     :checked (jetpacs-bool bloom-reminders-enabled)
     :label "Timed Org reminders"
     :on-change (jetpacs-action "bloom.settings.set"
                                :args (list :field "reminders-enabled")))
    (jetpacs-text
     (if (zerop bloom-reminder-lead-minutes)
         "Alert at the scheduled time"
       (format "Alert %d minutes early" bloom-reminder-lead-minutes))
     :style "caption")
    (jetpacs-flow-row
     (jetpacs-button
      "At time" (jetpacs-action "bloom.settings.set"
                                :args (list :field "reminder-lead" :value 0))
      :variant (if (zerop bloom-reminder-lead-minutes) "filled" "outlined"))
     (jetpacs-button
      "15 min" (jetpacs-action "bloom.settings.set"
                               :args (list :field "reminder-lead" :value 15))
      :variant (if (= bloom-reminder-lead-minutes 15) "filled" "outlined"))
     (jetpacs-button
      "1 hour" (jetpacs-action "bloom.settings.set"
                               :args (list :field "reminder-lead" :value 60))
      :variant (if (= bloom-reminder-lead-minutes 60) "filled" "outlined"))
     (jetpacs-button
      "1 day" (jetpacs-action "bloom.settings.set"
                              :args (list :field "reminder-lead" :value 1440))
      :variant (if (= bloom-reminder-lead-minutes 1440) "filled" "outlined")))
    (jetpacs-section-header "Bounds")
    (jetpacs-text
     (format "Agenda: %d days · Search: %d results · Library: %d notebooks"
             bloom-org-agenda-days bloom-search-result-limit
             bloom-library-notebook-limit)
     :style "caption")
    (jetpacs-flow-row
     (jetpacs-button "Agenda 7d"
                     (jetpacs-action "bloom.settings.set"
                                     :args (list :field "agenda-days" :value 7))
                     :variant "outlined")
     (jetpacs-button "Agenda 14d"
                     (jetpacs-action "bloom.settings.set"
                                     :args (list :field "agenda-days" :value 14))
                     :variant "outlined")
     (jetpacs-button "Search 50"
                     (jetpacs-action "bloom.settings.set"
                                     :args (list :field "search-limit" :value 50))
                     :variant "outlined")
     (jetpacs-button "Search 200"
                     (jetpacs-action "bloom.settings.set"
                                     :args (list :field "search-limit" :value 200))
                     :variant "outlined"))
    (jetpacs-divider)
    (jetpacs-text
     "Org roots, synchronization, backups, connection policy, and Android permissions remain owned by Emacs and the Jetpacs Companion."
     :style "caption")
    :spacing 10 :content-padding 12)
   :back back))

(defun bloom-navigation-presented-settings-screen (back)
  "Build the profile-selected Settings destination with BACK."
  (bloom-design-present-if-advertised
   (bloom-navigation-settings-screen back) 'settings))

(defun bloom-navigation--on-library-refresh (_args params)
  "Refresh library and outline projections for event PARAMS."
  (if (bloom-library-refresh)
      (progn
        (when bloom-navigation--notebook-id
          (ignore-errors
            (bloom-library-outline-refresh bloom-navigation--notebook-id)))
        (jetpacs-app-defer-refresh params)
        'accepted)
    'rejected))

(defun bloom-navigation--on-folder-open (args params)
  "Open opaque folder ARGS on PARAMS' surface."
  (let ((folder-id (plist-get args :folder-id)))
    (if (not (and (stringp folder-id)
                  (bloom-library-resolve-folder folder-id)))
        'stale
      (jetpacs-flow-continue
       (lambda ()
         (jetpacs-chrome-push-screen
          (bloom-navigation--surface params)
          (jetpacs-wire-id "bloom-folder" folder-id)
          (apply-partially
           #'bloom-navigation--present-notebooks-screen-for-folder folder-id))))
      'accepted)))

(defun bloom-navigation--on-folder-toggle (args params)
  "Expand or collapse ARGS' folder in place and refresh PARAMS' surface."
  (let ((folder-id (plist-get args :folder-id)))
    (if (not (and (stringp folder-id)
                  (bloom-library-resolve-folder folder-id)))
        'stale
      (bloom-library-set-folder-expanded
       folder-id (not (member folder-id bloom-library-expanded-folder-ids)))
      (jetpacs-app-defer-refresh params)
      'accepted)))

(defun bloom-navigation--on-folders-expand (args params)
  "Expand every folder when ARGS' expand is t, else collapse; refresh PARAMS."
  (let ((expand (eq (plist-get args :expand) t)))
    (dolist (folder (plist-get (bloom-library-snapshot) :folders))
      (bloom-library-set-folder-expanded (plist-get folder :id) expand))
    (jetpacs-app-defer-refresh params)
    'accepted))

(defun bloom-navigation--on-notebook-menu (args params)
  "Offer the app's long-press menu for ARGS' notebook on PARAMS' surface.
The choices are the app's: pin or unpin, rename, move, and delete, which
moves the notebook to the recoverable trash after a confirmation."
  (let* ((notebook-id (plist-get args :notebook-id))
         (record (bloom-library--notebook-record notebook-id))
         (surface (bloom-navigation--surface params)))
    (if (null record) 'stale
      (jetpacs-flow-continue
       (lambda ()
         (if (not (jetpacs-dialog-can-bridge-p))
             (jetpacs-shell-notify "Notebook menu is unavailable" surface)
           (let* ((pin (if (plist-get record :favorite) "Unpin" "Pin to top"))
                  (choice (condition-case nil
                              (completing-read
                               (format "%s: " (plist-get record :file-label))
                               (list pin "Rename" "Move to folder" "Delete")
                               nil t)
                            (quit ""))))
             (pcase choice
               ((pred (equal pin))
                (bloom-navigation--on-favorite args params)
                (jetpacs-shell-push surface))
               ("Rename" (bloom-navigation--on-rename args params))
               ("Move to folder" (bloom-navigation--on-move args params))
               ("Delete"
                (when (condition-case nil
                          (y-or-n-p (format "Delete %s? "
                                            (plist-get record :file-label)))
                        (quit nil))
                  (bloom-navigation--on-trash args params)
                  (jetpacs-shell-push surface))))))))
      'accepted)))

(defun bloom-navigation--on-view-toggle (_args params)
  "Toggle the library view and refresh PARAMS' surface."
  (bloom-navigation--save-setting
   'bloom-library-tree-view (not bloom-library-tree-view))
  (jetpacs-app-defer-refresh params)
  'accepted)

(defun bloom-navigation--prompt-name (prompt initial)
  "Read one trimmed notebook name using PROMPT and INITIAL."
  (string-trim
   (condition-case nil (read-string prompt initial) (quit ""))))

(defun bloom-navigation--on-create (args params)
  "Prompt for a notebook inside ARGS' folder for event PARAMS."
  (let* ((wire-folder-id (plist-get args :folder-id))
         (folder-id (bloom-navigation--folder-from-wire wire-folder-id))
         (surface (bloom-navigation--surface params)))
    (cond
     ((not (stringp wire-folder-id)) 'rejected)
     ((and folder-id (not (bloom-library-resolve-folder folder-id))) 'stale)
     (t
      (jetpacs-flow-continue
       (lambda ()
         (if (not (jetpacs-dialog-can-bridge-p))
             (jetpacs-shell-notify "Notebook dialog is unavailable" surface)
           (let ((name (bloom-navigation--prompt-name "New notebook: " "")))
             (unless (string-empty-p name)
               (condition-case err
                   (progn
                     (bloom-library-create-notebook folder-id name)
                     (jetpacs-shell-notify "Notebook created" surface)
                     (jetpacs-shell-push surface))
                 (error
                  (message "bloom: create failed (%s)" (car err))
                  (jetpacs-shell-notify
                   "Notebook could not be created" surface))))))))
      'accepted))))

(defun bloom-navigation--on-rename (args params)
  "Prompt for and rename ARGS' notebook for event PARAMS."
  (let* ((notebook-id (plist-get args :notebook-id))
         (record (bloom-library--notebook-record notebook-id))
         (surface (bloom-navigation--surface params)))
    (if (null record) 'stale
      (jetpacs-flow-continue
       (lambda ()
         (if (not (jetpacs-dialog-can-bridge-p))
             (jetpacs-shell-notify "Rename dialog is unavailable" surface)
           (let ((name (bloom-navigation--prompt-name
                        "Rename notebook: " (plist-get record :file-label))))
             (unless (string-empty-p name)
               (condition-case err
                   (progn
                     (bloom-library-rename-notebook notebook-id name)
                     (jetpacs-shell-notify "Notebook renamed" surface)
                     (jetpacs-shell-push surface))
                 (error
                  (message "bloom: rename failed (%s)" (car err))
                  (jetpacs-shell-notify "Notebook could not be renamed"
                                        surface))))))))
      'accepted)))

(defun bloom-navigation--folder-choices ()
  "Return unique display-name to opaque-folder-id choices."
  (cons (cons "Notebooks" "root")
        (mapcar (lambda (record)
                  (cons (plist-get record :path-label)
                        (plist-get record :id)))
                (plist-get (bloom-library-snapshot) :folders))))

(defun bloom-navigation--on-move (args params)
  "Prompt for and move ARGS' notebook for event PARAMS."
  (let ((notebook-id (plist-get args :notebook-id))
        (surface (bloom-navigation--surface params)))
    (if (null (bloom-library--notebook-record notebook-id)) 'stale
      (jetpacs-flow-continue
       (lambda ()
         (if (not (jetpacs-dialog-can-bridge-p))
             (jetpacs-shell-notify "Move dialog is unavailable" surface)
           (let* ((choices (bloom-navigation--folder-choices))
                  (selected
                   (condition-case nil
                       (completing-read "Move notebook to: " choices nil t)
                     (quit "")))
                  (folder-id (cdr (assoc selected choices))))
             (when folder-id
               (condition-case err
                   (progn
                     (bloom-library-move-notebook
                      notebook-id
                      (bloom-navigation--folder-from-wire folder-id))
                     (jetpacs-shell-notify "Notebook moved" surface)
                     (jetpacs-shell-push surface))
                 (error
                  (message "bloom: move failed (%s)" (car err))
                  (jetpacs-shell-notify "Notebook could not be moved"
                                        surface))))))))
      'accepted)))

(defun bloom-navigation--on-trash (args params)
  "Move ARGS' notebook to trash and update PARAMS' surface."
  (condition-case err
      (progn
        (bloom-library-trash-notebook (plist-get args :notebook-id))
        (jetpacs-shell-notify "Notebook moved to trash"
                              (bloom-navigation--surface params))
        (jetpacs-app-defer-refresh params)
        'accepted)
    (ebp-org-unresolved 'stale)
    (error
     (message "bloom: trash failed (%s)" (car err))
     (jetpacs-shell-notify "Recoverable trash is unavailable"
                           (bloom-navigation--surface params))
     'rejected)))

(defun bloom-navigation--on-favorite (args params)
  "Toggle ARGS' notebook favorite and refresh PARAMS' surface."
  (condition-case nil
      (progn
        (bloom-library-toggle-favorite (plist-get args :notebook-id))
        (jetpacs-app-defer-refresh params)
        'accepted)
    (ebp-org-unresolved 'stale)
    (error 'rejected)))

(defun bloom-navigation--on-outline-refresh (_args params)
  "Refresh the current outline for event PARAMS."
  (if (null bloom-navigation--notebook-id) 'stale
    (condition-case nil
        (progn
          (bloom-library-outline-refresh bloom-navigation--notebook-id)
          (jetpacs-app-defer-refresh params)
          'accepted)
      (ebp-org-unresolved 'stale)
      (error 'rejected))))

(defun bloom-navigation--on-search-run (args params)
  "Run native Org search using ARGS and captured PARAMS."
  (let* ((captured (plist-get (plist-get params :fields)
                              :bloom-search-query))
         (query (or (plist-get args :value) captured ""))
         (mode (or (plist-get args :mode)
                   (plist-get (bloom-library-search-snapshot) :mode)
                   "text")))
    (if (not (and (stringp query) (<= (length query) 512)
                  (member mode '("text" "match" "ql"))))
        'rejected
      (bloom-library-search query mode)
      (jetpacs-app-defer-refresh params)
      'accepted)))

(defun bloom-navigation--on-search-mode (args params)
  "Switch to ARGS' search mode and refresh PARAMS' surface."
  (let ((mode (plist-get args :mode)))
    (if (not (member mode '("text" "match" "ql"))) 'rejected
      (bloom-library-search-set-mode mode)
      (jetpacs-app-defer-refresh params)
      'accepted)))

(defun bloom-navigation--save-setting (symbol value)
  "Set Customize SYMBOL to VALUE and persist it."
  (let ((old (symbol-value symbol)))
    (condition-case err
        (unless (jetpacs-settings-apply symbol value)
          (user-error "Bloom setting was not accepted"))
      (error
       (set symbol old)
       (signal (car err) (cdr err))))))

(defun bloom-navigation--on-setting (args params)
  "Apply whitelisted setting ARGS and refresh PARAMS' surface."
  (let ((field (plist-get args :field))
        (value (plist-get args :value)))
    ;; Stateful controls inject their current value into ARGS.
    (if
        (pcase field
          ((or "tree" "drawers")
           (when (memq value '(t :json-false))
             (bloom-navigation--save-setting
              (if (equal field "tree")
                  'bloom-library-tree-view
                'bloom-reader-show-drawers)
              (eq value t))
             t))
          ("mode"
           (when (member value '("read" "edit"))
             (bloom-navigation--save-setting
              'bloom-reader-default-mode (intern value))
             t))
          ("agenda-days"
           (when (and (integerp value) (<= 1 value 31))
             (bloom-navigation--save-setting 'bloom-org-agenda-days value)
             (bloom-org-refresh)
             t))
          ("search-limit"
           (when (and (integerp value) (<= 1 value 512))
             (bloom-navigation--save-setting
              'bloom-search-result-limit value)
             t))
          ("reminders-enabled"
           (when (memq value '(t :json-false))
             (bloom-navigation--save-setting
              'bloom-reminders-enabled (eq value t))
             (bloom-reminders-schedule-sync)
             t))
          ("reminder-lead"
           (when (memq value '(0 5 10 15 30 60 1440))
             (bloom-navigation--save-setting
              'bloom-reminder-lead-minutes value)
             (bloom-reminders-schedule-sync)
             t)))
        (progn
          (jetpacs-app-defer-refresh params)
          'accepted)
      'rejected)))

(defconst bloom-navigation--verbs
  '("bloom.notebooks.open" "bloom.agenda.open" "bloom.search.open"
    "bloom.settings.open" "bloom.library.refresh"
    "bloom.library.folder.open" "bloom.library.view.toggle"
    "bloom.library.create" "bloom.library.rename" "bloom.library.move"
    "bloom.library.trash" "bloom.library.favorite" "bloom.notebook.open"
    "bloom.outline.refresh" "bloom.search.run" "bloom.search.mode"
    "bloom.settings.set" "bloom.library.folder.toggle"
    "bloom.library.folders.expand" "bloom.library.notebook.menu"
    "bloom.outline.toggle" "bloom.outline.expand" "bloom.outline.preface"
    "bloom.outline.menu")
  "Action names owned by Bloom navigation.")

(defun bloom-navigation-register ()
  "Register Bloom's destinations and library actions idempotently."
  (with-jetpacs-owner bloom-owner
                      (jetpacs-defaction
                       "bloom.notebooks.open"
                       (lambda (_args params)
                         (bloom-navigation-open-destination
                          "notebooks" "notebooks"
                          #'bloom-navigation-presented-notebooks-screen params)))
                      (jetpacs-defaction
                       "bloom.agenda.open"
                       (lambda (_args params)
                         (bloom-navigation-open-destination
                          "agenda" "agenda" #'bloom-ui-presented-screen params)))
                      (jetpacs-defaction
                       "bloom.search.open"
                       (lambda (_args params)
                         (bloom-navigation-open-destination
                          "search" "search"
                          #'bloom-navigation-presented-search-screen params)))
                      (jetpacs-defaction
                       "bloom.settings.open"
                       (lambda (_args params)
                         (bloom-navigation-open-destination
                          "settings" "settings"
                          #'bloom-navigation-presented-settings-screen params)))
                      (jetpacs-defaction "bloom.library.refresh"
                                         #'bloom-navigation--on-library-refresh)
                      (jetpacs-defaction "bloom.library.folder.open"
                                         #'bloom-navigation--on-folder-open)
                      (jetpacs-defaction "bloom.library.view.toggle"
                                         #'bloom-navigation--on-view-toggle)
                      (jetpacs-defaction
                       "bloom.library.folder.toggle"
                       #'bloom-navigation--on-folder-toggle
                       :args '((:name folder-id :type "ref" :required t))
                       :doc "Expand or collapse one Notebooks folder in place")
                      (jetpacs-defaction
                       "bloom.library.folders.expand"
                       #'bloom-navigation--on-folders-expand
                       :args '((:name expand :type "boolean" :required t))
                       :doc "Expand or collapse every Notebooks folder")
                      (jetpacs-defaction
                       "bloom.outline.toggle" #'bloom-navigation--on-outline-toggle
                       :args '((:name heading-id :type "ref" :required t))
                       :doc "Fold or unfold one heading of the open outline")
                      (jetpacs-defaction
                       "bloom.outline.expand" #'bloom-navigation--on-outline-expand
                       :doc "Unfold every heading of the open outline, or fold all")
                      (jetpacs-defaction
                       "bloom.outline.preface" #'bloom-navigation--on-preface-toggle
                       :doc "Expand or collapse the open outline's PREFACE band")
                      (jetpacs-defaction
                       "bloom.outline.menu" #'bloom-navigation--on-outline-menu
                       :args '((:name heading-id :type "ref" :required t))
                       :doc "Offer Bloom's structure operations for one heading")
                      (jetpacs-defaction
                       "bloom.library.notebook.menu"
                       #'bloom-navigation--on-notebook-menu
                       :args '((:name notebook-id :type "ref" :required t))
                       :doc "Offer pin, rename, move, and delete for a notebook")
                      (jetpacs-defaction "bloom.library.create"
                                         #'bloom-navigation--on-create)
                      (jetpacs-defaction "bloom.library.rename"
                                         #'bloom-navigation--on-rename)
                      (jetpacs-defaction "bloom.library.move"
                                         #'bloom-navigation--on-move)
                      (jetpacs-defaction "bloom.library.trash"
                                         #'bloom-navigation--on-trash)
                      (jetpacs-defaction "bloom.library.favorite"
                                         #'bloom-navigation--on-favorite)
                      (jetpacs-defaction
                       "bloom.notebook.open"
                       (lambda (args params)
                         (bloom-ui-open-outline (plist-get args :notebook-id) params)))
                      (jetpacs-defaction "bloom.outline.refresh"
                                         #'bloom-navigation--on-outline-refresh)
                      (jetpacs-defaction "bloom.search.run"
                                         #'bloom-navigation--on-search-run)
                      (jetpacs-defaction "bloom.search.mode"
                                         #'bloom-navigation--on-search-mode)
                      (jetpacs-defaction "bloom.settings.set"
                                         #'bloom-navigation--on-setting))
  t)

(defun bloom-navigation-unregister ()
  "Unregister Bloom navigation and clear its transient selections."
  (dolist (verb bloom-navigation--verbs) (jetpacs-undefaction verb))
  (setq bloom-navigation--notebook-id nil)
  t)

(provide 'bloom-navigation)
;;; bloom-navigation.el ends here
