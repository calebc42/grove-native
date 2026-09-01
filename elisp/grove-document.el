;;; grove-document.el --- Grove-native Org reader and editor -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Native Grove document presentation over the reusable Jetpacs Org renderer,
;; editor, toolbar, and dialogs.  Every mutation resolves an opaque Grove id
;; back to an Org ref and rechecks the current subtree before writing.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'ebp-org)
(require 'jetpacs-apps)
(require 'jetpacs-chrome)
(require 'jetpacs-editor-org)
(require 'jetpacs-org-render)
(require 'jetpacs-org-toolbar)
(require 'jetpacs-shell)
(require 'jetpacs-widgets)
(require 'grove-core)
(require 'grove-library)
(require 'grove-org)

(defconst grove-document--screen-id "document"
  "Stable chrome screen id for Grove's document presenter.")

(defconst grove-document--planning-screen-id "planning"
  "Stable chrome screen id for Grove's planning canvas.")

(defconst grove-document--save-ttl-seconds 86400
  "Lifetime of a queued Grove subtree save.")

(defvar grove-document--current nil
  "Current document identity as (:notebook-id ID :heading-id ID).")

(defvar grove-document--modes (make-hash-table :test #'equal)
  "Document composite key to `read' or `edit'.")

(defvar grove-document--planning nil
  "Current planning draft and source revision.")

(defvar grove-navigation--notebook-id nil
  "Notebook selected by Grove navigation, declared here for link handoff.")

(declare-function grove-ui-open-outline "grove-ui"
                  (notebook-id &optional params))

(defun grove-document--key (notebook-id heading-id)
  "Return NOTEBOOK-ID and HEADING-ID's local document-state key."
  (concat notebook-id "|" heading-id))

(defun grove-document--mode (notebook-id heading-id)
  "Return NOTEBOOK-ID/HEADING-ID's current presentation mode."
  (or (gethash (grove-document--key notebook-id heading-id)
               grove-document--modes)
      grove-reader-default-mode))

(defun grove-document-open-action (notebook-id heading-id)
  "Return a durable action opening NOTEBOOK-ID and HEADING-ID in Grove."
  (jetpacs-action
   "grove.document.open"
   :args (list :notebook-id notebook-id :heading-id heading-id)
   :open-surface grove-app-surface
   :when-offline 'wake :ttl-s 86400))

(defun grove-document--set-current (notebook-id heading-id)
  "Select NOTEBOOK-ID and HEADING-ID and produce its current snapshot."
  (grove-library-document-refresh notebook-id heading-id)
  (setq grove-document--current
        (list :notebook-id notebook-id :heading-id heading-id)))

(defun grove-document-open (notebook-id heading-id &optional params)
  "Open NOTEBOOK-ID and HEADING-ID on PARAMS' Grove surface.
Return a handler status."
  (cond
   ((not (and (stringp notebook-id) (stringp heading-id))) 'rejected)
   ((jetpacs-event-stale-p params) 'stale)
   (t
    (condition-case err
        (progn
          (grove-document--set-current notebook-id heading-id)
          (jetpacs-flow-continue
           (lambda ()
             (condition-case push-error
                 (jetpacs-chrome-push-screen
                  (or (plist-get params :surface) grove-app-surface)
                  grove-document--screen-id #'grove-document-screen)
               (error
                (message "grove: document push failed (%s)"
                         (car push-error))))))
          'accepted)
      (ebp-org-unresolved 'stale)
      (ebp-org-refused 'rejected)
      (error
       (message "grove: document open failed (%s)" (car err))
       'rejected)))))

(defun grove-document--current-info ()
  "Return the current private document snapshot, or signal stale."
  (let ((notebook-id (plist-get grove-document--current :notebook-id))
        (heading-id (plist-get grove-document--current :heading-id)))
    (unless (and notebook-id heading-id)
      (signal 'ebp-org-unresolved nil))
    (grove-library-document-snapshot notebook-id heading-id)))

(defun grove-document--live-subtree (ref)
  "Return REF's live subtree string and location metadata."
  (let ((marker (ebp-org-resolve-ref ref)))
    (unwind-protect
        (with-current-buffer (marker-buffer marker)
          (org-with-wide-buffer
           (goto-char marker)
           (org-back-to-heading t)
           (let ((beg (point))
                 (end (save-excursion (org-end-of-subtree t t) (point))))
             (list :buffer (current-buffer) :beg beg :end end
                   :content (buffer-substring-no-properties beg end)))))
      (set-marker marker nil))))

(defun grove-document--metadata-nodes (info)
  "Return Grove metadata nodes for private document INFO."
  (let ((tags (plist-get info :tags))
        (properties (plist-get info :properties))
        (identity (list :notebook-id (plist-get info :notebook-id)
                        :heading-id (plist-get info :heading-id))))
    (delq
     nil
     (list
      (when (or (plist-get info :todo) (plist-get info :priority))
        (apply #'jetpacs-flow-row
               (delq nil
                     (list
                      (when-let* ((todo (plist-get info :todo)))
                        (jetpacs-chip todo :selected t :icon "task_alt"))
                      (when-let* ((priority (plist-get info :priority)))
                        (jetpacs-badge (format "Priority %s" priority)
                                       :icon "priority_high"))))))
      (when tags
        (apply #'jetpacs-flow-row
               (mapcar (lambda (tag)
                         (jetpacs-chip tag :icon "label"))
                       tags)))
      (when (or (plist-get info :scheduled) (plist-get info :deadline))
        (jetpacs-flow-row
         (when-let* ((scheduled (plist-get info :scheduled)))
           (jetpacs-chip scheduled :icon "schedule"
                         :on-tap (jetpacs-action
                                  "grove.document.planning"
                                  :args identity)))
         (when-let* ((deadline (plist-get info :deadline)))
           (jetpacs-chip deadline :icon "event_busy"
                         :on-tap (jetpacs-action
                                  "grove.document.planning"
                                  :args identity)))))
      (when properties
        (jetpacs-collapsible
         "grove-document-properties"
         (jetpacs-text (format "Properties (%d)" (length properties))
                       :style "label")
         (mapcar
          (lambda (pair)
            (jetpacs-row
             (jetpacs-with-attrs (jetpacs-text (car pair) :style "caption")
                                 :weight 1)
             (jetpacs-text (or (cdr pair) "") :max-lines 2)))
          properties)
         :collapsed t))))))

(defun grove-document--heading-card (info)
  "Return INFO's Grove heading card with safe reveal-first swipe."
  (let* ((notebook-id (plist-get info :notebook-id))
         (heading-id (plist-get info :heading-id))
         (todo (plist-get info :todo))
         (done (and todo
                    (jetpacs-action
                     "grove.document.todo"
                     :args (list :notebook-id notebook-id
                                 :heading-id heading-id :state "done")
                     :when-offline 'wake :ttl-s 86400
                     :dedupe (jetpacs-wire-id "grove-doc-done" heading-id))))
         (swipe (and done
                     (jetpacs-swipe
                      (list
                       (jetpacs-swipe-action
                        "Done" :icon "done" :color "primary"
                        :on-trigger done)
                       (jetpacs-swipe-action
                        "Plan" :icon "event" :color "secondary"
                        :on-trigger (jetpacs-action
                                     "grove.document.planning"
                                     :args (list :notebook-id notebook-id
                                                 :heading-id heading-id))))
                      :commit t))))
    (apply #'jetpacs-card
           (append
            (list
             (jetpacs-column
              (jetpacs-text (plist-get info :headline) :style "title")
              (when-let* ((ancestors (plist-get info :ancestors)))
                (jetpacs-text (string-join ancestors " › ")
                              :style "caption" :max-lines 1))
              :spacing 4))
            (when swipe (list :swipe-end swipe))
            (list :variant "outlined")))))

(defun grove-document--linked-presenter
    (_source destination position surface)
  "Present Org link DESTINATION at POSITION through Grove on SURFACE."
  (let ((path (and (buffer-live-p destination)
                   (buffer-local-value 'buffer-file-name destination))))
    (cond
     ((null path) nil)
     (t
      (unless (grove-library--notebook-record
               (grove-library--notebook-id path))
        (grove-library-refresh))
      (let* ((notebook-id (grove-library--notebook-id path))
             (notebook (grove-library--notebook-record notebook-id)))
        (if (null notebook)
            (progn
              (jetpacs-shell-notify
               "Linked Org file is outside Grove's notebook scope" surface)
              t)
          (with-current-buffer destination
            (org-with-wide-buffer
             (goto-char (min (max (point-min) position) (point-max)))
             (if (not (org-before-first-heading-p))
                 (progn
                   (org-back-to-heading t)
                   (let* ((ref (ebp-org-ref-at-point))
                          (heading-id (grove-library--heading-id ref)))
                     (puthash
                      (grove-library--heading-key notebook-id heading-id)
                      ref grove-library--heading-refs)
                     (setq grove-navigation--notebook-id notebook-id)
                     (grove-document--set-current notebook-id heading-id)
                     (jetpacs-chrome-push-screen
                      surface grove-document--screen-id
                      #'grove-document-screen)))
               (grove-ui-open-outline notebook-id)))
            t)))))))

(defun grove-document--reader-body (info)
  "Return a Grove-native reader body for private document INFO."
  (let* ((buffer (get-buffer (plist-get info :buffer)))
         (beg (plist-get info :beg))
         (end (plist-get info :end))
         rendered)
    (unless (buffer-live-p buffer)
      (signal 'ebp-org-unresolved nil))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (unless (<= (point-min) beg end (point-max))
          (signal 'ebp-org-unresolved nil))
        (narrow-to-region beg end)
        (goto-char beg)
        (unless grove-reader-show-drawers
          (ignore-errors (org-fold-hide-drawer-all)))
        (setq rendered
              (jetpacs-org-render-in-context
               buffer :surface grove-app-surface
               :follow-destination #'grove-document--linked-presenter
               :reader-typography t :hide-widen t))))
    (apply #'jetpacs-lazy-column
           (append (list (grove-document--heading-card info))
                   (grove-document--metadata-nodes info)
                   (list (jetpacs-divider))
                   rendered
                   (list :spacing 8 :content-padding 12)))))

(defun grove-document--editor-body (info)
  "Return the narrowed subtree editor for private document INFO."
  (jetpacs-column
   (grove-document--heading-card info)
   (jetpacs-editor
    (jetpacs-wire-id "grove-editor" (plist-get info :heading-id))
    :value (plist-get info :content)
    :syntax "org"
    :toolbar (jetpacs-org-toolbar)
    :line-numbers :json-false
    :min-lines 12
    :on-save
    (jetpacs-action
     "grove.document.save"
     :args (list :notebook-id (plist-get info :notebook-id)
                 :heading-id (plist-get info :heading-id)
                 :revision (plist-get info :revision))
     :when-offline 'queue :ttl-s grove-document--save-ttl-seconds
     :dedupe (jetpacs-wire-id "grove-save" (plist-get info :heading-id))))
   :spacing 8))

(defun grove-document--top-actions (info)
  "Return Grove document top-bar actions for INFO."
  (let ((mode (grove-document--mode (plist-get info :notebook-id)
                                    (plist-get info :heading-id))))
    (list
     (jetpacs-icon-button
      (if (eq mode 'read) "edit" "visibility")
      (jetpacs-action
       "grove.document.mode"
       :args (list :notebook-id (plist-get info :notebook-id)
                   :heading-id (plist-get info :heading-id)))
      :content-description (if (eq mode 'read) "Edit" "Read"))
     (jetpacs-icon-button
      "event"
      (jetpacs-action
       "grove.document.planning"
       :args (list :notebook-id (plist-get info :notebook-id)
                   :heading-id (plist-get info :heading-id)))
      :content-description "Scheduling and deadline"))))

(defun grove-document-screen (back)
  "Build Grove's current native document screen with BACK."
  (condition-case err
      (let* ((info (grove-document--current-info))
             (mode (grove-document--mode (plist-get info :notebook-id)
                                         (plist-get info :heading-id))))
        (jetpacs-chrome-screen
         (plist-get info :headline)
         (if (eq mode 'read)
             (grove-document--reader-body info)
           (grove-document--editor-body info))
         :back back
         :actions (grove-document--top-actions info)))
    ((ebp-org-unresolved ebp-org-refused)
     (jetpacs-chrome-screen
      "Document"
      (jetpacs-empty-state
       :icon "search_off" :title "Heading moved or was removed"
       :caption "Go back and reopen it from a fresh outline.")
      :back back))
    (error
     (message "grove: document build failed (%s)" (car err))
     (jetpacs-chrome-screen
      "Document"
      (jetpacs-empty-state :icon "error" :title "Document unavailable"
                           :caption "Refresh the notebook and try again.")
      :back back))))

(defun grove-document--on-open (args params)
  "Open a document named by opaque ARGS on PARAMS' surface."
  (grove-document-open (plist-get args :notebook-id)
                       (plist-get args :heading-id) params))

(defun grove-document--on-mode (args params)
  "Toggle ARGS' current document mode and refresh PARAMS' surface."
  (let ((notebook-id (plist-get args :notebook-id))
        (heading-id (plist-get args :heading-id)))
    (if (or (jetpacs-event-stale-p params)
            (not (equal grove-document--current
                        (list :notebook-id notebook-id
                              :heading-id heading-id))))
        'stale
      (let ((key (grove-document--key notebook-id heading-id)))
        (puthash key
                 (if (eq (grove-document--mode notebook-id heading-id) 'read)
                     'edit 'read)
                 grove-document--modes)
        (jetpacs-app-defer-refresh params)
        'accepted))))

(defun grove-document--after-mutation (notebook-id heading-id)
  "Refresh Grove projections after mutating NOTEBOOK-ID and HEADING-ID."
  (grove-library-reset-projections)
  (grove-library-refresh)
  (grove-org-refresh)
  (ignore-errors (grove-library-outline-refresh notebook-id))
  (ignore-errors (grove-library-document-refresh notebook-id heading-id)))

(defun grove-document--replace-subtree (buffer beg end value)
  "Replace BUFFER's BEG..END subtree with VALUE and save durably.
Return the new private Org ref.  Any failure before durability restores the
entire buffer, point, restriction, and modified flag exactly."
  (with-current-buffer buffer
    (let* ((original-beg (point-min))
           (original-end (point-max))
           (original-point (point))
           (original-modified (buffer-modified-p))
           (original-full
            (save-restriction
              (widen)
              (buffer-substring-no-properties (point-min) (point-max))))
           (changed nil)
           (durable nil)
           new-ref)
      (save-restriction
        (unwind-protect
            (progn
              (widen)
              (unless (<= (point-min) beg end (point-max))
                (signal 'ebp-org-unresolved nil))
              (let ((inhibit-read-only t))
                (delete-region beg end)
                (goto-char beg)
                (insert value))
              (setq changed t)
              (goto-char beg)
              (org-back-to-heading t)
              (setq new-ref (ebp-org-ref-at-point))
              (jetpacs-editor-org-save-policy buffer)
              (setq durable t)
              new-ref)
          (when (and changed (not durable))
            (let ((inhibit-read-only t)
                  (buffer-undo-list t))
              (widen)
              (delete-region (point-min) (point-max))
              (insert original-full)
              (narrow-to-region original-beg original-end)
              (goto-char (min (max original-point (point-min))
                              (point-max)))
              (set-buffer-modified-p original-modified))))))))

(defun grove-document--on-save (args params)
  "Revision-check ARGS and replace one subtree for event PARAMS."
  (let ((notebook-id (plist-get args :notebook-id))
        (heading-id (plist-get args :heading-id))
        (revision (plist-get args :revision))
        (value (plist-get args :value))
        (surface (plist-get params :surface)))
    (cond
     ((not (and (stringp notebook-id) (stringp heading-id)
                (stringp revision) (stringp value)))
      'rejected)
     ((> (string-bytes value) jetpacs-files-max-bytes)
      (jetpacs-shell-notify "Section is too large to save" surface)
      'rejected)
     ((not (string-match-p "\\`\\*+ " value)) 'rejected)
     (t
      (condition-case err
          (let* ((ref (or (grove-library-resolve-heading
                           notebook-id heading-id)
                          (signal 'ebp-org-unresolved nil)))
                 (live (grove-document--live-subtree ref))
                 (buffer (plist-get live :buffer))
                 (beg (plist-get live :beg))
                 (end (plist-get live :end))
                 (original (plist-get live :content))
                 (submitted (if (string-suffix-p "\n" value)
                                value (concat value "\n")))
                 new-heading-id)
            (if (not (equal (secure-hash 'sha256 original) revision))
                'stale
              (let ((new-ref (grove-document--replace-subtree
                              buffer beg end submitted)))
                (setq new-heading-id (grove-library--heading-id new-ref))
                (puthash (grove-library--heading-key
                          notebook-id new-heading-id)
                         new-ref grove-library--heading-refs))
              (setq grove-document--current
                    (list :notebook-id notebook-id
                          :heading-id new-heading-id))
              (puthash (grove-document--key notebook-id new-heading-id)
                       'read grove-document--modes)
              (grove-document--after-mutation notebook-id new-heading-id)
              (jetpacs-shell-notify "Saved" surface)
              (jetpacs-app-defer-refresh params)
              'accepted))
        (ebp-org-unresolved 'stale)
        (ebp-org-refused 'rejected)
        (error
         (message "grove: document save failed (%s)" (car err))
         (jetpacs-shell-notify "Save failed" surface)
         'rejected))))))

(defun grove-document--on-todo (args params)
  "Set opaque ARGS' TODO state and refresh PARAMS' surface."
  (let ((notebook-id (plist-get args :notebook-id))
        (heading-id (plist-get args :heading-id))
        (state (plist-get args :state)))
    (if (not (and (stringp notebook-id) (stringp heading-id)
                  (equal state "done")))
        'rejected
      (condition-case err
          (let ((ref (or (grove-library-resolve-heading notebook-id heading-id)
                         (signal 'ebp-org-unresolved nil))))
            (ebp-org-toggle-todo ref 'grove 'done)
            (grove-document--after-mutation notebook-id heading-id)
            (jetpacs-app-defer-refresh params)
            'accepted)
        (ebp-org-unresolved 'stale)
        (ebp-org-refused 'rejected)
        (error
         (message "grove: TODO mutation failed (%s)" (car err))
         'rejected)))))

(defun grove-document--planning-field (params key fallback)
  "Return captured planning field KEY from PARAMS, or FALLBACK."
  (let ((value (plist-get (plist-get params :fields) key)))
    (if (stringp value) value fallback)))

(defun grove-document--open-planning (args params)
  "Initialize ARGS' planning canvas on PARAMS' surface."
  (let ((notebook-id (plist-get args :notebook-id))
        (heading-id (plist-get args :heading-id)))
    (cond
     ((not (and (stringp notebook-id) (stringp heading-id))) 'rejected)
     ((jetpacs-event-stale-p params) 'stale)
     (t
      (condition-case err
          (let ((info (progn
                        (grove-document--set-current notebook-id heading-id)
                        (grove-document--current-info))))
            (setq grove-document--planning
                  (list :notebook-id (plist-get info :notebook-id)
                        :heading-id (plist-get info :heading-id)
                        :revision (plist-get info :revision)
                        :scheduled (or (plist-get info :scheduled) "")
                        :deadline (or (plist-get info :deadline) "")))
            (jetpacs-flow-continue
             (lambda ()
               (jetpacs-chrome-push-screen
                (or (plist-get params :surface) grove-app-surface)
                grove-document--planning-screen-id
                #'grove-document-planning-screen)))
            'accepted)
        (ebp-org-unresolved 'stale)
        (error
         (message "grove: planning open failed (%s)" (car err))
         'rejected))))))

(defun grove-document-open-planning (notebook-id heading-id &optional params)
  "Open the planning canvas for opaque NOTEBOOK-ID and HEADING-ID.
PARAMS is the originating action context when one exists.  This public seam
lets context-less platform gestures reuse Grove's normal planning workflow
without duplicating its Org resolution or screen state."
  (grove-document--open-planning
   (list :notebook-id notebook-id :heading-id heading-id)
   params))

(defun grove-document--stamp-replace-date (stamp date)
  "Return STAMP with its date replaced by DATE."
  (if (and (stringp stamp)
           (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" stamp))
      (replace-match date t t stamp)
    date))

(defun grove-document--stamp-replace-time (stamp time)
  "Return STAMP with its time replaced by TIME."
  (let ((date (ebp-org-ts-date stamp))
        (repeater (ebp-org-ts-repeater stamp)))
    (string-join (delq nil (list date time repeater)) " ")))

(defun grove-document--on-planning-pick (args params)
  "Merge picker ARGS with the pair draft captured in PARAMS."
  (let* ((field (plist-get args :field))
         (kind (plist-get args :kind))
         (value (plist-get args :value))
         (scheduled (grove-document--planning-field
                     params :grove-scheduled
                     (plist-get grove-document--planning :scheduled)))
         (deadline (grove-document--planning-field
                    params :grove-deadline
                    (plist-get grove-document--planning :deadline))))
    (cond
     ((null grove-document--planning) 'stale)
     ((not (and (member field '("scheduled" "deadline"))
                (member kind '("date" "time")) (stringp value)))
      'rejected)
     ((jetpacs-event-stale-p params) 'stale)
     (t
      (let* ((old (if (equal field "scheduled") scheduled deadline))
             (new (if (equal kind "date")
                      (grove-document--stamp-replace-date old value)
                    (grove-document--stamp-replace-time old value))))
        (plist-put grove-document--planning :scheduled
                   (if (equal field "scheduled") new scheduled))
        (plist-put grove-document--planning :deadline
                   (if (equal field "deadline") new deadline))
        (jetpacs-app-defer-refresh params)
        'accepted)))))

(defun grove-document--on-planning-clear (args params)
  "Clear ARGS' field while retaining the other value from PARAMS."
  (let ((field (plist-get args :field)))
    (cond
     ((null grove-document--planning) 'stale)
     ((not (member field '("scheduled" "deadline"))) 'rejected)
     ((jetpacs-event-stale-p params) 'stale)
     (t
      (plist-put grove-document--planning :scheduled
                 (if (equal field "scheduled") ""
                   (grove-document--planning-field
                    params :grove-scheduled
                    (plist-get grove-document--planning :scheduled))))
      (plist-put grove-document--planning :deadline
                 (if (equal field "deadline") ""
                   (grove-document--planning-field
                    params :grove-deadline
                    (plist-get grove-document--planning :deadline))))
      (jetpacs-app-defer-refresh params)
      'accepted))))

(defun grove-document--on-planning-save (args params)
  "Atomically commit ARGS' planning fields captured in PARAMS."
  (let* ((notebook-id (plist-get args :notebook-id))
         (heading-id (plist-get args :heading-id))
         (revision (plist-get args :revision))
         (scheduled (grove-document--planning-field
                     params :grove-scheduled
                     (plist-get grove-document--planning :scheduled)))
         (deadline (grove-document--planning-field
                    params :grove-deadline
                    (plist-get grove-document--planning :deadline))))
    (if (not (and (stringp notebook-id) (stringp heading-id)
                  (stringp revision) (stringp scheduled)
                  (stringp deadline)))
        'rejected
      (condition-case err
          (let* ((ref (or (grove-library-resolve-heading notebook-id heading-id)
                          (signal 'ebp-org-unresolved nil)))
                 (live (grove-document--live-subtree ref)))
            (if (not (equal (secure-hash 'sha256 (plist-get live :content))
                            revision))
                'stale
              (ebp-org-set-planning-pair ref 'grove scheduled deadline)
              (grove-document--after-mutation notebook-id heading-id)
              (setq grove-document--planning nil)
              (jetpacs-flow-continue
               (lambda ()
                 (jetpacs-chrome-pop-screen
                  (or (plist-get params :surface) grove-app-surface))))
              'accepted))
        (ebp-org-unresolved 'stale)
        (ebp-org-refused 'rejected)
        (error
         (message "grove: planning save failed (%s)" (car err))
         (jetpacs-shell-notify "Planning values were not accepted"
                               (plist-get params :surface))
         'rejected)))))

(defun grove-document--planning-editor (field label value)
  "Return planning editor FIELD with LABEL and current VALUE."
  (let ((date (ebp-org-ts-date value))
        (time (ebp-org-ts-time value))
        (capture '("grove-scheduled" "grove-deadline")))
    (jetpacs-card
     (jetpacs-column
      (jetpacs-text label :style "title")
      (jetpacs-text-input
       (concat "grove-" field) :value value
       :label (concat label " timestamp or shorthand")
       :hint "fri 10:00 ++1w" :single-line t :max-length 160)
      (jetpacs-flow-row
       (jetpacs-date-button
        (or date "Choose date")
        (jetpacs-action "grove.planning.pick"
                        :args (list :field field :kind "date")
                        :capture-fields capture)
        :value date)
       (jetpacs-time-button
        (or time "Add time")
        (jetpacs-action "grove.planning.pick"
                        :args (list :field field :kind "time")
                        :capture-fields capture)
        :value time :display-mode "switchable")
       (jetpacs-button
        "Clear"
        (jetpacs-action "grove.planning.clear"
                        :args (list :field field)
                        :capture-fields capture)
        :variant "text"))
      :spacing 8)
     :variant "outlined")))

(defun grove-document-planning-screen (back)
  "Build Grove's shared scheduled/deadline planning canvas with BACK."
  (if (null grove-document--planning)
      (jetpacs-chrome-screen
       "Planning"
       (jetpacs-empty-state :icon "event_busy" :title "Planning expired"
                            :caption "Reopen it from the document.")
       :back back)
    (let ((scheduled (plist-get grove-document--planning :scheduled))
          (deadline (plist-get grove-document--planning :deadline)))
      (jetpacs-chrome-screen
       "Planning"
       (jetpacs-lazy-column
        (jetpacs-text
         "Schedule and deadline are committed together. Emacs accepts its normal Org date shorthand and repeater syntax."
         :style "caption")
        (grove-document--planning-editor
         "scheduled" "Scheduled" scheduled)
        (grove-document--planning-editor
         "deadline" "Deadline" deadline)
        (jetpacs-button
         "Save planning"
         (jetpacs-action
          "grove.planning.save"
          :args (list :notebook-id
                      (plist-get grove-document--planning :notebook-id)
                      :heading-id
                      (plist-get grove-document--planning :heading-id)
                      :revision
                      (plist-get grove-document--planning :revision))
          :capture-fields '("grove-scheduled" "grove-deadline")
          :when-offline 'queue :ttl-s 86400
          :dedupe (jetpacs-wire-id
                   "grove-planning"
                   (plist-get grove-document--planning :heading-id)))
         :icon "done" :variant "filled")
        :spacing 10 :content-padding 12)
       :back back))))

(defconst grove-document--verbs
  '("grove.document.open" "grove.document.mode" "grove.document.save"
    "grove.document.todo" "grove.document.planning"
    "grove.planning.pick" "grove.planning.clear" "grove.planning.save")
  "Action names owned by Grove's document module.")

(defun grove-document-register ()
  "Register Grove's document and planning actions idempotently."
  (with-jetpacs-owner grove-owner
                      (jetpacs-defaction "grove.document.open" #'grove-document--on-open)
                      (jetpacs-defaction "grove.document.mode" #'grove-document--on-mode)
                      (jetpacs-defaction "grove.document.save" #'grove-document--on-save)
                      (jetpacs-defaction "grove.document.todo" #'grove-document--on-todo)
                      (jetpacs-defaction "grove.document.planning"
                                         #'grove-document--open-planning)
                      (jetpacs-defaction "grove.planning.pick"
                                         #'grove-document--on-planning-pick)
                      (jetpacs-defaction "grove.planning.clear"
                                         #'grove-document--on-planning-clear)
                      (jetpacs-defaction "grove.planning.save"
                                         #'grove-document--on-planning-save))
  t)

(defun grove-document-unregister ()
  "Unregister Grove's document actions and clear transient state."
  (dolist (verb grove-document--verbs) (jetpacs-undefaction verb))
  (jetpacs-org-render-clear-context nil grove-app-surface)
  (setq grove-document--current nil
        grove-document--planning nil)
  (clrhash grove-document--modes)
  t)

(provide 'grove-document)
;;; grove-document.el ends here
