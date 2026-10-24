;;; bloom-document.el --- Bloom-native Org reader and editor -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Native Bloom document presentation over the reusable Jetpacs Org renderer,
;; editor, toolbar, and dialogs.  Every mutation resolves an opaque Bloom id
;; back to an Org ref and rechecks the current subtree before writing.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'ebp-org)
(require 'jetpacs-apps)
(require 'jetpacs-chrome)
(require 'jetpacs-data)
(require 'jetpacs-dates)
(require 'jetpacs-editor-org)
(require 'jetpacs-org-save)
(require 'jetpacs-org-render)
(require 'jetpacs-org-toolbar)
(require 'jetpacs-shell)
(require 'jetpacs-widgets)
(require 'bloom-core)
(require 'bloom-design)
(require 'bloom-library)
(require 'bloom-org)
(require 'bloom-read)
(require 'bloom-edit)

(defconst bloom-document--screen-id "document"
  "Stable chrome screen id for Bloom's document presenter.")

(defconst bloom-document--planning-screen-id "planning"
  "Stable chrome screen id for Bloom's planning canvas.")

(defconst bloom-document--save-ttl-seconds 86400
  "Lifetime of a queued Bloom subtree save.")

(defvar bloom-document--current nil
  "Current document identity as (:notebook-id ID :heading-id ID).")

(defvar bloom-document--modes (make-hash-table :test #'equal)
  "Document composite key to `read' or `edit'.")

(defvar bloom-document--planning nil
  "Current planning draft and source revision.")

(defconst bloom-document--planning-fields
  '("bloom-scheduled" "bloom-deadline")
  "Field ids captured by every planning action that can refresh the screen.")

(defconst bloom-document--planning-focuses '("scheduled" "deadline")
  "Closed set of planning targets authored by Bloom.")

(defvar bloom-navigation--notebook-id nil
  "Notebook selected by Bloom navigation, declared here for link handoff.")

(declare-function bloom-ui-open-outline "bloom-ui"
                  (notebook-id &optional params))

(defun bloom-document--key (notebook-id heading-id)
  "Return NOTEBOOK-ID and HEADING-ID's local document-state key."
  (concat notebook-id "|" heading-id))

(defun bloom-document--mode (notebook-id heading-id)
  "Return NOTEBOOK-ID/HEADING-ID's current presentation mode."
  (or (gethash (bloom-document--key notebook-id heading-id)
               bloom-document--modes)
      bloom-reader-default-mode))

(defun bloom-document-open-action (notebook-id heading-id)
  "Return a durable action opening NOTEBOOK-ID and HEADING-ID in Bloom."
  (jetpacs-action
   "bloom.document.open"
   :args (list :notebook-id notebook-id :heading-id heading-id)
   :open-surface bloom-app-surface
   :when-offline (jetpacs-durable-offline-policy) :ttl-s 86400))

(defun bloom-document--set-current (notebook-id heading-id)
  "Select NOTEBOOK-ID and HEADING-ID and produce its current snapshot."
  (bloom-library-document-refresh notebook-id heading-id)
  (setq bloom-document--current
        (list :notebook-id notebook-id :heading-id heading-id)))

(defun bloom-document-open (notebook-id heading-id &optional params)
  "Open NOTEBOOK-ID and HEADING-ID on PARAMS' Bloom surface.
Return a handler status."
  (cond
   ((not (and (stringp notebook-id) (stringp heading-id))) 'rejected)
   ((jetpacs-event-stale-p params) 'stale)
   (t
    (condition-case err
        (progn
          (bloom-document--set-current notebook-id heading-id)
          (jetpacs-flow-continue
           (lambda ()
             (condition-case push-error
                 (jetpacs-chrome-push-screen
                  (or (plist-get params :surface) bloom-app-surface)
                  bloom-document--screen-id #'bloom-document-presented-screen)
               (error
                (message "bloom: document push failed (%s)"
                         (car push-error))))))
          'accepted)
      (ebp-org-unresolved 'stale)
      (ebp-org-refused 'rejected)
      (error
       (message "bloom: document open failed (%s)" (car err))
       'rejected)))))

(defun bloom-document--current-info ()
  "Return the current private document snapshot, or signal stale."
  (let ((notebook-id (plist-get bloom-document--current :notebook-id))
        (heading-id (plist-get bloom-document--current :heading-id)))
    (unless (and notebook-id heading-id)
      (signal 'ebp-org-unresolved nil))
    (bloom-library-document-snapshot notebook-id heading-id)))

(defun bloom-document--metadata-nodes (info)
  "Return Bloom metadata nodes for private document INFO."
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
                                  "bloom.document.planning"
                                  :args (append identity
                                                '(:focus "scheduled")))))
         (when-let* ((deadline (plist-get info :deadline)))
           (jetpacs-chip deadline :icon "event_busy"
                         :on-tap (jetpacs-action
                                  "bloom.document.planning"
                                  :args (append identity
                                                '(:focus "deadline")))))))
      (when properties
        (jetpacs-collapsible
         "bloom-document-properties"
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

(defun bloom-document--heading-card (info)
  "Return INFO's Bloom heading card with safe reveal-first swipe."
  (let* ((notebook-id (plist-get info :notebook-id))
         (heading-id (plist-get info :heading-id))
         (todo (plist-get info :todo))
         (done (and todo
                    (jetpacs-action
                     "bloom.document.todo"
                     :args (list :notebook-id notebook-id
                                 :heading-id heading-id :state "done")
                     :when-offline (jetpacs-durable-offline-policy)
                     :ttl-s 86400
                     :dedupe (jetpacs-wire-id "bloom-doc-done" heading-id))))
         (swipe (and done
                     (jetpacs-swipe
                      (list
                       (jetpacs-swipe-action
                        "Done" :icon "done" :color "primary"
                        :on-trigger done)
                       (jetpacs-swipe-action
                        "Plan" :icon "event" :color "secondary"
                        :on-trigger (jetpacs-action
                                     "bloom.document.planning"
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

(defun bloom-document--linked-presenter
    (_source destination position surface)
  "Present Org link DESTINATION at POSITION through Bloom on SURFACE."
  (let ((path (and (buffer-live-p destination)
                   (buffer-local-value 'buffer-file-name destination))))
    (cond
     ((null path) nil)
     (t
      (unless (bloom-library--notebook-record
               (bloom-library--notebook-id path))
        (bloom-library-refresh))
      (let* ((notebook-id (bloom-library--notebook-id path))
             (notebook (bloom-library--notebook-record notebook-id)))
        (if (null notebook)
            (progn
              (jetpacs-shell-notify
               "Linked Org file is outside Bloom's notebook scope" surface)
              t)
          (with-current-buffer destination
            (org-with-wide-buffer
             (goto-char (min (max (point-min) position) (point-max)))
             (if (not (org-before-first-heading-p))
                 (progn
                   (org-back-to-heading t)
                   (let* ((ref (ebp-org-ref-at-point))
                          (heading-id (bloom-library--heading-id ref)))
                     (puthash
                      (bloom-library--heading-key notebook-id heading-id)
                      ref bloom-library--heading-refs)
                     (setq bloom-navigation--notebook-id notebook-id)
                     (bloom-document--set-current notebook-id heading-id)
                     (jetpacs-chrome-push-screen
                      surface bloom-document--screen-id
                      #'bloom-document-presented-screen)))
               (bloom-ui-open-outline notebook-id)))
            t)))))))

(defun bloom-document--reader-body (info)
  "Return a Bloom-native reader body for private document INFO."
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
        (unless bloom-reader-show-drawers
          (ignore-errors (org-fold-hide-drawer-all)))
        (setq rendered
              (jetpacs-org-render-in-context
               buffer :surface bloom-app-surface
               :follow-destination #'bloom-document--linked-presenter
               :reader-typography t :hide-widen t))))
    (apply #'jetpacs-lazy-column
           (append (list (bloom-document--heading-card info))
                   (bloom-document--metadata-nodes info)
                   (list (jetpacs-divider))
                   rendered
                   (list :spacing 8 :content-padding 12)))))

(defun bloom-document--editor-body (info)
  "Return the narrowed subtree editor for private document INFO."
  (jetpacs-column
   (bloom-document--heading-card info)
   (jetpacs-editor
    (jetpacs-wire-id "bloom-editor" (plist-get info :heading-id))
    :value (plist-get info :content)
    :syntax "org"
    :toolbar (jetpacs-org-toolbar)
    :line-numbers :json-false
    :min-lines 12
    :on-save
    (jetpacs-action
     "bloom.document.save"
     :args (list :notebook-id (plist-get info :notebook-id)
                 :heading-id (plist-get info :heading-id)
                 :revision (plist-get info :revision))
     :when-offline 'queue :ttl-s bloom-document--save-ttl-seconds
     :dedupe (jetpacs-wire-id "bloom-save" (plist-get info :heading-id))))
   :spacing 8))

(defun bloom-document--top-actions (info)
  "Return Bloom document top-bar actions for INFO."
  (let ((mode (bloom-document--mode (plist-get info :notebook-id)
                                    (plist-get info :heading-id))))
    (list
     (jetpacs-icon-button
      (if (eq mode 'read) "edit" "visibility")
      (jetpacs-action
       "bloom.document.mode"
       :args (list :notebook-id (plist-get info :notebook-id)
                   :heading-id (plist-get info :heading-id)))
      :content-description (if (eq mode 'read) "Edit" "Read"))
     (jetpacs-icon-button
      "event"
      (jetpacs-action
       "bloom.document.planning"
       :args (list :notebook-id (plist-get info :notebook-id)
                   :heading-id (plist-get info :heading-id)))
      :content-description "Scheduling and deadline"))))

(defun bloom-document-screen (back)
  "Build Bloom's current native document screen with BACK."
  (condition-case err
      (let* ((info (bloom-document--current-info))
             (mode (bloom-document--mode (plist-get info :notebook-id)
                                         (plist-get info :heading-id))))
        (jetpacs-chrome-screen
         (plist-get info :headline)
         (if (eq mode 'read)
             (bloom-document--reader-body info)
           (bloom-document--editor-body info))
         :back back
         :actions (bloom-document--top-actions info)))
    ((ebp-org-unresolved ebp-org-refused)
     (jetpacs-chrome-screen
      "Document"
      (jetpacs-empty-state
       :icon "search_off" :title "Heading moved or was removed"
       :caption "Go back and reopen it from a fresh outline.")
      :back back))
    (error
     (message "bloom: document build failed (%s)" (car err))
     (jetpacs-chrome-screen
      "Document"
      (jetpacs-empty-state :icon "error" :title "Document unavailable"
                           :caption "Refresh the notebook and try again.")
      :back back))))

(defun bloom-document--parity-read-info ()
  "Return the current document's info when read mode can draw parity, else nil.
Parity needs the design runtime, Bloom's parity faces, read mode, and a
parsed subtree in the snapshot."
  (when (and (jetpacs-extension-advertised-p "jetpacs.design" :app)
             (bloom-design-parity-available-p))
    (let ((info (condition-case nil (bloom-document--current-info) (error nil))))
      (and info (plist-get info :read)
           (eq (bloom-document--mode (plist-get info :notebook-id)
                                     (plist-get info :heading-id))
               'read)
           info))))

(defun bloom-document--parity-edit-info ()
  "Return the current document's info when edit mode can draw parity, else nil.
Parity needs the design runtime, Bloom's parity faces, edit mode, and a
Companion that hosts synchronized editors."
  (when (and (jetpacs-extension-advertised-p "jetpacs.design" :app)
             (bloom-design-parity-available-p)
             (bloom-edit-sync-available-p))
    (let ((info (condition-case nil (bloom-document--current-info) (error nil))))
      (and info
           (eq (bloom-document--mode (plist-get info :notebook-id)
                                     (plist-get info :heading-id))
               'edit)
           info))))

(defun bloom-document-presented-screen (back)
  "Build the profile-selected document reader or editor with BACK.
Under the design runtime both modes are the Android app's own screens."
  (cond
   ((bloom-document--parity-read-info)
    (bloom-read-screen (bloom-document--parity-read-info) back))
   ((bloom-document--parity-edit-info)
    (bloom-edit-screen (bloom-document--parity-edit-info) back))
   (t
    (bloom-design-present-if-advertised
     (bloom-document-screen back) 'document))))

(defun bloom-document--on-read-fold (args params)
  "Fold or unfold ARGS' heading in the current read view for PARAMS."
  (condition-case nil
      (bloom-read-on-fold (bloom-document--current-info) args params)
    (ebp-org-unresolved 'stale)))

(defun bloom-document--on-read-drawer (args params)
  "Open or close ARGS' drawer or block in the current read view for PARAMS."
  (condition-case nil
      (bloom-read-on-drawer (bloom-document--current-info) args params)
    (ebp-org-unresolved 'stale)))

(defun bloom-document--note-link-p (info target)
  "Whether TARGET is a link target written in document INFO's note.
Read mode names a link by its target text, so this is the check that the
device asks for a link the note shows, not one of its own making: Org
would open any link type it is handed."
  (and (stringp target)
       (not (string-empty-p target))
       (not (string-search "]" target))
       (string-search (concat "[[" target "]") (or (plist-get info :content) ""))
       t))

(defun bloom-document--on-read-link (args params)
  "Follow ARGS' Org link target from the current note on PARAMS' surface.
The target must be one the note itself contains, else `rejected'.  Org
resolves the link in the note's own buffer: a web link goes to the
browser through `browse-url', and a destination in a notebook opens in
Bloom through the same presenter the reader used."
  (let ((target (plist-get args :target))
        (surface (or (plist-get params :surface) bloom-app-surface)))
    (if (not (and (stringp target) (not (string-empty-p target)))) 'rejected
      (condition-case nil
          (let* ((info (bloom-document--current-info))
                 (source (get-buffer (plist-get info :buffer))))
            (unless (bloom-document--note-link-p info target)
              (signal 'ebp-org-refused (list 'link-not-in-note)))
            (jetpacs-flow-continue
             (lambda ()
               (condition-case err
                   (save-window-excursion
                     (with-current-buffer source
                       (org-link-open-from-string (format "[[%s]]" target)))
                     (let ((destination (current-buffer)))
                       (unless (or (eq destination source)
                                   (not (buffer-file-name destination)))
                         (bloom-document--linked-presenter
                          source destination (point) surface))))
                 (error
                  (message "bloom: link failed (%s)" (car err))
                  (jetpacs-shell-notify "Link could not be opened" surface)))))
            'accepted)
        (ebp-org-unresolved 'stale)
        (ebp-org-refused 'rejected)))))

(defun bloom-document--on-open (args params)
  "Open a document named by opaque ARGS on PARAMS' surface."
  (bloom-document-open (plist-get args :notebook-id)
                       (plist-get args :heading-id) params))

(defun bloom-document--on-mode (args params)
  "Toggle ARGS' current document mode and refresh PARAMS' surface."
  (let ((notebook-id (plist-get args :notebook-id))
        (heading-id (plist-get args :heading-id)))
    (if (or (jetpacs-event-stale-p params)
            (not (equal bloom-document--current
                        (list :notebook-id notebook-id
                              :heading-id heading-id))))
        'stale
      (let ((editing (eq (bloom-document--mode notebook-id heading-id) 'edit)))
        ;; Leaving the editor saves first, so Read shows what was typed.
        (when editing
          (bloom-edit-save)
          (setq heading-id (plist-get bloom-document--current :heading-id)))
        (puthash (bloom-document--key notebook-id heading-id)
                 (if editing 'read 'edit)
                 bloom-document--modes)
        (jetpacs-app-defer-refresh params)
        'accepted))))

(defun bloom-document--after-mutation (notebook-id heading-id)
  "Refresh Bloom projections after mutating NOTEBOOK-ID and HEADING-ID."
  (bloom-library-reset-projections)
  (bloom-library-refresh)
  (bloom-org-refresh)
  (ignore-errors (bloom-library-outline-refresh notebook-id))
  (ignore-errors (bloom-library-document-refresh notebook-id heading-id)))

(defun bloom-document-revision (notebook-id heading-id)
  "Return NOTEBOOK-ID's HEADING-ID subtree revision as it is now.
The revision is `ebp-org-subtree-revision''s, the one every Bloom write
checks against.  Signals `ebp-org-unresolved' for an unknown heading."
  (ebp-org-subtree-revision
   (or (bloom-library-resolve-heading notebook-id heading-id)
       (signal 'ebp-org-unresolved nil))))

(defun bloom-document-write-subtree (notebook-id heading-id revision value)
  "Replace NOTEBOOK-ID's HEADING-ID subtree with VALUE when REVISION holds.
REVISION is the `ebp-org-subtree-revision' the caller started from.
Return the heading's id after the write, which a retitle changes, or the
symbol `stale' when the subtree no longer matches REVISION.  The current
document and its mode follow the heading to its new id, and Bloom's
projections refresh.  The write is `jetpacs-editor-org-revision-splice':
a failure before durability restores the buffer exactly, and the
signal propagates."
  (let* ((ref (or (bloom-library-resolve-heading notebook-id heading-id)
                  (signal 'ebp-org-unresolved nil)))
         (submitted (if (string-suffix-p "\n" value) value (concat value "\n")))
         (new-ref (jetpacs-editor-org-revision-splice ref submitted revision)))
    (if (eq new-ref 'stale)
        'stale
      (let* ((new-heading-id (bloom-library--heading-id new-ref))
             (mode (bloom-document--mode notebook-id heading-id)))
        (puthash (bloom-library--heading-key notebook-id new-heading-id)
                 new-ref bloom-library--heading-refs)
        (when (equal bloom-document--current
                     (list :notebook-id notebook-id :heading-id heading-id))
          (setq bloom-document--current
                (list :notebook-id notebook-id :heading-id new-heading-id)))
        (puthash (bloom-document--key notebook-id new-heading-id)
                 mode bloom-document--modes)
        (bloom-document--after-mutation notebook-id new-heading-id)
        new-heading-id))))

(defun bloom-document--on-save (args params)
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
          (let ((result (bloom-document-write-subtree
                         notebook-id heading-id revision value)))
            (if (eq result 'stale)
                'stale
              (puthash (bloom-document--key notebook-id result)
                       'read bloom-document--modes)
              (jetpacs-shell-notify "Saved" surface)
              (jetpacs-app-defer-refresh params)
              'accepted))
        (ebp-org-unresolved 'stale)
        (ebp-org-refused 'rejected)
        (error
         (message "bloom: document save failed (%s)" (car err))
         (jetpacs-shell-notify "Save failed" surface)
         'rejected))))))

(defun bloom-document--on-todo (args params)
  "Set opaque ARGS' TODO state and refresh PARAMS' surface."
  (let ((notebook-id (plist-get args :notebook-id))
        (heading-id (plist-get args :heading-id))
        (state (plist-get args :state)))
    (if (not (and (stringp notebook-id) (stringp heading-id)
                  (equal state "done")))
        'rejected
      (condition-case err
          (let ((ref (or (bloom-library-resolve-heading notebook-id heading-id)
                         (signal 'ebp-org-unresolved nil))))
            (ebp-org-toggle-todo ref 'bloom 'done)
            (bloom-document--after-mutation notebook-id heading-id)
            (jetpacs-app-defer-refresh params)
            'accepted)
        (ebp-org-unresolved 'stale)
        (ebp-org-refused 'rejected)
        (error
         (message "bloom: TODO mutation failed (%s)" (car err))
         'rejected)))))

(defun bloom-document--planning-field (params key fallback)
  "Return captured planning field KEY from PARAMS, or FALLBACK."
  (let ((value (plist-get (plist-get params :fields) key)))
    (if (stringp value) value fallback)))

(defun bloom-document--planning-date (stamp)
  "Return STAMP's real ISO calendar date, or nil."
  (when-let* ((date (ebp-org-ts-date stamp))
              ((jetpacs-dates-valid-p date)))
    date))

(defun bloom-document--planning-time-valid-p (time)
  "Return non-nil when TIME is a canonical 24-hour HH:MM value."
  (and (stringp time)
       (string-match
        "\\`\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\'" time)
       (< (string-to-number (match-string 1 time)) 24)
       (< (string-to-number (match-string 2 time)) 60)))

(defun bloom-document--planning-month-for
    (scheduled deadline focus today)
  "Choose a deterministic month from the planning pair and TODAY.
SCHEDULED or DEADLINE wins according to FOCUS, followed by the other field."
  (let ((preferred (if (equal focus "deadline") deadline scheduled))
        (other (if (equal focus "deadline") scheduled deadline)))
    (substring (or (bloom-document--planning-date preferred)
                   (bloom-document--planning-date other)
                   (and (jetpacs-dates-valid-p today) today)
                   "1970-01-01")
               0 7)))

(defun bloom-document--planning-refresh-marks ()
  "Replace the current planning draft's bounded, privacy-safe month marks."
  (when bloom-document--planning
    (let ((month (plist-get bloom-document--planning :month)))
      (setq bloom-document--planning
            (plist-put
             bloom-document--planning :marks
             (condition-case err
                 (bloom-org-planning-marks month)
               (error
                ;; Agenda errors can contain paths or heading text.  Keep the
                ;; calendar usable and report only the condition class.
                (message "bloom: planning marks unavailable (%s)" (car err))
                nil)))))))

(defun bloom-document--planning-merge-captured (params)
  "Merge both planning fields captured in PARAMS into the current draft."
  (let ((scheduled
         (bloom-document--planning-field
          params :bloom-scheduled
          (plist-get bloom-document--planning :scheduled)))
        (deadline
         (bloom-document--planning-field
          params :bloom-deadline
          (plist-get bloom-document--planning :deadline))))
    (setq bloom-document--planning
          (plist-put bloom-document--planning :scheduled scheduled))
    (setq bloom-document--planning
          (plist-put bloom-document--planning :deadline deadline))
    (cons scheduled deadline)))

(defun bloom-document--open-planning (args params)
  "Initialize ARGS' planning canvas on PARAMS' surface."
  (let ((notebook-id (plist-get args :notebook-id))
        (heading-id (plist-get args :heading-id))
        (focus (or (plist-get args :focus) "scheduled")))
    (cond
     ((not (and (stringp notebook-id) (stringp heading-id)
                (member focus bloom-document--planning-focuses)))
      'rejected)
     ((jetpacs-event-stale-p params) 'stale)
     (t
      (condition-case err
          (let ((info (progn
                        (bloom-document--set-current notebook-id heading-id)
                        (bloom-document--current-info)))
                ;; SPEC 20.1.2 (#197): the device's civil day, not the
                ;; desktop's — they disagree for hours when the phone is
                ;; in another zone.
                (today (jetpacs-dates-today (jetpacs-device-time-zone))))
            (let ((scheduled (or (plist-get info :scheduled) ""))
                  (deadline (or (plist-get info :deadline) "")))
            (setq bloom-document--planning
                  (list :notebook-id (plist-get info :notebook-id)
                        :heading-id (plist-get info :heading-id)
                        :revision (plist-get info :revision)
                        :scheduled scheduled
                        :deadline deadline
                        :focus focus
                        :today today
                        :month (bloom-document--planning-month-for
                                scheduled deadline focus today)
                        :marks nil)))
            (bloom-document--planning-refresh-marks)
            (jetpacs-flow-continue
             (lambda ()
               (jetpacs-chrome-push-screen
                (or (plist-get params :surface) bloom-app-surface)
                bloom-document--planning-screen-id
                #'bloom-document-presented-planning-screen)))
            'accepted)
        (ebp-org-unresolved 'stale)
        (error
         (message "bloom: planning open failed (%s)" (car err))
         'rejected))))))

(defun bloom-document-open-planning
    (notebook-id heading-id &optional params focus)
  "Open the planning canvas for opaque NOTEBOOK-ID and HEADING-ID.
PARAMS is the originating action context when one exists.  This public seam
lets context-less platform gestures reuse Bloom's normal planning workflow
without duplicating its Org resolution or screen state.  FOCUS optionally
selects the `scheduled' or `deadline' editor."
  (bloom-document--open-planning
   (list :notebook-id notebook-id :heading-id heading-id :focus focus)
   params))

(defun bloom-document--stamp-replace-date (stamp date)
  "Return STAMP with its date replaced by DATE."
  (if (and (stringp stamp)
           (string-match
            "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?:[ \\t]+[[:alpha:]]+\\)?"
            stamp))
      ;; A weekday token is decorative Org syntax.  Drop it when the date
      ;; changes rather than leaving a now-false weekday beside the new date.
      (replace-match date t t stamp)
    date))

(defun bloom-document--stamp-replace-time (stamp time)
  "Return STAMP with its time replaced by TIME."
  (let ((date (ebp-org-ts-date stamp))
        (repeater (ebp-org-ts-repeater stamp)))
    (string-join (delq nil (list date time repeater)) " ")))

(defun bloom-document--planning-feedback-kind (scheduled deadline focus)
  "The feedback the pair SCHEDULED/DEADLINE gets while editing FOCUS.
One of `lead-time' (both dates), `runway' (a deadline alone), `prompt'
\(a scheduled date alone while the deadline has focus), or nil."
  (let ((scheduled-date (bloom-document--planning-date scheduled))
        (deadline-date (bloom-document--planning-date deadline)))
    (cond
     ((and scheduled-date deadline-date) 'lead-time)
     ((and deadline-date (null scheduled-date)) 'runway)
     ((and (equal focus "deadline") scheduled-date) 'prompt))))

(defun bloom-document--planning-shape ()
  "The node shape of the planning screen for the current draft.
Every value a calendar tap can change is bound to the surface data
document (SPEC 13.8), and since SPEC 13.8.6 the feedback cards are
conditional nodes reading /planning/feedback_kind, so the authored shape
no longer follows the draft at all: non-nil while a draft is open, and a
handler that compares it before and after its change always patches.
Kept as the publish seam's shape witness."
  (and bloom-document--planning t))

(defun bloom-document--planning-publish (params shape)
  "Show the changed draft on PARAMS' surface.
SHAPE is `bloom-document--planning-shape' from before the handler's
change.  Unchanged, the whole /planning object is patched into the
surface data document and the Companion re-materializes the screen it
holds; changed, the stack is rebuilt and re-pushed.  Both are deferred
past the dispatch (D2)."
  (if (equal shape (bloom-document--planning-shape))
      (jetpacs-app-defer-patch
       (list (jetpacs-data-set "/planning" (bloom-document-planning-data)))
       params)
    (jetpacs-app-defer-refresh params)))

(defun bloom-document--on-planning-pick (args params)
  "Merge picker ARGS with the pair draft captured in PARAMS.
ARGS' `field' may be omitted: the calendar and the quick picks serve
whichever target has focus, which the surface data document shows and
their arguments therefore never carry."
  (let* ((field (or (plist-get args :field)
                    (plist-get bloom-document--planning :focus)))
         (kind (plist-get args :kind))
         (value (plist-get args :value))
         (shape (bloom-document--planning-shape)))
    (cond
     ((null bloom-document--planning) 'stale)
     ((not (and (member field '("scheduled" "deadline"))
                (member kind '("date" "time"))
                (if (equal kind "date")
                    (jetpacs-dates-valid-p value)
                  (bloom-document--planning-time-valid-p value))))
      'rejected)
     ((jetpacs-event-stale-p params) 'stale)
     (t
      (pcase-let* ((`(,scheduled . ,deadline)
                    (bloom-document--planning-merge-captured params))
                   (old (if (equal field "scheduled") scheduled deadline))
                   (new (if (equal kind "date")
                            (bloom-document--stamp-replace-date old value)
                          (bloom-document--stamp-replace-time old value))))
        (setq bloom-document--planning
              (plist-put bloom-document--planning :scheduled
                         (if (equal field "scheduled") new scheduled)))
        (setq bloom-document--planning
              (plist-put bloom-document--planning :deadline
                         (if (equal field "deadline") new deadline)))
        (setq bloom-document--planning
              (plist-put bloom-document--planning :focus field))
        (when (equal kind "date")
          (let ((month (substring value 0 7)))
            (unless (equal month
                           (plist-get bloom-document--planning :month))
              (setq bloom-document--planning
                    (plist-put bloom-document--planning :month month))
              (bloom-document--planning-refresh-marks))))
        (bloom-document--planning-publish params shape)
        'accepted)))))

(defun bloom-document--on-planning-suggest (_args params)
  "Schedule the draft three days before its deadline, as the feedback offers.
The suggestion is computed here from the deadline captured in PARAMS,
never carried by the button: the feedback card's values are bound to
the surface data document, so its button must stay true to them."
  (cond
   ((null bloom-document--planning) 'stale)
   ((jetpacs-event-stale-p params) 'stale)
   (t
    (let ((deadline-date
           (bloom-document--planning-date
            (bloom-document--planning-field
             params :bloom-deadline
             (plist-get bloom-document--planning :deadline)))))
      (if (null deadline-date)
          'rejected
        (bloom-document--on-planning-pick
         (list :field "scheduled" :kind "date"
               :value (bloom-org-date-shift deadline-date -3))
         params))))))

(defun bloom-document--on-planning-focus (args params)
  "Select ARGS' planning field while preserving captured edits in PARAMS."
  (let ((field (plist-get args :field))
        (shape (bloom-document--planning-shape)))
    (cond
     ((null bloom-document--planning) 'stale)
     ((not (member field bloom-document--planning-focuses)) 'rejected)
     ((jetpacs-event-stale-p params) 'stale)
     (t
      (pcase-let* ((`(,scheduled . ,deadline)
                    (bloom-document--planning-merge-captured params))
                   (value (if (equal field "scheduled")
                              scheduled deadline))
                   (date (bloom-document--planning-date value)))
        (setq bloom-document--planning
              (plist-put bloom-document--planning :focus field))
        (when (and date
                   (not (equal (substring date 0 7)
                               (plist-get bloom-document--planning :month))))
          (setq bloom-document--planning
                (plist-put bloom-document--planning :month
                           (substring date 0 7)))
          (bloom-document--planning-refresh-marks))
        (bloom-document--planning-publish params shape)
        'accepted)))))

(defun bloom-document--on-planning-month (args params)
  "Browse to the ISO month injected into ARGS, preserving fields in PARAMS."
  (let ((month (plist-get args :value))
        (shape (bloom-document--planning-shape)))
    (cond
     ((null bloom-document--planning) 'stale)
     ((not (bloom-org-month-valid-p month)) 'rejected)
     ((jetpacs-event-stale-p params) 'stale)
     (t
      (bloom-document--planning-merge-captured params)
      (setq bloom-document--planning
            (plist-put bloom-document--planning :month month))
      (bloom-document--planning-refresh-marks)
      (bloom-document--planning-publish params shape)
      'accepted))))

(defun bloom-document--on-planning-clear (args params)
  "Clear ARGS' field while retaining the other value from PARAMS."
  (let ((field (plist-get args :field))
        (shape (bloom-document--planning-shape)))
    (cond
     ((null bloom-document--planning) 'stale)
     ((not (member field '("scheduled" "deadline" "both"))) 'rejected)
     ((jetpacs-event-stale-p params) 'stale)
     (t
      (pcase-let ((`(,scheduled . ,deadline)
                   (bloom-document--planning-merge-captured params)))
        (setq bloom-document--planning
              (plist-put bloom-document--planning :scheduled
                         (if (member field '("scheduled" "both"))
                             "" scheduled)))
        (setq bloom-document--planning
              (plist-put bloom-document--planning :deadline
                         (if (member field '("deadline" "both"))
                             "" deadline)))
        (when (member field bloom-document--planning-focuses)
          (setq bloom-document--planning
                (plist-put bloom-document--planning :focus field)))
        (bloom-document--planning-publish params shape)
        'accepted)))))

(defun bloom-document--on-planning-save (args params)
  "Atomically commit ARGS' planning fields captured in PARAMS."
  (let* ((notebook-id (plist-get args :notebook-id))
         (heading-id (plist-get args :heading-id))
         (revision (plist-get args :revision))
         (scheduled (bloom-document--planning-field
                     params :bloom-scheduled
                     (plist-get bloom-document--planning :scheduled)))
         (deadline (bloom-document--planning-field
                    params :bloom-deadline
                    (plist-get bloom-document--planning :deadline))))
    (if (not (and (stringp notebook-id) (stringp heading-id)
                  (stringp revision) (stringp scheduled)
                  (stringp deadline)))
        'rejected
      (condition-case err
          (let ((ref (or (bloom-library-resolve-heading notebook-id heading-id)
                         (signal 'ebp-org-unresolved nil))))
            (if (not (equal (ebp-org-subtree-revision ref) revision))
                'stale
              (ebp-org-set-planning-pair ref 'bloom scheduled deadline)
              (bloom-document--after-mutation notebook-id heading-id)
              (setq bloom-document--planning nil)
              (jetpacs-flow-continue
               (lambda ()
                 (jetpacs-chrome-pop-screen
                  (or (plist-get params :surface) bloom-app-surface))))
              'accepted))
        (ebp-org-unresolved 'stale)
        (ebp-org-refused 'rejected)
        (error
         (message "bloom: planning save failed (%s)" (car err))
         (jetpacs-shell-notify "Planning values were not accepted"
                               (plist-get params :surface))
         'rejected)))))

(defun bloom-document--planning-ref (&rest segments)
  "A `jetpacs-ref' into Bloom's planning document at /planning/SEGMENTS."
  (jetpacs-ref (concat "/planning"
                       (mapconcat (lambda (segment) (concat "/" segment))
                                  segments ""))))

(defun bloom-document--planning-editor (field label)
  "Return planning editor FIELD with LABEL.
Its values — the timestamp, the date and time behind the pickers and
their labels — are bound to /planning/FIELD in the surface data
document, which `bloom-document-planning-data' builds."
  (jetpacs-card
   (jetpacs-column
    (jetpacs-text label :style "title")
    (jetpacs-text-input
     (concat "bloom-" field)
     :value (bloom-document--planning-ref field "value")
     :label (concat label " timestamp or shorthand")
     :hint "fri 10:00 ++1w" :single-line t :max-length 160)
    (jetpacs-flow-row
     (jetpacs-date-button
      (bloom-document--planning-ref field "date_label")
      (jetpacs-action "bloom.planning.pick"
                      :args (list :field field :kind "date")
                      :capture-fields bloom-document--planning-fields)
      :value (bloom-document--planning-ref field "date"))
     (jetpacs-time-button
      (bloom-document--planning-ref field "time_label")
      (jetpacs-action "bloom.planning.pick"
                      :args (list :field field :kind "time")
                      :capture-fields bloom-document--planning-fields)
      :value (bloom-document--planning-ref field "time")
      :display-mode "switchable")
     (jetpacs-button
      "Clear"
      (jetpacs-action "bloom.planning.clear"
                      :args (list :field field)
                      :capture-fields bloom-document--planning-fields)
      :variant "text"))
    :spacing 8)
   :variant "outlined"))

(defun bloom-document--planning-target-button (field)
  "Build the planning target button for FIELD.
Its label and its focus variant are bound to /planning/FIELD."
  (jetpacs-button
   (bloom-document--planning-ref field "target_label")
   (jetpacs-action
    "bloom.planning.focus" :args (list :field field)
    :capture-fields bloom-document--planning-fields)
   :icon (if (equal field "deadline") "event_busy" "schedule")
   :variant (bloom-document--planning-ref field "target_variant")))

(defun bloom-document--planning-preset-dates (today)
  "Return Bloom quick-pick label/date pairs relative to ISO TODAY."
  (when (jetpacs-dates-valid-p today)
    (let* ((parts (jetpacs-dates-parts today))
           (weekday (calendar-day-of-week parts))
           (weekend-delta (mod (- 6 weekday) 7))
           (monday-delta (mod (- 1 weekday) 7)))
      (when (= monday-delta 0) (setq monday-delta 7))
      (list (cons "Today" today)
            (cons "Tomorrow" (bloom-org-date-shift today 1))
            (cons "This weekend"
                  (bloom-org-date-shift today weekend-delta))
            (cons "Next week"
                  (bloom-org-date-shift today monday-delta))))))

(defun bloom-document--planning-preset-row (today)
  "Build quick date picks relative to TODAY for the focused target."
  (apply
   #'jetpacs-flow-row
   (append
    (mapcar
     (lambda (preset)
       (jetpacs-button
        (car preset)
        (jetpacs-action
         "bloom.planning.pick"
         :args (list :kind "date" :value (cdr preset))
         :capture-fields bloom-document--planning-fields)
        :variant "tonal" :size "small"))
     (bloom-document--planning-preset-dates today))
    '(:spacing 6 :run-spacing 6))))

(defun bloom-document--planning-day-styles (scheduled deadline)
  "Return unique day styles for ISO SCHEDULED and DEADLINE dates."
  (let (styles)
    (when scheduled
      (push (cons scheduled
                  (jetpacs-month-day-style "primary" "on_primary"))
            styles))
    (when deadline
      (setq styles (cl-remove deadline styles :key #'car :test #'equal))
      (push (cons deadline
                  (jetpacs-month-day-style "error" "on_error"))
            styles))
    (sort styles (lambda (left right) (string< (car left) (car right))))))

(defun bloom-document--planning-calendar-marks
    (counts scheduled deadline styled)
  "Build month marks from COUNTS and planning endpoints.
When STYLED is nil, SCHEDULED and DEADLINE colors are folded into the mark
fallback so an older member profile still distinguishes the two targets."
  (let ((dots (make-hash-table :test #'equal))
        (colors (make-hash-table :test #'equal)))
    (dolist (mark counts)
      (when (and (jetpacs-dates-valid-p (car mark))
                 (integerp (cdr mark)))
        (puthash (car mark) (max 0 (min 3 (cdr mark))) dots)))
    (unless styled
      (when scheduled
        (puthash scheduled (max 1 (gethash scheduled dots 0)) dots)
        (puthash scheduled "primary" colors))
      (when deadline
        (puthash deadline (max 1 (gethash deadline dots 0)) dots)
        (puthash deadline "error" colors)))
    (let (marks)
      (maphash
       (lambda (date count)
         (when (> count 0)
           (push (cons date
                       (jetpacs-month-mark count
                                           :color (gethash date colors)))
                 marks)))
       dots)
      (sort marks (lambda (left right) (string< (car left) (car right)))))))

(defun bloom-document--planning-feedback-when (kind)
  "The SPEC 13.8.6 predicate vector that shows the feedback node of KIND."
  (vector (jetpacs-when "/planning/feedback_kind" "eq" (symbol-name kind))))

(defun bloom-document--planning-feedback-cards ()
  "Build the three feedback nodes, each conditional on /planning/feedback_kind.
SPEC 13.8.6: every kind `bloom-document--planning-feedback-kind' can
name is authored once, keyed, and shown only while the document says so;
a change of kind is a `surface.data' patch, never a snapshot.  Their words
and colors are bound to /planning/feedback; the runway's suggestion is the
handler's to compute when its button is tapped."
  (list
   (jetpacs-with-attrs (bloom-document--planning-feedback 'lead-time)
                       :key "feedback-lead-time"
                       :visible_when (bloom-document--planning-feedback-when 'lead-time))
   (jetpacs-with-attrs (bloom-document--planning-feedback 'runway)
                       :key "feedback-runway"
                       :visible_when (bloom-document--planning-feedback-when 'runway))
   (jetpacs-with-attrs (bloom-document--planning-feedback 'prompt)
                       :key "feedback-prompt"
                       :visible_when (bloom-document--planning-feedback-when 'prompt))))

(defun bloom-document--planning-feedback (kind)
  "Build the feedback node of KIND (`bloom-document--planning-feedback-kind').
Its words and colors are bound to /planning/feedback; the runway's
suggestion is the handler's to compute when its button is tapped."
  (pcase kind
    ('lead-time
     (jetpacs-card
      (jetpacs-row
       (jetpacs-icon (bloom-document--planning-ref "feedback" "icon")
                     :color (bloom-document--planning-ref
                             "feedback" "icon_color"))
       (jetpacs-with-attrs
        (jetpacs-text
         (bloom-document--planning-ref "feedback" "text")
         :style "caption"
         :color (bloom-document--planning-ref "feedback" "text_color"))
        :weight 1)
       :spacing 8 :align "center")
      :variant "outlined"))
    ('runway
     (jetpacs-card
      (jetpacs-column
       (jetpacs-text "Give yourself a runway" :style "label")
       (jetpacs-text (bloom-document--planning-ref "feedback" "text")
                     :style "caption")
       (jetpacs-button
        "Use suggested date"
        (jetpacs-action
         "bloom.planning.suggest"
         :capture-fields bloom-document--planning-fields)
        :icon "schedule" :variant "tonal" :size "small")
       :spacing 6)
      :variant "outlined"))
    ('prompt
     (jetpacs-text "Choose a deadline to see the available lead time."
                   :style "caption"))))

(defun bloom-document--planning-styled-p ()
  "Whether the live profile advertises month_grid day styles (SPEC 12)."
  (jetpacs-node-member-advertised-p "month_grid" "day_styles" :app))

(defun bloom-document--planning-calendar (today styled)
  "Build Bloom's calendar with its feedback cards and quick picks from TODAY.
STYLED binds day styles, which only an advertising profile may carry.
The month, its marks and styles, the selection or range, and the target
buttons' labels and variants are all bound to /planning, and the feedback
cards are conditional on /planning/feedback_kind (SPEC 13.8.6), so a tap
changes the document and never this tree.  Every child is keyed, as a
children array holding a conditional node must be."
  (jetpacs-card
   (apply
    #'jetpacs-column
    (jetpacs-with-attrs
     (jetpacs-flow-row
      (bloom-document--planning-target-button "scheduled")
      (bloom-document--planning-target-button "deadline")
      (jetpacs-button
       "Clear both"
       (jetpacs-action
        "bloom.planning.clear" :args '(:field "both")
        :capture-fields bloom-document--planning-fields)
       :variant "text" :size "small")
      :spacing 6 :run-spacing 6)
     :key "targets")
    (jetpacs-with-attrs
     (jetpacs-month-grid
      (bloom-document--planning-ref "month")
      :marks (bloom-document--planning-ref "marks")
      :day-styles (and styled (bloom-document--planning-ref "day_styles"))
      :selected (bloom-document--planning-ref "selected")
      :range-start (bloom-document--planning-ref "range_start")
      :range-end (bloom-document--planning-ref "range_end")
      :on-day-tap
      (jetpacs-action
       "bloom.planning.pick" :args '(:kind "date")
       :capture-fields bloom-document--planning-fields)
      :on-month-change
      (jetpacs-action
       "bloom.planning.month"
       :capture-fields bloom-document--planning-fields)
      :children
      (list (jetpacs-text
             "Use the date controls below if this renderer has no calendar."
             :style "caption")))
     :key "month")
    (jetpacs-with-attrs (bloom-document--planning-preset-row today) :key "presets")
    (append (bloom-document--planning-feedback-cards)
            (list :spacing 10)))
   :variant "outlined"))

(defun bloom-document--planning-field-data (field label focus)
  "The /planning/FIELD object: LABEL's draft, its pickers, its FOCUS state."
  (let* ((value (plist-get bloom-document--planning
                           (if (equal field "deadline") :deadline :scheduled)))
         (date (bloom-document--planning-date value))
         (raw-time (ebp-org-ts-time value))
         (time (and (bloom-document--planning-time-valid-p raw-time)
                    raw-time)))
    (append
     (list :value value
           :date_label (or date "Choose date")
           ;; SPEC 20.1.2 (#197): the label follows the device's clock
           ;; cycle; without advice it is the HH:MM it always was.
           :time_label (or (and time (jetpacs-dates-format-clock
                                      time (jetpacs-device-hour-cycle)))
                           "Add time")
           :target_label (concat label (if date (concat " · " date) ""))
           :target_variant (if (equal field focus) "filled" "outlined"))
     (and date (list :date date))
     (and time (list :time time)))))

(defun bloom-document--planning-feedback-data (kind scheduled-date deadline-date)
  "The /planning/feedback object for KIND over SCHEDULED-DATE and DEADLINE-DATE."
  (pcase kind
    ('lead-time
     (let ((days (jetpacs-dates-delta deadline-date scheduled-date)))
       (append
        (list :icon (if (< days 0) "warning" "date_range")
              :icon_color (if (< days 0) "error" "primary")
              :text
              (cond
               ((< days 0)
                (format "Deadline is %d day%s before the scheduled date."
                        (- days) (if (= days -1) "" "s")))
               ((= days 0) "Scheduled and due on the same day.")
               (t (format "%d day%s between scheduled and deadline."
                          days (if (= days 1) "" "s")))))
        (and (< days 0) (list :text_color "error")))))
    ('runway
     (list :text
           (format "Schedule this for %s, three days before the deadline."
                   (bloom-org-date-shift deadline-date -3))))))

(defun bloom-document-planning-data ()
  "The /planning object of Bloom's surface data document (SPEC 13.8).
A pure projection of `bloom-document--planning' and the live member
profile — the same inputs the planning screen's builders read — so a
snapshot and a patch always agree.  Nil without an open draft."
  (when bloom-document--planning
    (let* ((scheduled (plist-get bloom-document--planning :scheduled))
           (deadline (plist-get bloom-document--planning :deadline))
           (focus (or (plist-get bloom-document--planning :focus)
                      "scheduled"))
           (today (plist-get bloom-document--planning :today))
           (month (or (plist-get bloom-document--planning :month)
                      (bloom-document--planning-month-for
                       scheduled deadline focus today)))
           (scheduled-date (bloom-document--planning-date scheduled))
           (deadline-date (bloom-document--planning-date deadline))
           (styled (bloom-document--planning-styled-p))
           (chronological
            (and scheduled-date deadline-date
                 (not (string> scheduled-date deadline-date))))
           (selected
            (unless chronological
              (if (equal focus "deadline")
                  (or deadline-date scheduled-date)
                (or scheduled-date deadline-date))))
           (marks (jetpacs-month-marks
                   (bloom-document--planning-calendar-marks
                    (plist-get bloom-document--planning :marks)
                    scheduled-date deadline-date styled)))
           (styles (and styled
                        (jetpacs-month-day-styles
                         (bloom-document--planning-day-styles
                          scheduled-date deadline-date))))
           (kind (bloom-document--planning-feedback-kind
                  scheduled deadline focus))
           (feedback (bloom-document--planning-feedback-data
                      kind scheduled-date deadline-date)))
      (append
       (list :month month
             :scheduled (bloom-document--planning-field-data
                         "scheduled" "Scheduled" focus)
             :deadline (bloom-document--planning-field-data
                        "deadline" "Deadline" focus))
       (and marks (list :marks marks))
       (and styles (list :day_styles styles))
       (and selected (list :selected selected))
       (and chronological
            (list :range_start scheduled-date :range_end deadline-date))
       ;; SPEC 13.8.6: the conditional feedback cards read the kind by name.
       (and kind (list :feedback_kind (symbol-name kind)))
       (and feedback (list :feedback feedback))))))

(defun bloom-document-surface-data ()
  "Bloom's app-surface data document (SPEC 13.8).
The planning object while a draft is open; nil otherwise, which the
shell sends as the empty document."
  (when-let* ((planning (bloom-document-planning-data)))
    (list :planning planning)))

(defun bloom-document-planning-screen (back)
  "Build Bloom's shared scheduled/deadline planning canvas with BACK.
The tree never follows the draft: every value it shows is bound to the
surface data document \(`bloom-document-surface-data') and the feedback
cards are conditional on it (SPEC 13.8.6), so a picker tap patches the
document and the Companion re-materializes this screen without a new
snapshot."
  (if (null bloom-document--planning)
      (jetpacs-chrome-screen
       "Planning"
       (jetpacs-empty-state :icon "event_busy" :title "Planning expired"
                            :caption "Reopen it from the document.")
       :back back)
    (jetpacs-chrome-screen
     "Planning"
     (jetpacs-lazy-column
      (jetpacs-text
       "Pick both dates on one calendar, then refine either timestamp. Emacs still accepts normal Org shorthand and repeaters."
       :style "caption")
      (bloom-document--planning-calendar
       (plist-get bloom-document--planning :today)
       (bloom-document--planning-styled-p))
      (bloom-document--planning-editor "scheduled" "Scheduled")
      (bloom-document--planning-editor "deadline" "Deadline")
      (jetpacs-button
       "Save planning"
       (jetpacs-action
        "bloom.planning.save"
        :args (list :notebook-id
                    (plist-get bloom-document--planning :notebook-id)
                    :heading-id
                    (plist-get bloom-document--planning :heading-id)
                    :revision
                    (plist-get bloom-document--planning :revision))
        :capture-fields bloom-document--planning-fields
        :when-offline 'queue :ttl-s 86400
        :dedupe (jetpacs-wire-id
                 "bloom-planning"
                 (plist-get bloom-document--planning :heading-id)))
       :icon "done" :variant "filled")
      :spacing 10 :content-padding 12)
     :back back)))

(defun bloom-document-presented-planning-screen (back)
  "Build the profile-selected planning canvas with BACK."
  (bloom-design-present-if-advertised
   (bloom-document-planning-screen back) 'planning))

(defconst bloom-document--verbs
  '("bloom.document.open" "bloom.document.mode" "bloom.document.save"
    "bloom.document.todo" "bloom.document.planning"
    "bloom.planning.pick" "bloom.planning.suggest" "bloom.planning.focus"
    "bloom.planning.month" "bloom.planning.clear" "bloom.planning.save"
    "bloom.read.fold" "bloom.read.drawer" "bloom.read.link" "bloom.edit.save")
  "Action names owned by Bloom's document module.")

(defun bloom-document-register ()
  "Register Bloom's document and planning actions idempotently."
  (with-jetpacs-owner bloom-owner
                      (jetpacs-defaction "bloom.document.open" #'bloom-document--on-open)
                      (jetpacs-defaction "bloom.document.mode" #'bloom-document--on-mode)
                      (jetpacs-defaction "bloom.document.save" #'bloom-document--on-save)
                      (jetpacs-defaction "bloom.document.todo" #'bloom-document--on-todo)
                      (jetpacs-defaction "bloom.read.fold" #'bloom-document--on-read-fold
                                         :args '((:name index :type "number" :required t))
                                         :doc "Fold or unfold one heading of the open read view")
                      (jetpacs-defaction "bloom.read.drawer" #'bloom-document--on-read-drawer
                                         :args '((:name key :type "text" :required t))
                                         :doc "Open or close one drawer or block of the read view")
                      (jetpacs-defaction "bloom.edit.save" #'bloom-edit-on-save
                                         :doc "Save the note open in edit mode now")
                      (jetpacs-defaction "bloom.read.link" #'bloom-document--on-read-link
                                         :args '((:name target :type "text" :required t))
                                         :doc "Follow an Org link from the open note")
                      (jetpacs-defaction "bloom.document.planning"
                                         #'bloom-document--open-planning
                                         :args '((:name notebook-id :type "ref" :required t)
                                                 (:name heading-id :type "ref" :required t)
                                                 (:name focus :type "enum"))
                                         :doc "Open Bloom's paired Org planning calendar")
                      (jetpacs-defaction "bloom.planning.pick"
                                         #'bloom-document--on-planning-pick
                                         :args '((:name field :type "enum")
                                                 (:name kind :type "enum" :required t)
                                                 (:name value :type "text" :required t))
                                         :doc "Apply a validated date or time to one planning target, the focused one by default")
                      (jetpacs-defaction "bloom.planning.suggest"
                                         #'bloom-document--on-planning-suggest
                                         :doc "Schedule the draft three days before its captured deadline")
                      (jetpacs-defaction "bloom.planning.focus"
                                         #'bloom-document--on-planning-focus
                                         :args '((:name field :type "enum" :required t))
                                         :doc "Choose which planning target receives calendar taps")
                      (jetpacs-defaction "bloom.planning.month"
                                         #'bloom-document--on-planning-month
                                         :args '((:name value :type "month" :required t))
                                         :doc "Browse Bloom's planning calendar to a month")
                      (jetpacs-defaction "bloom.planning.clear"
                                         #'bloom-document--on-planning-clear
                                         :args '((:name field :type "enum" :required t))
                                         :doc "Clear one or both captured planning values")
                      (jetpacs-defaction "bloom.planning.save"
                                         #'bloom-document--on-planning-save
                                         :args '((:name notebook-id :type "ref" :required t)
                                                 (:name heading-id :type "ref" :required t)
                                                 (:name revision :type "text" :required t))
                                         :doc "Atomically commit both Org planning values"))
  t)

(defun bloom-document-unregister ()
  "Unregister Bloom's document actions and clear transient state."
  (dolist (verb bloom-document--verbs) (jetpacs-undefaction verb))
  (jetpacs-org-render-clear-context nil bloom-app-surface)
  (setq bloom-document--current nil
        bloom-document--planning nil)
  (clrhash bloom-document--modes)
  t)

(provide 'bloom-document)
;;; bloom-document.el ends here
