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
(require 'jetpacs-data)
(require 'jetpacs-dates)
(require 'jetpacs-editor-org)
(require 'jetpacs-org-render)
(require 'jetpacs-org-toolbar)
(require 'jetpacs-shell)
(require 'jetpacs-widgets)
(require 'grove-core)
(require 'grove-design)
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

(defconst grove-document--planning-fields
  '("grove-scheduled" "grove-deadline")
  "Field ids captured by every planning action that can refresh the screen.")

(defconst grove-document--planning-focuses '("scheduled" "deadline")
  "Closed set of planning targets authored by Grove.")

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
   :when-offline (jetpacs-durable-offline-policy) :ttl-s 86400))

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
                  grove-document--screen-id #'grove-document-presented-screen)
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
                                  :args (append identity
                                                '(:focus "scheduled")))))
         (when-let* ((deadline (plist-get info :deadline)))
           (jetpacs-chip deadline :icon "event_busy"
                         :on-tap (jetpacs-action
                                  "grove.document.planning"
                                  :args (append identity
                                                '(:focus "deadline")))))))
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
                     :when-offline (jetpacs-durable-offline-policy)
                     :ttl-s 86400
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
                      #'grove-document-presented-screen)))
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

(defun grove-document-presented-screen (back)
  "Build the profile-selected document reader or editor with BACK."
  (grove-design-present-if-advertised
   (grove-document-screen back) 'document))

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

(defun grove-document--planning-date (stamp)
  "Return STAMP's real ISO calendar date, or nil."
  (when-let* ((date (ebp-org-ts-date stamp))
              ((grove-org-date-valid-p date)))
    date))

(defun grove-document--planning-time-valid-p (time)
  "Return non-nil when TIME is a canonical 24-hour HH:MM value."
  (and (stringp time)
       (string-match
        "\\`\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\'" time)
       (< (string-to-number (match-string 1 time)) 24)
       (< (string-to-number (match-string 2 time)) 60)))

(defun grove-document--planning-month-for
    (scheduled deadline focus today)
  "Choose a deterministic month from the planning pair and TODAY.
SCHEDULED or DEADLINE wins according to FOCUS, followed by the other field."
  (let ((preferred (if (equal focus "deadline") deadline scheduled))
        (other (if (equal focus "deadline") scheduled deadline)))
    (substring (or (grove-document--planning-date preferred)
                   (grove-document--planning-date other)
                   (and (grove-org-date-valid-p today) today)
                   "1970-01-01")
               0 7)))

(defun grove-document--planning-refresh-marks ()
  "Replace the current planning draft's bounded, privacy-safe month marks."
  (when grove-document--planning
    (let ((month (plist-get grove-document--planning :month)))
      (setq grove-document--planning
            (plist-put
             grove-document--planning :marks
             (condition-case err
                 (grove-org-planning-marks month)
               (error
                ;; Agenda errors can contain paths or heading text.  Keep the
                ;; calendar usable and report only the condition class.
                (message "grove: planning marks unavailable (%s)" (car err))
                nil)))))))

(defun grove-document--planning-merge-captured (params)
  "Merge both planning fields captured in PARAMS into the current draft."
  (let ((scheduled
         (grove-document--planning-field
          params :grove-scheduled
          (plist-get grove-document--planning :scheduled)))
        (deadline
         (grove-document--planning-field
          params :grove-deadline
          (plist-get grove-document--planning :deadline))))
    (setq grove-document--planning
          (plist-put grove-document--planning :scheduled scheduled))
    (setq grove-document--planning
          (plist-put grove-document--planning :deadline deadline))
    (cons scheduled deadline)))

(defun grove-document--open-planning (args params)
  "Initialize ARGS' planning canvas on PARAMS' surface."
  (let ((notebook-id (plist-get args :notebook-id))
        (heading-id (plist-get args :heading-id))
        (focus (or (plist-get args :focus) "scheduled")))
    (cond
     ((not (and (stringp notebook-id) (stringp heading-id)
                (member focus grove-document--planning-focuses)))
      'rejected)
     ((jetpacs-event-stale-p params) 'stale)
     (t
      (condition-case err
          (let ((info (progn
                        (grove-document--set-current notebook-id heading-id)
                        (grove-document--current-info)))
                ;; SPEC 20.1.2 (#197): the device's civil day, not the
                ;; desktop's — they disagree for hours when the phone is
                ;; in another zone.
                (today (jetpacs-dates-today (jetpacs-device-time-zone))))
            (let ((scheduled (or (plist-get info :scheduled) ""))
                  (deadline (or (plist-get info :deadline) "")))
            (setq grove-document--planning
                  (list :notebook-id (plist-get info :notebook-id)
                        :heading-id (plist-get info :heading-id)
                        :revision (plist-get info :revision)
                        :scheduled scheduled
                        :deadline deadline
                        :focus focus
                        :today today
                        :month (grove-document--planning-month-for
                                scheduled deadline focus today)
                        :marks nil)))
            (grove-document--planning-refresh-marks)
            (jetpacs-flow-continue
             (lambda ()
               (jetpacs-chrome-push-screen
                (or (plist-get params :surface) grove-app-surface)
                grove-document--planning-screen-id
                #'grove-document-presented-planning-screen)))
            'accepted)
        (ebp-org-unresolved 'stale)
        (error
         (message "grove: planning open failed (%s)" (car err))
         'rejected))))))

(defun grove-document-open-planning
    (notebook-id heading-id &optional params focus)
  "Open the planning canvas for opaque NOTEBOOK-ID and HEADING-ID.
PARAMS is the originating action context when one exists.  This public seam
lets context-less platform gestures reuse Grove's normal planning workflow
without duplicating its Org resolution or screen state.  FOCUS optionally
selects the `scheduled' or `deadline' editor."
  (grove-document--open-planning
   (list :notebook-id notebook-id :heading-id heading-id :focus focus)
   params))

(defun grove-document--stamp-replace-date (stamp date)
  "Return STAMP with its date replaced by DATE."
  (if (and (stringp stamp)
           (string-match
            "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?:[ \\t]+[[:alpha:]]+\\)?"
            stamp))
      ;; A weekday token is decorative Org syntax.  Drop it when the date
      ;; changes rather than leaving a now-false weekday beside the new date.
      (replace-match date t t stamp)
    date))

(defun grove-document--stamp-replace-time (stamp time)
  "Return STAMP with its time replaced by TIME."
  (let ((date (ebp-org-ts-date stamp))
        (repeater (ebp-org-ts-repeater stamp)))
    (string-join (delq nil (list date time repeater)) " ")))

(defun grove-document--planning-feedback-kind (scheduled deadline focus)
  "The feedback the pair SCHEDULED/DEADLINE gets while editing FOCUS.
One of `lead-time' (both dates), `runway' (a deadline alone), `prompt'
\(a scheduled date alone while the deadline has focus), or nil."
  (let ((scheduled-date (grove-document--planning-date scheduled))
        (deadline-date (grove-document--planning-date deadline)))
    (cond
     ((and scheduled-date deadline-date) 'lead-time)
     ((and deadline-date (null scheduled-date)) 'runway)
     ((and (equal focus "deadline") scheduled-date) 'prompt))))

(defun grove-document--planning-shape ()
  "The node shape of the planning screen for the current draft.
Every value a calendar tap can change is bound to the surface data
document (SPEC 13.8); the feedback card is the one node whose SHAPE
follows the draft, so only a change of its kind needs a new snapshot."
  (and grove-document--planning
       (grove-document--planning-feedback-kind
        (plist-get grove-document--planning :scheduled)
        (plist-get grove-document--planning :deadline)
        (plist-get grove-document--planning :focus))))

(defun grove-document--planning-publish (params shape)
  "Show the changed draft on PARAMS' surface.
SHAPE is `grove-document--planning-shape' from before the handler's
change.  Unchanged, the whole /planning object is patched into the
surface data document and the Companion re-materializes the screen it
holds; changed, the stack is rebuilt and re-pushed.  Both are deferred
past the dispatch (D2)."
  (if (equal shape (grove-document--planning-shape))
      (jetpacs-app-defer-patch
       (list (jetpacs-data-set "/planning" (grove-document-planning-data)))
       params)
    (jetpacs-app-defer-refresh params)))

(defun grove-document--on-planning-pick (args params)
  "Merge picker ARGS with the pair draft captured in PARAMS.
ARGS' `field' may be omitted: the calendar and the quick picks serve
whichever target has focus, which the surface data document shows and
their arguments therefore never carry."
  (let* ((field (or (plist-get args :field)
                    (plist-get grove-document--planning :focus)))
         (kind (plist-get args :kind))
         (value (plist-get args :value))
         (shape (grove-document--planning-shape)))
    (cond
     ((null grove-document--planning) 'stale)
     ((not (and (member field '("scheduled" "deadline"))
                (member kind '("date" "time"))
                (if (equal kind "date")
                    (grove-org-date-valid-p value)
                  (grove-document--planning-time-valid-p value))))
      'rejected)
     ((jetpacs-event-stale-p params) 'stale)
     (t
      (pcase-let* ((`(,scheduled . ,deadline)
                    (grove-document--planning-merge-captured params))
                   (old (if (equal field "scheduled") scheduled deadline))
                   (new (if (equal kind "date")
                            (grove-document--stamp-replace-date old value)
                          (grove-document--stamp-replace-time old value))))
        (setq grove-document--planning
              (plist-put grove-document--planning :scheduled
                         (if (equal field "scheduled") new scheduled)))
        (setq grove-document--planning
              (plist-put grove-document--planning :deadline
                         (if (equal field "deadline") new deadline)))
        (setq grove-document--planning
              (plist-put grove-document--planning :focus field))
        (when (equal kind "date")
          (let ((month (substring value 0 7)))
            (unless (equal month
                           (plist-get grove-document--planning :month))
              (setq grove-document--planning
                    (plist-put grove-document--planning :month month))
              (grove-document--planning-refresh-marks))))
        (grove-document--planning-publish params shape)
        'accepted)))))

(defun grove-document--on-planning-suggest (_args params)
  "Schedule the draft three days before its deadline, as the feedback offers.
The suggestion is computed here from the deadline captured in PARAMS,
never carried by the button: the feedback card's values are bound to
the surface data document, so its button must stay true to them."
  (cond
   ((null grove-document--planning) 'stale)
   ((jetpacs-event-stale-p params) 'stale)
   (t
    (let ((deadline-date
           (grove-document--planning-date
            (grove-document--planning-field
             params :grove-deadline
             (plist-get grove-document--planning :deadline)))))
      (if (null deadline-date)
          'rejected
        (grove-document--on-planning-pick
         (list :field "scheduled" :kind "date"
               :value (grove-org-date-shift deadline-date -3))
         params))))))

(defun grove-document--on-planning-focus (args params)
  "Select ARGS' planning field while preserving captured edits in PARAMS."
  (let ((field (plist-get args :field))
        (shape (grove-document--planning-shape)))
    (cond
     ((null grove-document--planning) 'stale)
     ((not (member field grove-document--planning-focuses)) 'rejected)
     ((jetpacs-event-stale-p params) 'stale)
     (t
      (pcase-let* ((`(,scheduled . ,deadline)
                    (grove-document--planning-merge-captured params))
                   (value (if (equal field "scheduled")
                              scheduled deadline))
                   (date (grove-document--planning-date value)))
        (setq grove-document--planning
              (plist-put grove-document--planning :focus field))
        (when (and date
                   (not (equal (substring date 0 7)
                               (plist-get grove-document--planning :month))))
          (setq grove-document--planning
                (plist-put grove-document--planning :month
                           (substring date 0 7)))
          (grove-document--planning-refresh-marks))
        (grove-document--planning-publish params shape)
        'accepted)))))

(defun grove-document--on-planning-month (args params)
  "Browse to the ISO month injected into ARGS, preserving fields in PARAMS."
  (let ((month (plist-get args :value))
        (shape (grove-document--planning-shape)))
    (cond
     ((null grove-document--planning) 'stale)
     ((not (grove-org-month-valid-p month)) 'rejected)
     ((jetpacs-event-stale-p params) 'stale)
     (t
      (grove-document--planning-merge-captured params)
      (setq grove-document--planning
            (plist-put grove-document--planning :month month))
      (grove-document--planning-refresh-marks)
      (grove-document--planning-publish params shape)
      'accepted))))

(defun grove-document--on-planning-clear (args params)
  "Clear ARGS' field while retaining the other value from PARAMS."
  (let ((field (plist-get args :field))
        (shape (grove-document--planning-shape)))
    (cond
     ((null grove-document--planning) 'stale)
     ((not (member field '("scheduled" "deadline" "both"))) 'rejected)
     ((jetpacs-event-stale-p params) 'stale)
     (t
      (pcase-let ((`(,scheduled . ,deadline)
                   (grove-document--planning-merge-captured params)))
        (setq grove-document--planning
              (plist-put grove-document--planning :scheduled
                         (if (member field '("scheduled" "both"))
                             "" scheduled)))
        (setq grove-document--planning
              (plist-put grove-document--planning :deadline
                         (if (member field '("deadline" "both"))
                             "" deadline)))
        (when (member field grove-document--planning-focuses)
          (setq grove-document--planning
                (plist-put grove-document--planning :focus field)))
        (grove-document--planning-publish params shape)
        'accepted)))))

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

(defun grove-document--planning-ref (&rest segments)
  "A `jetpacs-ref' into Grove's planning document at /planning/SEGMENTS."
  (jetpacs-ref (concat "/planning"
                       (mapconcat (lambda (segment) (concat "/" segment))
                                  segments ""))))

(defun grove-document--planning-editor (field label)
  "Return planning editor FIELD with LABEL.
Its values — the timestamp, the date and time behind the pickers and
their labels — are bound to /planning/FIELD in the surface data
document, which `grove-document-planning-data' builds."
  (jetpacs-card
   (jetpacs-column
    (jetpacs-text label :style "title")
    (jetpacs-text-input
     (concat "grove-" field)
     :value (grove-document--planning-ref field "value")
     :label (concat label " timestamp or shorthand")
     :hint "fri 10:00 ++1w" :single-line t :max-length 160)
    (jetpacs-flow-row
     (jetpacs-date-button
      (grove-document--planning-ref field "date_label")
      (jetpacs-action "grove.planning.pick"
                      :args (list :field field :kind "date")
                      :capture-fields grove-document--planning-fields)
      :value (grove-document--planning-ref field "date"))
     (jetpacs-time-button
      (grove-document--planning-ref field "time_label")
      (jetpacs-action "grove.planning.pick"
                      :args (list :field field :kind "time")
                      :capture-fields grove-document--planning-fields)
      :value (grove-document--planning-ref field "time")
      :display-mode "switchable")
     (jetpacs-button
      "Clear"
      (jetpacs-action "grove.planning.clear"
                      :args (list :field field)
                      :capture-fields grove-document--planning-fields)
      :variant "text"))
    :spacing 8)
   :variant "outlined"))

(defun grove-document--planning-target-button (field)
  "Build the planning target button for FIELD.
Its label and its focus variant are bound to /planning/FIELD."
  (jetpacs-button
   (grove-document--planning-ref field "target_label")
   (jetpacs-action
    "grove.planning.focus" :args (list :field field)
    :capture-fields grove-document--planning-fields)
   :icon (if (equal field "deadline") "event_busy" "schedule")
   :variant (grove-document--planning-ref field "target_variant")))

(defun grove-document--planning-preset-dates (today)
  "Return Grove quick-pick label/date pairs relative to ISO TODAY."
  (when (grove-org-date-valid-p today)
    (let* ((parts (grove-org--date-parts today))
           (weekday (calendar-day-of-week parts))
           (weekend-delta (mod (- 6 weekday) 7))
           (monday-delta (mod (- 1 weekday) 7)))
      (when (= monday-delta 0) (setq monday-delta 7))
      (list (cons "Today" today)
            (cons "Tomorrow" (grove-org-date-shift today 1))
            (cons "This weekend"
                  (grove-org-date-shift today weekend-delta))
            (cons "Next week"
                  (grove-org-date-shift today monday-delta))))))

(defun grove-document--planning-preset-row (today)
  "Build quick date picks relative to TODAY for the focused target."
  (apply
   #'jetpacs-flow-row
   (append
    (mapcar
     (lambda (preset)
       (jetpacs-button
        (car preset)
        (jetpacs-action
         "grove.planning.pick"
         :args (list :kind "date" :value (cdr preset))
         :capture-fields grove-document--planning-fields)
        :variant "tonal" :size "small"))
     (grove-document--planning-preset-dates today))
    '(:spacing 6 :run-spacing 6))))

(defun grove-document--planning-day-styles (scheduled deadline)
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

(defun grove-document--planning-calendar-marks
    (counts scheduled deadline styled)
  "Build month marks from COUNTS and planning endpoints.
When STYLED is nil, SCHEDULED and DEADLINE colors are folded into the mark
fallback so an older member profile still distinguishes the two targets."
  (let ((dots (make-hash-table :test #'equal))
        (colors (make-hash-table :test #'equal)))
    (dolist (mark counts)
      (when (and (grove-org-date-valid-p (car mark))
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

(defun grove-document--planning-feedback (kind)
  "Build the feedback node of KIND (`grove-document--planning-feedback-kind').
Its words and colors are bound to /planning/feedback; the runway's
suggestion is the handler's to compute when its button is tapped."
  (pcase kind
    ('lead-time
     (jetpacs-card
      (jetpacs-row
       (jetpacs-icon (grove-document--planning-ref "feedback" "icon")
                     :color (grove-document--planning-ref
                             "feedback" "icon_color"))
       (jetpacs-with-attrs
        (jetpacs-text
         (grove-document--planning-ref "feedback" "text")
         :style "caption"
         :color (grove-document--planning-ref "feedback" "text_color"))
        :weight 1)
       :spacing 8 :align "center")
      :variant "outlined"))
    ('runway
     (jetpacs-card
      (jetpacs-column
       (jetpacs-text "Give yourself a runway" :style "label")
       (jetpacs-text (grove-document--planning-ref "feedback" "text")
                     :style "caption")
       (jetpacs-button
        "Use suggested date"
        (jetpacs-action
         "grove.planning.suggest"
         :capture-fields grove-document--planning-fields)
        :icon "schedule" :variant "tonal" :size "small")
       :spacing 6)
      :variant "outlined"))
    ('prompt
     (jetpacs-text "Choose a deadline to see the available lead time."
                   :style "caption"))))

(defun grove-document--planning-styled-p ()
  "Whether the live profile advertises month_grid day styles (SPEC 12)."
  (jetpacs-node-member-advertised-p "month_grid" "day_styles" :app))

(defun grove-document--planning-calendar (kind today styled)
  "Build Grove's calendar with feedback KIND and quick picks from TODAY.
STYLED binds day styles, which only an advertising profile may carry.
The month, its marks and styles, the selection or range, and the target
buttons' labels and variants are all bound to /planning, so a tap
changes the document and never this tree."
  (jetpacs-card
   (jetpacs-column
    (jetpacs-flow-row
     (grove-document--planning-target-button "scheduled")
     (grove-document--planning-target-button "deadline")
     (jetpacs-button
      "Clear both"
      (jetpacs-action
       "grove.planning.clear" :args '(:field "both")
       :capture-fields grove-document--planning-fields)
      :variant "text" :size "small")
     :spacing 6 :run-spacing 6)
    (jetpacs-month-grid
     (grove-document--planning-ref "month")
     :marks (grove-document--planning-ref "marks")
     :day-styles (and styled (grove-document--planning-ref "day_styles"))
     :selected (grove-document--planning-ref "selected")
     :range-start (grove-document--planning-ref "range_start")
     :range-end (grove-document--planning-ref "range_end")
     :on-day-tap
     (jetpacs-action
      "grove.planning.pick" :args '(:kind "date")
      :capture-fields grove-document--planning-fields)
     :on-month-change
     (jetpacs-action
      "grove.planning.month"
      :capture-fields grove-document--planning-fields)
     :children
     (list (jetpacs-text
            "Use the date controls below if this renderer has no calendar."
            :style "caption")))
    (grove-document--planning-preset-row today)
    (grove-document--planning-feedback kind)
    :spacing 10)
   :variant "outlined"))

(defun grove-document--planning-field-data (field label focus)
  "The /planning/FIELD object: LABEL's draft, its pickers, its FOCUS state."
  (let* ((value (plist-get grove-document--planning
                           (if (equal field "deadline") :deadline :scheduled)))
         (date (grove-document--planning-date value))
         (raw-time (ebp-org-ts-time value))
         (time (and (grove-document--planning-time-valid-p raw-time)
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

(defun grove-document--planning-feedback-data (kind scheduled-date deadline-date)
  "The /planning/feedback object for KIND over SCHEDULED-DATE and DEADLINE-DATE."
  (pcase kind
    ('lead-time
     (let ((days (grove-org--date-delta deadline-date scheduled-date)))
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
                   (grove-org-date-shift deadline-date -3))))))

(defun grove-document-planning-data ()
  "The /planning object of Grove's surface data document (SPEC 13.8).
A pure projection of `grove-document--planning' and the live member
profile — the same inputs the planning screen's builders read — so a
snapshot and a patch always agree.  Nil without an open draft."
  (when grove-document--planning
    (let* ((scheduled (plist-get grove-document--planning :scheduled))
           (deadline (plist-get grove-document--planning :deadline))
           (focus (or (plist-get grove-document--planning :focus)
                      "scheduled"))
           (today (plist-get grove-document--planning :today))
           (month (or (plist-get grove-document--planning :month)
                      (grove-document--planning-month-for
                       scheduled deadline focus today)))
           (scheduled-date (grove-document--planning-date scheduled))
           (deadline-date (grove-document--planning-date deadline))
           (styled (grove-document--planning-styled-p))
           (chronological
            (and scheduled-date deadline-date
                 (not (string> scheduled-date deadline-date))))
           (selected
            (unless chronological
              (if (equal focus "deadline")
                  (or deadline-date scheduled-date)
                (or scheduled-date deadline-date))))
           (marks (jetpacs-month-marks
                   (grove-document--planning-calendar-marks
                    (plist-get grove-document--planning :marks)
                    scheduled-date deadline-date styled)))
           (styles (and styled
                        (jetpacs-month-day-styles
                         (grove-document--planning-day-styles
                          scheduled-date deadline-date))))
           (feedback (grove-document--planning-feedback-data
                      (grove-document--planning-feedback-kind
                       scheduled deadline focus)
                      scheduled-date deadline-date)))
      (append
       (list :month month
             :scheduled (grove-document--planning-field-data
                         "scheduled" "Scheduled" focus)
             :deadline (grove-document--planning-field-data
                        "deadline" "Deadline" focus))
       (and marks (list :marks marks))
       (and styles (list :day_styles styles))
       (and selected (list :selected selected))
       (and chronological
            (list :range_start scheduled-date :range_end deadline-date))
       (and feedback (list :feedback feedback))))))

(defun grove-document-surface-data ()
  "Grove's app-surface data document (SPEC 13.8).
The planning object while a draft is open; nil otherwise, which the
shell sends as the empty document."
  (when-let* ((planning (grove-document-planning-data)))
    (list :planning planning)))

(defun grove-document-planning-screen (back)
  "Build Grove's shared scheduled/deadline planning canvas with BACK.
The tree is shaped by the draft's feedback kind alone; every value it
shows is bound to the surface data document
\(`grove-document-surface-data'), so a picker tap patches the document
and the Companion re-materializes this screen without a new snapshot."
  (if (null grove-document--planning)
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
      (grove-document--planning-calendar
       (grove-document--planning-shape)
       (plist-get grove-document--planning :today)
       (grove-document--planning-styled-p))
      (grove-document--planning-editor "scheduled" "Scheduled")
      (grove-document--planning-editor "deadline" "Deadline")
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
        :capture-fields grove-document--planning-fields
        :when-offline 'queue :ttl-s 86400
        :dedupe (jetpacs-wire-id
                 "grove-planning"
                 (plist-get grove-document--planning :heading-id)))
       :icon "done" :variant "filled")
      :spacing 10 :content-padding 12)
     :back back)))

(defun grove-document-presented-planning-screen (back)
  "Build the profile-selected planning canvas with BACK."
  (grove-design-present-if-advertised
   (grove-document-planning-screen back) 'planning))

(defconst grove-document--verbs
  '("grove.document.open" "grove.document.mode" "grove.document.save"
    "grove.document.todo" "grove.document.planning"
    "grove.planning.pick" "grove.planning.suggest" "grove.planning.focus"
    "grove.planning.month" "grove.planning.clear" "grove.planning.save")
  "Action names owned by Grove's document module.")

(defun grove-document-register ()
  "Register Grove's document and planning actions idempotently."
  (with-jetpacs-owner grove-owner
                      (jetpacs-defaction "grove.document.open" #'grove-document--on-open)
                      (jetpacs-defaction "grove.document.mode" #'grove-document--on-mode)
                      (jetpacs-defaction "grove.document.save" #'grove-document--on-save)
                      (jetpacs-defaction "grove.document.todo" #'grove-document--on-todo)
                      (jetpacs-defaction "grove.document.planning"
                                         #'grove-document--open-planning
                                         :args '((:name notebook-id :type "ref" :required t)
                                                 (:name heading-id :type "ref" :required t)
                                                 (:name focus :type "enum"))
                                         :doc "Open Grove's paired Org planning calendar")
                      (jetpacs-defaction "grove.planning.pick"
                                         #'grove-document--on-planning-pick
                                         :args '((:name field :type "enum")
                                                 (:name kind :type "enum" :required t)
                                                 (:name value :type "text" :required t))
                                         :doc "Apply a validated date or time to one planning target, the focused one by default")
                      (jetpacs-defaction "grove.planning.suggest"
                                         #'grove-document--on-planning-suggest
                                         :doc "Schedule the draft three days before its captured deadline")
                      (jetpacs-defaction "grove.planning.focus"
                                         #'grove-document--on-planning-focus
                                         :args '((:name field :type "enum" :required t))
                                         :doc "Choose which planning target receives calendar taps")
                      (jetpacs-defaction "grove.planning.month"
                                         #'grove-document--on-planning-month
                                         :args '((:name value :type "month" :required t))
                                         :doc "Browse Grove's planning calendar to a month")
                      (jetpacs-defaction "grove.planning.clear"
                                         #'grove-document--on-planning-clear
                                         :args '((:name field :type "enum" :required t))
                                         :doc "Clear one or both captured planning values")
                      (jetpacs-defaction "grove.planning.save"
                                         #'grove-document--on-planning-save
                                         :args '((:name notebook-id :type "ref" :required t)
                                                 (:name heading-id :type "ref" :required t)
                                                 (:name revision :type "text" :required t))
                                         :doc "Atomically commit both Org planning values"))
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
