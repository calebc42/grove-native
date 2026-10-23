;;; bloom-structure.el --- Bloom outline manipulation policy -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Bloom policy over Jetpacs' reusable Org structure sessions.  Every wire
;; action carries only opaque notebook/heading ids plus a subtree revision.
;; Refs, paths, clipboard contents, undo records, and refile destinations stay
;; private to Emacs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ebp-org)
(require 'jetpacs-apps)
(require 'jetpacs-dialog)
(require 'jetpacs-org-structure)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'jetpacs-widgets)
(require 'bloom-core)
(require 'bloom-library)

(defconst bloom-structure--action-ttl-seconds 86400
  "Lifetime of a queued Bloom outline action.")

(defvar bloom-structure--expanded (make-hash-table :test #'equal)
  "Opaque notebook id to non-nil when all outline groups start expanded.")

(defvar bloom-structure--last-refile-destination nil
  "Last private refile destination as opaque notebook and heading ids.")

(defvar bloom-structure--undo-notebooks nil
  "Opaque notebook ids changed by Bloom's one live undo receipt.")

(defconst bloom-structure--verbs
  '("bloom.outline.mutate" "bloom.outline.undo"
    "bloom.outline.widen" "bloom.outline.expansion")
  "Action names owned by Bloom outline structure policy.")

(defun bloom-structure--identity-args (notebook-id record operation)
  "Return opaque NOTEBOOK-ID/RECORD arguments for OPERATION."
  (list :notebook-id notebook-id
        :heading-id (plist-get record :id)
        :revision (plist-get record :revision)
        :operation operation))

(defun bloom-structure-action
    (notebook-id record operation &optional confirm)
  "Return a durable Bloom OPERATION action for NOTEBOOK-ID and RECORD.
CONFIRM, when non-nil, is the platform confirmation text."
  (jetpacs-action
   "bloom.outline.mutate"
   :args (bloom-structure--identity-args notebook-id record operation)
   :when-offline (jetpacs-durable-offline-policy)
   :ttl-s bloom-structure--action-ttl-seconds
   :confirm confirm))

(defun bloom-structure-swipe-start (notebook-id record)
  "Return NOTEBOOK-ID and RECORD's four-action reveal-only leading swipe."
  (jetpacs-swipe
   (list
    (jetpacs-swipe-action
     "Above" :icon "vertical_align_top" :color "secondary"
     :on-trigger (bloom-structure-action notebook-id record "insert-before"))
    (jetpacs-swipe-action
     "Below" :icon "vertical_align_bottom" :color "secondary"
     :on-trigger (bloom-structure-action notebook-id record "insert-after"))
    (jetpacs-swipe-action
     "Child" :icon "subdirectory_arrow_right" :color "tertiary"
     :on-trigger (bloom-structure-action notebook-id record "insert-child"))
    (jetpacs-swipe-action
     "Refile" :icon "drive_file_move" :color "tertiary"
     :on-trigger (bloom-structure-action notebook-id record "refile")))))

(defun bloom-structure-menu (notebook-id record)
  "Return NOTEBOOK-ID and RECORD's complete outline operation menu."
  (let ((paste-ready
         (and (jetpacs-org-structure-clipboard-summary bloom-owner) t)))
    (jetpacs-with-semantics
     (jetpacs-menu
      nil :icon "more_vert"
      :groups
      (list
       (jetpacs-menu-group
        "Add"
        (list
         (jetpacs-menu-item
          "Insert above"
          (bloom-structure-action notebook-id record "insert-before")
          :icon "vertical_align_top")
         (jetpacs-menu-item
          "Insert below"
          (bloom-structure-action notebook-id record "insert-after")
          :icon "vertical_align_bottom")
         (jetpacs-menu-item
          "Insert child"
          (bloom-structure-action notebook-id record "insert-child")
          :icon "subdirectory_arrow_right")
         (jetpacs-menu-item
          "Refile…" (bloom-structure-action notebook-id record "refile")
          :icon "drive_file_move")))
       (jetpacs-menu-group
        "Arrange"
        (list
         (jetpacs-menu-item
          "Move up" (bloom-structure-action notebook-id record "move-up")
          :icon "arrow_upward")
         (jetpacs-menu-item
          "Move down" (bloom-structure-action notebook-id record "move-down")
          :icon "arrow_downward")
         (jetpacs-menu-item
          "Promote" (bloom-structure-action notebook-id record "promote")
          :icon "keyboard_double_arrow_left")
         (jetpacs-menu-item
          "Demote" (bloom-structure-action notebook-id record "demote")
          :icon "keyboard_double_arrow_right")
         (jetpacs-menu-item
          "Duplicate" (bloom-structure-action notebook-id record "duplicate")
          :icon "content_copy")))
       (jetpacs-menu-group
        "Clipboard"
        (list
         (jetpacs-menu-item
          "Copy" (bloom-structure-action notebook-id record "copy")
          :icon "content_copy")
         (jetpacs-menu-item
          "Cut" (bloom-structure-action notebook-id record "cut")
          :icon "content_cut")
         (jetpacs-menu-item
          "Paste under" (bloom-structure-action notebook-id record "paste")
          :icon "content_paste"
          :enabled (if paste-ready t :json-false))))
       (jetpacs-menu-group
        "Focus"
        (list
         (jetpacs-menu-item
          "Narrow to subtree"
          (bloom-structure-action notebook-id record "narrow")
          :icon "filter_center_focus")))
       (jetpacs-menu-group
        "Remove"
        (list
         (jetpacs-menu-item
          "Archive"
          (bloom-structure-action
           notebook-id record "archive"
           "Archive this heading and its complete subtree?")
          :icon "archive")
         (jetpacs-menu-item
          "Delete"
          (bloom-structure-action
           notebook-id record "delete"
           "Delete this heading and its complete subtree?")
          :icon "delete")))))
     :name "Heading actions")))

(defun bloom-structure--focused-ref (notebook-id)
  "Return NOTEBOOK-ID's live private focus ref, or nil."
  (jetpacs-org-structure-focus-current
   bloom-owner (concat "outline:" notebook-id)))

(defun bloom-structure-outline-slice (notebook-id records)
  "Return NOTEBOOK-ID's focus-aware RECORDS and focus metadata.
The result is `(:records ROWS :focused BOOL :focus-id ID-OR-NIL)'."
  (let* ((focus-ref (bloom-structure--focused-ref notebook-id))
         (focus-id (and focus-ref (bloom-library--heading-id focus-ref)))
         (focus-record
          (and focus-id
               (cl-find focus-id records
                        :key (lambda (record) (plist-get record :id))
                        :test #'equal))))
    (if (null focus-record)
        (progn
          (when focus-ref
            (jetpacs-org-structure-focus-clear
             bloom-owner (concat "outline:" notebook-id)))
          (list :records records :focused nil :focus-id nil))
      (let ((level (plist-get focus-record :level))
            (collect nil)
            result)
        (dolist (record records)
          (cond
           ((equal (plist-get record :id) focus-id)
            (setq collect t)
            (push (plist-put (copy-tree record) :parent-id nil) result))
           ((and collect (> (plist-get record :level) level))
            (push record result))
           (collect (setq collect nil))))
        (list :records (nreverse result) :focused t :focus-id focus-id)))))

(defun bloom-structure-expanded-p (notebook-id)
  "Return non-nil when NOTEBOOK-ID's outline starts fully expanded."
  (and (gethash notebook-id bloom-structure--expanded) t))

(defun bloom-structure-collapsed-value (notebook-id)
  "Return the authored collapsible state for NOTEBOOK-ID."
  (if (bloom-structure-expanded-p notebook-id) :json-false t))

(defun bloom-structure-expansion-action (notebook-id expanded)
  "Return an action setting NOTEBOOK-ID's all-expanded state to EXPANDED."
  (jetpacs-action
   "bloom.outline.expansion"
   :args (list :notebook-id notebook-id
               :expanded (if expanded t :json-false))))

(defun bloom-structure-widen-action (notebook-id)
  "Return an action clearing NOTEBOOK-ID's subtree focus."
  (jetpacs-action "bloom.outline.widen"
                  :args (list :notebook-id notebook-id)))

(defun bloom-structure-undo-action ()
  "Return Bloom's session-only one-level outline undo action."
  (jetpacs-action "bloom.outline.undo"))

(defun bloom-structure--context (args params)
  "Resolve and revision-check opaque outline ARGS under event PARAMS."
  (let ((notebook-id (plist-get args :notebook-id))
        (heading-id (plist-get args :heading-id))
        (revision (plist-get args :revision)))
    (when (and params (jetpacs-event-stale-p params))
      (signal 'ebp-org-unresolved nil))
    (unless (and (stringp notebook-id) (stringp heading-id)
                 (stringp revision))
      (signal 'ebp-org-refused (list 'invalid-identity)))
    (let ((ref (or (bloom-library-resolve-heading notebook-id heading-id)
                   (signal 'ebp-org-unresolved nil))))
      (unless (equal revision (ebp-org-subtree-revision ref))
        (signal 'ebp-org-unresolved nil))
      (list :notebook-id notebook-id :heading-id heading-id
            :revision revision :ref ref))))

(defun bloom-structure--recheck-context (context)
  "Resolve CONTEXT again against its originally rendered revision."
  (bloom-structure--context
   (list :notebook-id (plist-get context :notebook-id)
         :heading-id (plist-get context :heading-id)
         :revision (plist-get context :revision))
   nil))

(defun bloom-structure--refresh-notebooks (notebook-ids params)
  "Refresh opaque NOTEBOOK-IDS and defer PARAMS' surface presentation."
  (dolist (notebook-id (delete-dups (delq nil (copy-sequence notebook-ids))))
    (when (bloom-library-resolve-notebook notebook-id)
      (bloom-library-outline-refresh notebook-id)))
  (jetpacs-app-defer-refresh params))

(defun bloom-structure--undo-now (surface)
  "Undo Bloom's current structural receipt and refresh it on SURFACE."
  (condition-case err
      (let* ((notebooks (copy-sequence bloom-structure--undo-notebooks))
             (outcome (jetpacs-org-structure-undo bloom-owner 'bloom)))
        (setq bloom-structure--undo-notebooks nil)
        (dolist (notebook-id notebooks)
          (when (bloom-library-resolve-notebook notebook-id)
            (bloom-library-outline-refresh notebook-id)))
        (jetpacs-shell-notify
         (if (plist-get outcome :pending-save)
             "Undo restored in Emacs; save pending"
           "Undone")
         surface)
        (jetpacs-shell-push surface)
        t)
    (ebp-org-unresolved
     (setq bloom-structure--undo-notebooks nil)
     (jetpacs-shell-notify "Undo expired after another edit" surface)
     nil)
    (error
     (message "bloom: outline undo failed (%s)" (car err))
     (jetpacs-shell-notify "Undo could not be applied" surface)
     nil)))

(defun bloom-structure--finish-mutation
    (label notebook-ids params &optional offer-undo)
  "Finish LABEL mutation across NOTEBOOK-IDS for event PARAMS.
When OFFER-UNDO is non-nil, expose the platform's one live receipt."
  (bloom-structure--refresh-notebooks notebook-ids params)
  (if (and offer-undo
           (jetpacs-org-structure-undo-summary bloom-owner))
      (let ((surface (or (plist-get params :surface) bloom-app-surface)))
        (setq bloom-structure--undo-notebooks
              (delete-dups (delq nil (copy-sequence notebook-ids))))
        (jetpacs-shell-notify
         label surface :action-label "Undo" :duration "long"
         :on-action (lambda () (bloom-structure--undo-now surface))))
    (setq bloom-structure--undo-notebooks nil)
    (jetpacs-shell-notify label (plist-get params :surface)))
  'accepted)

(defun bloom-structure--simple-mutation (operation context params)
  "Apply non-prompting OPERATION using private CONTEXT and event PARAMS."
  (let ((notebook-id (plist-get context :notebook-id))
        (ref (plist-get context :ref)))
    (pcase operation
      ("move-up"
       (jetpacs-org-structure-move bloom-owner ref 'bloom 'up)
       (bloom-structure--finish-mutation
        "Heading moved" (list notebook-id) params t))
      ("move-down"
       (jetpacs-org-structure-move bloom-owner ref 'bloom 'down)
       (bloom-structure--finish-mutation
        "Heading moved" (list notebook-id) params t))
      ("promote"
       (jetpacs-org-structure-promote bloom-owner ref 'bloom)
       (bloom-structure--finish-mutation
        "Heading promoted" (list notebook-id) params t))
      ("demote"
       (jetpacs-org-structure-demote bloom-owner ref 'bloom)
       (bloom-structure--finish-mutation
        "Heading demoted" (list notebook-id) params t))
      ("duplicate"
       (jetpacs-org-structure-duplicate bloom-owner ref 'bloom)
       (bloom-structure--finish-mutation
        "Heading duplicated" (list notebook-id) params t))
      ("delete"
       (jetpacs-org-structure-delete bloom-owner ref 'bloom)
       (bloom-structure--finish-mutation
        "Heading deleted" (list notebook-id) params t))
      ("archive"
       (jetpacs-org-structure-archive bloom-owner ref 'bloom)
       (bloom-structure--finish-mutation
        "Heading archived" (list notebook-id) params nil))
      ("copy"
       (jetpacs-org-structure-copy bloom-owner ref)
       (jetpacs-app-defer-refresh params)
       (jetpacs-shell-notify "Copied privately" (plist-get params :surface))
       'accepted)
      ("cut"
       (jetpacs-org-structure-cut bloom-owner ref)
       (jetpacs-app-defer-refresh params)
       (jetpacs-shell-notify "Cut armed; nothing moved yet"
                             (plist-get params :surface))
       'accepted)
      ("paste"
       (let ((file (or (bloom-library-resolve-notebook notebook-id)
                       (signal 'ebp-org-unresolved nil))))
         (jetpacs-org-structure-paste
          bloom-owner (list :file file :parent ref) 'bloom)
         (bloom-structure--finish-mutation
          "Heading pasted" (list notebook-id) params t)))
      ("narrow"
       (jetpacs-org-structure-focus-set
        bloom-owner (concat "outline:" notebook-id) ref)
       (jetpacs-app-defer-refresh params)
       'accepted)
      (_ 'rejected))))

(defun bloom-structure--prompt-title (operation context params)
  "Prompt for OPERATION's title, then mutate private CONTEXT for PARAMS."
  (let ((surface (or (plist-get params :surface) bloom-app-surface)))
    (if (not (jetpacs-dialog-can-bridge-p))
        (jetpacs-shell-notify "Heading dialog is unavailable" surface)
      (let ((title (string-trim
                    (condition-case nil
                        (read-string "Heading title: ")
                      (quit "")))))
        (unless (string-empty-p title)
          (condition-case err
              (let* ((fresh (bloom-structure--recheck-context context))
                     (relation
                      (pcase operation
                        ("insert-before" 'before)
                        ("insert-after" 'after)
                        (_ 'child)))
                     (notebook-id (plist-get fresh :notebook-id)))
                (jetpacs-org-structure-insert
                 bloom-owner (plist-get fresh :ref) 'bloom relation title)
                (bloom-structure--finish-mutation
                 "Heading inserted" (list notebook-id) params t))
            ((ebp-org-refused ebp-org-unresolved)
             (jetpacs-shell-notify
              "Heading changed before insertion" surface))
            (error
             (message "bloom: heading insertion failed (%s)" (car err))
             (jetpacs-shell-notify "Heading was not inserted" surface))))))))

(defun bloom-structure--notebook-choices ()
  "Return deterministic refile notebook display choices."
  (mapcar
   (lambda (record)
     (cons (format "%s — %s"
                   (plist-get record :folder-label)
                   (plist-get record :file-label))
           (plist-get record :id)))
   (plist-get (bloom-library-snapshot) :notebooks)))

(defun bloom-structure--heading-choices (notebook-id)
  "Return hierarchical refile choices for opaque NOTEBOOK-ID."
  (let* ((records
          (plist-get (bloom-library-outline-refresh notebook-id) :records))
         (by-id (make-hash-table :test #'equal)))
    (dolist (record records)
      (puthash (plist-get record :id) record by-id))
    (cons
     (cons "Top of notebook" "top")
     (cl-loop
      for record in records
      for index from 1
      collect
      (let ((cursor record)
            labels)
        (while cursor
          (push (plist-get cursor :title) labels)
          (setq cursor (gethash (plist-get cursor :parent-id) by-id)))
        (cons (format "%s · %d" (string-join labels " › ") index)
              (plist-get record :id)))))))

(defun bloom-structure--last-destination-valid-p ()
  "Return non-nil when the private last refile destination still resolves."
  (let ((notebook-id
         (plist-get bloom-structure--last-refile-destination :notebook-id))
        (heading-id
         (plist-get bloom-structure--last-refile-destination :heading-id)))
    (and notebook-id
         (bloom-library-resolve-notebook notebook-id)
         (or (equal heading-id "top")
             (bloom-library-resolve-heading notebook-id heading-id)))))

(defun bloom-structure--choose-refile-destination ()
  "Prompt for and return a private refile destination, `archive', or nil."
  (let* ((notebook-choices
          (append
           (when (bloom-structure--last-destination-valid-p)
             (list (cons "Last destination" "__last__")))
           (list (cons "Archive instead" "__archive__"))
           (bloom-structure--notebook-choices)))
         (notebook-label
          (condition-case nil
              (completing-read "Refile to notebook: "
                               notebook-choices nil t)
            (quit "")))
         (selection (cdr (assoc notebook-label notebook-choices))))
    (cond
     ((or (null selection) (string-empty-p selection)) nil)
     ((equal selection "__archive__") 'archive)
     ((equal selection "__last__")
      (copy-tree bloom-structure--last-refile-destination))
     (t
      (let* ((heading-choices
              (bloom-structure--heading-choices selection))
             (heading-label
              (condition-case nil
                  (completing-read "Refile under heading: "
                                   heading-choices nil t)
                (quit "")))
             (heading-id (cdr (assoc heading-label heading-choices))))
        (when heading-id
          (list :notebook-id selection :heading-id heading-id)))))))

(defun bloom-structure--prompt-refile (context params)
  "Prompt for a destination, then refile private CONTEXT for PARAMS."
  (let ((surface (or (plist-get params :surface) bloom-app-surface)))
    (if (not (jetpacs-dialog-can-bridge-p))
        (jetpacs-shell-notify "Refile dialog is unavailable" surface)
      (let ((destination (bloom-structure--choose-refile-destination)))
        (when destination
          (condition-case err
              (let* ((fresh (bloom-structure--recheck-context context))
                     (source-notebook (plist-get fresh :notebook-id))
                     (ref (plist-get fresh :ref)))
                (if (eq destination 'archive)
                    (progn
                      (jetpacs-org-structure-archive bloom-owner ref 'bloom)
                      (bloom-structure--finish-mutation
                       "Heading archived" (list source-notebook) params nil))
                  (let* ((target-notebook
                          (plist-get destination :notebook-id))
                         (heading-id (plist-get destination :heading-id))
                         (file
                          (or (bloom-library-resolve-notebook target-notebook)
                              (signal 'ebp-org-unresolved nil)))
                         (parent
                          (unless (equal heading-id "top")
                            (or (bloom-library-resolve-heading
                                 target-notebook heading-id)
                                (signal 'ebp-org-unresolved nil)))))
                    (jetpacs-org-structure-transfer
                     bloom-owner ref (list :file file :parent parent)
                     'bloom 'move)
                    (setq bloom-structure--last-refile-destination
                          (copy-tree destination))
                    (bloom-structure--finish-mutation
                     "Heading refiled"
                     (list source-notebook target-notebook) params t))))
            ((ebp-org-refused ebp-org-unresolved)
             (jetpacs-shell-notify
              "Heading or destination changed before refile" surface))
            (error
             (message "bloom: refile failed (%s)" (car err))
             (jetpacs-shell-notify "Heading was not refiled" surface))))))))

(defun bloom-structure--on-mutate (args params)
  "Apply ARGS' revision-bound outline operation for event PARAMS."
  (let ((operation (plist-get args :operation)))
    (if (not (member operation
                     '("insert-before" "insert-after" "insert-child"
                       "refile" "move-up" "move-down" "promote"
                       "demote" "duplicate" "delete" "archive"
                       "copy" "cut" "paste" "narrow")))
        'rejected
      (condition-case err
          (let ((context (bloom-structure--context args params)))
            (cond
             ((member operation
                      '("insert-before" "insert-after" "insert-child"))
              (jetpacs-flow-continue
               (lambda ()
                 (bloom-structure--prompt-title operation context params)))
              'accepted)
             ((equal operation "refile")
              (jetpacs-flow-continue
               (lambda () (bloom-structure--prompt-refile context params)))
              'accepted)
             (t (bloom-structure--simple-mutation
                 operation context params))))
        (ebp-org-unresolved 'stale)
        (ebp-org-refused 'rejected)
        (ebp-org-unavailable (jetpacs-retry-later))
        (error
         (message "bloom: outline operation failed (%s)" (car err))
         'rejected)))))

(defun bloom-structure--on-undo (_args params)
  "Undo Bloom's current structural receipt for event PARAMS."
  (if (jetpacs-event-stale-p params) 'stale
    (if (bloom-structure--undo-now
         (or (plist-get params :surface) bloom-app-surface))
        'accepted
      'stale)))

(defun bloom-structure--on-widen (args params)
  "Clear ARGS' notebook focus and refresh event PARAMS."
  (let ((notebook-id (plist-get args :notebook-id)))
    (if (not (and (stringp notebook-id)
                  (bloom-library-resolve-notebook notebook-id)))
        'stale
      (jetpacs-org-structure-focus-clear
       bloom-owner (concat "outline:" notebook-id))
      (jetpacs-app-defer-refresh params)
      'accepted)))

(defun bloom-structure--on-expansion (args params)
  "Set ARGS' whole-outline expansion preference for event PARAMS."
  (let ((notebook-id (plist-get args :notebook-id))
        (expanded (plist-get args :expanded)))
    (if (not (and (stringp notebook-id)
                  (memq expanded '(t :json-false))
                  (bloom-library-resolve-notebook notebook-id)))
        'rejected
      (puthash notebook-id (eq expanded t) bloom-structure--expanded)
      (jetpacs-app-defer-refresh params)
      'accepted)))

(defun bloom-structure-register ()
  "Register Bloom's structural outline actions idempotently."
  (with-jetpacs-owner bloom-owner
                      (jetpacs-defaction
                       "bloom.outline.mutate" #'bloom-structure--on-mutate
                       :doc "Apply one revision-bound Bloom outline operation")
                      (jetpacs-defaction
                       "bloom.outline.undo" #'bloom-structure--on-undo
                       :doc "Consume Bloom's session-only one-level structural undo")
                      (jetpacs-defaction
                       "bloom.outline.widen" #'bloom-structure--on-widen
                       :doc "Clear Bloom's presentation-only subtree focus")
                      (jetpacs-defaction
                       "bloom.outline.expansion" #'bloom-structure--on-expansion
                       :doc "Expand or collapse every Bloom outline group"))
  t)

(defun bloom-structure-unregister ()
  "Unregister Bloom outline actions and clear all session-only state."
  (dolist (verb bloom-structure--verbs) (jetpacs-undefaction verb))
  (clrhash bloom-structure--expanded)
  (setq bloom-structure--last-refile-destination nil
        bloom-structure--undo-notebooks nil)
  (jetpacs-org-structure-reset bloom-owner)
  t)

(provide 'bloom-structure)
;;; bloom-structure.el ends here
