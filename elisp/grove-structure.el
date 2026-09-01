;;; grove-structure.el --- Grove outline manipulation policy -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Grove policy over Jetpacs' reusable Org structure sessions.  Every wire
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
(require 'grove-core)
(require 'grove-library)

(defconst grove-structure--action-ttl-seconds 86400
  "Lifetime of a queued Grove outline action.")

(defvar grove-structure--expanded (make-hash-table :test #'equal)
  "Opaque notebook id to non-nil when all outline groups start expanded.")

(defvar grove-structure--last-refile-destination nil
  "Last private refile destination as opaque notebook and heading ids.")

(defvar grove-structure--undo-notebooks nil
  "Opaque notebook ids changed by Grove's one live undo receipt.")

(defconst grove-structure--verbs
  '("grove.outline.mutate" "grove.outline.undo"
    "grove.outline.widen" "grove.outline.expansion")
  "Action names owned by Grove outline structure policy.")

(defun grove-structure--identity-args (notebook-id record operation)
  "Return opaque NOTEBOOK-ID/RECORD arguments for OPERATION."
  (list :notebook-id notebook-id
        :heading-id (plist-get record :id)
        :revision (plist-get record :revision)
        :operation operation))

(defun grove-structure-action
    (notebook-id record operation &optional confirm)
  "Return a durable Grove OPERATION action for NOTEBOOK-ID and RECORD.
CONFIRM, when non-nil, is the platform confirmation text."
  (jetpacs-action
   "grove.outline.mutate"
   :args (grove-structure--identity-args notebook-id record operation)
   :when-offline (jetpacs-durable-offline-policy)
   :ttl-s grove-structure--action-ttl-seconds
   :confirm confirm))

(defun grove-structure-swipe-start (notebook-id record)
  "Return NOTEBOOK-ID and RECORD's four-action reveal-only leading swipe."
  (jetpacs-swipe
   (list
    (jetpacs-swipe-action
     "Above" :icon "vertical_align_top" :color "secondary"
     :on-trigger (grove-structure-action notebook-id record "insert-before"))
    (jetpacs-swipe-action
     "Below" :icon "vertical_align_bottom" :color "secondary"
     :on-trigger (grove-structure-action notebook-id record "insert-after"))
    (jetpacs-swipe-action
     "Child" :icon "subdirectory_arrow_right" :color "tertiary"
     :on-trigger (grove-structure-action notebook-id record "insert-child"))
    (jetpacs-swipe-action
     "Refile" :icon "drive_file_move" :color "tertiary"
     :on-trigger (grove-structure-action notebook-id record "refile")))))

(defun grove-structure-menu (notebook-id record)
  "Return NOTEBOOK-ID and RECORD's complete outline operation menu."
  (let ((paste-ready
         (and (jetpacs-org-structure-clipboard-summary grove-owner) t)))
    (jetpacs-menu
     nil :icon "more_vert"
     :groups
     (list
      (jetpacs-menu-group
       "Add"
       (list
        (jetpacs-menu-item
         "Insert above"
         (grove-structure-action notebook-id record "insert-before")
         :icon "vertical_align_top")
        (jetpacs-menu-item
         "Insert below"
         (grove-structure-action notebook-id record "insert-after")
         :icon "vertical_align_bottom")
        (jetpacs-menu-item
         "Insert child"
         (grove-structure-action notebook-id record "insert-child")
         :icon "subdirectory_arrow_right")
        (jetpacs-menu-item
         "Refile…" (grove-structure-action notebook-id record "refile")
         :icon "drive_file_move")))
      (jetpacs-menu-group
       "Arrange"
       (list
        (jetpacs-menu-item
         "Move up" (grove-structure-action notebook-id record "move-up")
         :icon "arrow_upward")
        (jetpacs-menu-item
         "Move down" (grove-structure-action notebook-id record "move-down")
         :icon "arrow_downward")
        (jetpacs-menu-item
         "Promote" (grove-structure-action notebook-id record "promote")
         :icon "keyboard_double_arrow_left")
        (jetpacs-menu-item
         "Demote" (grove-structure-action notebook-id record "demote")
         :icon "keyboard_double_arrow_right")
        (jetpacs-menu-item
         "Duplicate" (grove-structure-action notebook-id record "duplicate")
         :icon "content_copy")))
      (jetpacs-menu-group
       "Clipboard"
       (list
        (jetpacs-menu-item
         "Copy" (grove-structure-action notebook-id record "copy")
         :icon "content_copy")
        (jetpacs-menu-item
         "Cut" (grove-structure-action notebook-id record "cut")
         :icon "content_cut")
        (jetpacs-menu-item
         "Paste under" (grove-structure-action notebook-id record "paste")
         :icon "content_paste"
         :enabled (if paste-ready t :json-false))))
      (jetpacs-menu-group
       "Focus"
       (list
        (jetpacs-menu-item
         "Narrow to subtree"
         (grove-structure-action notebook-id record "narrow")
         :icon "filter_center_focus")))
      (jetpacs-menu-group
       "Remove"
       (list
        (jetpacs-menu-item
         "Archive"
         (grove-structure-action
          notebook-id record "archive"
          "Archive this heading and its complete subtree?")
         :icon "archive")
        (jetpacs-menu-item
         "Delete"
         (grove-structure-action
          notebook-id record "delete"
          "Delete this heading and its complete subtree?")
         :icon "delete")))))))

(defun grove-structure--focused-ref (notebook-id)
  "Return NOTEBOOK-ID's live private focus ref, or nil."
  (jetpacs-org-structure-focus-current
   grove-owner (concat "outline:" notebook-id)))

(defun grove-structure-outline-slice (notebook-id records)
  "Return NOTEBOOK-ID's focus-aware RECORDS and focus metadata.
The result is `(:records ROWS :focused BOOL :focus-id ID-OR-NIL)'."
  (let* ((focus-ref (grove-structure--focused-ref notebook-id))
         (focus-id (and focus-ref (grove-library--heading-id focus-ref)))
         (focus-record
          (and focus-id
               (cl-find focus-id records
                        :key (lambda (record) (plist-get record :id))
                        :test #'equal))))
    (if (null focus-record)
        (progn
          (when focus-ref
            (jetpacs-org-structure-focus-clear
             grove-owner (concat "outline:" notebook-id)))
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

(defun grove-structure-expanded-p (notebook-id)
  "Return non-nil when NOTEBOOK-ID's outline starts fully expanded."
  (and (gethash notebook-id grove-structure--expanded) t))

(defun grove-structure-collapsed-value (notebook-id)
  "Return the authored collapsible state for NOTEBOOK-ID."
  (if (grove-structure-expanded-p notebook-id) :json-false t))

(defun grove-structure-expansion-action (notebook-id expanded)
  "Return an action setting NOTEBOOK-ID's all-expanded state to EXPANDED."
  (jetpacs-action
   "grove.outline.expansion"
   :args (list :notebook-id notebook-id
               :expanded (if expanded t :json-false))))

(defun grove-structure-widen-action (notebook-id)
  "Return an action clearing NOTEBOOK-ID's subtree focus."
  (jetpacs-action "grove.outline.widen"
                  :args (list :notebook-id notebook-id)))

(defun grove-structure-undo-action ()
  "Return Grove's session-only one-level outline undo action."
  (jetpacs-action "grove.outline.undo"))

(defun grove-structure--context (args params)
  "Resolve and revision-check opaque outline ARGS under event PARAMS."
  (let ((notebook-id (plist-get args :notebook-id))
        (heading-id (plist-get args :heading-id))
        (revision (plist-get args :revision)))
    (when (and params (jetpacs-event-stale-p params))
      (signal 'ebp-org-unresolved nil))
    (unless (and (stringp notebook-id) (stringp heading-id)
                 (stringp revision))
      (signal 'ebp-org-refused (list 'invalid-identity)))
    (let ((ref (or (grove-library-resolve-heading notebook-id heading-id)
                   (signal 'ebp-org-unresolved nil))))
      (unless (equal revision (ebp-org-subtree-revision ref))
        (signal 'ebp-org-unresolved nil))
      (list :notebook-id notebook-id :heading-id heading-id
            :revision revision :ref ref))))

(defun grove-structure--recheck-context (context)
  "Resolve CONTEXT again against its originally rendered revision."
  (grove-structure--context
   (list :notebook-id (plist-get context :notebook-id)
         :heading-id (plist-get context :heading-id)
         :revision (plist-get context :revision))
   nil))

(defun grove-structure--refresh-notebooks (notebook-ids params)
  "Refresh opaque NOTEBOOK-IDS and defer PARAMS' surface presentation."
  (dolist (notebook-id (delete-dups (delq nil (copy-sequence notebook-ids))))
    (when (grove-library-resolve-notebook notebook-id)
      (grove-library-outline-refresh notebook-id)))
  (jetpacs-app-defer-refresh params))

(defun grove-structure--undo-now (surface)
  "Undo Grove's current structural receipt and refresh it on SURFACE."
  (condition-case err
      (let* ((notebooks (copy-sequence grove-structure--undo-notebooks))
             (outcome (jetpacs-org-structure-undo grove-owner 'grove)))
        (setq grove-structure--undo-notebooks nil)
        (dolist (notebook-id notebooks)
          (when (grove-library-resolve-notebook notebook-id)
            (grove-library-outline-refresh notebook-id)))
        (jetpacs-shell-notify
         (if (plist-get outcome :pending-save)
             "Undo restored in Emacs; save pending"
           "Undone")
         surface)
        (jetpacs-shell-push surface)
        t)
    (ebp-org-unresolved
     (setq grove-structure--undo-notebooks nil)
     (jetpacs-shell-notify "Undo expired after another edit" surface)
     nil)
    (error
     (message "grove: outline undo failed (%s)" (car err))
     (jetpacs-shell-notify "Undo could not be applied" surface)
     nil)))

(defun grove-structure--finish-mutation
    (label notebook-ids params &optional offer-undo)
  "Finish LABEL mutation across NOTEBOOK-IDS for event PARAMS.
When OFFER-UNDO is non-nil, expose the platform's one live receipt."
  (grove-structure--refresh-notebooks notebook-ids params)
  (if (and offer-undo
           (jetpacs-org-structure-undo-summary grove-owner))
      (let ((surface (or (plist-get params :surface) grove-app-surface)))
        (setq grove-structure--undo-notebooks
              (delete-dups (delq nil (copy-sequence notebook-ids))))
        (jetpacs-shell-notify
         label surface :action-label "Undo" :duration "long"
         :on-action (lambda () (grove-structure--undo-now surface))))
    (setq grove-structure--undo-notebooks nil)
    (jetpacs-shell-notify label (plist-get params :surface)))
  'accepted)

(defun grove-structure--simple-mutation (operation context params)
  "Apply non-prompting OPERATION using private CONTEXT and event PARAMS."
  (let ((notebook-id (plist-get context :notebook-id))
        (ref (plist-get context :ref)))
    (pcase operation
      ("move-up"
       (jetpacs-org-structure-move grove-owner ref 'grove 'up)
       (grove-structure--finish-mutation
        "Heading moved" (list notebook-id) params t))
      ("move-down"
       (jetpacs-org-structure-move grove-owner ref 'grove 'down)
       (grove-structure--finish-mutation
        "Heading moved" (list notebook-id) params t))
      ("promote"
       (jetpacs-org-structure-promote grove-owner ref 'grove)
       (grove-structure--finish-mutation
        "Heading promoted" (list notebook-id) params t))
      ("demote"
       (jetpacs-org-structure-demote grove-owner ref 'grove)
       (grove-structure--finish-mutation
        "Heading demoted" (list notebook-id) params t))
      ("duplicate"
       (jetpacs-org-structure-duplicate grove-owner ref 'grove)
       (grove-structure--finish-mutation
        "Heading duplicated" (list notebook-id) params t))
      ("delete"
       (jetpacs-org-structure-delete grove-owner ref 'grove)
       (grove-structure--finish-mutation
        "Heading deleted" (list notebook-id) params t))
      ("archive"
       (jetpacs-org-structure-archive grove-owner ref 'grove)
       (grove-structure--finish-mutation
        "Heading archived" (list notebook-id) params nil))
      ("copy"
       (jetpacs-org-structure-copy grove-owner ref)
       (jetpacs-app-defer-refresh params)
       (jetpacs-shell-notify "Copied privately" (plist-get params :surface))
       'accepted)
      ("cut"
       (jetpacs-org-structure-cut grove-owner ref)
       (jetpacs-app-defer-refresh params)
       (jetpacs-shell-notify "Cut armed; nothing moved yet"
                             (plist-get params :surface))
       'accepted)
      ("paste"
       (let ((file (or (grove-library-resolve-notebook notebook-id)
                       (signal 'ebp-org-unresolved nil))))
         (jetpacs-org-structure-paste
          grove-owner (list :file file :parent ref) 'grove)
         (grove-structure--finish-mutation
          "Heading pasted" (list notebook-id) params t)))
      ("narrow"
       (jetpacs-org-structure-focus-set
        grove-owner (concat "outline:" notebook-id) ref)
       (jetpacs-app-defer-refresh params)
       'accepted)
      (_ 'rejected))))

(defun grove-structure--prompt-title (operation context params)
  "Prompt for OPERATION's title, then mutate private CONTEXT for PARAMS."
  (let ((surface (or (plist-get params :surface) grove-app-surface)))
    (if (not (jetpacs-dialog-can-bridge-p))
        (jetpacs-shell-notify "Heading dialog is unavailable" surface)
      (let ((title (string-trim
                    (condition-case nil
                        (read-string "Heading title: ")
                      (quit "")))))
        (unless (string-empty-p title)
          (condition-case err
              (let* ((fresh (grove-structure--recheck-context context))
                     (relation
                      (pcase operation
                        ("insert-before" 'before)
                        ("insert-after" 'after)
                        (_ 'child)))
                     (notebook-id (plist-get fresh :notebook-id)))
                (jetpacs-org-structure-insert
                 grove-owner (plist-get fresh :ref) 'grove relation title)
                (grove-structure--finish-mutation
                 "Heading inserted" (list notebook-id) params t))
            ((ebp-org-refused ebp-org-unresolved)
             (jetpacs-shell-notify
              "Heading changed before insertion" surface))
            (error
             (message "grove: heading insertion failed (%s)" (car err))
             (jetpacs-shell-notify "Heading was not inserted" surface))))))))

(defun grove-structure--notebook-choices ()
  "Return deterministic refile notebook display choices."
  (mapcar
   (lambda (record)
     (cons (format "%s — %s"
                   (plist-get record :folder-label)
                   (plist-get record :file-label))
           (plist-get record :id)))
   (plist-get (grove-library-snapshot) :notebooks)))

(defun grove-structure--heading-choices (notebook-id)
  "Return hierarchical refile choices for opaque NOTEBOOK-ID."
  (let* ((records
          (plist-get (grove-library-outline-refresh notebook-id) :records))
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

(defun grove-structure--last-destination-valid-p ()
  "Return non-nil when the private last refile destination still resolves."
  (let ((notebook-id
         (plist-get grove-structure--last-refile-destination :notebook-id))
        (heading-id
         (plist-get grove-structure--last-refile-destination :heading-id)))
    (and notebook-id
         (grove-library-resolve-notebook notebook-id)
         (or (equal heading-id "top")
             (grove-library-resolve-heading notebook-id heading-id)))))

(defun grove-structure--choose-refile-destination ()
  "Prompt for and return a private refile destination, `archive', or nil."
  (let* ((notebook-choices
          (append
           (when (grove-structure--last-destination-valid-p)
             (list (cons "Last destination" "__last__")))
           (list (cons "Archive instead" "__archive__"))
           (grove-structure--notebook-choices)))
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
      (copy-tree grove-structure--last-refile-destination))
     (t
      (let* ((heading-choices
              (grove-structure--heading-choices selection))
             (heading-label
              (condition-case nil
                  (completing-read "Refile under heading: "
                                   heading-choices nil t)
                (quit "")))
             (heading-id (cdr (assoc heading-label heading-choices))))
        (when heading-id
          (list :notebook-id selection :heading-id heading-id)))))))

(defun grove-structure--prompt-refile (context params)
  "Prompt for a destination, then refile private CONTEXT for PARAMS."
  (let ((surface (or (plist-get params :surface) grove-app-surface)))
    (if (not (jetpacs-dialog-can-bridge-p))
        (jetpacs-shell-notify "Refile dialog is unavailable" surface)
      (let ((destination (grove-structure--choose-refile-destination)))
        (when destination
          (condition-case err
              (let* ((fresh (grove-structure--recheck-context context))
                     (source-notebook (plist-get fresh :notebook-id))
                     (ref (plist-get fresh :ref)))
                (if (eq destination 'archive)
                    (progn
                      (jetpacs-org-structure-archive grove-owner ref 'grove)
                      (grove-structure--finish-mutation
                       "Heading archived" (list source-notebook) params nil))
                  (let* ((target-notebook
                          (plist-get destination :notebook-id))
                         (heading-id (plist-get destination :heading-id))
                         (file
                          (or (grove-library-resolve-notebook target-notebook)
                              (signal 'ebp-org-unresolved nil)))
                         (parent
                          (unless (equal heading-id "top")
                            (or (grove-library-resolve-heading
                                 target-notebook heading-id)
                                (signal 'ebp-org-unresolved nil)))))
                    (jetpacs-org-structure-transfer
                     grove-owner ref (list :file file :parent parent)
                     'grove 'move)
                    (setq grove-structure--last-refile-destination
                          (copy-tree destination))
                    (grove-structure--finish-mutation
                     "Heading refiled"
                     (list source-notebook target-notebook) params t))))
            ((ebp-org-refused ebp-org-unresolved)
             (jetpacs-shell-notify
              "Heading or destination changed before refile" surface))
            (error
             (message "grove: refile failed (%s)" (car err))
             (jetpacs-shell-notify "Heading was not refiled" surface))))))))

(defun grove-structure--on-mutate (args params)
  "Apply ARGS' revision-bound outline operation for event PARAMS."
  (let ((operation (plist-get args :operation)))
    (if (not (member operation
                     '("insert-before" "insert-after" "insert-child"
                       "refile" "move-up" "move-down" "promote"
                       "demote" "duplicate" "delete" "archive"
                       "copy" "cut" "paste" "narrow")))
        'rejected
      (condition-case err
          (let ((context (grove-structure--context args params)))
            (cond
             ((member operation
                      '("insert-before" "insert-after" "insert-child"))
              (jetpacs-flow-continue
               (lambda ()
                 (grove-structure--prompt-title operation context params)))
              'accepted)
             ((equal operation "refile")
              (jetpacs-flow-continue
               (lambda () (grove-structure--prompt-refile context params)))
              'accepted)
             (t (grove-structure--simple-mutation
                 operation context params))))
        (ebp-org-unresolved 'stale)
        (ebp-org-refused 'rejected)
        (ebp-org-unavailable (jetpacs-retry-later))
        (error
         (message "grove: outline operation failed (%s)" (car err))
         'rejected)))))

(defun grove-structure--on-undo (_args params)
  "Undo Grove's current structural receipt for event PARAMS."
  (if (jetpacs-event-stale-p params) 'stale
    (if (grove-structure--undo-now
         (or (plist-get params :surface) grove-app-surface))
        'accepted
      'stale)))

(defun grove-structure--on-widen (args params)
  "Clear ARGS' notebook focus and refresh event PARAMS."
  (let ((notebook-id (plist-get args :notebook-id)))
    (if (not (and (stringp notebook-id)
                  (grove-library-resolve-notebook notebook-id)))
        'stale
      (jetpacs-org-structure-focus-clear
       grove-owner (concat "outline:" notebook-id))
      (jetpacs-app-defer-refresh params)
      'accepted)))

(defun grove-structure--on-expansion (args params)
  "Set ARGS' whole-outline expansion preference for event PARAMS."
  (let ((notebook-id (plist-get args :notebook-id))
        (expanded (plist-get args :expanded)))
    (if (not (and (stringp notebook-id)
                  (memq expanded '(t :json-false))
                  (grove-library-resolve-notebook notebook-id)))
        'rejected
      (puthash notebook-id (eq expanded t) grove-structure--expanded)
      (jetpacs-app-defer-refresh params)
      'accepted)))

(defun grove-structure-register ()
  "Register Grove's structural outline actions idempotently."
  (with-jetpacs-owner grove-owner
    (jetpacs-defaction
     "grove.outline.mutate" #'grove-structure--on-mutate
     :doc "Apply one revision-bound Grove outline operation")
    (jetpacs-defaction
     "grove.outline.undo" #'grove-structure--on-undo
     :doc "Consume Grove's session-only one-level structural undo")
    (jetpacs-defaction
     "grove.outline.widen" #'grove-structure--on-widen
     :doc "Clear Grove's presentation-only subtree focus")
    (jetpacs-defaction
     "grove.outline.expansion" #'grove-structure--on-expansion
     :doc "Expand or collapse every Grove outline group"))
  t)

(defun grove-structure-unregister ()
  "Unregister Grove outline actions and clear all session-only state."
  (dolist (verb grove-structure--verbs) (jetpacs-undefaction verb))
  (clrhash grove-structure--expanded)
  (setq grove-structure--last-refile-destination nil
        grove-structure--undo-notebooks nil)
  (jetpacs-org-structure-reset grove-owner)
  t)

(provide 'grove-structure)
;;; grove-structure.el ends here
