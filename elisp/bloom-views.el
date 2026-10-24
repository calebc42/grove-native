;;; bloom-views.el --- Bloom saved search views -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Hybrid saved views for Bloom.  Phone-authored text and Org match queries
;; persist through Customize.  Compatible `org-agenda-custom-commands' are
;; projected read-only through Jetpacs' canonical Org adapter.  Android sees
;; labels and opaque view ids; agenda keys, custom matchers, settings, files,
;; and every live Org ref remain private to Emacs.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ebp-org)
(require 'jetpacs-apps)
(require 'jetpacs-dialog)
(require 'jetpacs-settings)
(require 'jetpacs-org-reminders)
(require 'jetpacs-shell)
(require 'jetpacs-components)
(require 'jetpacs-widgets)
(require 'bloom-core)
(require 'bloom-library)

(defcustom bloom-saved-views
  '(("Scheduled Today" "match" "SCHEDULED=\"<today>\"")
    ("All TODO" "match" "/!")
    ("This Week" "match"
     "SCHEDULED>=\"<today>\"+SCHEDULED<=\"<+7d>\""))
  "Phone-authored Bloom search views.
Each entry is (NAME MODE QUERY), where MODE is the string \"text\",
\"match\" or \"ql\" (an org-ql Lisp form).  IDs are derived privately
from the complete entry, so this configuration contains no Android
identity or protocol state.  The default is the Android app's three
built-in searches, written as Org matches."
  :type '(repeat
          (list :tag "Saved view"
                (string :tag "Name")
                (choice (const :tag "Text / regexp" "text")
                        (const :tag "Org match" "match")
                        (const :tag "org-ql form" "ql"))
                (string :tag "Query")))
  :group 'bloom)

(defcustom bloom-saved-view-limit 64
  "Maximum combined Bloom and compatible Emacs views shown on the phone."
  :type '(integer 1 128)
  :group 'bloom)

(defconst bloom-views--action-ttl-seconds 86400
  "Lifetime of a queued saved-view action.")

(defconst bloom-views--verbs
  '("bloom.view.run" "bloom.view.save" "bloom.view.delete")
  "Action names owned by Bloom saved views.")

(defun bloom-views--phone-entry-p (entry)
  "Return non-nil when Customize ENTRY is a valid phone-authored view."
  (and (proper-list-p entry)
       (= (length entry) 3)
       (stringp (nth 0 entry))
       (not (string-blank-p (nth 0 entry)))
       (<= (length (nth 0 entry)) 80)
       (member (nth 1 entry) '("text" "match" "ql"))
       (stringp (nth 2 entry))
       (not (string-blank-p (nth 2 entry)))
       (<= (length (nth 2 entry)) 512)))

(defun bloom-views--phone-id (entry)
  "Return a stable opaque id for private phone-authored ENTRY."
  (let ((print-length nil)
        (print-level nil)
        (print-circle t))
    (concat "bloom-saved-view-"
            (substring (secure-hash 'sha256 (prin1-to-string entry)) 0 32))))

(defun bloom-views--phone-record (entry)
  "Return presentation record for valid phone-authored ENTRY."
  (list :id (bloom-views--phone-id entry)
        :label (nth 0 entry) :mode (nth 1 entry) :query (nth 2 entry)
        :kind (nth 1 entry) :source "bloom"))

(defun bloom-views--phone-records ()
  "Return bounded valid records from `bloom-saved-views'."
  (mapcar #'bloom-views--phone-record
          (seq-take (cl-remove-if-not #'bloom-views--phone-entry-p
                                      bloom-saved-views)
                    bloom-saved-view-limit)))

(defun bloom-views--emacs-records ()
  "Return compatible read-only Emacs custom agenda view records."
  (mapcar
   (lambda (view)
     (list :id (plist-get view :id) :label (plist-get view :label)
           :kind (plist-get view :kind) :source "emacs"))
   (jetpacs-org-mode-custom-views)))

(defun bloom-views-snapshot ()
  "Return bounded presentation-safe saved-view state."
  (let* ((all (append (bloom-views--phone-records)
                      (bloom-views--emacs-records)))
         (visible (seq-take all bloom-saved-view-limit)))
    (list :views visible :total-count (length all)
          :truncated (> (length all) (length visible)))))

(defun bloom-views-find (view-id)
  "Resolve opaque VIEW-ID against current saved-view configuration."
  (and (stringp view-id)
       (cl-find view-id (plist-get (bloom-views-snapshot) :views)
                :key (lambda (view) (plist-get view :id)) :test #'equal)))

(defun bloom-views-run-action (view)
  "Return a durable run action for saved VIEW."
  (jetpacs-action
   "bloom.view.run" :args (list :view-id (plist-get view :id))
   :when-offline (jetpacs-durable-offline-policy)
   :ttl-s bloom-views--action-ttl-seconds))

(defun bloom-views-save-action (mode)
  "Return an action saving the captured search input in MODE."
  (jetpacs-action
   "bloom.view.save" :args (list :mode mode)
   :capture-fields '("bloom-search-query")
   :when-offline (jetpacs-durable-offline-policy)
   :ttl-s bloom-views--action-ttl-seconds))

(defun bloom-views--delete-action (view)
  "Return a confirmed delete action for phone-authored VIEW."
  (jetpacs-action
   "bloom.view.delete" :args (list :view-id (plist-get view :id))
   :confirm (format "Delete saved view ‘%s’?" (plist-get view :label))
   :when-offline (jetpacs-durable-offline-policy)
   :ttl-s bloom-views--action-ttl-seconds))

(defun bloom-views--kind-label (view)
  "Return a concise source/kind label for VIEW."
  (if (equal (plist-get view :source) "emacs")
      (format "Read-only Emacs custom agenda · %s"
              (replace-regexp-in-string "-" " "
                                        (plist-get view :kind)))
    (format "%s · %s"
            (if (equal (plist-get view :mode) "match")
                "Org match" "Text / regexp")
            (plist-get view :query))))

(defun bloom-views-row (view)
  "Build one saved VIEW row."
  (let* ((leading (jetpacs-icon
                   (if (equal (plist-get view :source) "emacs")
                       "terminal" "bookmark")
                   :content-description
                   (if (equal (plist-get view :source) "emacs")
                       "Emacs custom agenda" "Bloom saved view")))
         (trailing (when (equal (plist-get view :source) "bloom")
                     (jetpacs-with-semantics
                      (jetpacs-menu
                       (list
                        (jetpacs-menu-item
                         "Delete" (bloom-views--delete-action view) :icon "delete"))
                       :icon "more_vert")
                      :name "View actions"))))
    (jetpacs-components-list-item
     (plist-get view :label)
     :subtitle (bloom-views--kind-label view)
     :title-max-lines 2 :subtitle-max-lines 2
     :leading leading
     :trailing trailing
     :on-tap (bloom-views-run-action view)
     :key (plist-get view :id))))

(defun bloom-views-nodes ()
  "Return saved-view section nodes for Bloom's Search screen."
  (let* ((snapshot (bloom-views-snapshot))
         (views (plist-get snapshot :views)))
    (when views
      (append
       (list (jetpacs-section-header "Saved views"))
       (mapcar #'bloom-views-row views)
       (when (plist-get snapshot :truncated)
         (list
          (jetpacs-text
           "More saved views exist; raise the Bloom saved-view limit in Customize."
           :style "caption")))))))

(defun bloom-views--persist (next)
  "Persist phone-authored saved-view list NEXT with rollback on failure."
  (let ((old (copy-tree bloom-saved-views)))
    (condition-case err
        (progn
          (setq bloom-saved-views next)
          (jetpacs-settings-save-variable 'bloom-saved-views next)
          t)
      (error
       (setq bloom-saved-views old)
       (signal (car err) (cdr err))))))

(defun bloom-views--add (name mode query)
  "Persist and return a new phone-authored view for NAME, MODE, and QUERY."
  (unless (stringp name) (user-error "Saved view name must be text"))
  (setq name (string-trim
              (replace-regexp-in-string "[\n\r]+" " " name)))
  (unless (and (not (string-blank-p name)) (<= (length name) 80)
               (member mode '("text" "match"))
               (stringp query) (not (string-blank-p query))
               (<= (length query) 512))
    (user-error "Saved view name, mode, or query is invalid"))
  (when (seq-find
         (lambda (entry)
           (and (bloom-views--phone-entry-p entry)
                (string-equal-ignore-case name (nth 0 entry))))
         bloom-saved-views)
    (user-error "A saved view already uses that name"))
  (when (>= (length (bloom-views--phone-records)) bloom-saved-view-limit)
    (user-error "Bloom's saved-view limit is already reached"))
  (let ((entry (list name mode query)))
    (bloom-views--persist (cons entry (copy-tree bloom-saved-views)))
    (bloom-views--phone-record entry)))

(defun bloom-views--delete (view-id)
  "Delete phone-authored VIEW-ID and return non-nil when it existed."
  (let ((view (bloom-views-find view-id)))
    (unless view (signal 'ebp-org-unresolved nil))
    (unless (equal (plist-get view :source) "bloom")
      (signal 'ebp-org-refused (list 'read-only-view)))
    (let ((found nil)
          next)
      (dolist (entry bloom-saved-views)
        (if (and (not found) (bloom-views--phone-entry-p entry)
                 (equal view-id (bloom-views--phone-id entry)))
            (setq found t)
          (push entry next)))
      (unless found (signal 'ebp-org-unresolved nil))
      (bloom-views--persist (nreverse next))
      (bloom-library-clear-active-view view-id)
      t)))

(defun bloom-views--run (view)
  "Run resolved saved VIEW through the canonical Bloom library projection."
  (if (equal (plist-get view :source) "emacs")
      (bloom-library-custom-view-search
       (plist-get view :id) (plist-get view :label) (plist-get view :kind))
    (bloom-library-search
     (plist-get view :query) (plist-get view :mode) view)))

(defun bloom-views--on-run (args params)
  "Run ARGS' current opaque saved view for event PARAMS."
  (cond
   ((jetpacs-event-stale-p params) 'stale)
   ((not (stringp (plist-get args :view-id))) 'rejected)
   ((not (bloom-views-find (plist-get args :view-id))) 'stale)
   (t
    (condition-case err
        (progn
          (bloom-views--run
           (bloom-views-find (plist-get args :view-id)))
          (jetpacs-app-defer-refresh params)
          'accepted)
      (ebp-org-unresolved 'stale)
      (ebp-org-refused 'rejected)
      (ebp-org-unavailable (jetpacs-retry-later))
      (error
       (message "bloom: saved view run failed (%s)" (car err))
       'rejected)))))

(defun bloom-views--prompt-save (query mode params)
  "Prompt for a name, then save captured QUERY in MODE for event PARAMS."
  (let ((surface (or (plist-get params :surface) bloom-app-surface)))
    (if (not (jetpacs-dialog-can-bridge-p))
        (jetpacs-shell-notify "Saved-view dialog is unavailable" surface)
      (let ((name (condition-case nil
                      (read-string "Saved view name: ")
                    (quit ""))))
        (unless (string-blank-p name)
          (condition-case err
              (progn
                ;; Validate through the same native engine before persisting.
                (bloom-library-search-rows query mode)
                (let ((view (bloom-views--add name mode query)))
                  (bloom-library-search query mode view)
                  (jetpacs-app-defer-refresh params)
                  (jetpacs-shell-notify "Saved view created" surface)))
            (error
             (message "bloom: saved view create failed (%s)" (car err))
             (jetpacs-shell-notify "Saved view was not created" surface))))))))

(defun bloom-views--on-save (args params)
  "Save the captured search described by ARGS and event PARAMS."
  (let* ((mode (plist-get args :mode))
         (query (plist-get (plist-get params :fields)
                           :bloom-search-query)))
    (cond
     ((jetpacs-event-stale-p params) 'stale)
     ((not (and (member mode '("text" "match" "ql"))
                (stringp query) (not (string-blank-p query))
                (<= (length query) 512)))
      'rejected)
     (t
      (jetpacs-flow-continue
       (lambda () (bloom-views--prompt-save query mode params)))
      'accepted))))

(defun bloom-views--on-delete (args params)
  "Delete ARGS' phone-authored view for event PARAMS."
  (cond
   ((jetpacs-event-stale-p params) 'stale)
   ((not (stringp (plist-get args :view-id))) 'rejected)
   (t
    (condition-case err
        (progn
          (bloom-views--delete (plist-get args :view-id))
          (jetpacs-app-defer-refresh params)
          (jetpacs-shell-notify
           "Saved view deleted"
           (or (plist-get params :surface) bloom-app-surface))
          'accepted)
      (ebp-org-unresolved 'stale)
      (ebp-org-refused 'rejected)
      (error
       (message "bloom: saved view delete failed (%s)" (car err))
       'rejected)))))

(defun bloom-views-register ()
  "Register Bloom saved-view actions idempotently."
  (with-jetpacs-owner bloom-owner
    (jetpacs-defaction
     "bloom.view.run" #'bloom-views--on-run
     :doc "Run a current opaque Bloom or Emacs saved view")
    (jetpacs-defaction
     "bloom.view.save" #'bloom-views--on-save
     :doc "Persist a captured native Org search as a Bloom view")
    (jetpacs-defaction
     "bloom.view.delete" #'bloom-views--on-delete
     :doc "Delete a phone-authored Bloom saved view"))
  t)

(defun bloom-views-unregister ()
  "Unregister Bloom saved-view actions without deleting user configuration."
  (dolist (verb bloom-views--verbs) (jetpacs-undefaction verb))
  t)

(provide 'bloom-views)
;;; bloom-views.el ends here
