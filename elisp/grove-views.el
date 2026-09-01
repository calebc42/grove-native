;;; grove-views.el --- Grove saved search views -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Hybrid saved views for Grove.  Phone-authored text and Org match queries
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
(require 'jetpacs-org-reminders)
(require 'jetpacs-shell)
(require 'jetpacs-widgets)
(require 'grove-core)
(require 'grove-library)

(defcustom grove-saved-views nil
  "Phone-authored Grove search views.
Each entry is (NAME MODE QUERY), where MODE is the string \"text\" or
\"match\".  IDs are derived privately from the complete entry, so this
configuration contains no Android identity or protocol state."
  :type '(repeat
          (list :tag "Saved view"
                (string :tag "Name")
                (choice (const :tag "Text / regexp" "text")
                        (const :tag "Org match" "match"))
                (string :tag "Query")))
  :group 'grove)

(defcustom grove-saved-view-limit 64
  "Maximum combined Grove and compatible Emacs views shown on the phone."
  :type '(integer 1 128)
  :group 'grove)

(defconst grove-views--action-ttl-seconds 86400
  "Lifetime of a queued saved-view action.")

(defconst grove-views--verbs
  '("grove.view.run" "grove.view.save" "grove.view.delete")
  "Action names owned by Grove saved views.")

(defun grove-views--phone-entry-p (entry)
  "Return non-nil when Customize ENTRY is a valid phone-authored view."
  (and (proper-list-p entry)
       (= (length entry) 3)
       (stringp (nth 0 entry))
       (not (string-blank-p (nth 0 entry)))
       (<= (length (nth 0 entry)) 80)
       (member (nth 1 entry) '("text" "match"))
       (stringp (nth 2 entry))
       (not (string-blank-p (nth 2 entry)))
       (<= (length (nth 2 entry)) 512)))

(defun grove-views--phone-id (entry)
  "Return a stable opaque id for private phone-authored ENTRY."
  (let ((print-length nil)
        (print-level nil)
        (print-circle t))
    (concat "grove-saved-view-"
            (substring (secure-hash 'sha256 (prin1-to-string entry)) 0 32))))

(defun grove-views--phone-record (entry)
  "Return presentation record for valid phone-authored ENTRY."
  (list :id (grove-views--phone-id entry)
        :label (nth 0 entry) :mode (nth 1 entry) :query (nth 2 entry)
        :kind (nth 1 entry) :source "grove"))

(defun grove-views--phone-records ()
  "Return bounded valid records from `grove-saved-views'."
  (mapcar #'grove-views--phone-record
          (seq-take (cl-remove-if-not #'grove-views--phone-entry-p
                                      grove-saved-views)
                    grove-saved-view-limit)))

(defun grove-views--emacs-records ()
  "Return compatible read-only Emacs custom agenda view records."
  (mapcar
   (lambda (view)
     (list :id (plist-get view :id) :label (plist-get view :label)
           :kind (plist-get view :kind) :source "emacs"))
   (jetpacs-org-mode-custom-views)))

(defun grove-views-snapshot ()
  "Return bounded presentation-safe saved-view state."
  (let* ((all (append (grove-views--phone-records)
                      (grove-views--emacs-records)))
         (visible (seq-take all grove-saved-view-limit)))
    (list :views visible :total-count (length all)
          :truncated (> (length all) (length visible)))))

(defun grove-views-find (view-id)
  "Resolve opaque VIEW-ID against current saved-view configuration."
  (and (stringp view-id)
       (cl-find view-id (plist-get (grove-views-snapshot) :views)
                :key (lambda (view) (plist-get view :id)) :test #'equal)))

(defun grove-views-run-action (view)
  "Return a durable run action for saved VIEW."
  (jetpacs-action
   "grove.view.run" :args (list :view-id (plist-get view :id))
   :when-offline 'wake :ttl-s grove-views--action-ttl-seconds))

(defun grove-views-save-action (mode)
  "Return an action saving the captured search input in MODE."
  (jetpacs-action
   "grove.view.save" :args (list :mode mode)
   :capture-fields '("grove-search-query")
   :when-offline 'wake :ttl-s grove-views--action-ttl-seconds))

(defun grove-views--delete-action (view)
  "Return a confirmed delete action for phone-authored VIEW."
  (jetpacs-action
   "grove.view.delete" :args (list :view-id (plist-get view :id))
   :confirm (format "Delete saved view ‘%s’?" (plist-get view :label))
   :when-offline 'wake :ttl-s grove-views--action-ttl-seconds))

(defun grove-views--kind-label (view)
  "Return a concise source/kind label for VIEW."
  (if (equal (plist-get view :source) "emacs")
      (format "Read-only Emacs custom agenda · %s"
              (replace-regexp-in-string "-" " "
                                        (plist-get view :kind)))
    (format "%s · %s"
            (if (equal (plist-get view :mode) "match")
                "Org match" "Text / regexp")
            (plist-get view :query))))

(defun grove-views-row (view)
  "Build one saved VIEW row."
  (jetpacs-with-attrs
   (jetpacs-card
    (jetpacs-row
     (jetpacs-icon
      (if (equal (plist-get view :source) "emacs")
          "terminal" "bookmark")
      :content-description
      (if (equal (plist-get view :source) "emacs")
          "Emacs custom agenda" "Grove saved view"))
     (jetpacs-with-attrs
      (jetpacs-column
       (jetpacs-text (plist-get view :label) :max-lines 2)
       (jetpacs-text (grove-views--kind-label view)
                     :style "caption" :max-lines 2)
       :spacing 2)
      :weight 1)
     (when (equal (plist-get view :source) "grove")
       (jetpacs-menu
        (list
         (jetpacs-menu-item
          "Delete" (grove-views--delete-action view) :icon "delete"))
        :icon "more_vert"))
     :spacing 8 :align "center")
    :on-tap (grove-views-run-action view) :variant "outlined")
   :key (plist-get view :id)))

(defun grove-views-nodes ()
  "Return saved-view section nodes for Grove's Search screen."
  (let* ((snapshot (grove-views-snapshot))
         (views (plist-get snapshot :views)))
    (when views
      (append
       (list (jetpacs-section-header "Saved views"))
       (mapcar #'grove-views-row views)
       (when (plist-get snapshot :truncated)
         (list
          (jetpacs-text
           "More saved views exist; raise the Grove saved-view limit in Customize."
           :style "caption")))))))

(defun grove-views--persist (next)
  "Persist phone-authored saved-view list NEXT with rollback on failure."
  (let ((old (copy-tree grove-saved-views)))
    (condition-case err
        (progn
          (setq grove-saved-views next)
          (customize-save-variable 'grove-saved-views next)
          t)
      (error
       (setq grove-saved-views old)
       (signal (car err) (cdr err))))))

(defun grove-views--add (name mode query)
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
           (and (grove-views--phone-entry-p entry)
                (string-equal-ignore-case name (nth 0 entry))))
         grove-saved-views)
    (user-error "A saved view already uses that name"))
  (when (>= (length (grove-views--phone-records)) grove-saved-view-limit)
    (user-error "Grove's saved-view limit is already reached"))
  (let ((entry (list name mode query)))
    (grove-views--persist (cons entry (copy-tree grove-saved-views)))
    (grove-views--phone-record entry)))

(defun grove-views--delete (view-id)
  "Delete phone-authored VIEW-ID and return non-nil when it existed."
  (let ((view (grove-views-find view-id)))
    (unless view (signal 'ebp-org-unresolved nil))
    (unless (equal (plist-get view :source) "grove")
      (signal 'ebp-org-refused (list 'read-only-view)))
    (let ((found nil)
          next)
      (dolist (entry grove-saved-views)
        (if (and (not found) (grove-views--phone-entry-p entry)
                 (equal view-id (grove-views--phone-id entry)))
            (setq found t)
          (push entry next)))
      (unless found (signal 'ebp-org-unresolved nil))
      (grove-views--persist (nreverse next))
      (grove-library-clear-active-view view-id)
      t)))

(defun grove-views--run (view)
  "Run resolved saved VIEW through the canonical Grove library projection."
  (if (equal (plist-get view :source) "emacs")
      (grove-library-custom-view-search
       (plist-get view :id) (plist-get view :label) (plist-get view :kind))
    (grove-library-search
     (plist-get view :query) (plist-get view :mode) view)))

(defun grove-views--on-run (args params)
  "Run ARGS' current opaque saved view for event PARAMS."
  (cond
   ((jetpacs-event-stale-p params) 'stale)
   ((not (stringp (plist-get args :view-id))) 'rejected)
   ((not (grove-views-find (plist-get args :view-id))) 'stale)
   (t
    (condition-case err
        (progn
          (grove-views--run
           (grove-views-find (plist-get args :view-id)))
          (jetpacs-app-defer-refresh params)
          'accepted)
      (ebp-org-unresolved 'stale)
      (ebp-org-refused 'rejected)
      (ebp-org-unavailable (jetpacs-retry-later))
      (error
       (message "grove: saved view run failed (%s)" (car err))
       'rejected)))))

(defun grove-views--prompt-save (query mode params)
  "Prompt for a name, then save captured QUERY in MODE for event PARAMS."
  (let ((surface (or (plist-get params :surface) grove-app-surface)))
    (if (not (jetpacs-dialog-can-bridge-p))
        (jetpacs-shell-notify "Saved-view dialog is unavailable" surface)
      (let ((name (condition-case nil
                      (read-string "Saved view name: ")
                    (quit ""))))
        (unless (string-blank-p name)
          (condition-case err
              (progn
                ;; Validate through the same native engine before persisting.
                (jetpacs-org-mode-search-items
                 query (if (equal mode "match") 'match 'text)
                 grove-search-result-limit)
                (let ((view (grove-views--add name mode query)))
                  (grove-library-search query mode view)
                  (jetpacs-app-defer-refresh params)
                  (jetpacs-shell-notify "Saved view created" surface)))
            (error
             (message "grove: saved view create failed (%s)" (car err))
             (jetpacs-shell-notify "Saved view was not created" surface))))))))

(defun grove-views--on-save (args params)
  "Save the captured search described by ARGS and event PARAMS."
  (let* ((mode (plist-get args :mode))
         (query (plist-get (plist-get params :fields)
                           :grove-search-query)))
    (cond
     ((jetpacs-event-stale-p params) 'stale)
     ((not (and (member mode '("text" "match"))
                (stringp query) (not (string-blank-p query))
                (<= (length query) 512)))
      'rejected)
     (t
      (jetpacs-flow-continue
       (lambda () (grove-views--prompt-save query mode params)))
      'accepted))))

(defun grove-views--on-delete (args params)
  "Delete ARGS' phone-authored view for event PARAMS."
  (cond
   ((jetpacs-event-stale-p params) 'stale)
   ((not (stringp (plist-get args :view-id))) 'rejected)
   (t
    (condition-case err
        (progn
          (grove-views--delete (plist-get args :view-id))
          (jetpacs-app-defer-refresh params)
          (jetpacs-shell-notify
           "Saved view deleted"
           (or (plist-get params :surface) grove-app-surface))
          'accepted)
      (ebp-org-unresolved 'stale)
      (ebp-org-refused 'rejected)
      (error
       (message "grove: saved view delete failed (%s)" (car err))
       'rejected)))))

(defun grove-views-register ()
  "Register Grove saved-view actions idempotently."
  (with-jetpacs-owner grove-owner
    (jetpacs-defaction
     "grove.view.run" #'grove-views--on-run
     :doc "Run a current opaque Grove or Emacs saved view")
    (jetpacs-defaction
     "grove.view.save" #'grove-views--on-save
     :doc "Persist a captured native Org search as a Grove view")
    (jetpacs-defaction
     "grove.view.delete" #'grove-views--on-delete
     :doc "Delete a phone-authored Grove saved view"))
  t)

(defun grove-views-unregister ()
  "Unregister Grove saved-view actions without deleting user configuration."
  (dolist (verb grove-views--verbs) (jetpacs-undefaction verb))
  t)

(provide 'grove-views)
;;; grove-views.el ends here
