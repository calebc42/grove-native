;;; bloom.el --- Bloom as an Org-backed Jetpacs applet -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Bloom rebuilt as a Tier-1 Jetpacs applet.  Emacs Org remains the parser,
;; agenda engine, capture engine, state owner, and mutation authority.  Bloom
;; contributes the mobile information architecture and presentation: a
;; Notebooks-first app surface, native outline/reader/editor/search screens,
;; capture affordances, rich reveal-first swipes, and two generic widgets.
;;
;; `require' only loads: nothing registers at load.  The `bloom' command
;; or the Apps screen activates the app; a desktop init that loads Bloom
;; adds `(bloom-register)' after the `require'.

;;; Code:

(eval-and-compile
  (let* ((here (or load-file-name buffer-file-name))
         (dir (and here (file-name-directory here))))
    (when (and dir (file-directory-p dir))
      (add-to-list 'load-path dir))))

(require 'ebp-org)
(require 'jetpacs-apps)
(require 'jetpacs-chrome)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'jetpacs-widgets)
(require 'bloom-core)
(require 'bloom-design)
(require 'bloom-library)
(require 'bloom-org)
(require 'bloom-ui)
(require 'bloom-document)
(require 'bloom-agenda)
(require 'bloom-reminders)
(require 'bloom-structure)
(require 'bloom-views)
(require 'bloom-navigation)
(require 'bloom-widgets)

(defvar bloom--push-timer nil
  "Pending coalesced presentation refresh after an Org snapshot change.")

(defun bloom--push-current-surfaces ()
  "Push Bloom's app and widget roots after a snapshot change."
  (setq bloom--push-timer nil)
  (when (jetpacs-connected-p)
    (dolist (surface
             (append (list bloom-app-surface)
                     (when (jetpacs-granted-p "surfaces.widget")
                       (list bloom-capture-widget-surface
                             bloom-agenda-widget-surface))))
      (condition-case err
          (jetpacs-shell-push surface)
        (error
         (message "bloom: refresh of %s failed (%s)" surface (car err)))))))

(defun bloom--schedule-pushes ()
  "Coalesce presentation refreshes after Bloom's snapshot changes."
  (unless (timerp bloom--push-timer)
    (setq bloom--push-timer
          (run-with-idle-timer 0.1 nil #'bloom--push-current-surfaces))))

(defun bloom--on-refresh (_args _params)
  "Refresh Bloom's explicit Org snapshot before acknowledging the action."
  (if (bloom-org-refresh) 'accepted 'rejected))

(defun bloom--on-select-range (args params)
  "Select ARGS' agenda range and defer PARAMS' presentation refresh."
  (let ((range (or (plist-get args :range) (plist-get args :value))))
    (cond
     ((not (member range '("today" "upcoming"))) 'rejected)
     ((jetpacs-event-stale-p params) 'stale)
     (t
      (setq bloom-agenda-range range)
      (jetpacs-app-defer-refresh params)
      'accepted))))

(defun bloom--on-item-open (args params)
  "Resolve ARGS' item id and present it on PARAMS' Bloom surface."
  (let ((item-id (plist-get args :item-id)))
    (cond
     ((not (stringp item-id)) 'rejected)
     ((not (bloom-org-ref-for-id item-id)) 'stale)
     (t
      (condition-case nil
          (let* ((ref (bloom-org-ref-for-id item-id))
                 (file (plist-get ref :file))
                 (notebook-id (bloom-library--notebook-id file))
                 (heading-id (bloom-library--heading-id ref)))
            (unless (bloom-library-resolve-notebook notebook-id)
              (signal 'ebp-org-unresolved nil))
            (puthash (bloom-library--heading-key notebook-id heading-id)
                     ref bloom-library--heading-refs)
            (bloom-document-open notebook-id heading-id params))
        (ebp-org-unresolved 'stale)
        (ebp-org-refused 'rejected))))))

(defun bloom--on-item-done (args params)
  "Mark ARGS' opaque agenda item done and notify PARAMS' surface."
  (let ((item-id (plist-get args :item-id)))
    (cond
     ((not (stringp item-id)) 'rejected)
     ((not (bloom-org-ref-for-id item-id)) 'stale)
     (t
      (condition-case err
          (progn
            ;; `done' means Org's configured done state, not a hard-coded
            ;; keyword.  Org owns repeaters, logging, blockers, and file edits.
            (ebp-org-toggle-todo
             (bloom-org-ref-for-id item-id) 'bloom 'done)
            (bloom-org-refresh)
            (jetpacs-shell-notify "Completed" (plist-get params :surface))
            'accepted)
        (ebp-org-unresolved 'stale)
        (ebp-org-refused 'rejected)
        (error
         (message "bloom: Org TODO mutation failed (%s)" (car err))
         'rejected))))))

(defun bloom-register ()
  "Register Bloom's app, actions, Org adapter, and widget roots idempotently."
  (bloom-design-register)
  ;; Keep the literal owner visible to Jetpacs' static applet inventory; the
  ;; constant above remains the runtime identity used everywhere else.
  (with-jetpacs-owner "bloom"
    ;; Ledgered, so the app's teardown detaches them.
    (jetpacs-add-hook 'bloom-org-after-refresh-hook #'bloom--schedule-pushes)
    (jetpacs-add-hook 'bloom-library-after-refresh-hook
                      #'bloom--schedule-pushes)
    (jetpacs-add-hook 'jetpacs-design-profile-changed-hook
                      #'bloom--schedule-pushes)
    (jetpacs-chrome-define-root "bloom" "notebooks"
                                #'bloom-navigation-presented-notebooks-screen
                                :required t
                                :data-builder #'bloom-document-surface-data)
    (jetpacs-defaction
     "bloom.agenda.refresh" #'bloom--on-refresh
     :doc "Refresh Bloom's agenda snapshot from Emacs Org")
    (jetpacs-defaction
     "bloom.agenda.select" #'bloom--on-select-range
     :args '((:name range :type "enum" :required t))
     :doc "Select Bloom's Today or Upcoming agenda range")
    (jetpacs-defaction
     "bloom.agenda.item.open" #'bloom--on-item-open
     :args '((:name item-id :type "ref" :required t))
     :doc "Open the current Org heading represented by an opaque Bloom id")
    (jetpacs-defaction
     "bloom.agenda.done" #'bloom--on-item-done
     :args '((:name item-id :type "ref" :required t))
     :doc "Move the current Org heading to its configured done state"))
  (bloom-navigation-register)
  (bloom-structure-register)
  (bloom-views-register)
  (bloom-document-register)
  (bloom-agenda-register)
  (bloom-reminders-register)
  (bloom-widgets-register)
  (jetpacs-defapp "bloom"
                  :label bloom-title
                  :icon bloom-icon
                  :surfaces '("bloom")
                  ;; Standalone, like the Android app: no host bottom bar
                  ;; or global actions; Bloom draws its own drawer and dock.
                  :chrome 'standalone
                  :fab #'bloom-navigation-capture-fab
                  :home-route "notebooks"
                  :destinations bloom-navigation-destinations
                  :order 45)
  (bloom-org-register)
  (bloom-library-refresh)
  t)

(defun bloom-unregister ()
  "Remove every Bloom registration, hook, timer, and private snapshot.
`jetpacs-apps-unregister' tears down everything attributed to the owner:
actions, the chrome root and both widget roots, ledgered hooks and the
app entry.  The module unregisters below reset state only they hold."
  (when (timerp bloom--push-timer)
    (cancel-timer bloom--push-timer))
  (setq bloom--push-timer nil)
  (jetpacs-apps-unregister bloom-owner)
  (bloom-navigation-unregister)
  (bloom-structure-unregister)
  (bloom-views-unregister)
  (bloom-reminders-unregister)
  (bloom-document-unregister)
  (bloom-org-unregister)
  (bloom-library-reset)
  (bloom-design-unregister)
  t)

;;;###autoload
(defun bloom ()
  "Register Bloom and open it on the connected Jetpacs device."
  (interactive)
  (bloom-register)
  (jetpacs-apps-seed-current bloom-owner "notebooks")
  (jetpacs-chrome-reset-screens bloom-owner))

(defun bloom-unload-function ()
  "Remove Bloom's live registrations before unloading the feature."
  (bloom-unregister)
  nil)

(provide 'bloom)
;;; bloom.el ends here
