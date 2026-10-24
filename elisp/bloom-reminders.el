;;; bloom-reminders.el --- Bloom reminder projection and actions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Bloom authors reminder presentation and semantics in Elisp over Jetpacs'
;; durable owner-scoped reminder platform.  The wire carries only opaque Bloom
;; coordinates.  Android resolves taps from Room and admits their authored EBP
;; actions; it never knows what "complete" or "reschedule" means.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ebp-org)
(require 'jetpacs-device)
(require 'jetpacs-org-reminders)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'jetpacs-widgets)
(require 'bloom-core)
(require 'bloom-document)
(require 'bloom-library)
(require 'bloom-org)

(defcustom bloom-reminders-enabled t
  "Whether Bloom projects timed Org items as durable device reminders."
  :type 'boolean
  :group 'bloom)

(defcustom bloom-reminder-horizon-hours 168
  "How many hours ahead Bloom projects timed Org items as reminders."
  :type '(integer 1 744)
  :group 'bloom)

(defcustom bloom-reminder-lead-minutes 0
  "How many minutes before an Org timestamp Bloom presents its reminder."
  :type '(choice
          (const :tag "At event time" 0)
          (const :tag "5 minutes" 5)
          (const :tag "10 minutes" 10)
          (const :tag "15 minutes" 15)
          (const :tag "30 minutes" 30)
          (const :tag "1 hour" 60)
          (const :tag "1 day" 1440))
  :group 'bloom)

(defvar bloom-reminders--last-reminders 'unset
  "Last Bloom reminder set confirmed by the Companion.")

(defvar bloom-reminders--sync-timer nil
  "Pending coalesced Bloom reminder synchronization timer.")

(defconst bloom-reminders--action-ttl-seconds 604800
  "Maximum durable lifetime of an offline reminder action.")

(defun bloom-reminders--offline-policy ()
  "Return the strongest currently granted durable reminder policy."
  (if (jetpacs-granted-p "offline.wake") 'wake 'queue))

(defun bloom-reminders--action-args (coordinates)
  "Return authored action args for opaque Bloom COORDINATES."
  (list :notebook-id (plist-get coordinates :notebook-id)
        :heading-id (plist-get coordinates :heading-id)))

(defun bloom-reminders--remote-action
    (name coordinates ttl &optional open-surface dedupe extra-args)
  "Build NAME for COORDINATES with TTL.
OPEN-SURFACE is the optional receiver-local app target and DEDUPE is an
optional durable occurrence key.  EXTRA-ARGS is an optional trusted plist
authored from the reminder projection."
  (jetpacs-action
   name
   :args (append (bloom-reminders--action-args coordinates) extra-args)
   :when-offline (bloom-reminders--offline-policy)
   :ttl-s ttl
   :dedupe dedupe
   :open-surface open-surface))

(defun bloom-reminders--planning-focus (candidate)
  "Return the planning target represented by reminder CANDIDATE."
  (if (and (stringp (plist-get candidate :type))
           (string-match-p "deadline"
                           (downcase (plist-get candidate :type))))
      "deadline"
    "scheduled"))

(defun bloom-reminders--body (candidate)
  "Return safe supporting text for neutral Org CANDIDATE."
  (string-join
   (delq nil
         (list (plist-get candidate :time)
               (when-let* ((type (plist-get candidate :type))
                           ((stringp type)))
                 type)
               (when (> bloom-reminder-lead-minutes 0)
                 (format "%d min early" bloom-reminder-lead-minutes))))
   " · "))

(defun bloom-reminders--project-one (candidate rich-actions)
  "Project neutral Org CANDIDATE to one EBP reminder.
RICH-ACTIONS means the live session granted `reminders.actions'."
  (when-let* ((identity (plist-get candidate :identity))
              ((stringp identity))
              (coordinates
               (bloom-library-identify-heading (plist-get candidate :ref))))
    (let* ((reminder-id
            (jetpacs-opaque-id "bloom-reminder-" identity))
           (open-surface (and rich-actions bloom-app-surface))
           (body-action
            (bloom-reminders--remote-action
             "bloom.document.open" coordinates 86400 open-surface))
           (complete-dedupe
            (jetpacs-opaque-id "bloom-complete-" identity))
           (actions
            (and rich-actions
                 (vector
                  (jetpacs-reminder-action
                   "Complete"
                   (bloom-reminders--remote-action
                    "bloom.reminder.complete" coordinates
                    bloom-reminders--action-ttl-seconds nil complete-dedupe)
                   :icon "done")
                  (jetpacs-reminder-action
                   "Reschedule"
                   (bloom-reminders--remote-action
                    "bloom.reminder.reschedule" coordinates 86400
                    bloom-app-surface nil
                    (list :focus
                          (bloom-reminders--planning-focus candidate)))
                   :icon "edit")))))
      (jetpacs-make-node
       nil
       :id reminder-id
       :title (plist-get candidate :headline)
       :body (bloom-reminders--body candidate)
       :at_ms (plist-get candidate :at_ms)
       :on_tap body-action
       :actions actions))))

(defun bloom-reminders--projection ()
  "Return Bloom's current reminder descriptor set.
Signal on extraction failure so the caller cannot confuse failure with an
authoritative empty set."
  (let ((rich-actions (jetpacs-granted-p "reminders.actions")))
    (delq nil
          (mapcar
           (lambda (candidate)
             (bloom-reminders--project-one candidate rich-actions))
           (jetpacs-org-mode-reminder-candidates
            bloom-reminder-horizon-hours nil bloom-reminder-lead-minutes)))))

(defun bloom-reminders--sync-now ()
  "Replace Bloom's durable device reminder set when connected and granted."
  (setq bloom-reminders--sync-timer nil)
  (when (and (jetpacs-connected-p)
             (jetpacs-granted-p "reminders.owner"))
    (when-let* ((scan
                 (condition-case err
                     (cons t (if bloom-reminders-enabled
                                 (bloom-reminders--projection)
                               nil))
                   (error
                    (message "bloom-reminders: scan failed (%s)"
                             (jetpacs-error-label err))
                    nil))))
      (let ((reminders (cdr scan)))
        (unless (equal reminders bloom-reminders--last-reminders)
          (condition-case err
              (jetpacs-reminders-set
               reminders :owner bloom-owner
               :callback
               (lambda (_count error)
                 (unless error
                   (setq bloom-reminders--last-reminders reminders))))
            (error
             (message "bloom-reminders: sync failed (%s)"
                      (jetpacs-error-label err)))))))))

(defun bloom-reminders--run-scheduled-sync ()
  "Run and clear a coalesced reminder synchronization."
  (setq bloom-reminders--sync-timer nil)
  (bloom-reminders--sync-now))

(defun bloom-reminders-schedule-sync (&rest _ignored)
  "Coalesce reminder reconciliation after Org, setting, or session changes."
  (unless (timerp bloom-reminders--sync-timer)
    (setq bloom-reminders--sync-timer
          (if (jetpacs-in-action-p)
              (jetpacs-flow-continue
               #'bloom-reminders--run-scheduled-sync)
            (run-with-idle-timer
             0.1 nil #'bloom-reminders--run-scheduled-sync))))
  t)

(defun bloom-reminders--coordinates-from-action (args)
  "Resolve and return ARGS' authenticated opaque reminder coordinates."
  (let ((notebook-id (plist-get args :notebook-id))
        (heading-id (plist-get args :heading-id))
        (owner (plist-get args :owner))
        (reminder-id (plist-get args :reminder_id)))
    (when (and (equal owner bloom-owner)
               (stringp reminder-id)
               (string-prefix-p "bloom-reminder-" reminder-id)
               (stringp notebook-id)
               (stringp heading-id)
               (bloom-library-resolve-heading notebook-id heading-id))
      (list :notebook-id notebook-id :heading-id heading-id))))

(defun bloom-reminders--on-complete (args _params)
  "Complete the Org heading addressed by reminder action ARGS."
  (if-let* ((coordinates (bloom-reminders--coordinates-from-action args))
            (notebook-id (plist-get coordinates :notebook-id))
            (heading-id (plist-get coordinates :heading-id))
            (ref (bloom-library-resolve-heading notebook-id heading-id)))
      (condition-case err
          (progn
            (ebp-org-toggle-todo ref 'bloom 'done)
            (bloom-library-reset-projections)
            (bloom-library-refresh)
            (bloom-org-refresh)
            'accepted)
        (ebp-org-unresolved 'stale)
        (ebp-org-refused 'rejected)
        (error
         (message "bloom-reminders: completion failed (%s)" (car err))
         'rejected))
    'stale))

(defun bloom-reminders--on-reschedule (args params)
  "Open Bloom's native planning canvas for reminder ARGS and context PARAMS."
  (if-let* ((coordinates (bloom-reminders--coordinates-from-action args)))
      (bloom-document-open-planning
       (plist-get coordinates :notebook-id)
       (plist-get coordinates :heading-id)
       params
       (plist-get args :focus))
    'stale))

(defconst bloom-reminders--verbs
  '("bloom.reminder.complete" "bloom.reminder.reschedule")
  "Action names owned by Bloom's reminder projection.")

(defun bloom-reminders-register ()
  "Register Bloom reminder handlers and reconciliation hooks idempotently."
  (with-jetpacs-owner bloom-owner
    (jetpacs-defaction "bloom.reminder.complete"
                       #'bloom-reminders--on-complete)
    (jetpacs-defaction "bloom.reminder.reschedule"
                       #'bloom-reminders--on-reschedule)
    (jetpacs-add-hook 'bloom-org-after-refresh-hook
                      #'bloom-reminders-schedule-sync)
    (jetpacs-add-hook 'jetpacs-shell-after-push-hook
                      #'bloom-reminders-schedule-sync))
  (bloom-reminders-schedule-sync)
  t)

(defun bloom-reminders-unregister ()
  "Remove Bloom reminder handlers and local reconciliation state.
The Companion's accepted set is deliberately not cleared as an unload side
effect; disabling reminders through settings sends an explicit empty set."
  (jetpacs-remove-hook 'bloom-org-after-refresh-hook
                       #'bloom-reminders-schedule-sync)
  (jetpacs-remove-hook 'jetpacs-shell-after-push-hook
                       #'bloom-reminders-schedule-sync)
  (when (timerp bloom-reminders--sync-timer)
    (cancel-timer bloom-reminders--sync-timer))
  (setq bloom-reminders--sync-timer nil
        bloom-reminders--last-reminders 'unset)
  (dolist (verb bloom-reminders--verbs) (jetpacs-undefaction verb))
  t)

(provide 'bloom-reminders)
;;; bloom-reminders.el ends here
