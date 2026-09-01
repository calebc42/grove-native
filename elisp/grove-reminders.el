;;; grove-reminders.el --- Grove reminder projection and actions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Grove authors reminder presentation and semantics in Elisp over Jetpacs'
;; durable owner-scoped reminder platform.  The wire carries only opaque Grove
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
(require 'grove-core)
(require 'grove-document)
(require 'grove-library)
(require 'grove-org)

(defcustom grove-reminders-enabled t
  "Whether Grove projects timed Org items as durable device reminders."
  :type 'boolean
  :group 'grove)

(defcustom grove-reminder-horizon-hours 168
  "How many hours ahead Grove projects timed Org items as reminders."
  :type '(integer 1 744)
  :group 'grove)

(defcustom grove-reminder-lead-minutes 0
  "How many minutes before an Org timestamp Grove presents its reminder."
  :type '(choice
          (const :tag "At event time" 0)
          (const :tag "5 minutes" 5)
          (const :tag "10 minutes" 10)
          (const :tag "15 minutes" 15)
          (const :tag "30 minutes" 30)
          (const :tag "1 hour" 60)
          (const :tag "1 day" 1440))
  :group 'grove)

(defvar grove-reminders--last-reminders 'unset
  "Last Grove reminder set confirmed by the Companion.")

(defvar grove-reminders--sync-timer nil
  "Pending coalesced Grove reminder synchronization timer.")

(defconst grove-reminders--action-ttl-seconds 604800
  "Maximum durable lifetime of an offline reminder action.")

(defun grove-reminders--opaque-id (prefix identity)
  "Return a stable wire-safe opaque id using PREFIX and private IDENTITY."
  (concat prefix (substring (secure-hash 'sha256 identity) 0 32)))

(defun grove-reminders--offline-policy ()
  "Return the strongest currently granted durable reminder policy."
  (if (jetpacs-granted-p "offline.wake") 'wake 'queue))

(defun grove-reminders--action-args (coordinates)
  "Return authored action args for opaque Grove COORDINATES."
  (list :notebook-id (plist-get coordinates :notebook-id)
        :heading-id (plist-get coordinates :heading-id)))

(defun grove-reminders--remote-action
    (name coordinates ttl &optional open-surface dedupe)
  "Build NAME for COORDINATES with TTL.
OPEN-SURFACE is the optional receiver-local app target and DEDUPE is an
optional durable occurrence key."
  (jetpacs-action
   name
   :args (grove-reminders--action-args coordinates)
   :when-offline (grove-reminders--offline-policy)
   :ttl-s ttl
   :dedupe dedupe
   :open-surface open-surface))

(defun grove-reminders--body (candidate)
  "Return safe supporting text for neutral Org CANDIDATE."
  (string-join
   (delq nil
         (list (plist-get candidate :time)
               (when-let* ((type (plist-get candidate :type))
                           ((stringp type)))
                 type)
               (when (> grove-reminder-lead-minutes 0)
                 (format "%d min early" grove-reminder-lead-minutes))))
   " · "))

(defun grove-reminders--project-one (candidate rich-actions)
  "Project neutral Org CANDIDATE to one EBP reminder.
RICH-ACTIONS means the live session granted `reminders.actions'."
  (when-let* ((identity (plist-get candidate :identity))
              ((stringp identity))
              (coordinates
               (grove-library-identify-heading (plist-get candidate :ref))))
    (let* ((reminder-id
            (grove-reminders--opaque-id "grove-reminder-" identity))
           (open-surface (and rich-actions grove-app-surface))
           (body-action
            (grove-reminders--remote-action
             "grove.document.open" coordinates 86400 open-surface))
           (complete-dedupe
            (grove-reminders--opaque-id "grove-complete-" identity))
           (actions
            (and rich-actions
                 (vector
                  (jetpacs-reminder-action
                   "Complete"
                   (grove-reminders--remote-action
                    "grove.reminder.complete" coordinates
                    grove-reminders--action-ttl-seconds nil complete-dedupe)
                   :icon "done")
                  (jetpacs-reminder-action
                   "Reschedule"
                   (grove-reminders--remote-action
                    "grove.reminder.reschedule" coordinates 86400
                    grove-app-surface)
                   :icon "edit")))))
      (jetpacs-make-node
       nil
       :id reminder-id
       :title (plist-get candidate :headline)
       :body (grove-reminders--body candidate)
       :at_ms (plist-get candidate :at_ms)
       :on_tap body-action
       :actions actions))))

(defun grove-reminders--projection ()
  "Return Grove's current reminder descriptor set.
Signal on extraction failure so the caller cannot confuse failure with an
authoritative empty set."
  (let ((rich-actions (jetpacs-granted-p "reminders.actions")))
    (delq nil
          (mapcar
           (lambda (candidate)
             (grove-reminders--project-one candidate rich-actions))
           (jetpacs-org-mode-reminder-candidates
            grove-reminder-horizon-hours nil grove-reminder-lead-minutes)))))

(defun grove-reminders--sync-now ()
  "Replace Grove's durable device reminder set when connected and granted."
  (setq grove-reminders--sync-timer nil)
  (when (and (jetpacs-connected-p)
             (jetpacs-granted-p "reminders.owner"))
    (when-let* ((scan
                 (condition-case err
                     (cons t (if grove-reminders-enabled
                                 (grove-reminders--projection)
                               nil))
                   (error
                    (message "grove-reminders: scan failed (%s)"
                             (jetpacs-error-label err))
                    nil))))
      (let ((reminders (cdr scan)))
        (unless (equal reminders grove-reminders--last-reminders)
          (condition-case err
              (jetpacs-reminders-set
               reminders :owner grove-owner
               :callback
               (lambda (_count error)
                 (unless error
                   (setq grove-reminders--last-reminders reminders))))
            (error
             (message "grove-reminders: sync failed (%s)"
                      (jetpacs-error-label err)))))))))

(defun grove-reminders--run-scheduled-sync ()
  "Run and clear a coalesced reminder synchronization."
  (setq grove-reminders--sync-timer nil)
  (grove-reminders--sync-now))

(defun grove-reminders-schedule-sync (&rest _ignored)
  "Coalesce reminder reconciliation after Org, setting, or session changes."
  (unless (timerp grove-reminders--sync-timer)
    (setq grove-reminders--sync-timer
          (if (jetpacs-in-action-p)
              (jetpacs-flow-continue
               #'grove-reminders--run-scheduled-sync)
            (run-with-idle-timer
             0.1 nil #'grove-reminders--run-scheduled-sync))))
  t)

(defun grove-reminders--coordinates-from-action (args)
  "Resolve and return ARGS' authenticated opaque reminder coordinates."
  (let ((notebook-id (plist-get args :notebook-id))
        (heading-id (plist-get args :heading-id))
        (owner (plist-get args :owner))
        (reminder-id (plist-get args :reminder_id)))
    (when (and (equal owner grove-owner)
               (stringp reminder-id)
               (string-prefix-p "grove-reminder-" reminder-id)
               (stringp notebook-id)
               (stringp heading-id)
               (grove-library-resolve-heading notebook-id heading-id))
      (list :notebook-id notebook-id :heading-id heading-id))))

(defun grove-reminders--on-complete (args _params)
  "Complete the Org heading addressed by reminder action ARGS."
  (if-let* ((coordinates (grove-reminders--coordinates-from-action args))
            (notebook-id (plist-get coordinates :notebook-id))
            (heading-id (plist-get coordinates :heading-id))
            (ref (grove-library-resolve-heading notebook-id heading-id)))
      (condition-case err
          (progn
            (ebp-org-toggle-todo ref 'grove 'done)
            (grove-library-reset-projections)
            (grove-library-refresh)
            (grove-org-refresh)
            'accepted)
        (ebp-org-unresolved 'stale)
        (ebp-org-refused 'rejected)
        (error
         (message "grove-reminders: completion failed (%s)" (car err))
         'rejected))
    'stale))

(defun grove-reminders--on-reschedule (args params)
  "Open Grove's native planning canvas for reminder ARGS and context PARAMS."
  (if-let* ((coordinates (grove-reminders--coordinates-from-action args)))
      (grove-document-open-planning
       (plist-get coordinates :notebook-id)
       (plist-get coordinates :heading-id)
       params)
    'stale))

(defconst grove-reminders--verbs
  '("grove.reminder.complete" "grove.reminder.reschedule")
  "Action names owned by Grove's reminder projection.")

(defun grove-reminders-register ()
  "Register Grove reminder handlers and reconciliation hooks idempotently."
  (with-jetpacs-owner grove-owner
    (jetpacs-defaction "grove.reminder.complete"
                       #'grove-reminders--on-complete)
    (jetpacs-defaction "grove.reminder.reschedule"
                       #'grove-reminders--on-reschedule))
  (remove-hook 'grove-org-after-refresh-hook
               #'grove-reminders-schedule-sync)
  (add-hook 'grove-org-after-refresh-hook
            #'grove-reminders-schedule-sync)
  (remove-hook 'jetpacs-shell-after-push-hook
               #'grove-reminders-schedule-sync)
  (add-hook 'jetpacs-shell-after-push-hook
            #'grove-reminders-schedule-sync)
  (grove-reminders-schedule-sync)
  t)

(defun grove-reminders-unregister ()
  "Remove Grove reminder handlers and local reconciliation state.
The Companion's accepted set is deliberately not cleared as an unload side
effect; disabling reminders through settings sends an explicit empty set."
  (remove-hook 'grove-org-after-refresh-hook
               #'grove-reminders-schedule-sync)
  (remove-hook 'jetpacs-shell-after-push-hook
               #'grove-reminders-schedule-sync)
  (when (timerp grove-reminders--sync-timer)
    (cancel-timer grove-reminders--sync-timer))
  (setq grove-reminders--sync-timer nil
        grove-reminders--last-reminders 'unset)
  (dolist (verb grove-reminders--verbs) (jetpacs-undefaction verb))
  t)

(provide 'grove-reminders)
;;; grove-reminders.el ends here
