;;; grove.el --- Grove as an Org-backed Jetpacs applet -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Grove rebuilt as a Tier-1 Jetpacs applet.  Emacs Org remains the parser,
;; agenda engine, capture engine, state owner, and mutation authority.  Grove
;; contributes the mobile information architecture and presentation: an
;; Agenda-first app surface, capture affordances, rich reveal-first swipes,
;; and two generic Glance widget surfaces.

;;; Code:

(eval-and-compile
  (let* ((here (or load-file-name buffer-file-name))
         (dir (and here (file-name-directory here))))
    (when (and dir (file-directory-p dir))
      (add-to-list 'load-path dir))))

(require 'ebp-org)
(require 'jetpacs-apps)
(require 'jetpacs-chrome)
(require 'jetpacs-navigate)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'jetpacs-widgets)
(require 'grove-core)
(require 'grove-org)
(require 'grove-ui)
(require 'grove-widgets)

(defvar grove--push-timer nil
  "Pending coalesced presentation refresh after an Org snapshot change.")

(defun grove--push-current-surfaces ()
  "Push Grove's app and widget roots after a snapshot change."
  (setq grove--push-timer nil)
  (when (jetpacs-connected-p)
    (dolist (surface (list grove-app-surface
                           grove-capture-widget-surface
                           grove-agenda-widget-surface))
      (condition-case err
          (jetpacs-shell-push surface)
        (error
         (message "grove: refresh of %s failed (%s)" surface (car err)))))))

(defun grove--schedule-pushes ()
  "Coalesce presentation refreshes after Grove's snapshot changes."
  (unless (timerp grove--push-timer)
    (setq grove--push-timer
          (run-with-idle-timer 0.1 nil #'grove--push-current-surfaces))))

(defun grove--on-refresh (_args _params)
  "Refresh Grove's explicit Org snapshot before acknowledging the action."
  (if (grove-org-refresh) 'accepted 'rejected))

(defun grove--on-select-range (args params)
  "Select ARGS' agenda range and defer PARAMS' presentation refresh."
  (let ((range (plist-get args :range)))
    (cond
     ((not (member range '("today" "upcoming"))) 'rejected)
     ((jetpacs-event-stale-p params) 'stale)
     (t
      (setq grove-agenda-range range)
      (jetpacs-app-defer-refresh params)
      'accepted))))

(defun grove--on-item-open (args _params)
  "Resolve ARGS' opaque item id and present its Org heading in Grove."
  (let ((item-id (plist-get args :item-id)))
    (cond
     ((not (stringp item-id)) 'rejected)
     ((not (grove-org-ref-for-id item-id)) 'stale)
     (t
      (condition-case nil
          (let* ((ref (grove-org-ref-for-id item-id))
                 (marker (ebp-org-resolve-ref ref))
                 (buffer (marker-buffer marker))
                 (position (marker-position marker))
                 (label (plist-get ref :headline)))
            (set-marker marker nil)
            (jetpacs-flow-continue
             (lambda ()
               (jetpacs-navigate-buffer
                buffer grove-app-surface label position)))
            'accepted)
        (ebp-org-unresolved 'stale)
        (ebp-org-refused 'rejected))))))

(defun grove--on-item-done (args params)
  "Mark ARGS' opaque agenda item done and notify PARAMS' surface."
  (let ((item-id (plist-get args :item-id)))
    (cond
     ((not (stringp item-id)) 'rejected)
     ((not (grove-org-ref-for-id item-id)) 'stale)
     (t
      (condition-case err
          (progn
            ;; `done' means Org's configured done state, not a hard-coded
            ;; keyword.  Org owns repeaters, logging, blockers, and file edits.
            (ebp-org-toggle-todo
             (grove-org-ref-for-id item-id) 'grove 'done)
            (grove-org-refresh)
            (jetpacs-shell-notify "Completed" (plist-get params :surface))
            'accepted)
        (ebp-org-unresolved 'stale)
        (ebp-org-refused 'rejected)
        (error
         (message "grove: Org TODO mutation failed (%s)" (car err))
         'rejected))))))

(defun grove-register ()
  "Register Grove's app, actions, Org adapter, and widget roots idempotently."
  (remove-hook 'grove-org-after-refresh-hook #'grove--schedule-pushes)
  (add-hook 'grove-org-after-refresh-hook #'grove--schedule-pushes)
  ;; Keep the literal owner visible to Jetpacs' static applet inventory; the
  ;; constant above remains the runtime identity used everywhere else.
  (with-jetpacs-owner "grove"
    (jetpacs-chrome-define-root "grove" "agenda" #'grove-ui-screen
                                :required t)
    (jetpacs-defaction
     "grove.agenda.refresh" #'grove--on-refresh
     :doc "Refresh Grove's agenda snapshot from Emacs Org")
    (jetpacs-defaction
     "grove.agenda.select" #'grove--on-select-range
     :args '((:name range :type "enum" :required t))
     :doc "Select Grove's Today or Upcoming agenda range")
    (jetpacs-defaction
     "grove.agenda.item.open" #'grove--on-item-open
     :args '((:name item-id :type "ref" :required t))
     :doc "Open the current Org heading represented by an opaque Grove id")
    (jetpacs-defaction
     "grove.agenda.done" #'grove--on-item-done
     :args '((:name item-id :type "ref" :required t))
     :doc "Move the current Org heading to its configured done state"))
  (grove-widgets-register)
  (jetpacs-defapp "grove"
                  :label grove-title
                  :icon grove-icon
                  :surfaces '("grove")
                  :order 45)
  (grove-org-register)
  t)

(defun grove-unregister ()
  "Remove every Grove registration, hook, timer, and private snapshot."
  (remove-hook 'grove-org-after-refresh-hook #'grove--schedule-pushes)
  (when (timerp grove--push-timer)
    (cancel-timer grove--push-timer))
  (setq grove--push-timer nil)
  (grove-org-unregister)
  (grove-widgets-unregister)
  (dolist (name '("grove.agenda.refresh" "grove.agenda.select"
                  "grove.agenda.item.open" "grove.agenda.done"))
    (jetpacs-undefaction name))
  (jetpacs-apps-unregister grove-owner)
  (jetpacs-chrome-remove grove-owner)
  t)

(grove-register)

;;;###autoload
(defun grove ()
  "Open Grove on the connected Jetpacs device."
  (interactive)
  (jetpacs-chrome-reset-screens grove-owner))

(defun grove-unload-function ()
  "Remove Grove's live registrations before unloading the feature."
  (grove-unregister)
  nil)

(provide 'grove)
;;; grove.el ends here
