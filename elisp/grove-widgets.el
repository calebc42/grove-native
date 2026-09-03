;;; grove-widgets.el --- Elisp-authored Grove widgets -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Live Grove visual fixtures for Jetpacs' one generic AppWidget provider.
;; They consume the same explicit Org snapshot as the app screen.  Android
;; sees only arbitrary `widget:*' surfaces and opaque action descriptors; it
;; contains no capture, agenda, or mark-done branch for Grove.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'jetpacs-widgets)
(require 'grove-core)
(require 'grove-org)
(require 'grove-ui)

(defcustom grove-widget-agenda-item-limit 10
  "Maximum agenda rows authored into Grove's full widget document."
  :type '(integer 1 20)
  :group 'grove)

(defcustom grove-widget-stale-after-seconds 900
  "Seconds before a disconnected Grove Agenda widget is labeled offline."
  :type '(integer 1 86400)
  :group 'grove)

(defun grove-widgets-capture-surface ()
  "Build Grove's deterministic one-tap Capture widget SurfaceSpec."
  (jetpacs-widget-surface
   "Capture"
   (jetpacs-box
    (jetpacs-row
     (jetpacs-icon "add" :content-description "Capture")
     (jetpacs-text "Capture" :font-weight 500)
     :align "center" :spacing 8)
    :alignment "center"
    :on-tap (grove-ui-capture-action t))
   :empty (jetpacs-text "Capture unavailable")))

(defun grove-widgets--summary (snapshot)
  "Return the compact widget summary for SNAPSHOT."
  (format "%d today · %d in %d days"
          (plist-get snapshot :today-count)
          (plist-get snapshot :total-count)
          grove-org-agenda-days))

(defun grove-widgets--body (snapshot limit interactive)
  "Build a widget ledger from SNAPSHOT bounded to LIMIT rows.
INTERACTIVE non-nil gives each row a durable open action."
  (let* ((items (seq-take (plist-get snapshot :items) limit))
         (children
          (list (jetpacs-text (grove-widgets--summary snapshot)
                              :style "caption")
                (jetpacs-divider))))
    (if (null items)
        (setq children
              (append
               children
               (list
                (jetpacs-empty-state
                 :icon "event_available"
                 :title (if (plist-get snapshot :error)
                            "Agenda unavailable" "Nothing scheduled")
                 :action-label (and interactive "Open Grove")
                 :on-tap (and interactive
                              (jetpacs-surface-open grove-app-surface))))))
      (dolist (group (grove-ui-group-items items))
        (setq children
              (append
               children
               (list
                (jetpacs-section-header
                 (car group)
                 :trailing
                 (jetpacs-badge (number-to-string (length (cdr group))))))
               (mapcar (lambda (item)
                         ;; The widget target advertises no extension nodes.
                         (grove-ui-agenda-row item t interactive nil :widget))
                       (cdr group))))))
    (apply #'jetpacs-lazy-column
           (append children (list :spacing 6 :content-padding 8)))))

(defun grove-widgets-agenda-surface ()
  "Build Grove's live Org-backed Agenda widget SurfaceSpec."
  (let ((snapshot (grove-org-snapshot)))
    (jetpacs-widget-surface
     "Agenda"
     (grove-widgets--body snapshot grove-widget-agenda-item-limit t)
     :empty (jetpacs-empty-state
             :icon "event_available" :title "Nothing scheduled"
             :action-label "Open Grove"
             :on-tap (jetpacs-surface-open grove-app-surface))
     :header-action (grove-ui-capture-action t)
     :size-variants
     (list
      (jetpacs-widget-size-variant
       280 160 (grove-widgets--body snapshot 4 t))
      (jetpacs-widget-size-variant
       180 80 (grove-widgets--body snapshot 1 t))))))

(defun grove-widgets-agenda-stale-surface ()
  "Build Grove's non-actionable offline Agenda widget SurfaceSpec."
  (let ((snapshot (grove-org-snapshot)))
    (jetpacs-widget-surface
     "Agenda · Offline"
     (grove-widgets--body snapshot grove-widget-agenda-item-limit nil)
     :empty (jetpacs-text "Open Grove to reconnect")
     ;; Capture is independent of the stale agenda projection: a wake action
     ;; durably reaches the existing Org capture engine after reconnection.
     :header-action (grove-ui-capture-action t)
     :size-variants
     (list
      (jetpacs-widget-size-variant
       280 160 (grove-widgets--body snapshot 4 nil))
      (jetpacs-widget-size-variant
       180 80 (grove-widgets--body snapshot 1 nil))))))

(defun grove-widgets-register ()
  "Register Grove's two generic widget roots idempotently."
  (with-jetpacs-owner "grove"
    (jetpacs-shell-define-root
     grove-capture-widget-surface #'grove-widgets-capture-surface
     :required t)
    (jetpacs-shell-define-root
     grove-agenda-widget-surface #'grove-widgets-agenda-surface
     :required t
     :stale-after-s grove-widget-stale-after-seconds
     :stale-builder #'grove-widgets-agenda-stale-surface))
  t)

(defun grove-widgets-unregister ()
  "Remove Grove's generic widget roots and queue their tombstones."
  (jetpacs-shell-remove-root grove-capture-widget-surface)
  (jetpacs-shell-remove-root grove-agenda-widget-surface)
  t)

(provide 'grove-widgets)
;;; grove-widgets.el ends here
