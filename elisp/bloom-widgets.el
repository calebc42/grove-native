;;; bloom-widgets.el --- Elisp-authored Bloom widgets -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Live Bloom visual fixtures for Jetpacs' one generic AppWidget provider.
;; They consume the same explicit Org snapshot as the app screen.  Android
;; sees only arbitrary `widget:*' surfaces and opaque action descriptors; it
;; contains no capture, agenda, or mark-done branch for Bloom.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'jetpacs-widgets)
(require 'bloom-core)
(require 'bloom-org)
(require 'bloom-ui)

(defcustom bloom-widget-agenda-item-limit 10
  "Maximum agenda rows authored into Bloom's full widget document."
  :type '(integer 1 20)
  :group 'bloom)

(defcustom bloom-widget-stale-after-seconds 900
  "Seconds before a disconnected Bloom Agenda widget is labeled offline."
  :type '(integer 1 86400)
  :group 'bloom)

(defun bloom-widgets-capture-surface ()
  "Build Bloom's deterministic one-tap Capture widget SurfaceSpec."
  (jetpacs-widget-surface
   "Capture"
   (jetpacs-box
    (jetpacs-row
     (jetpacs-icon "add" :content-description "Capture")
     (jetpacs-text "Capture" :font-weight 500)
     :align "center" :spacing 8)
    :alignment "center"
    :on-tap (bloom-ui-capture-action t))
   :empty (jetpacs-text "Capture unavailable")))

(defun bloom-widgets--summary (snapshot)
  "Return the compact widget summary for SNAPSHOT."
  (format "%d today · %d in %d days"
          (plist-get snapshot :today-count)
          (plist-get snapshot :total-count)
          bloom-org-agenda-days))

(defun bloom-widgets--body (snapshot limit interactive)
  "Build a widget ledger from SNAPSHOT bounded to LIMIT rows.
INTERACTIVE non-nil gives each row a durable open action."
  (let* ((items (seq-take (plist-get snapshot :items) limit))
         (children
          (list (jetpacs-text (bloom-widgets--summary snapshot)
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
                 :action-label (and interactive "Open Bloom")
                 :on-tap (and interactive
                              (jetpacs-surface-open bloom-app-surface))))))
      (dolist (group (bloom-ui-group-items items))
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
                         (bloom-ui-agenda-row item t interactive nil :widget))
                       (cdr group))))))
    (apply #'jetpacs-lazy-column
           (append children (list :spacing 6 :content-padding 8)))))

(defun bloom-widgets--agenda-document (title interactive empty)
  "Bloom's Agenda widget SurfaceSpec titled TITLE, fitted to its budget.
INTERACTIVE gives rows their actions; EMPTY is the empty-state node.  The
full list sheds rows, from `bloom-widget-agenda-item-limit' down, until
the whole document, size variants included, fits the widget profile's
`max_nodes': the receiver refuses a widget over it entirely."
  (let ((snapshot (bloom-org-snapshot))
        (budget (jetpacs-shell-target-limit bloom-agenda-widget-surface
                                            :max_nodes))
        spec)
    (cl-loop
     for limit downfrom bloom-widget-agenda-item-limit to 1
     do (setq spec
              (jetpacs-widget-surface
               title
               (bloom-widgets--body snapshot limit interactive)
               :empty empty
               ;; Capture is independent of the agenda projection: offline,
               ;; a wake action durably reaches the Org capture engine
               ;; after reconnection.
               :header-action (bloom-ui-capture-action t)
               :size-variants
               (list
                (jetpacs-widget-size-variant
                 280 160 (bloom-widgets--body snapshot (min 4 limit) interactive))
                (jetpacs-widget-size-variant
                 180 80 (bloom-widgets--body snapshot 1 interactive)))))
     until (or (null budget) (<= (jetpacs-shell-node-count spec) budget)))
    spec))

(defun bloom-widgets-agenda-surface ()
  "Build Bloom's live Org-backed Agenda widget SurfaceSpec."
  (bloom-widgets--agenda-document
   "Agenda" t
   (jetpacs-empty-state
    :icon "event_available" :title "Nothing scheduled"
    :action-label "Open Bloom"
    :on-tap (jetpacs-surface-open bloom-app-surface))))

(defun bloom-widgets-agenda-stale-surface ()
  "Build Bloom's non-actionable offline Agenda widget SurfaceSpec."
  (bloom-widgets--agenda-document
   "Agenda · Offline" nil (jetpacs-text "Open Bloom to reconnect")))

(defun bloom-widgets-register ()
  "Register Bloom's two generic widget roots idempotently."
  (with-jetpacs-owner "bloom"
    (jetpacs-shell-define-root
     bloom-capture-widget-surface #'bloom-widgets-capture-surface
     :required t)
    (jetpacs-shell-define-root
     bloom-agenda-widget-surface #'bloom-widgets-agenda-surface
     :required t
     :stale-after-s bloom-widget-stale-after-seconds
     :stale-builder #'bloom-widgets-agenda-stale-surface))
  t)

(defun bloom-widgets-unregister ()
  "Remove Bloom's generic widget roots and queue their tombstones."
  (jetpacs-shell-remove-root bloom-capture-widget-surface)
  (jetpacs-shell-remove-root bloom-agenda-widget-surface)
  t)

(provide 'bloom-widgets)
;;; bloom-widgets.el ends here
