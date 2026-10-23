;;; bloom-core.el --- Shared Bloom applet identity -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Stable identifiers shared by Bloom's Elisp adapter, app surface, and
;; generic Jetpacs widgets.  Keeping these values in a leaf module avoids a
;; dependency cycle between the pure presentation modules and the Org-backed
;; state producer.

;;; Code:

(defgroup bloom nil
  "Bloom's Org-backed Jetpacs applet."
  :group 'applications)

(defconst bloom-owner "bloom"
  "Stable owner and app identity on the EBP wire.")

(defconst bloom-title "Bloom"
  "Human-facing title of the Bloom applet.")

(defconst bloom-icon "local_florist"
  "Material icon used for Bloom's app destination.")

(defconst bloom-app-surface "app:bloom"
  "Bloom's primary Jetpacs application surface.")

(defconst bloom-capture-widget-surface "widget:bloom.capture"
  "Generic widget surface for Bloom's capture affordance.")

(defconst bloom-agenda-widget-surface "widget:bloom.agenda"
  "Generic widget surface for Bloom's compact agenda ledger.")

;;;; Migration from the Grove name

;; The applet was Grove until upstream renamed the Android app to Bloom.
;; Every Customize option keeps its former name as an obsolete alias, made
;; here before any module defines the new one, so a `custom-set-variables'
;; form or `setq' written against the old name still takes effect.

(defconst bloom-core--renamed-options
  '(library-expanded-folder-ids library-favorite-ids library-notebook-limit
    library-tree-view org-agenda-days org-agenda-item-limit
    org-planning-mark-limit reader-default-mode reader-show-drawers
    reminder-horizon-hours reminder-lead-minutes reminders-enabled
    saved-view-limit saved-views search-result-limit
    widget-agenda-item-limit widget-stale-after-seconds)
  "Option name stems that were `grove-STEM' before the rename to Bloom.")

(dolist (stem bloom-core--renamed-options)
  (define-obsolete-variable-alias
    (intern (format "grove-%s" stem)) (intern (format "bloom-%s" stem))
    "Bloom 2026-10"))

(defun bloom-core-migrate-id (id)
  "Return opaque ID with a pre-rename `grove-' prefix as `bloom-'.
Stored ids (favorites, expanded folders) name their kind in a prefix; the
hash after it is unchanged by the rename."
  (if (and (stringp id) (string-prefix-p "grove-" id))
      (concat "bloom-" (substring id (length "grove-")))
    id))

(provide 'bloom-core)
;;; bloom-core.el ends here
