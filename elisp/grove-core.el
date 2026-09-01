;;; grove-core.el --- Shared Grove applet identity -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Stable identifiers shared by Grove's Elisp adapter, app surface, and
;; generic Jetpacs widgets.  Keeping these values in a leaf module avoids a
;; dependency cycle between the pure presentation modules and the Org-backed
;; state producer.

;;; Code:

(defgroup grove nil
  "Grove's Org-backed Jetpacs applet."
  :group 'applications)

(defconst grove-owner "grove"
  "Stable owner and app identity on the EBP wire.")

(defconst grove-title "Grove"
  "Human-facing title of the Grove applet.")

(defconst grove-icon "forest"
  "Material icon used for Grove's app destination.")

(defconst grove-app-surface "app:grove"
  "Grove's primary Jetpacs application surface.")

(defconst grove-capture-widget-surface "widget:grove.capture"
  "Generic widget surface for Grove's capture affordance.")

(defconst grove-agenda-widget-surface "widget:grove.agenda"
  "Generic widget surface for Grove's compact agenda ledger.")

(provide 'grove-core)
;;; grove-core.el ends here
