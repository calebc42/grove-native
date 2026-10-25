;;; bloom-design.el --- configurable Bloom presentation for Jetpacs -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Bloom's bounded `jetpacs.design' preset.  Colors remain symbolic EBP theme
;; roles, so the active Jetpacs theme (including Modus and ef projections)
;; stays authoritative in light and dark mode.  The preset supplies Bloom's
;; typography, spacing, motion, and semantic-component bindings and is exposed
;; to Jetpacs Design Lab as an immutable starting point.
;;
;; Material screen builders remain the fallback authority.  When the app
;; target advertises `jetpacs.design', this module projects interactive button
;; and Agenda range nodes onto Jetpacs semantic components and places the same
;; screen, actions, identities, and data under the selected design profile.

;;; Code:

(require 'cl-lib)
(require 'jetpacs-components)
(require 'jetpacs-design-profiles)
(require 'jetpacs-surfaces)
(require 'jetpacs-widgets)

(defconst bloom-design-profile-id "bloom.default"
  "Identifier of Bloom's immutable palette-neutral default design profile.")

(defconst bloom-design-light-profile-id "bloom.light"
  "Identifier of the immutable Bloom Light preset (the Android app's default).")

(defconst bloom-design-dark-profile-id "bloom.dark"
  "Identifier of the immutable Bloom Dark preset.")

(defconst bloom-design-grove-light-profile-id "bloom.grove-light"
  "Identifier of the immutable Grove Light preset, the app's original theme.")

(defconst bloom-design-grove-dark-profile-id "bloom.grove-dark"
  "Identifier of the immutable Grove Dark preset, the app's original dark theme.")

(defconst bloom-design-preset-ids
  (list bloom-design-profile-id bloom-design-light-profile-id
        bloom-design-dark-profile-id bloom-design-grove-light-profile-id
        bloom-design-grove-dark-profile-id)
  "Every preset Bloom registers, in registration order.")

(defconst bloom-design-screen-kinds
  '(notebooks agenda outline search settings document planning)
  "Closed set of Bloom app screen kinds that accept the design profile.")

(defun bloom-design--role (name)
  "Return symbolic theme role NAME as a design value."
  (jetpacs-design-theme-role name))

(defun bloom-design--token (name)
  "Return token reference NAME as a design value."
  (jetpacs-design-token name))

(defconst bloom-design-typography
  '(("display" . (:family "plex-sans" :size 30 :weight 600
                  :line-height 39 :letter-spacing 0))
    ("screen-title" . (:family "plex-sans" :size 19 :weight 600
                       :line-height 24.7 :letter-spacing 0))
    ("section-title" . (:family "plex-sans" :size 17 :weight 600
                        :line-height 22.1 :letter-spacing 0))
    ("item-title" . (:family "plex-sans" :size 14.5 :weight 600
                     :line-height 18.85 :letter-spacing 0))
    ("reading-body" . (:family "plex-serif" :size 16 :weight 400
                       :line-height 26.4 :letter-spacing 0))
    ("body" . (:family "plex-sans" :size 15 :weight 400
               :line-height 19.5 :letter-spacing 0))
    ("supporting" . (:family "plex-sans" :size 13.5 :weight 400
                     :line-height 17.55 :letter-spacing 0))
    ("label-large" . (:family "plex-sans" :size 15 :weight 600
                      :line-height 19.5 :letter-spacing 0))
    ("label" . (:family "plex-sans" :size 12.5 :weight 600
                :line-height 16.25 :letter-spacing 0))
    ("caption" . (:family "plex-sans" :size 12.5 :weight 400
                  :line-height 16.25 :letter-spacing 0))
    ("code" . (:family "plex-mono" :size 13.5 :weight 400
               :line-height 24.975 :letter-spacing 0))
    ("metadata" . (:family "plex-mono" :size 11.5 :weight 400
                   :line-height 14.95 :letter-spacing 0)))
  "Bloom's IBM Plex type scale, taken from the Android app's effective sizes.
Sans carries chrome and lists, Serif carries read-mode prose, and Mono
carries the editor, timestamps, tags, and file names.  Every Plex face is
1.3 em tall (ascent 1025, descent 275, no gap), and the app sets no line
height except read-mode prose (1.65 em) and the editor (1.85 em), so every
other role carries exactly 1.3 times its size.")

(defconst bloom-design--semantic-color-names
  '("primary" "on-primary" "secondary" "on-secondary" "error" "on-error"
    "background" "on-background" "surface" "on-surface" "outline"
    "success" "warning")
  "The 13 EBP theme roles every Bloom preset binds as `color.NAME' tokens.")

(defconst bloom-design--extended-color-names
  '("muted" "faint" "surface-raised" "surface-sunken" "line" "accent-soft"
    "success-soft" "warning-soft" "error-soft" "info" "info-soft" "violet"
    "violet-soft" "syn-star" "syn-todo" "syn-done" "syn-keyword" "syn-timestamp" "syn-tag"
    "syn-link" "syn-property")
  "Bloom-specific colors beyond the EBP roles, also bound as `color.NAME'.
Every preset defines all of them so a Design Lab copy of any preset carries
the complete vocabulary Bloom components reference.")

(defun bloom-design--palette-neutral-colors ()
  "Return the default preset's colors: every token resolves a live theme role.
Bloom-specific colors collapse onto the nearest neutral role, so the preset
follows Modus, ef, and light/dark changes without a fixed palette."
  (append
   (mapcar (lambda (name)
             (cons name (bloom-design--role
                         (replace-regexp-in-string "-" "_" name))))
           bloom-design--semantic-color-names)
   `(("muted" . ,(bloom-design--role "outline"))
     ("faint" . ,(bloom-design--role "outline"))
     ("surface-raised" . ,(bloom-design--role "surface"))
     ("surface-sunken" . ,(bloom-design--role "surface"))
     ("line" . ,(bloom-design--role "outline"))
     ("accent-soft" . ,(bloom-design--role "surface"))
     ("success-soft" . ,(bloom-design--role "surface"))
     ("warning-soft" . ,(bloom-design--role "surface"))
     ("error-soft" . ,(bloom-design--role "surface"))
     ("info" . ,(bloom-design--role "secondary"))
     ("info-soft" . ,(bloom-design--role "surface"))
     ("violet" . ,(bloom-design--role "secondary"))
     ("violet-soft" . ,(bloom-design--role "surface"))
     ("syn-star" . ,(bloom-design--role "success"))
     ("syn-todo" . ,(bloom-design--role "warning"))
     ("syn-done" . ,(bloom-design--role "success"))
     ("syn-keyword" . ,(bloom-design--role "outline"))
     ("syn-timestamp" . ,(bloom-design--role "secondary"))
     ("syn-tag" . ,(bloom-design--role "primary"))
     ("syn-link" . ,(bloom-design--role "secondary"))
     ("syn-property" . ,(bloom-design--role "outline")))))

(defconst bloom-design-light-palette
  '(("primary" . "#C06A6E") ("on-primary" . "#FDF7F3")
    ("secondary" . "#6E6888") ("on-secondary" . "#FCF8F4")
    ("error" . "#B4637A") ("on-error" . "#FCF8F4") ("background" . "#F8F3EE")
    ("on-background" . "#4A4466") ("surface" . "#FCF8F4")
    ("on-surface" . "#4A4466") ("outline" . "#DDD2CA")
    ("success" . "#3F8A8F") ("warning" . "#D0913A") ("muted" . "#6E6888")
    ("faint" . "#9893AB") ("surface-raised" . "#F0E9E3")
    ("surface-sunken" . "#E6DDD6") ("line" . "#EBE2DB")
    ("accent-soft" . "#21C06A6E") ("success-soft" . "#213F8A8F")
    ("warning-soft" . "#21D0913A") ("error-soft" . "#21B4637A")
    ("info" . "#3E7FA3") ("info-soft" . "#213E7FA3") ("violet" . "#907AA9")
    ("violet-soft" . "#21907AA9") ("syn-star" . "#C06A6E")
    ("syn-todo" . "#B4637A") ("syn-done" . "#3F8A8F")
    ("syn-keyword" . "#6E6888") ("syn-timestamp" . "#3E7FA3")
    ("syn-tag" . "#907AA9") ("syn-link" . "#3E7FA3")
    ("syn-property" . "#9893AB"))
  "Bloom Light: the Android app's default rosewater palette, verbatim.
Soft fills carry their real alpha (`#AARRGGBB') rather than opaque tints.")

(defconst bloom-design-dark-palette
  '(("primary" . "#EBA8A2") ("on-primary" . "#25223A")
    ("secondary" . "#A9A4C4") ("on-secondary" . "#2B2842")
    ("error" . "#E57D99") ("on-error" . "#2B2842") ("background" . "#25223A")
    ("on-background" . "#E2DFF2") ("surface" . "#2B2842")
    ("on-surface" . "#E2DFF2") ("outline" . "#433E5D")
    ("success" . "#9FCFC4") ("warning" . "#F2C48D") ("muted" . "#A9A4C4")
    ("faint" . "#736E8F") ("surface-raised" . "#322E4B")
    ("surface-sunken" . "#3B3756") ("line" . "#34304C")
    ("accent-soft" . "#29EBA8A2") ("success-soft" . "#299FCFC4")
    ("warning-soft" . "#29F2C48D") ("error-soft" . "#29E57D99")
    ("info" . "#A3D0DA") ("info-soft" . "#29A3D0DA") ("violet" . "#C4A9E7")
    ("violet-soft" . "#29C4A9E7") ("syn-star" . "#EBA8A2")
    ("syn-todo" . "#E57D99") ("syn-done" . "#9FCFC4")
    ("syn-keyword" . "#A9A4C4") ("syn-timestamp" . "#A3D0DA")
    ("syn-tag" . "#C4A9E7") ("syn-link" . "#A3D0DA")
    ("syn-property" . "#736E8F"))
  "Bloom Dark: the Android app's rosewater night palette, verbatim.")

(defconst bloom-design-grove-light-palette
  '(("primary" . "#8A5A2B") ("on-primary" . "#FFFAF2")
    ("secondary" . "#6C6356") ("on-secondary" . "#FBF8F1")
    ("error" . "#A5462F") ("on-error" . "#FBF8F1") ("background" . "#F3EDE1")
    ("on-background" . "#2A251F") ("surface" . "#FBF8F1")
    ("on-surface" . "#2A251F") ("outline" . "#D3C9B6")
    ("success" . "#4F7A3A") ("warning" . "#A9761D") ("muted" . "#6C6356")
    ("faint" . "#9C9384") ("surface-raised" . "#ECE4D5")
    ("surface-sunken" . "#E3D9C6") ("line" . "#E3DBCB")
    ("accent-soft" . "#1F8A5A2B") ("success-soft" . "#244F7A3A")
    ("warning-soft" . "#29A9761D") ("error-soft" . "#21A5462F")
    ("info" . "#3F6F86") ("info-soft" . "#213F6F86") ("violet" . "#6E5A8E")
    ("violet-soft" . "#216E5A8E") ("syn-star" . "#4F7A3A")
    ("syn-todo" . "#A9761D") ("syn-done" . "#4F7A3A")
    ("syn-keyword" . "#7D7466") ("syn-timestamp" . "#3F6F86")
    ("syn-tag" . "#8A5A2B") ("syn-link" . "#3F6F86")
    ("syn-property" . "#A39A89"))
  "Grove Light: the app's original warm paper palette, verbatim.")

(defconst bloom-design-grove-dark-palette
  '(("primary" . "#CB9D62") ("on-primary" . "#1A160D")
    ("secondary" . "#B4A98F") ("on-secondary" . "#201C15")
    ("error" . "#D2856A") ("on-error" . "#201C15") ("background" . "#16130E")
    ("on-background" . "#ECE4D5") ("surface" . "#201C15")
    ("on-surface" . "#ECE4D5") ("outline" . "#433A2B")
    ("success" . "#8FB46A") ("warning" . "#D7A64F") ("muted" . "#B4A98F")
    ("faint" . "#7C7460") ("surface-raised" . "#2A251B")
    ("surface-sunken" . "#352F23") ("line" . "#322C20")
    ("accent-soft" . "#2ECB9D62") ("success-soft" . "#2B8FB46A")
    ("warning-soft" . "#2BD7A64F") ("error-soft" . "#29D2856A")
    ("info" . "#7FB0C4") ("info-soft" . "#297FB0C4") ("violet" . "#AD9BD1")
    ("violet-soft" . "#29AD9BD1") ("syn-star" . "#8FB46A")
    ("syn-todo" . "#D7A64F") ("syn-done" . "#8FB46A")
    ("syn-keyword" . "#AD9BD1") ("syn-timestamp" . "#7FB0C4")
    ("syn-tag" . "#CB9D62") ("syn-link" . "#7FB0C4")
    ("syn-property" . "#7C7460"))
  "Grove Dark: the app's original warm charcoal palette, verbatim.")

(defun bloom-design--literal-colors (palette)
  "Return PALETTE's `(NAME . HEX)' pairs as literal design color values."
  (mapcar (lambda (pair) (cons (car pair) (jetpacs-design-color (cdr pair))))
          palette))

(defun bloom-design--tokens (colors)
  "Return Bloom's complete token alist around color alist COLORS."
  (let ((names (append bloom-design--semantic-color-names
                       bloom-design--extended-color-names)))
    (dolist (name names)
      (unless (assoc name colors)
        (error "Bloom design palette is missing color %s" name)))
    (append
     (mapcar (lambda (name)
               (cons (concat "color." name) (cdr (assoc name colors))))
             names)
     (mapcar (lambda (pair) (cons (concat "color." (car pair)) (cdr pair)))
             (bloom-design--edge-colors colors))
     `(("space.compact" . ,(jetpacs-design-dimension 6))
       ("space.control-x" . ,(jetpacs-design-dimension 14))
       ("space.control-y" . ,(jetpacs-design-dimension 10))
       ("space.panel" . ,(jetpacs-design-dimension 14))
       ("space.gutter" . ,(jetpacs-design-dimension 14))
       ("radius.chip" . ,(jetpacs-design-dimension 6))
       ("radius.control" . ,(jetpacs-design-dimension 10))
       ("radius.panel" . ,(jetpacs-design-dimension 14))
       ("radius.pill" . ,(jetpacs-design-dimension 20))))))

(defun bloom-design--styles ()
  "Return Bloom's style alist; every color goes through a `color.*' token."
  `(("bloom.screen"
     . ,(jetpacs-design-style
         `(("background_color" . ,(bloom-design--token "color.background"))
           ("content_color" . ,(bloom-design--token "color.on-background"))
           ("fill_width" . ,(jetpacs-design-boolean t)))))
    ("bloom.surface"
     . ,(jetpacs-design-style
         `(("background_color" . ,(bloom-design--token "color.surface"))
           ("content_color" . ,(bloom-design--token "color.on-surface"))
           ("border_color" . ,(bloom-design--token "color.line"))
           ("border_width" . ,(jetpacs-design-dimension 1))
           ("corner_radius" . ,(bloom-design--token "radius.panel"))
           ("padding" . ,(bloom-design--token "space.panel")))))
    ("bloom.interactive"
     . ,(jetpacs-design-style
         `(("content_color" . ,(bloom-design--token "color.on-surface"))
           ("corner_radius" . ,(bloom-design--token "radius.control"))
           ("padding_horizontal" . ,(bloom-design--token "space.control-x"))
           ("padding_vertical" . ,(bloom-design--token "space.control-y"))
           ("min_height" . ,(jetpacs-design-dimension 48)))
         :rules
         (list
          (jetpacs-design-rule
           "hovered" `(("alpha" . ,(jetpacs-design-number 0.92)))
           :motion "hover-focus")
          (jetpacs-design-rule
           "focused"
           `(("border_color" . ,(bloom-design--token "color.primary"))
             ("border_width" . ,(jetpacs-design-dimension 2)))
           :motion "hover-focus")
          (jetpacs-design-rule
           "pressed" `(("scale" . ,(jetpacs-design-number 0.98)))
           :motion "press")
          (jetpacs-design-rule
           "selected"
           `(("background_color" . ,(bloom-design--token "color.primary"))
             ("content_color" . ,(bloom-design--token "color.on-primary")))
           :motion "selection")
          (jetpacs-design-rule
           "toggled"
           `(("background_color" . ,(bloom-design--token "color.primary"))
             ("content_color" . ,(bloom-design--token "color.on-primary")))
           :motion "selection")
          (jetpacs-design-rule
           "disabled" `(("alpha" . ,(jetpacs-design-number 0.38)))
           :motion "hover-focus"))))
    ("bloom.action"
     . ,(jetpacs-design-style
         `(("background_color" . ,(bloom-design--token "color.primary"))
           ("content_color" . ,(bloom-design--token "color.on-primary"))
           ("corner_radius" . ,(bloom-design--token "radius.pill")))))
    ("bloom.indicator"
     . ,(jetpacs-design-style
         `(("background_color" . ,(bloom-design--token "color.primary"))
           ("content_color" . ,(bloom-design--token "color.on-primary"))
           ("corner_radius" . ,(jetpacs-design-dimension 4)))))
    ("bloom.field"
     . ,(jetpacs-design-style
         `(("background_color" . ,(bloom-design--token "color.surface"))
           ("content_color" . ,(bloom-design--token "color.on-surface"))
           ("border_color" . ,(bloom-design--token "color.line"))
           ("border_width" . ,(jetpacs-design-dimension 1))
           ("corner_radius" . ,(bloom-design--token "radius.control"))
           ("padding_horizontal" . ,(bloom-design--token "space.control-x"))
           ("padding_vertical" . ,(bloom-design--token "space.control-y")))))
    ("bloom.popup"
     . ,(jetpacs-design-style
         `(("background_color" . ,(bloom-design--token "color.surface"))
           ("content_color" . ,(bloom-design--token "color.on-surface"))
           ("border_color" . ,(bloom-design--token "color.line"))
           ("border_width" . ,(jetpacs-design-dimension 1))
           ("corner_radius" . ,(bloom-design--token "radius.control")))))
    ("bloom.pill"
     . ,(jetpacs-design-style
         `(("background_color" . ,(bloom-design--token "color.accent-soft"))
           ("content_color" . ,(bloom-design--token "color.primary"))
           ("corner_radius" . ,(bloom-design--token "radius.pill"))
           ("padding_horizontal" . ,(jetpacs-design-dimension 9))
           ("padding_vertical" . ,(jetpacs-design-dimension 3)))))
    ("bloom.chip"
     . ,(jetpacs-design-style
         `(("background_color" . ,(bloom-design--token "color.surface-raised"))
           ("content_color" . ,(bloom-design--token "color.on-surface"))
           ("corner_radius" . ,(bloom-design--token "radius.chip"))
           ("padding_horizontal" . ,(jetpacs-design-dimension 8))
           ("padding_vertical" . ,(jetpacs-design-dimension 4)))
         :rules
         (list
          (jetpacs-design-rule
           "selected"
           `(("background_color" . ,(bloom-design--token "color.accent-soft"))
             ("content_color" . ,(bloom-design--token "color.primary")))
           :motion "selection"))))
    ("bloom.line"
     . ,(jetpacs-design-style
         `(("background_color" . ,(bloom-design--token "color.line")))))
    ;; A Bloom row is flat: the app draws no border or fill behind one, only
    ;; a rounded press face, so the list reads as a column of text rather
    ;; than a stack of objects.
    ("bloom.row"
     . ,(jetpacs-design-style
         `(("content_color" . ,(bloom-design--token "color.on-background"))
           ("corner_radius" . ,(bloom-design--token "radius.panel"))
           ("padding_horizontal" . ,(jetpacs-design-dimension 8))
           ("padding_vertical" . ,(jetpacs-design-dimension 10))
           ("min_height" . ,(jetpacs-design-dimension 48)))
         :rules
         (list
          (jetpacs-design-rule
           "hovered"
           `(("background_color" . ,(bloom-design--token "color.surface-raised")))
           :motion "hover-focus")
          (jetpacs-design-rule
           "focused"
           `(("border_color" . ,(bloom-design--token "color.primary"))
             ("border_width" . ,(jetpacs-design-dimension 2)))
           :motion "hover-focus")
          (jetpacs-design-rule
           "pressed"
           `(("background_color" . ,(bloom-design--token "color.accent-soft")))
           :motion "press")
          (jetpacs-design-rule
           "selected"
           `(("background_color" . ,(bloom-design--token "color.accent-soft"))
             ("content_color" . ,(bloom-design--token "color.primary")))
           :motion "selection")
          (jetpacs-design-rule
           "disabled" `(("alpha" . ,(jetpacs-design-number 0.38)))
           :motion "hover-focus"))))
    ("bloom.swipe-cell"
     . ,(jetpacs-design-style
         `(("background_color" . ,(bloom-design--token "color.surface-raised")))))
    ;; The app's own switch is a 40x23 pill: raised surface off, accent on,
    ;; with an 18 dp surface thumb.  The sizes ride on the slot so the
    ;; renderer's default metrics never have to know Bloom's.
    ("bloom.switch-track"
     . ,(jetpacs-design-style
         `(("width" . ,(jetpacs-design-dimension 40))
           ("height" . ,(jetpacs-design-dimension 23))
           ("corner_radius" . ,(jetpacs-design-dimension 12))
           ("background_color" . ,(bloom-design--token "color.surface-raised"))
           ("border_width" . ,(jetpacs-design-dimension 0)))
         :rules
         (list
          (jetpacs-design-rule
           "selected"
           `(("background_color" . ,(bloom-design--token "color.primary")))
           :motion "selection")
          (jetpacs-design-rule
           "focused"
           `(("border_color" . ,(bloom-design--token "color.primary"))
             ("border_width" . ,(jetpacs-design-dimension 2)))
           :motion "hover-focus")
          (jetpacs-design-rule
           "disabled" `(("alpha" . ,(jetpacs-design-number 0.38)))
           :motion "hover-focus"))))
    ("bloom.switch-thumb"
     . ,(jetpacs-design-style
         `(("width" . ,(jetpacs-design-dimension 18))
           ("height" . ,(jetpacs-design-dimension 18))
           ("corner_radius" . ,(jetpacs-design-dimension 9))
           ("background_color" . ,(bloom-design--token "color.surface"))
           ("content_color" . ,(bloom-design--token "color.primary")))
         :rules
         (list
          (jetpacs-design-rule
           "selected"
           `(("background_color" . ,(bloom-design--token "color.on-primary")))
           :motion "selection"))))
    ;; An outline heading discloses its children from a small muted chevron;
    ;; the header row itself stays flat and the children indent under it.
    ("bloom.disclosure"
     . ,(jetpacs-design-style
         `(("content_color" . ,(bloom-design--token "color.on-background"))
           ("min_height" . ,(jetpacs-design-dimension 48)))
         :rules
         (list
          (jetpacs-design-rule
           "hovered"
           `(("background_color" . ,(bloom-design--token "color.surface-raised")))
           :motion "hover-focus")
          (jetpacs-design-rule
           "focused"
           `(("border_color" . ,(bloom-design--token "color.primary"))
             ("border_width" . ,(jetpacs-design-dimension 2)))
           :motion "hover-focus"))))
    ("bloom.indent"
     . ,(jetpacs-design-style
         `(("padding_start" . ,(jetpacs-design-dimension 20)))))
    ;; The planning calendar: the app's 37 dp rounded day cells in Plex Mono,
    ;; ten-point weekday initials, a hairline ring for today, and the soft
    ;; accent band between the two endpoints.
    ("bloom.calendar-weekday"
     . ,(jetpacs-design-style
         `(("font_size" . ,(jetpacs-design-dimension 10))
           ("content_color" . ,(bloom-design--token "color.faint")))))
    ("bloom.day"
     . ,(jetpacs-design-style
         `(("min_height" . ,(jetpacs-design-dimension 37))
           ("corner_radius" . ,(jetpacs-design-dimension 10))
           ("content_color" . ,(bloom-design--token "color.on-background")))
         :rules
         (list
          (jetpacs-design-rule
           "selected"
           `(("background_color" . ,(bloom-design--token "color.primary"))
             ("content_color" . ,(bloom-design--token "color.on-primary")))
           :motion "selection")
          (jetpacs-design-rule
           "hovered"
           `(("background_color" . ,(bloom-design--token "color.surface-raised")))
           :motion "hover-focus")
          (jetpacs-design-rule
           "focused"
           `(("border_color" . ,(bloom-design--token "color.primary"))
             ("border_width" . ,(jetpacs-design-dimension 2)))
           :motion "hover-focus")
          (jetpacs-design-rule
           "disabled" `(("alpha" . ,(jetpacs-design-number 0.38)))
           :motion "hover-focus"))))
    ("bloom.today"
     . ,(jetpacs-design-style
         `(("border_width" . ,(jetpacs-design-dimension 1))
           ("border_color" . ,(bloom-design--token "color.line")))))
    ("bloom.band"
     . ,(jetpacs-design-style
         `(("background_color" . ,(bloom-design--token "color.accent-soft")))))
    ("bloom.accent"
     . ,(jetpacs-design-style
         `(("content_color" . ,(bloom-design--token "color.primary")))))
    ("bloom.section"
     . ,(jetpacs-design-style
         `(("padding_top" . ,(jetpacs-design-dimension 16))
           ("padding_bottom" . ,(jetpacs-design-dimension 6)))))
    ("bloom.muted"
     . ,(jetpacs-design-style
         `(("content_color" . ,(bloom-design--token "color.muted")))))
    ("bloom.faint"
     . ,(jetpacs-design-style
         `(("content_color" . ,(bloom-design--token "color.faint")))))
    ("bloom.error"
     . ,(jetpacs-design-style
         `(("content_color" . ,(bloom-design--token "color.error")))))))

;;;; Parity faces: the Android app's exact metrics

(defconst bloom-design-monogram-palette
  '(("green" "success" "success-soft")
    ("blue" "info" "info-soft")
    ("amber" "warning" "warning-soft")
    ("red" "error" "error-soft")
    ("accent" "primary" "accent-soft")
    ("neutral" "muted" "surface-raised"))
  "Monogram palette keys with their foreground and soft-fill color names.
The order is the app's `MONOGRAM_PALETTE_KEYS', which its name hash indexes.")

(defconst bloom-design-text-roles
  '(("top-title" "plex-sans" 19 600 "on-background" 0 1.3)
    ("row-title" "plex-sans" 15 600 "on-background")
    ("folder-title" "plex-sans" 14.5 600 "on-background")
    ("row-meta" "plex-sans" 12.5 400 "muted")
    ("folder-meta" "plex-sans" 12 400 "muted")
    ("row-path" "plex-mono" 11 400 "faint")
    ("monogram" "plex-mono" 16.8 600 nil)
    ("folder-glyph" "plex-mono" 15 400 nil)
    ("chevron" "plex-mono" 13 400 "faint")
    ("chevron-flat" "plex-mono" 15 400 "faint")
    ("dock-plus" "plex-sans" 22 400 "on-primary")
    ("dock-label" "plex-sans" 15 600 "on-primary")
    ("drawer-app" "plex-sans" 18 600 "on-surface")
    ("drawer-path" "plex-mono" 12 400 "muted")
    ("drawer-item" "plex-sans" 14.5 400 "on-surface")
    ("drawer-item-active" "plex-sans" 14.5 600 "primary")
    ("drawer-section" "plex-sans" 11 600 "faint" 1)
    ("empty-glyph" "plex-sans" 28 400 "faint")
    ("empty-title" "plex-sans" 15 600 "on-background")
    ("empty-caption" "plex-sans" 13 400 "muted")
    ("outline-file" "plex-mono" 17 600 "on-background")
    ("outline-count" "plex-sans" 11.5 400 "muted")
    ("outline-title" "plex-sans" 14.5 600 "on-background")
    ("outline-title-sub" "plex-sans" 14.5 500 "on-background")
    ("outline-title-done" "plex-sans" 14.5 600 "faint")
    ("outline-title-sub-done" "plex-sans" 14.5 500 "faint")
    ("outline-stars" "plex-mono" 13 600 nil)
    ("outline-chip" "plex-mono" 11 700 nil)
    ("outline-meta" "plex-mono" 11 400 nil)
    ("outline-preview" "plex-sans" 12.5 400 "faint")
    ("kv" "plex-mono" 12 400 nil 0 1.5)
    ("kv-caret" "plex-mono" 10 400 "faint")
    ("kv-label" "plex-mono" 12 400 "faint")
    ("kv-count" "plex-mono" 11 400 "faint")
    ("intro" "plex-sans" 13.5 400 "on-background" 0 1.4815)
    ("fab-plus" "plex-sans" 26 400 "on-primary")
    ("pill" "plex-sans" 11 600 nil)
    ("breadcrumb" "plex-mono" 11.5 400 "faint")
    ("read-title" "plex-serif" 25 600 "on-background" 0 1.3)
    ("read-h1" "plex-serif" 19 600 "on-background" 0 1.3)
    ("read-h2" "plex-serif" 17 600 "on-background" 0 1.3)
    ("read-h3" "plex-serif" 16 600 "on-background" 0 1.3)
    ("read-body" "plex-serif" 16 400 "on-background" 0 1.65 trimmed)
    ("read-item" "plex-serif" 16 400 "on-background" 0 1.55)
    ("read-item-done" "plex-serif" 16 400 "faint" 0 1.55)
    ("read-ordinal" "plex-serif" 16 400 nil)
    ("read-priority" "plex-mono" 12 700 nil)
    ("read-count" "plex-mono" 12 400 nil)
    ("read-chip" "plex-mono" 12.5 400 nil)
    ("read-fab" "plex-sans" 20 400 "on-primary")
    ("agenda-day" "plex-sans" 21 600 "on-background")
    ("agenda-date" "plex-mono" 12.5 400 "muted")
    ("agenda-levers" "plex-sans" 15 400 nil)
    ("agenda-tab" "plex-sans" 12.5 600 nil)
    ("agenda-lever-label" "plex-sans" 10.5 400 "faint" 0.74)
    ("agenda-lever-chip" "plex-sans" 12 600 nil)
    ("agenda-toggle" "plex-sans" 13 400 "on-background")
    ("agenda-overdue-caret" "plex-sans" 12 400 "error")
    ("agenda-overdue-count" "plex-sans" 13.5 600 "on-background")
    ("agenda-move" "plex-sans" 11.5 600 "error")
    ("agenda-group" "plex-sans" 11 700 "muted" 0.77)
    ("agenda-group-count" "plex-mono" 11 400 "faint")
    ("agenda-title" "plex-sans" 14 500 "on-background" 0 1.35 trimmed)
    ("agenda-title-done" "plex-sans" 14 500 "faint" 0 1.35 trimmed)
    ("agenda-state" "plex-mono" 9.5 700 nil 0.48)
    ("agenda-meta" "plex-mono" 11.5 400 nil)
    ("agenda-priority" "plex-mono" 10 700 nil)
    ("agenda-empty-caption" "plex-sans" 12.5 400 "muted")
    ("search-input" "plex-sans" 15 400 "on-background" 0 1.3)
    ("search-clear" "plex-mono" 15 400 "faint")
    ("search-star" "plex-mono" 22 400 "on-background")
    ("search-count" "plex-sans" 12 400 "muted")
    ("search-advanced" "plex-sans" 12 600 nil)
    ("search-ops-label" "plex-sans" 11.5 600 "muted" 0 1 trimmed)
    ("search-op" "plex-mono" 11 400 "muted")
    ("search-section" "plex-sans" 11 600 "faint" 0.77)
    ("search-card-title" "plex-sans" 14 600 "on-background")
    ("search-card-meta" "plex-sans" 11.5 400 "muted")
    ("search-saved-star" "plex-sans" 14 400 "primary")
    ("search-saved-name" "plex-sans" 14 500 "on-background")
    ("search-saved-query" "plex-mono" 11 400 "faint")
    ("search-caret" "plex-sans" 11 400 "faint")
    ("search-file" "plex-mono" 12.5 600 "on-background")
    ("search-file-meta" "plex-sans" 11.5 400 "faint")
    ("search-pill" "plex-sans" 11 600 nil)
    ("search-priority" "plex-mono" 11 700 nil)
    ("search-title" "plex-sans" 14.5 400 "on-background")
    ("search-title-done" "plex-sans" 14.5 400 "faint")
    ("search-date" "plex-mono" 10.5 600 nil)
    ("search-tags" "plex-mono" 11 400 "syn-tag")
    ("search-loosen" "plex-sans" 13 600 "on-background")
    ("search-filters" "plex-sans" 14.5 600 "on-primary")
    ("settings-label" "plex-sans" 14.5 500 "on-background")
    ("settings-description" "plex-sans" 12 400 "muted")
    ("settings-chevron" "plex-mono" 14 400 "muted")
    ("settings-section" "plex-sans" 12 600 "primary" 1)
    ("settings-id" "plex-mono" 11.5 400 "faint")
    ("settings-segment" "plex-sans" 11.5 600 nil))
  "The Android app's literal text roles, one face per distinct `Text' call.
Each entry is name, family, size, weight, color, tracking, line height in
em, and optionally `trimmed', and becomes style `bloom.t.NAME'.  A
`trimmed' role is set in the app with an explicit `TextStyle' line height,
which Compose trims at the ends as the receiver does, so it gets no
restored leading.  The line height
defaults to 1.65 em: the app's `Text' calls that name no style inherit
Material's `bodyLarge', which Bloom sets to 1.65 em, while those that name a
style (the screen title's `titleLarge') keep Plex's natural 1.3 em.")

(defconst bloom-design-struck-roles
  '("outline-title-done" "outline-title-sub-done" "agenda-title-done"
    "search-title-done")
  "Text roles the app strikes through: a finished heading's title.")

(defconst bloom-design--screen-text-role-prefixes '("agenda-" "search-")
  "Text roles that live in their screen's scope instead of the profile.
The receiver caps the styles in effect at 256, the profile's and a screen
scope's together; text roles only one screen uses would otherwise count
against every screen.")

(defun bloom-design--screen-text-role-p (name &optional screen)
  "Non-nil when text role NAME belongs to SCREEN's scope.
SCREEN is a prefix such as \"agenda-\"; nil means any screen's."
  (if screen
      (string-prefix-p screen name)
    (seq-some (lambda (prefix) (string-prefix-p prefix name))
              bloom-design--screen-text-role-prefixes)))

(defun bloom-design--text-role-styles (&optional screen)
  "Return one exact text style per entry of `bloom-design-text-roles'.
Without SCREEN, the shared roles the profile carries; with SCREEN, a
prefix in `bloom-design--screen-text-role-prefixes', only that screen's.
Roles in `bloom-design-struck-roles' also strike through."
  (mapcar
   (pcase-lambda (`(,name ,family ,size ,weight ,color ,tracking ,line ,leading))
     (cons (concat "bloom.t." name)
           (jetpacs-design-style
            (append
             `(("font_family" . ,(jetpacs-design-font-family family))
               ("font_size" . ,(jetpacs-design-dimension size))
               ("font_weight" . ,(jetpacs-design-font-weight weight))
               ("line_height"
                . ,(jetpacs-design-dimension (* size (or line 1.65))))
               ("letter_spacing" . ,(jetpacs-design-dimension (or tracking 0))))
             (and color
                  `(("content_color"
                     . ,(bloom-design--token (concat "color." color)))))
             (and (member name bloom-design-struck-roles)
                  `(("strike_through" . ,(jetpacs-design-boolean t))))
             (unless (eq leading 'trimmed)
               (bloom-design--untrimmed-leading size (or line 1.65)))))))
   (seq-filter (lambda (role)
                 (if screen
                     (bloom-design--screen-text-role-p (car role) screen)
                   (not (bloom-design--screen-text-role-p (car role)))))
               bloom-design-text-roles)))

(defun bloom-design--untrimmed-leading (size line)
  "Return the box that restores the leading a trimmed renderer drops.
The receiver trims the extra line height above a text's first line and below
its last; the app's Compose does not.  For a SIZE sp face at LINE em, half
the extra over Plex's natural 1.3 em goes above and half below, as the app's
centered line-height alignment places it, so N lines measure N line boxes.
A minimum height of one whole line box makes a single line round to device
pixels exactly as the app's does."
  (let ((extra (- (* size line) (* size 1.3))))
    (when (> extra 0)
      (let ((half (jetpacs-design-dimension (/ (round (* 500 extra)) 1000.0))))
        `(("min_height" . ,(jetpacs-design-dimension
                            (/ (round (* 1000 size line)) 1000.0)))
          ("padding_top" . ,half)
          ("padding_bottom" . ,half))))))

(defun bloom-design--monogram-styles ()
  "Return the 42 dp monogram tile and one tint per palette key.
`bloom.tile.KEY' fills the tile with the soft color and inks its letter;
`bloom.tile-edge.KEY' adds a folder tile's 40 percent hairline."
  (append
   `(("bloom.tile"
      . ,(jetpacs-design-style
          `(("width" . ,(jetpacs-design-dimension 42))
            ("height" . ,(jetpacs-design-dimension 42))
            ("corner_radius" . ,(jetpacs-design-dimension 12))))))
   (cl-loop
    for (key fg soft) in bloom-design-monogram-palette
    collect (cons (concat "bloom.tile." key)
                  (jetpacs-design-style
                   `(("background_color"
                      . ,(bloom-design--token (concat "color." soft)))
                     ("content_color"
                      . ,(bloom-design--token (concat "color." fg))))))
    collect (cons (concat "bloom.tile-edge." key)
                  (jetpacs-design-style
                   `(("border_width" . ,(jetpacs-design-dimension 1))
                     ("border_color"
                      . ,(bloom-design--token (concat "color.edge-" key)))))))))

(defconst bloom-design-ink-names
  '("success" "info" "warning" "error" "violet" "primary" "faint" "muted"
    "syn-tag" "syn-keyword" "on-background" "syn-todo" "on-primary")
  "Colors a parity text may take on top of its role, as `bloom.fg.NAME'.")

(defconst bloom-design-chip-names '("success" "info" "warning" "error" "violet")
  "Soft chip fills, as `bloom.chip.NAME': the soft color behind the color.")

(defun bloom-design--ink-styles ()
  "Return one content-color style per `bloom-design-ink-names' entry."
  (mapcar (lambda (name)
            (cons (concat "bloom.fg." name)
                  (jetpacs-design-style
                   `(("content_color"
                      . ,(bloom-design--token (concat "color." name)))))))
          bloom-design-ink-names))

(defun bloom-design--chip-styles ()
  "Return the app's 5 dp soft chips, one per `bloom-design-chip-names'."
  (mapcar (lambda (name)
            (cons (concat "bloom.chip." name)
                  (jetpacs-design-style
                   `(("background_color"
                      . ,(bloom-design--token
                          (concat "color." name "-soft")))
                     ("content_color"
                      . ,(bloom-design--token (concat "color." name)))
                     ("corner_radius" . ,(jetpacs-design-dimension 5))
                     ("padding_horizontal" . ,(jetpacs-design-dimension 5))
                     ("padding_vertical" . ,(jetpacs-design-dimension 1))))))
          bloom-design-chip-names))

(defconst bloom-design-soft-names
  '("success-soft" "warning-soft" "error-soft" "info-soft" "accent-soft"
    "surface-raised")
  "Soft fills behind read-mode pills and planning chips.")

(defun bloom-design--read-styles ()
  "Return read mode's pill, outline pill, planning chip, and switch faces."
  (append
   (cl-loop
    for soft in bloom-design-soft-names
    collect (cons (concat "bloom.pill." soft)
                  (jetpacs-design-style
                   `(("background_color" . ,(bloom-design--token (concat "color." soft)))
                     ("corner_radius" . ,(jetpacs-design-dimension 20))
                     ("padding_horizontal" . ,(jetpacs-design-dimension 9))
                     ("padding_vertical" . ,(jetpacs-design-dimension 3)))))
    collect (cons (concat "bloom.read-chip." soft)
                  (jetpacs-design-style
                   `(("background_color" . ,(bloom-design--token (concat "color." soft)))
                     ("corner_radius" . ,(jetpacs-design-dimension 6))
                     ("padding_horizontal" . ,(jetpacs-design-dimension 8))
                     ("padding_vertical" . ,(jetpacs-design-dimension 3))))))
   (cl-loop
    for ink in bloom-design-ink-names
    collect (cons (concat "bloom.pill-outline." ink)
                  (jetpacs-design-style
                   `(("border_color" . ,(bloom-design--token (concat "color." ink)))
                     ("border_width" . ,(jetpacs-design-dimension 1))
                     ("corner_radius" . ,(jetpacs-design-dimension 20))
                     ("padding_horizontal" . ,(jetpacs-design-dimension 9))
                     ("padding_vertical" . ,(jetpacs-design-dimension 3))))))
   `(("bloom.toggle"
      . ,(jetpacs-design-style
          `(("background_color" . ,(bloom-design--token "color.surface-raised"))
            ("corner_radius" . ,(jetpacs-design-dimension 10))
            ("padding" . ,(jetpacs-design-dimension 3)))))
     ("bloom.toggle-on"
      . ,(jetpacs-design-style
          `(("background_color" . ,(bloom-design--token "color.primary"))
            ("corner_radius" . ,(jetpacs-design-dimension 8))
            ("padding_horizontal" . ,(jetpacs-design-dimension 14))
            ("padding_vertical" . ,(jetpacs-design-dimension 6)))))
     ("bloom.toggle-off"
      . ,(jetpacs-design-style
          `(("corner_radius" . ,(jetpacs-design-dimension 8))
            ("padding_horizontal" . ,(jetpacs-design-dimension 14))
            ("padding_vertical" . ,(jetpacs-design-dimension 6)))))
     ("bloom.read-fab"
      . ,(jetpacs-design-style
          `(("width" . ,(jetpacs-design-dimension 54))
            ("height" . ,(jetpacs-design-dimension 54))
            ("corner_radius" . ,(jetpacs-design-dimension 18))
            ("background_color" . ,(bloom-design--token "color.primary"))))))))

;;;; Agenda

(defun bloom-design--box-style (properties)
  "Return a style of PROPERTIES, an alist of (NAME KIND VALUE).
KIND is `dp', `color' (a palette name), `bool' or `weight'."
  (jetpacs-design-style
   (mapcar (pcase-lambda (`(,name ,kind ,value))
             (cons name
                   (pcase kind
                     ('dp (jetpacs-design-dimension value))
                     ('color (bloom-design--token (concat "color." value)))
                     ('bool (jetpacs-design-boolean (if value t :json-false))))))
           properties)))

(defun bloom-design-agenda-styles ()
  "Return the Agenda's text roles and boxes: levers, tabs, panel, chips,
card and rows."
  (append
   (bloom-design--text-role-styles "agenda-")
   `(("bloom.agenda.levers"
      . ,(bloom-design--box-style '(("width" dp 36) ("height" dp 36)
                                    ("corner_radius" dp 11))))
     ("bloom.agenda.levers-off"
      . ,(bloom-design--box-style '(("background_color" color "surface-raised"))))
     ("bloom.agenda.levers-on"
      . ,(bloom-design--box-style '(("background_color" color "accent-soft"))))
     ("bloom.agenda.tabs"
      . ,(bloom-design--box-style '(("corner_radius" dp 11)
                                    ("background_color" color "surface-raised")
                                    ("padding" dp 3) ("fill_width" bool t))))
     ("bloom.agenda.tab"
      . ,(bloom-design--box-style '(("corner_radius" dp 8) ("fill_width" bool t)
                                    ("padding_horizontal" dp 4)
                                    ;; 7 dp, plus the app's whole-pixel line box.
                                    ("padding_vertical" dp 7.2))))
     ("bloom.agenda.tab-on"
      . ,(bloom-design--box-style '(("background_color" color "surface-sunken"))))
     ("bloom.agenda.tab-off"
      . ,(bloom-design--box-style '(("background_color" color "surface-raised"))))
     ("bloom.agenda.panel"
      . ,(bloom-design--box-style '(("corner_radius" dp 15) ("fill_width" bool t)
                                    ("background_color" color "surface")
                                    ("border_width" dp 1) ("border_color" color "line")
                                    ;; The receiver insets content by the border;
                                    ;; the app's border takes no room.
                                    ("padding_start" dp 12) ("padding_top" dp 11)
                                    ("padding_end" dp 12) ("padding_bottom" dp 12))))
     ("bloom.agenda.chip"
      . ,(bloom-design--box-style '(("corner_radius" dp 9) ("border_width" dp 1)
                                    ("padding_horizontal" dp 11)
                                    ("padding_vertical" dp 6))))
     ("bloom.agenda.chip-off"
      . ,(bloom-design--box-style '(("border_color" color "line")
                                    ("background_color" color "surface"))))
     ("bloom.agenda.chip-on"
      . ,(bloom-design--box-style '(("border_color" color "primary")
                                    ("background_color" color "primary"))))
     ("bloom.agenda.track"
      . ,(bloom-design--box-style '(("width" dp 36) ("height" dp 21)
                                    ("corner_radius" dp 11))))
     ("bloom.agenda.track-on"
      . ,(bloom-design--box-style '(("background_color" color "primary"))))
     ("bloom.agenda.track-off"
      . ,(bloom-design--box-style '(("background_color" color "surface-sunken"))))
     ("bloom.agenda.knob"
      . ,(bloom-design--box-style '(("width" dp 16) ("height" dp 16)
                                    ("corner_radius" dp 8)
                                    ("background_color" color "surface"))))
     ("bloom.agenda.overdue"
      . ,(bloom-design--box-style '(("corner_radius" dp 15) ("fill_width" bool t)
                                    ("background_color" color "error-soft")
                                    ("border_width" dp 1) ("border_color" color "error")
                                    ("padding_start" dp 11) ("padding_top" dp 10)
                                    ("padding_end" dp 11) ("padding_bottom" dp 11))))
     ("bloom.agenda.move"
      . ,(bloom-design--box-style '(("corner_radius" dp 9)
                                    ("background_color" color "surface")
                                    ("padding_horizontal" dp 10)
                                    ("padding_vertical" dp 6))))
     ("bloom.agenda.hairline"
      . ,(bloom-design--box-style '(("height" dp 1) ("fill_width" bool t)
                                    ("background_color" color "line"))))
     ("bloom.agenda.dot"
      . ,(bloom-design--box-style '(("width" dp 7) ("height" dp 7)
                                    ("corner_radius" dp 3.5)
                                    ("background_color" color "violet"))))
     ("bloom.agenda.check"
      . ,(bloom-design--box-style '(("width" dp 20) ("height" dp 20)
                                    ("corner_radius" dp 10) ("border_width" dp 1.5))))
     ("bloom.agenda.priority"
      . ,(bloom-design--box-style '(("width" dp 17) ("height" dp 17)
                                    ("corner_radius" dp 5)
                                    ("background_color" color "surface-raised"))))
     ("bloom.agenda.row"
      . ,(bloom-design--box-style '(("corner_radius" dp 12) ("fill_width" bool t)
                                    ("background_color" color "background")
                                    ("border_width" dp 0)
                                    ("border_color" color "background")
                                    ("padding" dp 0)))))
   (mapcar (lambda (ink)
             (cons (concat "bloom.agenda.check-" ink)
                   (bloom-design--box-style
                    (append `(("border_color" color ,ink))
                            (if (equal ink "success")
                                '(("background_color" color "success"))
                              '(("background_color" color "background")))))))
           '("success" "error" "warning" "info" "faint" "outline"))
   (mapcar (pcase-lambda (`(,name . ,fill))
             (cons (concat "bloom.agenda.state-" name)
                   (bloom-design--box-style
                    `(("corner_radius" dp 4) ("background_color" color ,fill)
                      ("padding_horizontal" dp 5) ("padding_vertical" dp 2)))))
           '(("warning" . "warning-soft") ("success" . "success-soft")
             ("raised" . "surface-raised")))))

(defun bloom-design-screen-scope (styles children)
  "Return a design scope declaring screen STYLES around CHILDREN.
A profile holds at most 256 styles, so styles used by one screen live in
a scope on that screen.  The receiver also caps the styles in effect, the
profile's and the scope's together, at 256, so STYLES must stay small.
The scope re-declares the active profile's color tokens, which its
styles name, so every preset still colors them."
  (let ((profile (jetpacs-design-active-profile bloom-design-profile-id)))
    (jetpacs-design-scope
     (seq-filter (lambda (token) (string-prefix-p "color." (car token)))
                 (bloom-design--profile-pairs (plist-get profile :tokens)))
     styles children)))

(defun bloom-design--profile-pairs (map)
  "Return identifier MAP, an alist or plist, as (NAME . VALUE) pairs."
  (cond
   ((and (consp map) (consp (car map))) map)
   ((hash-table-p map)
    (let (pairs) (maphash (lambda (k v) (push (cons k v) pairs)) map)
         (nreverse pairs)))
   (t (cl-loop for (key value) on map by #'cddr
               collect (cons (string-remove-prefix ":" (symbol-name key)) value)))))

(defun bloom-design-settings-styles ()
  "Return Settings' boxes: group card, segmented control, Material switch."
  `(("bloom.settings.group"
     . ,(bloom-design--box-style '(("corner_radius" dp 15) ("fill_width" bool t)
                                   ("background_color" color "surface")
                                   ("border_width" dp 1) ("border_color" color "line"))))
    ("bloom.settings.segments"
     . ,(bloom-design--box-style '(("corner_radius" dp 10) ("padding" dp 3)
                                   ("background_color" color "surface-raised"))))
    ("bloom.settings.segment"
     . ,(bloom-design--box-style '(("corner_radius" dp 8) ("fill_width" bool t)
                                   ("padding_horizontal" dp 14)
                                   ("padding_vertical" dp 6))))
    ("bloom.settings.segment-on"
     . ,(bloom-design--box-style '(("background_color" color "primary"))))
    ("bloom.settings.segment-off"
     . ,(bloom-design--box-style '(("background_color" color "surface-raised"))))
    ;; The app's settings switch is Material 3's, drawn from boxes: a
    ;; 52x32 track with a 2 dp outline while off, a 16 dp thumb 8 dp in
    ;; while off and a 24 dp thumb 4 dp from the end while on.
    ("bloom.settings.track"
     . ,(bloom-design--box-style '(("width" dp 52) ("height" dp 32)
                                   ("corner_radius" dp 16))))
    ("bloom.settings.track-off"
     . ,(bloom-design--box-style '(("background_color" color "surface-sunken")
                                   ("border_width" dp 2) ("border_color" color "outline"))))
    ("bloom.settings.track-on"
     . ,(bloom-design--box-style '(("background_color" color "primary"))))
    ("bloom.settings.thumb-off"
     . ,(bloom-design--box-style '(("width" dp 16) ("height" dp 16)
                                   ("corner_radius" dp 8)
                                   ("background_color" color "surface"))))
    ("bloom.settings.thumb-on"
     . ,(bloom-design--box-style '(("width" dp 24) ("height" dp 24)
                                   ("corner_radius" dp 12)
                                   ("background_color" color "on-primary"))))))

(defun bloom-design-search-styles ()
  "Return Search's text roles and boxes: field, toggles, cards, groups,
pills and bars.  A bordered box pads one dp less than the app, which the
receiver insets."
  (append
   (bloom-design--text-role-styles "search-")
   `(("bloom.search.field"
      . ,(bloom-design--box-style '(("height" dp 42) ("corner_radius" dp 13)
                                    ("background_color" color "surface")
                                    ("border_width" dp 1) ("border_color" color "line")
                                    ("padding_horizontal" dp 11) ("fill_width" bool t))))
     ;; The text input sits bare inside the field box.  The receiver's
     ;; field is at least 48 dp tall with its text at the top, centred over
     ;; the 42 dp box; 10 dp above the text puts it on the box's centre.
     ("bloom.search.input"
      . ,(bloom-design--box-style '(("background_color" color "surface")
                                    ("border_width" dp 0) ("border_color" color "surface")
                                    ("padding" dp 0) ("padding_top" dp 10)
                                    ("fill_width" bool t))))
     ("bloom.search.advanced"
      . ,(bloom-design--box-style '(("corner_radius" dp 8) ("padding_horizontal" dp 10)
                                    ("padding_vertical" dp 5))))
     ("bloom.search.advanced-on"
      . ,(bloom-design--box-style '(("background_color" color "primary"))))
     ("bloom.search.advanced-off"
      . ,(bloom-design--box-style '(("background_color" color "surface-raised"))))
     ("bloom.search.ops"
      . ,(bloom-design--box-style '(("corner_radius" dp 13) ("fill_width" bool t)
                                    ("background_color" color "surface")
                                    ("border_width" dp 1) ("border_color" color "line")
                                    ("padding" dp 12))))
     ("bloom.search.op"
      . ,(bloom-design--box-style '(("corner_radius" dp 6)
                                    ("background_color" color "surface-raised")
                                    ("padding_horizontal" dp 8) ("padding_vertical" dp 3))))
     ("bloom.search.card"
      . ,(bloom-design--box-style '(("corner_radius" dp 14) ("fill_width" bool t)
                                    ("background_color" color "surface")
                                    ("border_width" dp 1) ("border_color" color "line")
                                    ("padding_horizontal" dp 12) ("padding_vertical" dp 11))))
     ("bloom.search.saved"
      . ,(bloom-design--box-style '(("corner_radius" dp 12) ("fill_width" bool t)
                                    ("padding_horizontal" dp 12) ("padding_vertical" dp 10))))
     ("bloom.search.group"
      . ,(bloom-design--box-style '(("fill_width" bool t)
                                    ("background_color" color "background")
                                    ("padding_horizontal" dp 6) ("padding_vertical" dp 9))))
     ("bloom.search.loosen"
      . ,(bloom-design--box-style '(("corner_radius" dp 11)
                                    ("background_color" color "surface-raised")
                                    ("padding_horizontal" dp 17) ("padding_vertical" dp 10))))
     ("bloom.search.filters-bar"
      . ,(bloom-design--box-style '(("fill_width" bool t)
                                    ("background_color" color "surface")
                                    ("padding_horizontal" dp 12) ("padding_vertical" dp 10))))
     ("bloom.search.filters"
      . ,(bloom-design--box-style '(("corner_radius" dp 13) ("fill_width" bool t)
                                    ("background_color" color "primary")
                                    ("padding_vertical" dp 13)))))
   (mapcar (pcase-lambda (`(,tone . ,fill))
             (cons (concat "bloom.search.card-icon-" tone)
                   (bloom-design--box-style
                    `(("width" dp 30) ("height" dp 30) ("corner_radius" dp 9)
                      ("background_color" color ,fill)))))
           '(("error" . "error-soft") ("warning" . "warning-soft")
             ("info" . "info-soft") ("syn-tag" . "accent-soft")))
   (mapcar (pcase-lambda (`(,tone . ,fill))
             (cons (concat "bloom.search.pill-" tone)
                   (bloom-design--box-style
                    `(("corner_radius" dp 20) ("background_color" color ,fill)
                      ("padding_horizontal" dp 9) ("padding_vertical" dp 3)))))
           '(("warning" . "warning-soft") ("success" . "success-soft")))
   (mapcar (pcase-lambda (`(,tone . ,fill))
             (cons (concat "bloom.search.date-" tone)
                   (bloom-design--box-style
                    `(("corner_radius" dp 6) ("background_color" color ,fill)
                      ("padding_horizontal" dp 7) ("padding_vertical" dp 3)))))
           '(("error" . "error-soft") ("warning" . "warning-soft")
             ("info" . "info-soft")))))

;;;; Edit mode

(defconst bloom-design-star-colors
  '("success" "info" "warning" "error" "violet" "primary")
  "Heading-star colors by outline depth, cycled from level 1.
The app's `starColor': green, blue, amber, red, violet, then accent.")

(defconst bloom-design-syntax-roles
  '(("heading" "bloom.syn.heading" (("font_weight" . 600)))
    ("function" "bloom.syn.headline"
     (("content_color" . "on-background") ("font_weight" . 600)))
    ("todo" "bloom.syn.todo" (("content_color" . "syn-todo") ("font_weight" . 700)))
    ("done" "bloom.syn.done" (("content_color" . "syn-done") ("font_weight" . 700)))
    ("tag" "bloom.syn.tag" (("content_color" . "syn-tag") ("font_weight" . 400)))
    ("comment" "bloom.syn.planning"
     (("content_color" . "syn-keyword") ("font_weight" . 400) ("italic" . nil)))
    ("keyword" "bloom.syn.planning-keyword"
     (("content_color" . "syn-keyword") ("font_weight" . 600)))
    ("number" "bloom.syn.timestamp" (("content_color" . "syn-timestamp")))
    ("preprocessor" "bloom.syn.property" (("content_color" . "syn-property")))
    ("string" "bloom.syn.code"
     (("content_color" . "primary") ("background_color" . "surface-raised")))
    ("constant" "bloom.syn.bold" (("content_color" . "syn-tag") ("font_weight" . 600)))
    ("type" "bloom.syn.italic" (("content_color" . "success") ("italic" . t)))
    ("variable" "bloom.syn.underline"
     (("content_color" . "on-background") ("underline" . t)))
    ("operator" "bloom.syn.strike"
     (("content_color" . "on-background") ("strike_through" . t)))
    ("link" "bloom.syn.link" (("content_color" . "syn-link") ("underline" . t))))
  "Each EBP syntax role Bloom's editor sends, its style, and that style.
The app's raw-Org highlighter has more distinct styles than EBP has roles
that name them, so `bloom-edit' assigns the roles and these styles give
each the app's color, weight, slant, and line.  Colors name palette
tokens; nil turns a role default off.")

(defun bloom-design--syntax-value (property value)
  "Return the design value for syntax style PROPERTY set to VALUE."
  (pcase property
    ("font_weight" (jetpacs-design-font-weight value))
    ((or "italic" "underline" "strike_through")
     (jetpacs-design-boolean (if value t :json-false)))
    (_ (bloom-design--token (concat "color." value)))))

(defun bloom-design--edit-styles ()
  "Return the raw-Org editor's text, surface, and syntax styles."
  (append
   `(("bloom.edit-text"
      . ,(jetpacs-design-style
          `(("font_family" . ,(jetpacs-design-font-family "plex-mono"))
            ("font_size" . ,(jetpacs-design-dimension 13.5))
            ("font_weight" . ,(jetpacs-design-font-weight 400))
            ("line_height" . ,(jetpacs-design-dimension 24.975))
            ("letter_spacing" . ,(jetpacs-design-dimension 0))
            ("content_color" . ,(bloom-design--token "color.on-background")))))
     ;; The app's field: 18 dp sides and 80 dp of room below so the last
     ;; line clears the menu button.  Its 18 dp top inset slides the whole
     ;; field down rather than padding it, so the visible text runs 18 dp
     ;; further down than the 80 dp alone allows: 62 dp below here.
     ("bloom.edit-surface"
      . ,(jetpacs-design-style
          `(("padding_start" . ,(jetpacs-design-dimension 18))
            ("padding_end" . ,(jetpacs-design-dimension 18))
            ("padding_top" . ,(jetpacs-design-dimension 18))
            ("padding_bottom" . ,(jetpacs-design-dimension 62))))))
   (mapcar
    (pcase-lambda (`(,_role ,style ,properties))
      (cons style
            (jetpacs-design-style
             (mapcar (pcase-lambda (`(,property . ,value))
                       (cons property
                             (bloom-design--syntax-value property value)))
                     properties))))
    bloom-design-syntax-roles)
   (cl-loop for level from 1 to 8
            collect
            (cons (format "bloom.syn.heading-%d" level)
                  (jetpacs-design-style
                   `(("content_color"
                      . ,(bloom-design--token
                          (concat "color."
                                  (nth (% (1- level) 6)
                                       bloom-design-star-colors))))))))))

(defun bloom-design--syntax-bindings ()
  "Return the `editor.syntax.*' slot bindings for Bloom's editor."
  (append
   (mapcar (pcase-lambda (`(,role ,style ,_))
             (cons (concat "editor.syntax." role) (list style)))
           bloom-design-syntax-roles)
   (cl-loop for level from 1 to 8
            collect (cons (format "editor.syntax.heading-%d" level)
                          (list (format "bloom.syn.heading-%d" level))))))

(defun bloom-design--parity-styles ()
  "Return the faces that reproduce the Android app's chrome metrics."
  (append
   (bloom-design--text-role-styles)
   (bloom-design--read-styles)
   (bloom-design--edit-styles)
   (bloom-design--monogram-styles)
   (bloom-design--ink-styles)
   (bloom-design--chip-styles)
   `(("bloom.rail"
      . ,(jetpacs-design-style
          `(("background_color" . ,(bloom-design--token "color.outline"))
            ("corner_radius" . ,(jetpacs-design-dimension 2)))))
     ;; The reachability dock: a 60 dp raised capsule with a hairline and
     ;; 6 dp inset, and the 72 dp accent pill that overhangs it.
     ("bloom.dock"
      . ,(jetpacs-design-style
          `(("background_color" . ,(bloom-design--token "color.surface-raised"))
            ("border_color" . ,(bloom-design--token "color.line"))
            ("border_width" . ,(jetpacs-design-dimension 1))
            ("corner_radius" . ,(jetpacs-design-dimension 30))
            ("height" . ,(jetpacs-design-dimension 60))
            ("padding" . ,(jetpacs-design-dimension 6)))))
     ("bloom.dock-pill"
      . ,(jetpacs-design-style
          `(("background_color" . ,(bloom-design--token "color.primary"))
            ("corner_radius" . ,(jetpacs-design-dimension 36))
            ("height" . ,(jetpacs-design-dimension 72))
            ("padding_horizontal" . ,(jetpacs-design-dimension 22)))))
     ;; Material's standard 360 dp modal sheet on the surface color, as the
     ;; app's `ModalDrawerSheet' draws it.
     ("bloom.drawer"
      . ,(jetpacs-design-style
          `(("background_color" . ,(bloom-design--token "color.surface"))
            ("width" . ,(jetpacs-design-dimension 360)))))
     ("bloom.drawer-active"
      . ,(jetpacs-design-style
          `(("background_color" . ,(bloom-design--token "color.accent-soft")))))
     ;; The modal drawer keeps Material's own scrim.
     ("bloom.scrim" . ,(jetpacs-design-style nil))
     ;; Outline: the faded PREFACE band, 44 dp glyph buttons, the 54 dp
     ;; add-note FAB, and the flat heading row on the screen color.
     ("bloom.preface"
      . ,(jetpacs-design-style
          `(("background_color" . ,(bloom-design--token "color.surface-raised"))
            ("corner_radius" . ,(jetpacs-design-dimension 10)))))
     ("bloom.glyph-button"
      . ,(jetpacs-design-style
          `(("width" . ,(jetpacs-design-dimension 44))
            ("height" . ,(jetpacs-design-dimension 44))
            ("corner_radius" . ,(jetpacs-design-dimension 10))
            ("content_color" . ,(bloom-design--token "color.on-background")))))
     ("bloom.fab"
      . ,(jetpacs-design-style
          `(("width" . ,(jetpacs-design-dimension 54))
            ("height" . ,(jetpacs-design-dimension 54))
            ("corner_radius" . ,(jetpacs-design-dimension 18))
            ("background_color" . ,(bloom-design--token "color.primary")))))
     ("bloom.flat-row"
      . ,(jetpacs-design-style
          `(("background_color" . ,(bloom-design--token "color.background"))
            ("content_color" . ,(bloom-design--token "color.on-background"))
            ("corner_radius" . ,(jetpacs-design-dimension 10))
            ;; The receiver's base card keeps a hairline even at zero width;
            ;; drawing it in the screen color makes the row flat, as the app's.
            ("border_width" . ,(jetpacs-design-dimension 0))
            ("border_color" . ,(bloom-design--token "color.background"))
            ("padding" . ,(jetpacs-design-dimension 0)))))
     ("bloom.hairline"
      . ,(jetpacs-design-style
          `(("background_color" . ,(bloom-design--token "color.line"))
            ("height" . ,(jetpacs-design-dimension 1))))))))

(defun bloom-design--edge-colors (colors)
  "Return the folder-tile hairline colors derived from COLORS.
A literal `#RRGGBB' foreground gains the app's 40 percent alpha; a theme
role passes through, since a role has no alpha to scale."
  (cl-loop
   for (key fg _soft) in bloom-design-monogram-palette
   collect
   (let ((value (cdr (assoc fg colors))))
     (cons (concat "edge-" key)
           (if (and (equal (plist-get value :kind) "color")
                    (= (length (plist-get value :value)) 7))
               (jetpacs-design-color
                (concat "#66" (substring (plist-get value :value) 1)))
             value)))))

(defun bloom-design-color (name)
  "Return the active profile's color NAME as a canvas color.
A literal token gives its `#RRGGBB' or `#AARRGGBB' value; a theme-role token
gives the role, so canvas drawing follows the profile like styled nodes do.
Falls back to the `outline' role when NAME is not a color token."
  (let* ((profile (jetpacs-design-active-profile bloom-design-profile-id))
         (value (cdr (assoc (concat "color." name)
                            (plist-get profile :tokens)))))
    (pcase (plist-get value :kind)
      ("color" (plist-get value :value))
      ("theme-role" (plist-get value :value))
      (_ "outline"))))

(defun bloom-design-opaque-color (name)
  "Return the active profile's color NAME made opaque over the background.
A soft fill such as `warning-soft' carries alpha as `#AARRGGBB'.  Places
that read colors differently, such as rich-text spans, get the same tint
as an opaque `#RRGGBB' instead.  A theme-role color comes back as is."
  (let ((fill (bloom-design-color name))
        (under (bloom-design-color "background")))
    (if (and (string-match-p "\\`#[[:xdigit:]]\\{8\\}\\'" fill)
             (string-match-p "\\`#[[:xdigit:]]\\{6\\}\\'" under))
        (let ((alpha (/ (string-to-number (substring fill 1 3) 16) 255.0)))
          (apply #'format "#%02X%02X%02X"
                 (cl-loop for i from 0 below 3
                          collect (round
                                   (+ (* alpha (string-to-number
                                                (substring fill (+ 3 (* 2 i)) (+ 5 (* 2 i))) 16))
                                      (* (- 1 alpha) (string-to-number
                                                      (substring under (+ 1 (* 2 i)) (+ 3 (* 2 i))) 16)))))))
      fill)))

(defun bloom-design-parity-available-p (&optional profile)
  "Return non-nil when PROFILE declares Bloom's exact parity faces.
PROFILE defaults to the active profile.  A portable profile copied before
these faces existed lacks them; Bloom then keeps its Material builders so it
never sends a style reference the receiver cannot resolve."
  (let ((profile (or profile
                     (jetpacs-design-active-profile bloom-design-profile-id))))
    (and profile
         (cl-every (lambda (name)
                     (assoc name (plist-get profile :styles)))
                   '("bloom.t.row-title" "bloom.tile" "bloom.dock")))))

(defconst bloom-design-motions
  `(("press" . ,(jetpacs-design-motion 90 "ease-out"))
    ("hover-focus" . ,(jetpacs-design-motion 120 "ease-out"))
    ("selection" . ,(jetpacs-design-motion 160 "ease-in-out")))
  "Bloom's bounded state motions.")

(defconst bloom-design-component-styles
  '(("text.body" . ("typography.body"))
    ("text.title" . ("typography.screen-title"))
    ("text.headline" . ("typography.section-title"))
    ("text.caption" . ("typography.caption" "bloom.muted"))
    ("text.label" . ("typography.label"))
    ("text.mono" . ("typography.code"))
    ("button.filled" . ("bloom.interactive" "bloom.action"))
    ("button.tonal" . ("bloom.interactive" "bloom.surface"))
    ("button.elevated" . ("bloom.interactive" "bloom.surface"))
    ("button.outlined" . ("bloom.interactive" "bloom.surface"))
    ("button.text" . ("bloom.interactive" "bloom.accent"))
    ("button.label" . ("typography.label"))
    ("chip.container" . ("bloom.chip"))
    ("chip.label" . ("typography.label"))
    ("divider.line" . ("bloom.line"))
    ("section-header.container" . ("bloom.section"))
    ("section-header.title" . ("typography.label" "bloom.accent"))
    ("icon-button.container" . ("bloom.interactive"))
    ("menu.trigger" . ("bloom.interactive"))
    ("menu.container" . ("bloom.popup"))
    ("menu.item" . ("bloom.interactive"))
    ("menu.item-label" . ("typography.label-large"))
    ("menu.item-supporting" . ("typography.caption" "bloom.muted"))
    ("menu.group-label" . ("typography.label" "bloom.muted"))
    ("switch.track" . ("bloom.switch-track"))
    ("switch.thumb" . ("bloom.switch-thumb"))
    ("switch.label" . ("typography.body"))
    ("collapsible.header" . ("bloom.disclosure"))
    ("collapsible.chevron" . ("bloom.muted"))
    ("collapsible.body" . ("bloom.indent"))
    ("month-grid.container" . ("bloom.surface"))
    ("month-grid.header" . ("typography.item-title"))
    ("month-grid.nav" . ("bloom.interactive"))
    ("month-grid.weekday" . ("typography.caption" "bloom.calendar-weekday"))
    ("month-grid.day" . ("typography.code" "bloom.day"))
    ("month-grid.today" . ("bloom.today"))
    ("month-grid.range" . ("bloom.band"))
    ("month-grid.mark" . ("bloom.accent"))
    ("chrome.tab-label" . ("typography.label"))
    ("chrome.tab-indicator" . ("bloom.indicator"))
    ("chrome.rail-label" . ("typography.label"))
    ("chrome.rail-indicator" . ("bloom.band"))
    ("chrome.drawer" . ("bloom.drawer"))
    ("chrome.scrim" . ("bloom.scrim"))
    ("chrome.bottom-bar" . ("bloom.scrim"))
    ("chrome.dialog" . ("bloom.scrim"))
    ("chrome.snackbar" . ("bloom.popup"))
    ("badge.container" . ("bloom.chip"))
    ("badge.label" . ("typography.metadata"))
    ("empty-state.container" . ("bloom.screen"))
    ("empty-state.title" . ("typography.section-title"))
    ("empty-state.caption" . ("typography.body" "bloom.muted"))
    ("card.filled" . ("bloom.surface"))
    ("card.elevated" . ("bloom.surface"))
    ("card.outlined" . ("bloom.surface"))
    ("list-item.container" . ("bloom.row"))
    ("list-item.overline" . ("typography.label" "bloom.muted"))
    ("list-item.title" . ("typography.item-title"))
    ("list-item.subtitle" . ("typography.caption" "bloom.muted"))
    ("swipe.cell" . ("bloom.swipe-cell"))
    ("swipe.label" . ("typography.metadata"))
    ("action.container" . ("bloom.interactive" "bloom.action"))
    ("action.label" . ("typography.label-large"))
    ("choice.container" . ("bloom.interactive"))
    ("choice.indicator" . ("bloom.indicator"))
    ("choice.label" . ("typography.body"))
    ("panel.container" . ("bloom.surface"))
    ("panel.label" . ("typography.section-title"))
    ("picker.container" . ("bloom.interactive" "bloom.surface"))
    ("picker.label" . ("typography.label"))
    ("tabs.container" . ("bloom.surface"))
    ("tabs.item" . ("bloom.interactive"))
    ("tabs.indicator" . ("bloom.indicator"))
    ("tabs.label" . ("typography.label"))
    ("section-navigator.container" . ("bloom.surface"))
    ("section-navigator.option" . ("bloom.interactive"))
    ("section-navigator.label" . ("typography.body"))
    ("section-navigator.button" . ("bloom.interactive"))
    ("section-navigator.selector" . ("bloom.interactive"))
    ("section-navigator.popup" . ("bloom.popup"))
    ("section-navigator.popup-item" . ("bloom.interactive"))
    ("text-field.outlined" . ("bloom.field"))
    ("text-field.filled" . ("bloom.field"))
    ("text-field.text" . ("typography.body"))
    ("text-field.label" . ("typography.label"))
    ("text-field.placeholder" . ("typography.body" "bloom.muted"))
    ("text-field.supporting" . ("typography.supporting"))
    ("text-field.affix" . ("typography.label"))
    ("dropdown.field" . ("bloom.field"))
    ("dropdown.text" . ("typography.body"))
    ("dropdown.label" . ("typography.label"))
    ("dropdown.popup" . ("bloom.popup"))
    ("dropdown.item" . ("bloom.interactive"))
    ("editor.surface" . ("bloom.surface"))
    ("editor.chromeless" . ("bloom.screen"))
    ("editor.text" . ("typography.code"))
    ("editor.gutter" . ("typography.metadata" "bloom.muted"))
    ("editor.toolbar" . ("bloom.surface"))
    ("editor.toolbar-item" . ("bloom.interactive" "typography.label"))
    ("editor.sync-status" . ("typography.metadata"))
    ("editor.completion-list" . ("bloom.popup"))
    ("editor.completion-item" . ("bloom.interactive"))
    ("editor.candidate-document" . ("typography.reading-body"))
    ("editor.tooling-status" . ("typography.metadata")))
  "Every closed component slot, bound to Bloom typography and styles.
The six `text.*' slots give ordinary EBP text nodes Bloom's Plex scale by
canonical style name, so screens need no per-node wrappers.")

(defun bloom-design--chrome-roles ()
  "Map every EBP theme role onto the preset's own `color.*' token.
A literal-palette preset re-declares the roles so the receiver's chrome (the
scaffold background, top bar, tab indicator, cards, and sheets) follows the
app's palette instead of the ambient Emacs theme."
  (mapcar (lambda (role)
            (cons role
                  (bloom-design--token
                   (concat "color." (replace-regexp-in-string "_" "-" role)))))
          jetpacs-theme-roles))

(cl-defun bloom-design--make-profile (id label colors &key chrome)
  "Build and validate the Bloom profile ID labeled LABEL over color alist COLORS.
With CHROME non-nil the profile also re-declares the EBP theme roles."
  (jetpacs-design-make-profile
   id label
   :tokens (bloom-design--tokens colors)
   :typography bloom-design-typography
   :styles (append (bloom-design--styles) (bloom-design--parity-styles))
   :motions bloom-design-motions
   :component-styles (append bloom-design-component-styles
                             (bloom-design--syntax-bindings))
   :theme-roles (and chrome (bloom-design--chrome-roles))))

(defun bloom-design-profile ()
  "Return Bloom's validated palette-neutral default profile.
It declares no theme roles, so chrome keeps following the Emacs theme."
  (bloom-design--make-profile
   bloom-design-profile-id "Bloom" (bloom-design--palette-neutral-colors)))

(defun bloom-design-light-profile ()
  "Return the validated Bloom Light preset with the app's literal palette."
  (bloom-design--make-profile
   bloom-design-light-profile-id "Bloom Light"
   (bloom-design--literal-colors bloom-design-light-palette)
   :chrome t))

(defun bloom-design-dark-profile ()
  "Return the validated Bloom Dark preset with the app's literal palette."
  (bloom-design--make-profile
   bloom-design-dark-profile-id "Bloom Dark"
   (bloom-design--literal-colors bloom-design-dark-palette)
   :chrome t))

(defun bloom-design-grove-light-profile ()
  "Return the validated Grove Light preset, the app's original palette."
  (bloom-design--make-profile
   bloom-design-grove-light-profile-id "Grove Light"
   (bloom-design--literal-colors bloom-design-grove-light-palette)
   :chrome t))

(defun bloom-design-grove-dark-profile ()
  "Return the validated Grove Dark preset, the app's original dark palette."
  (bloom-design--make-profile
   bloom-design-grove-dark-profile-id "Grove Dark"
   (bloom-design--literal-colors bloom-design-grove-dark-palette)
   :chrome t))

(defun bloom-design-register ()
  "Register Bloom's five immutable design presets idempotently.
Bloom Light and Bloom Dark are the app's defaults; Grove Light and Grove
Dark are the original palettes the app still offers."
  (jetpacs-design-register-profile (bloom-design-profile))
  (jetpacs-design-register-profile (bloom-design-light-profile))
  (jetpacs-design-register-profile (bloom-design-dark-profile))
  (jetpacs-design-register-profile (bloom-design-grove-light-profile))
  (jetpacs-design-register-profile (bloom-design-grove-dark-profile)))

(defun bloom-design-unregister ()
  "Remove Bloom's presets without altering user profile snapshots."
  (dolist (id bloom-design-preset-ids)
    (jetpacs-design-unregister-profile id)))

(defun bloom-design--copy-universal-attributes (source target)
  "Copy SOURCE's authored universal attributes onto TARGET."
  (let ((result target))
    (dolist (attribute jetpacs-universal-attributes result)
      (when (plist-member source attribute)
        (setq result
              (plist-put result attribute (plist-get source attribute)))))))

(defun bloom-design--agenda-option (node)
  "Return NODE's Agenda range option pair, or nil."
  (when (and (equal (plist-get node :t) "button")
             (equal (plist-get (plist-get node :on_tap) :action)
                    "bloom.agenda.select"))
    (when-let* ((value
                 (plist-get
                  (plist-get (plist-get node :on_tap) :args)
                  :range)))
      (and (member value '("today" "upcoming"))
           (cons value node)))))

(defun bloom-design--agenda-tabs (node)
  "Return semantic controlled tabs for Agenda range row NODE, or nil."
  (when (equal (plist-get node :t) "row")
    (let* ((children (append (plist-get node :children) nil))
           (options (mapcar #'bloom-design--agenda-option children)))
      (when (and (= (length children) 2)
                 (cl-every #'identity options)
                 (equal (sort (mapcar #'car options) #'string<)
                        '("today" "upcoming")))
        (let ((selected
               (or (car (cl-find-if
                         (lambda (option)
                           (equal (plist-get (cdr option) :variant) "filled"))
                         options))
                   (caar options))))
          (bloom-design--copy-universal-attributes
           node
           (jetpacs-component-tabs
            "bloom-agenda-range"
            (mapcar (lambda (option)
                      (jetpacs-component-tab
                       (plist-get (cdr option) :label) (car option)))
                    options)
            selected (jetpacs-action "bloom.agenda.select")
            :variant "fixed")))))))

(defun bloom-design--project-value (value kind profile)
  "Copy and recursively project typed nodes in VALUE for KIND under PROFILE."
  (cond
   ((vectorp value)
    (vconcat (mapcar (lambda (child)
                       (bloom-design--project-value child kind profile))
                     value)))
   ((and (listp value) (keywordp (car value))
         (stringp (plist-get value :t)))
    (bloom-design--project-node value kind profile))
   ((consp value)
    (cons (bloom-design--project-value (car value) kind profile)
          (bloom-design--project-value (cdr value) kind profile)))
   (t value)))

(defun bloom-design--project-node (node kind profile)
  "Copy and project one typed NODE for Bloom screen KIND under PROFILE.
PROFILE is the active design profile, kept for projections that must know
which styles it declares."
  (ignore profile)
  (or (and (eq kind 'agenda) (bloom-design--agenda-tabs node))
      ;; Labeled buttons stay canonical: inside the scope the receiver's
      ;; design button override renders them through the profile's
      ;; `button.*' slots, so no Bloom-private projection is needed.
      (cl-loop for (key value) on node by #'cddr
               append (list key (bloom-design--project-value value kind profile)))))

(defun bloom-design--reading-body-scope (screen profile)
  "Rebind body text under SCREEN's scaffold body to PROFILE's reading prose.
Read mode is the one place Bloom sets Serif: a nested scope rebinds only the
`text.body' slot for the document body, so headings, captions, and chips keep
their Sans and Mono roles.  A portable profile without a `reading-body'
typography role leaves SCREEN unchanged."
  (if (and (equal (plist-get screen :t) "scaffold")
           (plist-get screen :body)
           (assoc "reading-body" (plist-get profile :typography)))
      (plist-put (copy-sequence screen) :body
                 (jetpacs-design-scope
                  nil nil (list (plist-get screen :body))
                  :component-styles
                  (list (jetpacs-design-component-style
                         "text.body" '("typography.reading-body")))))
    screen))

(defun bloom-design-present-screen (screen kind)
  "Return a profile-scoped copy of Bloom SCREEN for closed screen KIND."
  (unless (and (jetpacs-root-node-p screen)
               (memq kind bloom-design-screen-kinds))
    (error "Bloom design expected a screen node and known presentation kind"))
  (let* ((profile (jetpacs-design-active-profile bloom-design-profile-id))
         (result (progn
                   (unless profile
                     (error "Bloom design profile is not registered"))
                   (bloom-design--project-node screen kind profile))))
    (when (eq kind 'document)
      (setq result (bloom-design--reading-body-scope result profile)))
    (jetpacs-design-profile-scope profile (list result))))

(defun bloom-design-present-if-advertised (screen kind)
  "Return designed SCREEN for KIND when advertised, otherwise SCREEN itself."
  (if (jetpacs-extension-advertised-p "jetpacs.design" :app)
      (bloom-design-present-screen screen kind)
    screen))

(provide 'bloom-design)
;;; bloom-design.el ends here
