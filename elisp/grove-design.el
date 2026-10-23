;;; grove-design.el --- configurable Grove presentation for Jetpacs -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Grove's bounded `jetpacs.design' preset.  Colors remain symbolic EBP theme
;; roles, so the active Jetpacs theme (including Modus and ef projections)
;; stays authoritative in light and dark mode.  The preset supplies Grove's
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

(defconst grove-design-profile-id "grove.default"
  "Identifier of Grove's immutable palette-neutral default design profile.")

(defconst grove-design-light-profile-id "grove.light"
  "Identifier of the immutable Grove Light preset (the Android app's default).")

(defconst grove-design-dark-profile-id "grove.dark"
  "Identifier of the immutable Grove Dark preset.")

(defconst grove-design-preset-ids
  (list grove-design-profile-id grove-design-light-profile-id
        grove-design-dark-profile-id)
  "Every preset Grove registers, in registration order.")

(defconst grove-design-screen-kinds
  '(notebooks agenda outline search settings document planning)
  "Closed set of Grove app screen kinds that accept the design profile.")

(defun grove-design--role (name)
  "Return symbolic theme role NAME as a design value."
  (jetpacs-design-theme-role name))

(defun grove-design--token (name)
  "Return token reference NAME as a design value."
  (jetpacs-design-token name))

(defconst grove-design-typography
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
  "Grove's IBM Plex type scale, taken from the Android app's effective sizes.
Sans carries chrome and lists, Serif carries read-mode prose, and Mono
carries the editor, timestamps, tags, and file names.  Every Plex face is
1.3 em tall (ascent 1025, descent 275, no gap), and the app sets no line
height except read-mode prose (1.65 em) and the editor (1.85 em), so every
other role carries exactly 1.3 times its size.")

(defconst grove-design--semantic-color-names
  '("primary" "on-primary" "secondary" "on-secondary" "error" "on-error"
    "background" "on-background" "surface" "on-surface" "outline"
    "success" "warning")
  "The 13 EBP theme roles every Grove preset binds as `color.NAME' tokens.")

(defconst grove-design--extended-color-names
  '("muted" "faint" "surface-raised" "surface-sunken" "line" "accent-soft"
    "success-soft" "warning-soft" "error-soft" "info" "info-soft" "violet"
    "syn-star" "syn-todo" "syn-done" "syn-keyword" "syn-timestamp" "syn-tag"
    "syn-link" "syn-property")
  "Grove-specific colors beyond the EBP roles, also bound as `color.NAME'.
Every preset defines all of them so a Design Lab copy of any preset carries
the complete vocabulary Grove components reference.")

(defun grove-design--palette-neutral-colors ()
  "Return the default preset's colors: every token resolves a live theme role.
Grove-specific colors collapse onto the nearest neutral role, so the preset
follows Modus, ef, and light/dark changes without a fixed palette."
  (append
   (mapcar (lambda (name)
             (cons name (grove-design--role
                         (replace-regexp-in-string "-" "_" name))))
           grove-design--semantic-color-names)
   `(("muted" . ,(grove-design--role "outline"))
     ("faint" . ,(grove-design--role "outline"))
     ("surface-raised" . ,(grove-design--role "surface"))
     ("surface-sunken" . ,(grove-design--role "surface"))
     ("line" . ,(grove-design--role "outline"))
     ("accent-soft" . ,(grove-design--role "surface"))
     ("success-soft" . ,(grove-design--role "surface"))
     ("warning-soft" . ,(grove-design--role "surface"))
     ("error-soft" . ,(grove-design--role "surface"))
     ("info" . ,(grove-design--role "secondary"))
     ("info-soft" . ,(grove-design--role "surface"))
     ("violet" . ,(grove-design--role "secondary"))
     ("syn-star" . ,(grove-design--role "success"))
     ("syn-todo" . ,(grove-design--role "warning"))
     ("syn-done" . ,(grove-design--role "success"))
     ("syn-keyword" . ,(grove-design--role "outline"))
     ("syn-timestamp" . ,(grove-design--role "secondary"))
     ("syn-tag" . ,(grove-design--role "primary"))
     ("syn-link" . ,(grove-design--role "secondary"))
     ("syn-property" . ,(grove-design--role "outline")))))

(defconst grove-design-light-palette
  '(("primary" . "#8A5A2B") ("on-primary" . "#FFFAF2")
    ("secondary" . "#6C6356") ("on-secondary" . "#FBF8F1")
    ("error" . "#A5462F") ("on-error" . "#FBF8F1")
    ("background" . "#F3EDE1") ("on-background" . "#2A251F")
    ("surface" . "#FBF8F1") ("on-surface" . "#2A251F")
    ("outline" . "#D3C9B6") ("success" . "#4F7A3A") ("warning" . "#A9761D")
    ("muted" . "#6C6356") ("faint" . "#9C9384")
    ("surface-raised" . "#ECE4D5") ("surface-sunken" . "#E3D9C6")
    ("line" . "#E3DBCB") ("accent-soft" . "#1F8A5A2B")
    ("success-soft" . "#244F7A3A") ("warning-soft" . "#29A9761D")
    ("error-soft" . "#21A5462F") ("info" . "#3F6F86") ("info-soft" . "#213F6F86")
    ("violet" . "#6E5A8E")
    ("syn-star" . "#4F7A3A") ("syn-todo" . "#A9761D") ("syn-done" . "#4F7A3A")
    ("syn-keyword" . "#7D7466") ("syn-timestamp" . "#3F6F86")
    ("syn-tag" . "#8A5A2B") ("syn-link" . "#3F6F86") ("syn-property" . "#A39A89"))
  "Grove Light: the Android app's warm paper palette, verbatim.
Soft fills carry their real alpha (`#AARRGGBB') rather than opaque tints.")

(defconst grove-design-dark-palette
  '(("primary" . "#CB9D62") ("on-primary" . "#1A160D")
    ("secondary" . "#B4A98F") ("on-secondary" . "#201C15")
    ("error" . "#D2856A") ("on-error" . "#201C15")
    ("background" . "#16130E") ("on-background" . "#ECE4D5")
    ("surface" . "#201C15") ("on-surface" . "#ECE4D5")
    ("outline" . "#433A2B") ("success" . "#8FB46A") ("warning" . "#D7A64F")
    ("muted" . "#B4A98F") ("faint" . "#7C7460")
    ("surface-raised" . "#2A251B") ("surface-sunken" . "#352F23")
    ("line" . "#322C20") ("accent-soft" . "#2ECB9D62")
    ("success-soft" . "#2B8FB46A") ("warning-soft" . "#2BD7A64F")
    ("error-soft" . "#29D2856A") ("info" . "#7FB0C4") ("info-soft" . "#297FB0C4")
    ("violet" . "#AD9BD1")
    ("syn-star" . "#8FB46A") ("syn-todo" . "#D7A64F") ("syn-done" . "#8FB46A")
    ("syn-keyword" . "#AD9BD1") ("syn-timestamp" . "#7FB0C4")
    ("syn-tag" . "#CB9D62") ("syn-link" . "#7FB0C4") ("syn-property" . "#7C7460"))
  "Grove Dark: the Android app's warm charcoal palette, verbatim.")

(defun grove-design--literal-colors (palette)
  "Return PALETTE's `(NAME . HEX)' pairs as literal design color values."
  (mapcar (lambda (pair) (cons (car pair) (jetpacs-design-color (cdr pair))))
          palette))

(defun grove-design--tokens (colors)
  "Return Grove's complete token alist around color alist COLORS."
  (let ((names (append grove-design--semantic-color-names
                       grove-design--extended-color-names)))
    (dolist (name names)
      (unless (assoc name colors)
        (error "Grove design palette is missing color %s" name)))
    (append
     (mapcar (lambda (name)
               (cons (concat "color." name) (cdr (assoc name colors))))
             names)
     (mapcar (lambda (pair) (cons (concat "color." (car pair)) (cdr pair)))
             (grove-design--edge-colors colors))
     `(("space.compact" . ,(jetpacs-design-dimension 6))
       ("space.control-x" . ,(jetpacs-design-dimension 14))
       ("space.control-y" . ,(jetpacs-design-dimension 10))
       ("space.panel" . ,(jetpacs-design-dimension 14))
       ("space.gutter" . ,(jetpacs-design-dimension 14))
       ("radius.chip" . ,(jetpacs-design-dimension 6))
       ("radius.control" . ,(jetpacs-design-dimension 10))
       ("radius.panel" . ,(jetpacs-design-dimension 14))
       ("radius.pill" . ,(jetpacs-design-dimension 20))))))

(defun grove-design--styles ()
  "Return Grove's style alist; every color goes through a `color.*' token."
  `(("grove.screen"
     . ,(jetpacs-design-style
         `(("background_color" . ,(grove-design--token "color.background"))
           ("content_color" . ,(grove-design--token "color.on-background"))
           ("fill_width" . ,(jetpacs-design-boolean t)))))
    ("grove.surface"
     . ,(jetpacs-design-style
         `(("background_color" . ,(grove-design--token "color.surface"))
           ("content_color" . ,(grove-design--token "color.on-surface"))
           ("border_color" . ,(grove-design--token "color.line"))
           ("border_width" . ,(jetpacs-design-dimension 1))
           ("corner_radius" . ,(grove-design--token "radius.panel"))
           ("padding" . ,(grove-design--token "space.panel")))))
    ("grove.interactive"
     . ,(jetpacs-design-style
         `(("content_color" . ,(grove-design--token "color.on-surface"))
           ("corner_radius" . ,(grove-design--token "radius.control"))
           ("padding_horizontal" . ,(grove-design--token "space.control-x"))
           ("padding_vertical" . ,(grove-design--token "space.control-y"))
           ("min_height" . ,(jetpacs-design-dimension 48)))
         :rules
         (list
          (jetpacs-design-rule
           "hovered" `(("alpha" . ,(jetpacs-design-number 0.92)))
           :motion "hover-focus")
          (jetpacs-design-rule
           "focused"
           `(("border_color" . ,(grove-design--token "color.primary"))
             ("border_width" . ,(jetpacs-design-dimension 2)))
           :motion "hover-focus")
          (jetpacs-design-rule
           "pressed" `(("scale" . ,(jetpacs-design-number 0.98)))
           :motion "press")
          (jetpacs-design-rule
           "selected"
           `(("background_color" . ,(grove-design--token "color.primary"))
             ("content_color" . ,(grove-design--token "color.on-primary")))
           :motion "selection")
          (jetpacs-design-rule
           "toggled"
           `(("background_color" . ,(grove-design--token "color.primary"))
             ("content_color" . ,(grove-design--token "color.on-primary")))
           :motion "selection")
          (jetpacs-design-rule
           "disabled" `(("alpha" . ,(jetpacs-design-number 0.38)))
           :motion "hover-focus"))))
    ("grove.action"
     . ,(jetpacs-design-style
         `(("background_color" . ,(grove-design--token "color.primary"))
           ("content_color" . ,(grove-design--token "color.on-primary"))
           ("corner_radius" . ,(grove-design--token "radius.pill")))))
    ("grove.indicator"
     . ,(jetpacs-design-style
         `(("background_color" . ,(grove-design--token "color.primary"))
           ("content_color" . ,(grove-design--token "color.on-primary"))
           ("corner_radius" . ,(jetpacs-design-dimension 4)))))
    ("grove.field"
     . ,(jetpacs-design-style
         `(("background_color" . ,(grove-design--token "color.surface"))
           ("content_color" . ,(grove-design--token "color.on-surface"))
           ("border_color" . ,(grove-design--token "color.line"))
           ("border_width" . ,(jetpacs-design-dimension 1))
           ("corner_radius" . ,(grove-design--token "radius.control"))
           ("padding_horizontal" . ,(grove-design--token "space.control-x"))
           ("padding_vertical" . ,(grove-design--token "space.control-y")))))
    ("grove.popup"
     . ,(jetpacs-design-style
         `(("background_color" . ,(grove-design--token "color.surface"))
           ("content_color" . ,(grove-design--token "color.on-surface"))
           ("border_color" . ,(grove-design--token "color.line"))
           ("border_width" . ,(jetpacs-design-dimension 1))
           ("corner_radius" . ,(grove-design--token "radius.control")))))
    ("grove.pill"
     . ,(jetpacs-design-style
         `(("background_color" . ,(grove-design--token "color.accent-soft"))
           ("content_color" . ,(grove-design--token "color.primary"))
           ("corner_radius" . ,(grove-design--token "radius.pill"))
           ("padding_horizontal" . ,(jetpacs-design-dimension 9))
           ("padding_vertical" . ,(jetpacs-design-dimension 3)))))
    ("grove.chip"
     . ,(jetpacs-design-style
         `(("background_color" . ,(grove-design--token "color.surface-raised"))
           ("content_color" . ,(grove-design--token "color.on-surface"))
           ("corner_radius" . ,(grove-design--token "radius.chip"))
           ("padding_horizontal" . ,(jetpacs-design-dimension 8))
           ("padding_vertical" . ,(jetpacs-design-dimension 4)))
         :rules
         (list
          (jetpacs-design-rule
           "selected"
           `(("background_color" . ,(grove-design--token "color.accent-soft"))
             ("content_color" . ,(grove-design--token "color.primary")))
           :motion "selection"))))
    ("grove.line"
     . ,(jetpacs-design-style
         `(("background_color" . ,(grove-design--token "color.line")))))
    ;; A Grove row is flat: the app draws no border or fill behind one, only
    ;; a rounded press face, so the list reads as a column of text rather
    ;; than a stack of objects.
    ("grove.row"
     . ,(jetpacs-design-style
         `(("content_color" . ,(grove-design--token "color.on-background"))
           ("corner_radius" . ,(grove-design--token "radius.panel"))
           ("padding_horizontal" . ,(jetpacs-design-dimension 8))
           ("padding_vertical" . ,(jetpacs-design-dimension 10))
           ("min_height" . ,(jetpacs-design-dimension 48)))
         :rules
         (list
          (jetpacs-design-rule
           "hovered"
           `(("background_color" . ,(grove-design--token "color.surface-raised")))
           :motion "hover-focus")
          (jetpacs-design-rule
           "focused"
           `(("border_color" . ,(grove-design--token "color.primary"))
             ("border_width" . ,(jetpacs-design-dimension 2)))
           :motion "hover-focus")
          (jetpacs-design-rule
           "pressed"
           `(("background_color" . ,(grove-design--token "color.accent-soft")))
           :motion "press")
          (jetpacs-design-rule
           "selected"
           `(("background_color" . ,(grove-design--token "color.accent-soft"))
             ("content_color" . ,(grove-design--token "color.primary")))
           :motion "selection")
          (jetpacs-design-rule
           "disabled" `(("alpha" . ,(jetpacs-design-number 0.38)))
           :motion "hover-focus"))))
    ("grove.swipe-cell"
     . ,(jetpacs-design-style
         `(("background_color" . ,(grove-design--token "color.surface-raised")))))
    ;; The app's own switch is a 40x23 pill: raised surface off, accent on,
    ;; with an 18 dp surface thumb.  The sizes ride on the slot so the
    ;; renderer's default metrics never have to know Grove's.
    ("grove.switch-track"
     . ,(jetpacs-design-style
         `(("width" . ,(jetpacs-design-dimension 40))
           ("height" . ,(jetpacs-design-dimension 23))
           ("corner_radius" . ,(jetpacs-design-dimension 12))
           ("background_color" . ,(grove-design--token "color.surface-raised"))
           ("border_width" . ,(jetpacs-design-dimension 0)))
         :rules
         (list
          (jetpacs-design-rule
           "selected"
           `(("background_color" . ,(grove-design--token "color.primary")))
           :motion "selection")
          (jetpacs-design-rule
           "focused"
           `(("border_color" . ,(grove-design--token "color.primary"))
             ("border_width" . ,(jetpacs-design-dimension 2)))
           :motion "hover-focus")
          (jetpacs-design-rule
           "disabled" `(("alpha" . ,(jetpacs-design-number 0.38)))
           :motion "hover-focus"))))
    ("grove.switch-thumb"
     . ,(jetpacs-design-style
         `(("width" . ,(jetpacs-design-dimension 18))
           ("height" . ,(jetpacs-design-dimension 18))
           ("corner_radius" . ,(jetpacs-design-dimension 9))
           ("background_color" . ,(grove-design--token "color.surface"))
           ("content_color" . ,(grove-design--token "color.primary")))
         :rules
         (list
          (jetpacs-design-rule
           "selected"
           `(("background_color" . ,(grove-design--token "color.on-primary")))
           :motion "selection"))))
    ;; An outline heading discloses its children from a small muted chevron;
    ;; the header row itself stays flat and the children indent under it.
    ("grove.disclosure"
     . ,(jetpacs-design-style
         `(("content_color" . ,(grove-design--token "color.on-background"))
           ("min_height" . ,(jetpacs-design-dimension 48)))
         :rules
         (list
          (jetpacs-design-rule
           "hovered"
           `(("background_color" . ,(grove-design--token "color.surface-raised")))
           :motion "hover-focus")
          (jetpacs-design-rule
           "focused"
           `(("border_color" . ,(grove-design--token "color.primary"))
             ("border_width" . ,(jetpacs-design-dimension 2)))
           :motion "hover-focus"))))
    ("grove.indent"
     . ,(jetpacs-design-style
         `(("padding_start" . ,(jetpacs-design-dimension 20)))))
    ;; The planning calendar: the app's 37 dp rounded day cells in Plex Mono,
    ;; ten-point weekday initials, a hairline ring for today, and the soft
    ;; accent band between the two endpoints.
    ("grove.calendar-weekday"
     . ,(jetpacs-design-style
         `(("font_size" . ,(jetpacs-design-dimension 10))
           ("content_color" . ,(grove-design--token "color.faint")))))
    ("grove.day"
     . ,(jetpacs-design-style
         `(("min_height" . ,(jetpacs-design-dimension 37))
           ("corner_radius" . ,(jetpacs-design-dimension 10))
           ("content_color" . ,(grove-design--token "color.on-background")))
         :rules
         (list
          (jetpacs-design-rule
           "selected"
           `(("background_color" . ,(grove-design--token "color.primary"))
             ("content_color" . ,(grove-design--token "color.on-primary")))
           :motion "selection")
          (jetpacs-design-rule
           "hovered"
           `(("background_color" . ,(grove-design--token "color.surface-raised")))
           :motion "hover-focus")
          (jetpacs-design-rule
           "focused"
           `(("border_color" . ,(grove-design--token "color.primary"))
             ("border_width" . ,(jetpacs-design-dimension 2)))
           :motion "hover-focus")
          (jetpacs-design-rule
           "disabled" `(("alpha" . ,(jetpacs-design-number 0.38)))
           :motion "hover-focus"))))
    ("grove.today"
     . ,(jetpacs-design-style
         `(("border_width" . ,(jetpacs-design-dimension 1))
           ("border_color" . ,(grove-design--token "color.line")))))
    ("grove.band"
     . ,(jetpacs-design-style
         `(("background_color" . ,(grove-design--token "color.accent-soft")))))
    ("grove.accent"
     . ,(jetpacs-design-style
         `(("content_color" . ,(grove-design--token "color.primary")))))
    ("grove.section"
     . ,(jetpacs-design-style
         `(("padding_top" . ,(jetpacs-design-dimension 16))
           ("padding_bottom" . ,(jetpacs-design-dimension 6)))))
    ("grove.muted"
     . ,(jetpacs-design-style
         `(("content_color" . ,(grove-design--token "color.muted")))))
    ("grove.faint"
     . ,(jetpacs-design-style
         `(("content_color" . ,(grove-design--token "color.faint")))))
    ("grove.error"
     . ,(jetpacs-design-style
         `(("content_color" . ,(grove-design--token "color.error")))))))

;;;; Parity faces: the Android app's exact metrics

(defconst grove-design-monogram-palette
  '(("green" "success" "success-soft")
    ("blue" "info" "info-soft")
    ("amber" "warning" "warning-soft")
    ("red" "error" "error-soft")
    ("accent" "primary" "accent-soft")
    ("neutral" "muted" "surface-raised"))
  "Monogram palette keys with their foreground and soft-fill color names.
The order is the app's `MONOGRAM_PALETTE_KEYS', which its name hash indexes.")

(defconst grove-design-text-roles
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
    ("fab-plus" "plex-sans" 26 400 "on-primary"))
  "The Android app's literal text roles, one face per distinct `Text' call.
Each entry is name, family, size, weight, color, tracking, and line height
in em, and becomes style `grove.t.NAME'.  The line height
defaults to 1.65 em: the app's `Text' calls that name no style inherit
Material's `bodyLarge', which Grove sets to 1.65 em, while those that name a
style (the screen title's `titleLarge') keep Plex's natural 1.3 em.")

(defun grove-design--text-role-styles ()
  "Return one exact text style per entry of `grove-design-text-roles'."
  (mapcar
   (pcase-lambda (`(,name ,family ,size ,weight ,color ,tracking ,line))
     (cons (concat "grove.t." name)
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
                     . ,(grove-design--token (concat "color." color)))))
             (grove-design--untrimmed-leading size (or line 1.65))))))
   grove-design-text-roles))

(defun grove-design--untrimmed-leading (size line)
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

(defun grove-design--monogram-styles ()
  "Return the 42 dp monogram tile and one tint per palette key.
`grove.tile.KEY' fills the tile with the soft color and inks its letter;
`grove.tile-edge.KEY' adds a folder tile's 40 percent hairline."
  (append
   `(("grove.tile"
      . ,(jetpacs-design-style
          `(("width" . ,(jetpacs-design-dimension 42))
            ("height" . ,(jetpacs-design-dimension 42))
            ("corner_radius" . ,(jetpacs-design-dimension 12))))))
   (cl-loop
    for (key fg soft) in grove-design-monogram-palette
    collect (cons (concat "grove.tile." key)
                  (jetpacs-design-style
                   `(("background_color"
                      . ,(grove-design--token (concat "color." soft)))
                     ("content_color"
                      . ,(grove-design--token (concat "color." fg))))))
    collect (cons (concat "grove.tile-edge." key)
                  (jetpacs-design-style
                   `(("border_width" . ,(jetpacs-design-dimension 1))
                     ("border_color"
                      . ,(grove-design--token (concat "color.edge-" key)))))))))

(defconst grove-design-ink-names
  '("success" "info" "warning" "error" "violet" "primary" "faint" "muted"
    "syn-tag" "syn-keyword" "on-background")
  "Colors a parity text may take on top of its role, as `grove.fg.NAME'.")

(defconst grove-design-chip-names '("success" "info" "warning" "error" "violet")
  "Soft chip fills, as `grove.chip.NAME': the soft color behind the color.")

(defun grove-design--ink-styles ()
  "Return one content-color style per `grove-design-ink-names' entry."
  (mapcar (lambda (name)
            (cons (concat "grove.fg." name)
                  (jetpacs-design-style
                   `(("content_color"
                      . ,(grove-design--token (concat "color." name)))))))
          grove-design-ink-names))

(defun grove-design--chip-styles ()
  "Return the app's 5 dp soft chips, one per `grove-design-chip-names'."
  (mapcar (lambda (name)
            (cons (concat "grove.chip." name)
                  (jetpacs-design-style
                   `(("background_color"
                      . ,(grove-design--token
                          (concat "color." (if (equal name "violet")
                                               "accent-soft"
                                             (concat name "-soft")))))
                     ("content_color"
                      . ,(grove-design--token (concat "color." name)))
                     ("corner_radius" . ,(jetpacs-design-dimension 5))
                     ("padding_horizontal" . ,(jetpacs-design-dimension 5))
                     ("padding_vertical" . ,(jetpacs-design-dimension 1))))))
          grove-design-chip-names))

(defun grove-design--parity-styles ()
  "Return the faces that reproduce the Android app's chrome metrics."
  (append
   (grove-design--text-role-styles)
   (grove-design--monogram-styles)
   (grove-design--ink-styles)
   (grove-design--chip-styles)
   `(("grove.rail"
      . ,(jetpacs-design-style
          `(("background_color" . ,(grove-design--token "color.outline"))
            ("corner_radius" . ,(jetpacs-design-dimension 2)))))
     ;; The reachability dock: a 60 dp raised capsule with a hairline and
     ;; 6 dp inset, and the 72 dp accent pill that overhangs it.
     ("grove.dock"
      . ,(jetpacs-design-style
          `(("background_color" . ,(grove-design--token "color.surface-raised"))
            ("border_color" . ,(grove-design--token "color.line"))
            ("border_width" . ,(jetpacs-design-dimension 1))
            ("corner_radius" . ,(jetpacs-design-dimension 30))
            ("height" . ,(jetpacs-design-dimension 60))
            ("padding" . ,(jetpacs-design-dimension 6)))))
     ("grove.dock-pill"
      . ,(jetpacs-design-style
          `(("background_color" . ,(grove-design--token "color.primary"))
            ("corner_radius" . ,(jetpacs-design-dimension 36))
            ("height" . ,(jetpacs-design-dimension 72))
            ("padding_horizontal" . ,(jetpacs-design-dimension 22)))))
     ;; Material's standard 360 dp modal sheet on the surface color, as the
     ;; app's `ModalDrawerSheet' draws it.
     ("grove.drawer"
      . ,(jetpacs-design-style
          `(("background_color" . ,(grove-design--token "color.surface"))
            ("width" . ,(jetpacs-design-dimension 360)))))
     ("grove.drawer-active"
      . ,(jetpacs-design-style
          `(("background_color" . ,(grove-design--token "color.accent-soft")))))
     ;; Outline: the faded PREFACE band, 44 dp glyph buttons, the 54 dp
     ;; add-note FAB, and the flat heading row on the screen color.
     ("grove.preface"
      . ,(jetpacs-design-style
          `(("background_color" . ,(grove-design--token "color.surface-raised"))
            ("corner_radius" . ,(jetpacs-design-dimension 10)))))
     ("grove.glyph-button"
      . ,(jetpacs-design-style
          `(("width" . ,(jetpacs-design-dimension 44))
            ("height" . ,(jetpacs-design-dimension 44))
            ("corner_radius" . ,(jetpacs-design-dimension 10))
            ("content_color" . ,(grove-design--token "color.on-background")))))
     ("grove.fab"
      . ,(jetpacs-design-style
          `(("width" . ,(jetpacs-design-dimension 54))
            ("height" . ,(jetpacs-design-dimension 54))
            ("corner_radius" . ,(jetpacs-design-dimension 18))
            ("background_color" . ,(grove-design--token "color.primary")))))
     ("grove.flat-row"
      . ,(jetpacs-design-style
          `(("background_color" . ,(grove-design--token "color.background"))
            ("content_color" . ,(grove-design--token "color.on-background"))
            ("corner_radius" . ,(jetpacs-design-dimension 10))
            ;; The receiver's base card keeps a hairline even at zero width;
            ;; drawing it in the screen color makes the row flat, as the app's.
            ("border_width" . ,(jetpacs-design-dimension 0))
            ("border_color" . ,(grove-design--token "color.background"))
            ("padding" . ,(jetpacs-design-dimension 0)))))
     ("grove.hairline"
      . ,(jetpacs-design-style
          `(("background_color" . ,(grove-design--token "color.line"))
            ("height" . ,(jetpacs-design-dimension 1))))))))

(defun grove-design--edge-colors (colors)
  "Return the folder-tile hairline colors derived from COLORS.
A literal `#RRGGBB' foreground gains the app's 40 percent alpha; a theme
role passes through, since a role has no alpha to scale."
  (cl-loop
   for (key fg _soft) in grove-design-monogram-palette
   collect
   (let ((value (cdr (assoc fg colors))))
     (cons (concat "edge-" key)
           (if (and (equal (plist-get value :kind) "color")
                    (= (length (plist-get value :value)) 7))
               (jetpacs-design-color
                (concat "#66" (substring (plist-get value :value) 1)))
             value)))))

(defun grove-design-color (name)
  "Return the active profile's color NAME as a canvas color.
A literal token gives its `#RRGGBB' or `#AARRGGBB' value; a theme-role token
gives the role, so canvas drawing follows the profile like styled nodes do.
Falls back to the `outline' role when NAME is not a color token."
  (let* ((profile (jetpacs-design-active-profile grove-design-profile-id))
         (value (cdr (assoc (concat "color." name)
                            (plist-get profile :tokens)))))
    (pcase (plist-get value :kind)
      ("color" (plist-get value :value))
      ("theme-role" (plist-get value :value))
      (_ "outline"))))

(defun grove-design-parity-available-p (&optional profile)
  "Return non-nil when PROFILE declares Grove's exact parity faces.
PROFILE defaults to the active profile.  A portable profile copied before
these faces existed lacks them; Grove then keeps its Material builders so it
never sends a style reference the receiver cannot resolve."
  (let ((profile (or profile
                     (jetpacs-design-active-profile grove-design-profile-id))))
    (and profile
         (cl-every (lambda (name)
                     (assoc name (plist-get profile :styles)))
                   '("grove.t.row-title" "grove.tile" "grove.dock")))))

(defconst grove-design-motions
  `(("press" . ,(jetpacs-design-motion 90 "ease-out"))
    ("hover-focus" . ,(jetpacs-design-motion 120 "ease-out"))
    ("selection" . ,(jetpacs-design-motion 160 "ease-in-out")))
  "Grove's bounded state motions.")

(defconst grove-design-component-styles
  '(("text.body" . ("typography.body"))
    ("text.title" . ("typography.screen-title"))
    ("text.headline" . ("typography.section-title"))
    ("text.caption" . ("typography.caption" "grove.muted"))
    ("text.label" . ("typography.label"))
    ("text.mono" . ("typography.code"))
    ("button.filled" . ("grove.interactive" "grove.action"))
    ("button.tonal" . ("grove.interactive" "grove.surface"))
    ("button.elevated" . ("grove.interactive" "grove.surface"))
    ("button.outlined" . ("grove.interactive" "grove.surface"))
    ("button.text" . ("grove.interactive" "grove.accent"))
    ("button.label" . ("typography.label"))
    ("chip.container" . ("grove.chip"))
    ("chip.label" . ("typography.label"))
    ("divider.line" . ("grove.line"))
    ("section-header.container" . ("grove.section"))
    ("section-header.title" . ("typography.label" "grove.accent"))
    ("icon-button.container" . ("grove.interactive"))
    ("menu.trigger" . ("grove.interactive"))
    ("menu.container" . ("grove.popup"))
    ("menu.item" . ("grove.interactive"))
    ("menu.item-label" . ("typography.label-large"))
    ("menu.item-supporting" . ("typography.caption" "grove.muted"))
    ("menu.group-label" . ("typography.label" "grove.muted"))
    ("switch.track" . ("grove.switch-track"))
    ("switch.thumb" . ("grove.switch-thumb"))
    ("switch.label" . ("typography.body"))
    ("collapsible.header" . ("grove.disclosure"))
    ("collapsible.chevron" . ("grove.muted"))
    ("collapsible.body" . ("grove.indent"))
    ("month-grid.container" . ("grove.surface"))
    ("month-grid.header" . ("typography.item-title"))
    ("month-grid.nav" . ("grove.interactive"))
    ("month-grid.weekday" . ("typography.caption" "grove.calendar-weekday"))
    ("month-grid.day" . ("typography.code" "grove.day"))
    ("month-grid.today" . ("grove.today"))
    ("month-grid.range" . ("grove.band"))
    ("month-grid.mark" . ("grove.accent"))
    ("chrome.tab-label" . ("typography.label"))
    ("chrome.tab-indicator" . ("grove.indicator"))
    ("chrome.rail-label" . ("typography.label"))
    ("chrome.rail-indicator" . ("grove.band"))
    ("chrome.drawer" . ("grove.drawer"))
    ("chrome.snackbar" . ("grove.popup"))
    ("badge.container" . ("grove.chip"))
    ("badge.label" . ("typography.metadata"))
    ("empty-state.container" . ("grove.screen"))
    ("empty-state.title" . ("typography.section-title"))
    ("empty-state.caption" . ("typography.body" "grove.muted"))
    ("card.filled" . ("grove.surface"))
    ("card.elevated" . ("grove.surface"))
    ("card.outlined" . ("grove.surface"))
    ("list-item.container" . ("grove.row"))
    ("list-item.overline" . ("typography.label" "grove.muted"))
    ("list-item.title" . ("typography.item-title"))
    ("list-item.subtitle" . ("typography.caption" "grove.muted"))
    ("swipe.cell" . ("grove.swipe-cell"))
    ("swipe.label" . ("typography.metadata"))
    ("action.container" . ("grove.interactive" "grove.action"))
    ("action.label" . ("typography.label-large"))
    ("choice.container" . ("grove.interactive"))
    ("choice.indicator" . ("grove.indicator"))
    ("choice.label" . ("typography.body"))
    ("panel.container" . ("grove.surface"))
    ("panel.label" . ("typography.section-title"))
    ("tabs.container" . ("grove.surface"))
    ("tabs.item" . ("grove.interactive"))
    ("tabs.indicator" . ("grove.indicator"))
    ("tabs.label" . ("typography.label"))
    ("section-navigator.container" . ("grove.surface"))
    ("section-navigator.option" . ("grove.interactive"))
    ("section-navigator.label" . ("typography.body"))
    ("section-navigator.button" . ("grove.interactive"))
    ("section-navigator.selector" . ("grove.interactive"))
    ("section-navigator.popup" . ("grove.popup"))
    ("section-navigator.popup-item" . ("grove.interactive"))
    ("text-field.outlined" . ("grove.field"))
    ("text-field.filled" . ("grove.field"))
    ("text-field.text" . ("typography.body"))
    ("text-field.label" . ("typography.label"))
    ("text-field.placeholder" . ("typography.body" "grove.muted"))
    ("text-field.supporting" . ("typography.supporting"))
    ("text-field.affix" . ("typography.label"))
    ("dropdown.field" . ("grove.field"))
    ("dropdown.text" . ("typography.body"))
    ("dropdown.label" . ("typography.label"))
    ("dropdown.popup" . ("grove.popup"))
    ("dropdown.item" . ("grove.interactive"))
    ("editor.surface" . ("grove.surface"))
    ("editor.chromeless" . ("grove.screen"))
    ("editor.text" . ("typography.code"))
    ("editor.gutter" . ("typography.metadata" "grove.muted"))
    ("editor.toolbar" . ("grove.surface"))
    ("editor.toolbar-item" . ("grove.interactive" "typography.label"))
    ("editor.sync-status" . ("typography.metadata"))
    ("editor.completion-list" . ("grove.popup"))
    ("editor.completion-item" . ("grove.interactive"))
    ("editor.candidate-document" . ("typography.reading-body"))
    ("editor.tooling-status" . ("typography.metadata")))
  "Every closed component slot, bound to Grove typography and styles.
The six `text.*' slots give ordinary EBP text nodes Grove's Plex scale by
canonical style name, so screens need no per-node wrappers.")

(defun grove-design--chrome-roles ()
  "Map every EBP theme role onto the preset's own `color.*' token.
A literal-palette preset re-declares the roles so the receiver's chrome (the
scaffold background, top bar, tab indicator, cards, and sheets) follows the
app's palette instead of the ambient Emacs theme."
  (mapcar (lambda (role)
            (cons role
                  (grove-design--token
                   (concat "color." (replace-regexp-in-string "_" "-" role)))))
          jetpacs-theme-roles))

(cl-defun grove-design--make-profile (id label colors &key chrome)
  "Build and validate the Grove profile ID labeled LABEL over color alist COLORS.
With CHROME non-nil the profile also re-declares the EBP theme roles."
  (jetpacs-design-make-profile
   id label
   :tokens (grove-design--tokens colors)
   :typography grove-design-typography
   :styles (append (grove-design--styles) (grove-design--parity-styles))
   :motions grove-design-motions
   :component-styles grove-design-component-styles
   :theme-roles (and chrome (grove-design--chrome-roles))))

(defun grove-design-profile ()
  "Return Grove's validated palette-neutral default profile.
It declares no theme roles, so chrome keeps following the Emacs theme."
  (grove-design--make-profile
   grove-design-profile-id "Grove" (grove-design--palette-neutral-colors)))

(defun grove-design-light-profile ()
  "Return the validated Grove Light preset with the app's literal palette."
  (grove-design--make-profile
   grove-design-light-profile-id "Grove Light"
   (grove-design--literal-colors grove-design-light-palette)
   :chrome t))

(defun grove-design-dark-profile ()
  "Return the validated Grove Dark preset with the app's literal palette."
  (grove-design--make-profile
   grove-design-dark-profile-id "Grove Dark"
   (grove-design--literal-colors grove-design-dark-palette)
   :chrome t))

(defun grove-design-register ()
  "Register Grove's three immutable design presets idempotently."
  (jetpacs-design-register-profile (grove-design-profile))
  (jetpacs-design-register-profile (grove-design-light-profile))
  (jetpacs-design-register-profile (grove-design-dark-profile)))

(defun grove-design-unregister ()
  "Remove Grove's presets without altering user profile snapshots."
  (dolist (id grove-design-preset-ids)
    (jetpacs-design-unregister-profile id)))

(defun grove-design--copy-universal-attributes (source target)
  "Copy SOURCE's authored universal attributes onto TARGET."
  (let ((result target))
    (dolist (attribute jetpacs-universal-attributes result)
      (when (plist-member source attribute)
        (setq result
              (plist-put result attribute (plist-get source attribute)))))))

(defun grove-design--agenda-option (node)
  "Return NODE's Agenda range option pair, or nil."
  (when (and (equal (plist-get node :t) "button")
             (equal (plist-get (plist-get node :on_tap) :action)
                    "grove.agenda.select"))
    (when-let* ((value
                 (plist-get
                  (plist-get (plist-get node :on_tap) :args)
                  :range)))
      (and (member value '("today" "upcoming"))
           (cons value node)))))

(defun grove-design--agenda-tabs (node)
  "Return semantic controlled tabs for Agenda range row NODE, or nil."
  (when (equal (plist-get node :t) "row")
    (let* ((children (append (plist-get node :children) nil))
           (options (mapcar #'grove-design--agenda-option children)))
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
          (grove-design--copy-universal-attributes
           node
           (jetpacs-component-tabs
            "grove-agenda-range"
            (mapcar (lambda (option)
                      (jetpacs-component-tab
                       (plist-get (cdr option) :label) (car option)))
                    options)
            selected (jetpacs-action "grove.agenda.select")
            :variant "fixed")))))))

(defun grove-design--project-value (value kind profile)
  "Copy and recursively project typed nodes in VALUE for KIND under PROFILE."
  (cond
   ((vectorp value)
    (vconcat (mapcar (lambda (child)
                       (grove-design--project-value child kind profile))
                     value)))
   ((and (listp value) (keywordp (car value))
         (stringp (plist-get value :t)))
    (grove-design--project-node value kind profile))
   ((consp value)
    (cons (grove-design--project-value (car value) kind profile)
          (grove-design--project-value (cdr value) kind profile)))
   (t value)))

(defun grove-design--project-node (node kind profile)
  "Copy and project one typed NODE for Grove screen KIND under PROFILE.
PROFILE is the active design profile, kept for projections that must know
which styles it declares."
  (ignore profile)
  (or (and (eq kind 'agenda) (grove-design--agenda-tabs node))
      ;; Labeled buttons stay canonical: inside the scope the receiver's
      ;; design button override renders them through the profile's
      ;; `button.*' slots, so no Grove-private projection is needed.
      (cl-loop for (key value) on node by #'cddr
               append (list key (grove-design--project-value value kind profile)))))

(defun grove-design--reading-body-scope (screen profile)
  "Rebind body text under SCREEN's scaffold body to PROFILE's reading prose.
Read mode is the one place Grove sets Serif: a nested scope rebinds only the
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

(defun grove-design-present-screen (screen kind)
  "Return a profile-scoped copy of Grove SCREEN for closed screen KIND."
  (unless (and (jetpacs-root-node-p screen)
               (memq kind grove-design-screen-kinds))
    (error "Grove design expected a screen node and known presentation kind"))
  (let* ((profile (jetpacs-design-active-profile grove-design-profile-id))
         (result (progn
                   (unless profile
                     (error "Grove design profile is not registered"))
                   (grove-design--project-node screen kind profile))))
    (when (eq kind 'document)
      (setq result (grove-design--reading-body-scope result profile)))
    (jetpacs-design-profile-scope profile (list result))))

(defun grove-design-present-if-advertised (screen kind)
  "Return designed SCREEN for KIND when advertised, otherwise SCREEN itself."
  (if (jetpacs-extension-advertised-p "jetpacs.design" :app)
      (grove-design-present-screen screen kind)
    screen))

(provide 'grove-design)
;;; grove-design.el ends here
