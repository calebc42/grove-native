;;; grove-design.el --- Grove Light presentation for Jetpacs -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; A bounded `jetpacs.design' projection of Grove Light.  Existing Grove
;; builders remain the source of screen composition, data, identities, and
;; actions; this module decorates their resulting node trees only when the
;; active app profile advertises the experimental renderer extension.

;;; Code:

(require 'cl-lib)
(require 'jetpacs-design-material)

(defconst grove-design-light-tokens
  `(("color.primary" . ,(jetpacs-design-color "#8A5A2B"))
    ("color.on-primary" . ,(jetpacs-design-color "#FFFAF2"))
    ("color.surface" . ,(jetpacs-design-color "#FBF8F1"))
    ("color.on-surface" . ,(jetpacs-design-color "#2A251F"))
    ("color.outline" . ,(jetpacs-design-color "#D3C9B6"))
    ("color.background" . ,(jetpacs-design-color "#F3EDE1"))
    ("color.surface-variant" . ,(jetpacs-design-color "#ECE4D5"))
    ("color.secondary-text" . ,(jetpacs-design-color "#6C6356"))
    ("space.small" . ,(jetpacs-design-dimension 8))
    ("space.medium" . ,(jetpacs-design-dimension 12))
    ("radius.medium" . ,(jetpacs-design-dimension 12)))
  "Grove Light values projected into the proof library's token vocabulary.")

(defconst grove-design-light-styles
  `(("grove.screen.body"
     . ,(jetpacs-design-style
         `(("background_color"
            . ,(jetpacs-design-token "color.background"))
           ("content_color"
            . ,(jetpacs-design-token "color.on-surface"))
           ("font_family" . ,(jetpacs-design-font-family "plex-sans"))
           ("fill_width" . ,(jetpacs-design-boolean t))))))
  "Grove-specific styles layered beside the proof component styles.")

(defun grove-design--copy-universal-attributes (source target)
  "Copy SOURCE's authored universal attributes onto TARGET."
  (let ((result target))
    (dolist (attribute jetpacs-universal-attributes result)
      (when (plist-member source attribute)
        (setq result
              (plist-put result attribute (plist-get source attribute)))))))

(defun grove-design--filled-button (node)
  "Project simple Material button NODE through the proof filled button."
  (grove-design--copy-universal-attributes
   node
   (jetpacs-design-material-filled-button
    (plist-get node :label)
    (plist-get node :on_tap)
    :enabled (plist-get node :enabled))))

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

(defun grove-design--agenda-selector (node)
  "Return a proof selector for Agenda range row NODE, or nil."
  (when (equal (plist-get node :t) "row")
    (let* ((children (append (plist-get node :children) nil))
           (options (mapcar #'grove-design--agenda-option children)))
      (when (and (= (length children) 2)
                 (cl-every #'identity options)
                 (equal (sort (mapcar #'car options) #'string<)
                        '("today" "upcoming")))
        (let* ((first (car options))
               (second (cadr options))
               (selected
                (or (car (cl-find-if
                          (lambda (option)
                            (equal (plist-get (cdr option) :variant) "filled"))
                          options))
                    (caar options)))
               (actions (mapcar
                         (lambda (option)
                           (cons (car option)
                                 (plist-get (cdr option) :on_tap)))
                         options))
               (selector
                (jetpacs-design-material-two-option-selector
                 (plist-get (cdr first) :label) (car first)
                 (plist-get (cdr second) :label) (car second)
                 selected
                 (lambda (value) (cdr (assoc value actions)))))
               (source-buttons (mapcar #'cdr options))
               (target-buttons (append (plist-get selector :children) nil)))
          (setq target-buttons
                (cl-mapcar #'grove-design--copy-universal-attributes
                           source-buttons target-buttons))
          (setq selector (plist-put selector :children
                                    (vconcat target-buttons)))
          (dolist (member '(:align :spacing))
            (when (plist-member node member)
              (setq selector
                    (plist-put selector member (plist-get node member)))))
          (grove-design--copy-universal-attributes node selector))))))

(defun grove-design--decorate-node (node agenda)
  "Decorate NODE with proof components; AGENDA enables its selector."
  (or (and agenda (grove-design--agenda-selector node))
      (and (equal (plist-get node :t) "button")
           (plist-get node :label)
           (plist-get node :on_tap)
           (grove-design--filled-button node))
      (let ((result (copy-sequence node)))
        (when (plist-member result :children)
          (setq result
                (plist-put
                 result :children
                 (vconcat
                  (mapcar
                   (lambda (child)
                     (grove-design--decorate-node child agenda))
                   (append (plist-get result :children) nil))))))
        result)))

(defun grove-design-present-screen (screen kind)
  "Return Grove Light presentation of already-built SCREEN for KIND.
KIND is `notebooks' or `agenda'.  The original node is not mutated."
  (unless (and (jetpacs-root-node-p screen)
               (memq kind '(notebooks agenda)))
    (error "Grove design expected a screen node and known presentation kind"))
  (let* ((result (copy-sequence screen))
         (body (plist-get result :body))
         (fab (plist-get result :fab)))
    (when body
      (setq result
            (plist-put
             result :body
             (jetpacs-design-styled
              '("grove.screen.body")
              (list (grove-design--decorate-node
                     body (eq kind 'agenda)))))))
    (when fab
      (setq result
            (plist-put result :fab
                       (grove-design--decorate-node fab nil))))
    (jetpacs-design-scope
     grove-design-light-tokens
     (append jetpacs-design-material-styles grove-design-light-styles)
     (list result)
     :motions jetpacs-design-material-motions)))

(provide 'grove-design)
;;; grove-design.el ends here
