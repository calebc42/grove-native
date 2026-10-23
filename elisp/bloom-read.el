;;; bloom-read.el --- Bloom's read mode, as the Android app draws it -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; The parity read screen: the note's own tags, keyword, title, planning
;; chips, drawers, and body, then its subtree as foldable heading rows,
;; all in the app's Plex Serif reading typography.  The structure comes
;; from `bloom-library-parse-subtree' at snapshot time; this module only
;; reads it, plus the screen-local fold and drawer state below.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'jetpacs-widgets)
(require 'jetpacs-components)
(require 'jetpacs-design)
(require 'jetpacs-design-profiles)
(require 'bloom-core)
(require 'bloom-design)
(require 'bloom-library)

(defvar bloom-read--folded (make-hash-table :test #'equal)
  "Note key to the heading indexes folded in its read view.")

(defvar bloom-read--open (make-hash-table :test #'equal)
  "Note key to the drawer and block keys toggled from their default.")

(defconst bloom-read-large-subtree 60
  "A note with more headings than this opens with its inner headings folded.")

(defun bloom-read--key (info)
  "Return the fold-state key of document INFO."
  (concat (plist-get info :notebook-id) "|" (plist-get info :heading-id)))

(defun bloom-read--default-folds (headings)
  "Return the heading indexes a fresh read of HEADINGS starts folded.
Like the app, only a large subtree folds, and then only headings with
descendants, so the note body and one level of sections show."
  (when (> (length headings) bloom-read-large-subtree)
    (cl-loop for (heading next) on (cdr headings)
             when (and next (> (plist-get next :level) (plist-get heading :level)))
             collect (plist-get heading :index))))

(defun bloom-read--folds (info)
  "Return the folded heading indexes of document INFO."
  (let ((key (bloom-read--key info)))
    (or (gethash key bloom-read--folded)
        (puthash key (cons 'seeded (bloom-read--default-folds (plist-get info :read)))
                 bloom-read--folded))))

(defun bloom-read--folded-p (info index)
  "Return non-nil when heading INDEX of document INFO is folded."
  (memq index (cdr (bloom-read--folds info))))

(defun bloom-read--toggled-p (info key)
  "Return non-nil when drawer or block KEY of INFO is toggled from its default."
  (member key (gethash (bloom-read--key info) bloom-read--open)))

;;;; Inline markup

(defconst bloom-read--inline-re
  (concat
   "\\(\\[\\[\\([^]]+\\)\\]\\(?:\\[\\([^]]+\\)\\]\\)?\\]\\)"      ; 1 link: 2 target, 3 desc
   "\\|\\([<[][0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^]>\n]*[]>]\\)" ; 4 timestamp
   "\\|\\(?:^\\|[ \t(\"'{]\\)\\(\\([*/_+=~]\\)\\([^ \t\n*/_+=~]\\|[^ \t\n][^\n]*?[^ \t\n]\\)\\6\\)"
   "\\(?:$\\|[- \t.,:!?;'\")}\\[]\\)")
  "Org inline markup: links, timestamps, and emphasis (marker in group 6).")

(defun bloom-read--span (text kind &optional target)
  "Return a rich-text span of TEXT styled as inline KIND, linking TARGET."
  (pcase kind
    ('bold (jetpacs-span text :font-weight 600))
    ('italic (jetpacs-span text :italic t))
    ('underline (jetpacs-span text :underline t))
    ('code (jetpacs-span text :mono t :bg (bloom-design-color "surface-raised")))
    ('timestamp (jetpacs-span text :mono t :color (bloom-design-color "syn-timestamp")))
    ('link (jetpacs-span text :underline t :color (bloom-design-color "syn-link")
                         :on-tap (jetpacs-action "bloom.read.link"
                                                 :args (list :target target))))
    (_ (jetpacs-span text))))

(defun bloom-read-spans (text)
  "Return TEXT's Org inline markup as rich-text spans, as the app styles it.
Bold is semibold, code and verbatim are mono on the raised surface,
timestamps are mono in the timestamp color, and links are underlined
in the link color and open through `bloom.read.link'.  Strike-through has
no span property and renders plain."
  (let ((start 0) spans)
    (while (string-match bloom-read--inline-re text start)
      (let* ((whole-beg (or (match-beginning 1) (match-beginning 4)
                            (match-beginning 5)))
             (whole-end (or (match-end 1) (match-end 4) (match-end 5))))
        (when (> whole-beg start)
          (push (bloom-read--span (substring text start whole-beg) 'plain) spans))
        (cond
         ((match-beginning 1)
          (let ((target (match-string 2 text)) (desc (match-string 3 text)))
            (push (bloom-read--span (or desc target) 'link target) spans)))
         ((match-beginning 4)
          (push (bloom-read--span (match-string 4 text) 'timestamp) spans))
         (t
          (let ((inner (match-string 7 text)))
            (push (bloom-read--span
                   inner (pcase (match-string 6 text)
                           ("*" 'bold) ("/" 'italic) ("_" 'underline)
                           ((or "=" "~") 'code) (_ 'plain)))
                  spans))))
        (setq start whole-end)))
    (when (< start (length text))
      (push (bloom-read--span (substring text start) 'plain) spans))
    (or (nreverse spans) (list (jetpacs-span text)))))

(defun bloom-read--rich (role text &optional ink)
  "Return TEXT with inline markup in text ROLE, optionally in INK."
  (jetpacs-design-styled
   (delq nil (list (concat "bloom.t." role) (and ink (concat "bloom.fg." ink))))
   (list (jetpacs-rich-text (bloom-read-spans text)))))

(defun bloom-read--text (role text &optional ink)
  "Return plain TEXT in text ROLE, optionally in INK."
  (jetpacs-design-styled
   (delq nil (list (concat "bloom.t." role) (and ink (concat "bloom.fg." ink))))
   (list (jetpacs-text text))))

;;;; Pieces

(defun bloom-read--pill (text ink soft &optional outline)
  "Return the app's 20 dp pill: TEXT in INK on SOFT, or OUTLINE in INK."
  (jetpacs-design-styled
   (list (if outline (concat "bloom.pill-outline." ink) (concat "bloom.pill." soft)))
   (list (bloom-read--text "pill" text ink))))

(defun bloom-read--planning-chip (icon ink soft label &optional icon-size)
  "Return a read-mode planning chip: ICON and LABEL in INK on SOFT."
  (jetpacs-design-styled
   (list (concat "bloom.read-chip." soft))
   (list (jetpacs-row
          (jetpacs-icon icon :size (or icon-size 13)
                        :color (bloom-design-color ink))
          (bloom-read--text "read-chip" label ink)
          :spacing 4 :align "center"))))

(defun bloom-read--planning (heading)
  "Return HEADING's planning chips as a wrapping row, or nil."
  (let ((chips
         (delq nil
               (list
                (when-let* ((label (plist-get heading :scheduled)))
                  (bloom-read--planning-chip "calendar_month" "info" "info-soft" label))
                (when-let* ((label (plist-get heading :deadline)))
                  (bloom-read--planning-chip "flag_filled" "error" "error-soft" label))
                (when-let* ((label (plist-get heading :closed)))
                  (bloom-read--planning-chip "check_filled" "faint" "surface-raised"
                                             label))))))
    (when chips
      (apply #'jetpacs-flow-row (append chips (list :spacing 6 :run-spacing 6))))))

(defun bloom-read--band (key label count body-nodes open)
  "Return a faded collapsible band KEY with LABEL and COUNT lines.
BODY-NODES show when OPEN, as the app's collapsible drawers and blocks do."
  (jetpacs-with-attrs
   (jetpacs-design-styled
    '("bloom.preface")
    (list
     (apply
      #'jetpacs-column
      (append
       (list
        (jetpacs-with-attrs
         (jetpacs-box
          (jetpacs-row
           (jetpacs-with-attrs
            (bloom-read--text "kv-caret" (if open "\u25be" "\u25b8")) :width 10)
           (jetpacs-spacer :width 8)
           (jetpacs-with-attrs (bloom-read--text "kv-label" label) :weight 1)
           (bloom-read--text "kv-count" (number-to-string count))
           :align "center" :fill t)
          :on-tap (jetpacs-action "bloom.read.drawer" :args (list :key key)))
         :pad '(:horizontal 12 :vertical 8) :fill_fraction 1))
       (when open
         (list (jetpacs-with-attrs
                (apply #'jetpacs-column (append body-nodes (list :spacing 3)))
                :pad '(:start 30 :end 12 :bottom 10))))))))
   :alpha 0.66 :fill_fraction 1))

(defun bloom-read--drawer (info key label count body-nodes)
  "Return INFO's drawer KEY with LABEL, COUNT and BODY-NODES; it starts folded."
  (bloom-read--band key label count body-nodes (bloom-read--toggled-p info key)))

(defun bloom-read--drawers (info heading key)
  "Return HEADING's PROPERTIES and LOGBOOK drawers in INFO under KEY, or nil."
  (let ((props (plist-get heading :properties))
        (log (plist-get heading :logbook)))
    (delq nil
          (list
           (and props
                (bloom-read--drawer
                 info key ":PROPERTIES:" (length props)
                 (mapcar (lambda (pair)
                           (jetpacs-row
                            (bloom-read--text "kv" (concat (car pair) " ") "syn-keyword")
                            (bloom-read--text "kv" (cdr pair) "muted")))
                         props)))
           (and props log (jetpacs-spacer :height 6))
           (and log
                (bloom-read--drawer
                 info (concat key "-logbook") ":LOGBOOK:" (length log)
                 (mapcar (lambda (line) (bloom-read--text "kv" line "muted")) log)))))))

(defun bloom-read--rounded-rect (x y w h r)
  "Return canvas points tracing an X Y W H rectangle with R dp corners.
The canvas draws polylines only, so each quarter arc is sampled."
  (let (points)
    (dolist (corner `((,(+ x w (- r)) ,(+ y r) -90) (,(+ x w (- r)) ,(+ y h (- r)) 0)
                      (,(+ x r) ,(+ y h (- r)) 90) (,(+ x r) ,(+ y r) 180)))
      (pcase-let ((`(,cx ,cy ,start) corner))
        (dotimes (step 7)
          (let ((angle (degrees-to-radians (+ start (* 15 step)))))
            (push (jetpacs-canvas-point
                   (/ (round (* 100 (+ cx (* r (cos angle))))) 100.0)
                   (/ (round (* 100 (+ cy (* r (sin angle))))) 100.0))
                  points)))))
    (nreverse points)))

(defun bloom-read--checkbox (state ink)
  "Return the app's 18 dp checkbox canvas for STATE in INK.
A 16 dp box inset 1 dp with 3 dp corners: filled with a background check
when done, a dash when in progress, otherwise an outline."
  (let* ((color (bloom-design-color ink))
         (box (bloom-read--rounded-rect 1.75 1.75 14.5 14.5 3)))
    (jetpacs-canvas
     18 18
     (append
      (list (if (member state '("X" "x"))
                (jetpacs-canvas-path box :fill color :closed t)
              (jetpacs-canvas-path box :color color :stroke-width 1.5 :closed t)))
      (pcase state
        ((or "X" "x")
         (let ((bg (bloom-design-color "background")))
           (list (jetpacs-canvas-path
                  (list (jetpacs-canvas-point 4.5 9.4) (jetpacs-canvas-point 7.6 12.4)
                        (jetpacs-canvas-point 13.2 5.6))
                  :color bg :stroke-width 1.5))))
        ("-" (list (jetpacs-canvas-line 5 9 13 9 :color color :width 1.5))))))))

(defun bloom-read--list (block)
  "Return list BLOCK as the app draws it: markers in a 20 dp column."
  (jetpacs-with-attrs
   (apply
    #'jetpacs-column
    (mapcar
     (lambda (item)
       (let* ((box (plist-get item :checkbox))
              (done (member box '("X" "x")))
              (ink (if done "faint" "muted")))
         (jetpacs-with-attrs
          (jetpacs-row
           (jetpacs-with-attrs
            (jetpacs-box
             (cond (box (bloom-read--checkbox box ink))
                   ((plist-get item :ordered)
                    (bloom-read--text "read-ordinal"
                                      (format "%d." (plist-get item :ordinal)) ink))
                   (t (jetpacs-canvas 6 6 (list (jetpacs-canvas-circle
                                                 3 3 3 :fill (bloom-design-color ink))))))
             :alignment "center")
            :width 20 :height 25)
           (jetpacs-spacer :width 8)
           (jetpacs-with-attrs
            (bloom-read--rich (if done "read-item-done" "read-item") (plist-get item :text))
            :weight 1)
           :align "top" :fill t)
          :pad (list :vertical 2 :start (* 20 (plist-get item :indent))))))
     (plist-get block :items)))
   :pad '(:start 8)))

(defun bloom-read--blocks (info key blocks)
  "Return body BLOCKS of document INFO, drawer keys prefixed by KEY."
  (let ((index 0))
    (cl-mapcan
     (lambda (block)
       (setq index (1+ index))
       (list
        (pcase (plist-get block :type)
          ('paragraph
           (bloom-read--rich "read-body"
                             (mapconcat #'string-trim (plist-get block :lines) "\n")))
          ('list (bloom-read--list block))
          ('table
           (bloom-read--text "kv" (string-join (plist-get block :lines) "\n") "on-background"))
          ('block
           (let* ((bkey (format "%s-block-%d" key index))
                  (lines (append (plist-get block :affiliated)
                                 (plist-get block :lines)))
                  (toggled (bloom-read--toggled-p info bkey)))
             ;; Blocks are note content and open expanded; a keyword run is
             ;; metadata and opens folded.
             (bloom-read--band
              bkey
              (concat (plist-get block :kind)
                      (when-let* ((lang (plist-get block :language)))
                        (concat "  " lang)))
              (length lines)
              (mapcar (lambda (line) (bloom-read--text "kv" line "muted")) lines)
              (if (plist-get block :keyword-run) toggled (not toggled))))))
        (jetpacs-spacer :height 12)))
     blocks)))

(defun bloom-read--keyword-pill (heading)
  "Return HEADING's keyword pill, or nil."
  (when-let* ((todo (plist-get heading :todo)))
    (if (plist-get heading :done)
        (bloom-read--pill todo "success" "success-soft")
      (bloom-read--pill todo "warning" "warning-soft"))))

(defun bloom-read--priority (heading)
  "Return HEADING's `[#P]' cookie in its urgency ink, or nil."
  (when-let* ((priority (plist-get heading :priority)))
    (bloom-read--text "read-priority" (format "[#%s]" priority)
                      (pcase (upcase priority)
                        ("A" "error") ("B" "warning") ("C" "success") (_ "muted")))))

(defun bloom-read--own (info heading)
  "Return the note's own header and body for document INFO's HEADING."
  (let ((tags (plist-get heading :tags))
        (pill (bloom-read--keyword-pill heading))
        (priority (bloom-read--priority heading))
        (planning (bloom-read--planning heading))
        (drawers (bloom-read--drawers info heading "own")))
    (apply
     #'jetpacs-column
     (append
      (when tags
        (list (apply #'jetpacs-row
                     (append (mapcar (lambda (tag)
                                       (bloom-read--pill tag "primary" "accent-soft" t))
                                     tags)
                             (list :spacing 7)))
              (jetpacs-spacer :height 12)))
      (when (or pill priority)
        (list (apply #'jetpacs-row
                     (append (delq nil (list pill priority)) (list :spacing 8 :align "center")))))
      (list (bloom-read--rich "read-title" (plist-get heading :title)))
      (when planning (list (jetpacs-spacer :height 6) planning))
      (when drawers
        (append (list (jetpacs-spacer :height 10)) drawers
                (list (jetpacs-spacer :height 20))))
      (list (jetpacs-spacer :height 16))
      (bloom-read--blocks info "own" (plist-get heading :blocks))))))

(defun bloom-read--heading-row (info heading base-level foldable count)
  "Return subtree HEADING of document INFO, BASE-LEVEL being the note's own.
FOLDABLE headings get a caret, and folded ones show their COUNT children."
  (let* ((index (plist-get heading :index))
         (rel (max 1 (- (plist-get heading :level) base-level)))
         (folded (and foldable (bloom-read--folded-p info index)))
         (key (format "child:%d" index))
         (drawers (bloom-read--drawers info heading key)))
    (apply
     #'jetpacs-column
     (append
      (list
       (jetpacs-spacer :height 20)
       (apply
        #'jetpacs-row
        (append
         (delq
          nil
          (list
           (and foldable
                (jetpacs-with-attrs
                 (jetpacs-box (bloom-read--caret folded) :alignment "center"
                              :on-tap (jetpacs-action "bloom.read.fold"
                                                      :args (list :index index)))
                 :width 22 :height 22 :corner 6 :clip t :pad '(:end 6)))
           (and (plist-get heading :todo)
                (jetpacs-with-attrs (bloom-read--keyword-pill heading) :pad '(:end 8)))
           (and (plist-get heading :priority)
                (jetpacs-with-attrs (bloom-read--priority heading) :pad '(:top 2 :end 8)))
           (jetpacs-with-attrs
            (bloom-read--rich (format "read-h%d" (min rel 3)) (plist-get heading :title))
            :weight 1)
           (and folded
                (jetpacs-with-attrs
                 (bloom-read--text "read-count" (format "… %d" count) "faint")
                 :pad '(:top 3 :start 6)))))
         (list :align "top" :fill t))))
      (if drawers
          (append (list (jetpacs-spacer :height 10)) drawers
                  (list (jetpacs-spacer :height 14)))
        (list (jetpacs-spacer :height 8)))
      (unless folded
        (bloom-read--blocks info key (plist-get heading :blocks)))))))

(defun bloom-read--caret (folded)
  "Return the 9 dp fold triangle, pointing right when FOLDED."
  (let ((ink (bloom-design-color "faint")))
    (jetpacs-canvas
     9 9 (list (jetpacs-canvas-path
                (if folded
                    (list (jetpacs-canvas-point 0 0) (jetpacs-canvas-point 0 9)
                          (jetpacs-canvas-point 9 4.5))
                  (list (jetpacs-canvas-point 0 0) (jetpacs-canvas-point 9 0)
                        (jetpacs-canvas-point 4.5 9)))
                :fill ink :closed t)))))

(defun bloom-read--subtree-rows (info headings)
  "Return the visible subtree rows of document INFO's HEADINGS.
Headings under a folded ancestor are omitted, as the app never composes them."
  (let* ((base (plist-get (car headings) :level))
         (rest (cdr headings))
         hidden-below rows)
    (cl-loop for (heading . more) on rest
             do (let ((level (plist-get heading :level)))
                  (when (and hidden-below (<= level hidden-below))
                    (setq hidden-below nil))
                  (unless hidden-below
                    (let* ((children (cl-loop for next in more
                                              while (> (plist-get next :level) level)
                                              count (= (plist-get next :level)
                                                       (1+ level))))
                           (foldable (and more (> (plist-get (car more) :level) level)))
                           (folded (and foldable
                                        (bloom-read--folded-p info (plist-get heading :index)))))
                      (push (bloom-read--heading-row info heading base foldable children)
                            rows)
                      (when folded (setq hidden-below level))))))
    (nreverse rows)))

;;;; Screen

(defun bloom-read--toggle (info)
  "Return the app's Read/Edit switch for document INFO, Read selected."
  (jetpacs-with-attrs
   (jetpacs-design-styled
    '("bloom.toggle")
    (list
     (jetpacs-box
      (jetpacs-row
       (jetpacs-design-styled
        '("bloom.toggle-on")
        (list (jetpacs-box (jetpacs-icon "visibility" :size 15
                                         :color (bloom-design-color "on-primary")
                                         :content-description "Read")
                           :alignment "center")))
       (jetpacs-design-styled
        '("bloom.toggle-off")
        (list (jetpacs-box (jetpacs-icon "edit" :size 15
                                         :color (bloom-design-color "muted")
                                         :content-description "Edit")
                           :alignment "center")))
       :align "center")
      :on-tap (jetpacs-action "bloom.document.mode"
                              :args (list :notebook-id (plist-get info :notebook-id)
                                          :heading-id (plist-get info :heading-id)
                                          :mode "edit")))))
   :pad '(:end 8)))

(defun bloom-read--top-bar (info back)
  "Return document INFO's read bar: BACK, the mode switch, and breadcrumb."
  (jetpacs-column
   (jetpacs-with-attrs
    (jetpacs-row
     (jetpacs-icon-button "arrow_back" back :content-description "Back")
     (jetpacs-spacer :weight 1)
     (bloom-read--toggle info)
     :align "center" :fill t)
    :height 56 :pad '(:start 6 :end 8))
   (jetpacs-with-attrs
    (bloom-read--text
     "breadcrumb"
     (string-join (append (list (or (plist-get info :file-label) ""))
                          (plist-get info :ancestors)
                          (list (plist-get info :headline)))
                  " › "))
    ;; Measured on device: the app's content starts 8.2 dp below the
    ;; breadcrumb's restored line box, 1.8 dp short of its nominal 10.
    :pad '(:start 16 :end 16 :bottom 8.2))))

(defun bloom-read-screen (info back)
  "Return the parity read screen for document INFO with BACK."
  (let* ((headings (plist-get info :read))
         (own (car headings)))
    (jetpacs-design-profile-scope
     (jetpacs-design-active-profile bloom-design-profile-id)
     (list
      (jetpacs-with-semantics
       (jetpacs-scaffold
        :top-bar (bloom-read--top-bar info back)
        :body
        (jetpacs-with-attrs
         (apply #'jetpacs-lazy-column
                (append (list (jetpacs-spacer :height 8)
                              (bloom-read--own info own))
                        (bloom-read--subtree-rows info headings)
                        (list (jetpacs-spacer :height 40)
                              (jetpacs-spacer :height 48))))
         :pad '(:start 24 :end 24))
        :fab (jetpacs-design-styled
              '("bloom.read-fab")
              (list (jetpacs-box (bloom-read--text "read-fab" "☰")
                                 :alignment "center"
                                 :on-tap (jetpacs-action
                                          "bloom.document.mode"
                                          :args (list :notebook-id (plist-get info :notebook-id)
                                                      :heading-id (plist-get info :heading-id)
                                                      :mode "edit"))))))
       :pane-title (or (plist-get info :headline) "Note"))))))

;;;; Actions

(defun bloom-read--toggle-key (table key value)
  "Toggle VALUE in TABLE's list under KEY."
  (let ((items (gethash key table)))
    (puthash key (if (member value items) (remove value items) (cons value items))
             table)))

(defun bloom-read-on-fold (info args params)
  "Fold or unfold ARGS' heading index of document INFO; refresh PARAMS."
  (let ((index (plist-get args :index)))
    (if (not (integerp index)) 'rejected
      (let* ((key (bloom-read--key info))
             (folds (bloom-read--folds info)))
        (puthash key (cons 'seeded (if (memq index (cdr folds))
                                       (remq index (cdr folds))
                                     (cons index (cdr folds))))
                 bloom-read--folded))
      (jetpacs-app-defer-refresh params)
      'accepted)))

(defun bloom-read-on-drawer (info args params)
  "Open or close ARGS' drawer or block of document INFO; refresh PARAMS."
  (let ((key (plist-get args :key)))
    (if (not (stringp key)) 'rejected
      (bloom-read--toggle-key bloom-read--open (bloom-read--key info) key)
      (jetpacs-app-defer-refresh params)
      'accepted)))

(provide 'bloom-read)
;;; bloom-read.el ends here
