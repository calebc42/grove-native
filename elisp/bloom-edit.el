;;; bloom-edit.el --- The Android app's raw-Org edit mode -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Edit mode as Bloom draws it: the read bar with its Edit segment lit,
;; then the note's raw Org subtree in a full-width, chromeless Plex Mono
;; field, highlighted line by line the way the app's `OrgSyntaxHighlight'
;; colors it, with the menu button floating at the end.
;;
;; The field is a SPEC 19 synchronized editor over a private buffer that
;; holds only the subtree.  The buffer is the whole synchronized document,
;; so the subtree's offsets are honest, and `ebp-sync-fontify-function'
;; ships this module's highlighting as `fontify.show' runs.  EBP names only
;; fifteen syntax roles, so each of the app's styles travels as the role
;; `bloom-design-syntax-roles' assigns it, and Bloom's profile styles each
;; role through the `editor.syntax.*' slots.
;;
;; Like the app, edit mode saves by itself five seconds after the last
;; change, shows a save mark while the note is dirty or after a save, and
;; saves at once when the user switches back to Read.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ebp-sync)
(require 'jetpacs-widgets)
(require 'jetpacs-design)
(require 'bloom-core)
(require 'bloom-design)
(require 'bloom-read)

(declare-function bloom-document-write-subtree "bloom-document"
                  (notebook-id heading-id revision value))
(declare-function bloom-document-revision "bloom-document"
                  (notebook-id heading-id))
(declare-function jetpacs-client "jetpacs-connect")
(declare-function jetpacs-granted-p "jetpacs-surfaces")
(declare-function jetpacs-node-advertised-p "jetpacs-surfaces")
(declare-function jetpacs-chrome-defer-current-view-refresh "jetpacs-chrome")
(declare-function jetpacs-shell-notify "jetpacs-shell")

(defcustom bloom-edit-autosave-delay 5
  "Seconds after the last edit before Bloom saves the note, as the app does."
  :type 'number :group 'bloom)

;;;; Highlighting

(defconst bloom-edit--headline-re "\\`\\(\\*+\\)[ \t\n\r\f\v]+\\(.*\\)\\'"
  "The app's headline pattern: stars, whitespace, and the rest.")

(defconst bloom-edit--tags-re
  "[ \t\n\r\f\v]+\\(:[A-Za-z0-9_@#%-]+\\(?::[A-Za-z0-9_@#%-]+\\)*:\\)[ \t\n\r\f\v]*\\'"
  "Tags at the end of a headline, with the whitespace before them.")

(defconst bloom-edit--planning-timestamp-re "[<[][^]>]*[]>]"
  "A timestamp on a planning line, as loosely as the app finds one.")

(defconst bloom-edit--planning-keyword-re "\\(?:SCHEDULED\\|DEADLINE\\|CLOSED\\):"
  "A planning keyword with its colon.")

(defconst bloom-edit--property-re
  "\\`[ \t\n\r\f\v]*:\\([^: \t\n\r\f\v]+\\):[ \t\n\r\f\v]*\\(.*\\)\\'"
  "A property line: key between colons, then the value.")

(defconst bloom-edit--link-re "\\[\\[\\([^][]+\\)\\]\\(?:\\[\\([^][]+\\)\\]\\)?\\]"
  "An Org bracket link with an optional description.")

(defconst bloom-edit--timestamp-re
  "[<[][0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^]>\n]*[]>]"
  "An inline Org timestamp.")

(defconst bloom-edit--url-re
  "\\(?:https?://\\|mailto:\\|tel:\\|sms:\\|geo:\\|file:\\)[^][ \t\n\r\f\v<>]+"
  "A bare URL in a scheme Org recognizes unbracketed.")

(defconst bloom-edit--phone-re
  (concat "\\(?:\\+?1[ \t\n\r\f\v.-]\\)?"
          "\\(?:([0-9]\\{3\\})[ \t\n\r\f\v.-]?\\|[0-9]\\{3\\}[ \t\n\r\f\v.-]\\)"
          "[0-9]\\{3\\}[ \t\n\r\f\v.-][0-9]\\{4\\}")
  "A North American phone number written with separators.")

(defconst bloom-edit--emphasis-before " \t\n\r\f\v('\"{>"
  "Characters that may precede an opening emphasis marker.")

(defconst bloom-edit--emphasis-after " \t\n\r\f\v.,:!?;)'\"}]<"
  "Characters that may follow a closing emphasis marker.")

(defconst bloom-edit--emphasis-kinds
  '((?* . bold) (?/ . italic) (?_ . underline) (?+ . strike)
    (?~ . code) (?` . code) (?= . code))
  "Emphasis markers in the app's claiming order, with their style.")

(defun bloom-edit--word-char-p (char)
  "Non-nil when CHAR is a Java regex word character."
  (and char (or (<= ?a char ?z) (<= ?A char ?Z) (<= ?0 char ?9) (eq char ?_))))

(defun bloom-edit--space-p (char)
  "Non-nil when CHAR is Java regex whitespace."
  (and char (memq char '(?\s ?\t ?\n ?\r ?\f ?\v))))

(defun bloom-edit--matches (regexp line &optional accept)
  "Return LINE's successive non-overlapping REGEXP matches as (START . END).
ACCEPT, when given, vets each match the way a lookaround would; a refused
match is retried one character later."
  (let ((case-fold-search nil) (pos 0) found)
    (while (and (< pos (length line)) (string-match regexp line pos))
      (let ((beg (match-beginning 0)) (end (match-end 0)))
        (cond
         ((= beg end) (setq pos (1+ end)))
         ((or (null accept) (funcall accept beg end))
          (push (cons beg end) found)
          (setq pos end))
         (t (setq pos (1+ beg))))))
    (nreverse found)))

(defun bloom-edit--emphasis-matches (line marker)
  "Return LINE's emphasis spans delimited by MARKER as (START . END).
The app's pattern: the marker after a boundary, content that neither
starts nor ends with whitespace and holds no marker, then the marker
before a boundary."
  (let ((len (length line)) (i 0) found)
    (while (< i len)
      (let ((close (and (eq (aref line i) marker)
                        (or (zerop i)
                            (memq (aref line (1- i))
                                  (append bloom-edit--emphasis-before nil)))
                        (< (1+ i) len)
                        (not (eq (aref line (1+ i)) marker))
                        (not (bloom-edit--space-p (aref line (1+ i))))
                        (cl-position marker line :start (+ i 2)))))
        (if (and close
                 (not (bloom-edit--space-p (aref line (1- close))))
                 (not (cl-position ?\n line :start i :end close))
                 (or (= (1+ close) len)
                     (memq (aref line (1+ close))
                           (append bloom-edit--emphasis-after nil))))
            (progn (push (cons i (1+ close)) found)
                   (setq i (1+ close)))
          (setq i (1+ i)))))
    (nreverse found)))

(defun bloom-edit-inline-tokens (line)
  "Return LINE's inline Org tokens as (START END KIND), sorted.
A port of the app's `InlineTokenizer': links, timestamps, bare URLs and
phone numbers claim their characters first, then each emphasis marker in
turn, and a later match that touches a claimed character is dropped.
KIND is `link', `timestamp', `bold', `italic', `underline', `strike' or
`code'."
  (let ((claimed (make-bool-vector (length line) nil)) tokens)
    (cl-flet ((claim (spans kind)
                (dolist (span spans)
                  (unless (cl-loop for i from (car span) below (cdr span)
                                   thereis (aref claimed i))
                    (cl-loop for i from (car span) below (cdr span)
                             do (aset claimed i t))
                    (push (list (car span) (cdr span) kind) tokens)))))
      (claim (bloom-edit--matches bloom-edit--link-re line) 'link)
      (claim (bloom-edit--matches bloom-edit--timestamp-re line) 'timestamp)
      (claim (bloom-edit--matches bloom-edit--url-re line) 'link)
      (claim (bloom-edit--matches
              bloom-edit--phone-re line
              (lambda (beg end)
                (and (or (zerop beg)
                         (let ((before (aref line (1- beg))))
                           (not (or (bloom-edit--word-char-p before)
                                    (eq before ?.)))))
                     (or (= end (length line))
                         (not (bloom-edit--word-char-p (aref line end)))))))
             'link)
      (pcase-dolist (`(,marker . ,kind) bloom-edit--emphasis-kinds)
        (claim (bloom-edit--emphasis-matches line marker) kind)))
    (sort tokens (lambda (a b) (< (car a) (car b))))))

(defun bloom-edit--fill (styles start end key)
  "Set STYLES from START below END to KEY, later layers winning."
  (cl-loop for i from (max 0 start) below (min end (length styles))
           do (aset styles i key)))

(defun bloom-edit-line-styles (line keywords done)
  "Return a vector of LINE's per-character style keys, nil for plain.
KEYWORDS are the TODO keywords and DONE the finished ones.  A port of the
app's `OrgSyntaxHighlight.styleLine', layer for layer."
  (let ((styles (make-vector (length line) nil))
        (case-fold-search nil)
        (trimmed (string-trim line)))
    (cond
     ((string-match bloom-edit--headline-re line)
      (let* ((stars (length (match-string 1 line)))
             (rest (match-string 2 line))
             (first (car (split-string rest " ")))
             (start (1+ stars)))
        (bloom-edit--fill styles 0 stars (cons 'star stars))
        (bloom-edit--fill styles start (length line) 'headline)
        (when (member first keywords)
          (bloom-edit--fill styles start (+ start (length first))
                            (if (member first done) 'done 'todo)))
        (when (string-match bloom-edit--tags-re line)
          (bloom-edit--fill styles (match-beginning 0) (match-end 0) 'tag))))
     ((string-match-p "\\`[ \t]*\\(?:SCHEDULED\\|DEADLINE\\|CLOSED\\):" line)
      (bloom-edit--fill styles 0 (length line) 'planning)
      (dolist (span (bloom-edit--matches bloom-edit--planning-timestamp-re line))
        (bloom-edit--fill styles (car span) (cdr span) 'timestamp))
      (dolist (span (bloom-edit--matches bloom-edit--planning-keyword-re line))
        (bloom-edit--fill styles (car span) (cdr span) 'planning-keyword)))
     ((member (upcase trimmed) '(":PROPERTIES:" ":END:"))
      (bloom-edit--fill styles 0 (length line) 'property))
     ((string-match-p bloom-edit--property-re line)
      (let ((key-end (1+ (cl-position ?: line
                                      :start (1+ (cl-position ?: line))))))
        (bloom-edit--fill styles 0 key-end 'property-key)
        (bloom-edit--fill styles key-end (length line) 'property)))
     (t
      (pcase-dolist (`(,beg ,end ,kind) (bloom-edit-inline-tokens line))
        (bloom-edit--fill styles beg end kind))))
    styles))

(defun bloom-edit--role (key)
  "Return the (ROLE . LEVEL) a style KEY travels as, or nil for plain."
  (pcase key
    (`(star . ,level) (cons "heading" level))
    ('headline '("function"))
    ('todo '("todo"))
    ('done '("done"))
    ('tag '("tag"))
    ((or 'planning 'property-key) '("comment"))
    ('planning-keyword '("keyword"))
    ('timestamp '("number"))
    ('property '("preprocessor"))
    ('bold '("constant"))
    ('italic '("type"))
    ('underline '("variable"))
    ('strike '("operator"))
    ('code '("string"))
    ('link '("link"))))

(defvar-local bloom-edit--keywords nil
  "The note's TODO keywords, all states, from its Org buffer.")

(defvar-local bloom-edit--done-keywords nil
  "The note's finished TODO keywords, from its Org buffer.")

(defun bloom-edit-text-runs (text keywords done)
  "Return TEXT's SPEC 19.5 fontify runs under TODO KEYWORDS and DONE.
Offsets are 0-based characters into TEXT; adjacent characters with the
same role merge, and plain stretches ship nothing."
  (let ((offset 0) runs)
    (dolist (line (split-string text "\n"))
      (let ((styles (bloom-edit-line-styles line keywords done))
            (i 0))
        (while (< i (length styles))
          (let ((role (bloom-edit--role (aref styles i)))
                (j (1+ i)))
            (while (and (< j (length styles))
                        (equal (bloom-edit--role (aref styles j)) role))
              (setq j (1+ j)))
            (when role
              (push (append (list :start (+ offset i) :end (+ offset j)
                                  :role (car role))
                            (and (cdr role) (list :level (cdr role))))
                    runs))
            (setq i j))))
      (setq offset (+ offset (length line) 1)))
    (nreverse runs)))

(defun bloom-edit-fontify-runs ()
  "Return the current edit buffer's fontify runs.
This is the buffer's `ebp-sync-fontify-function'."
  (save-restriction
    (widen)
    (bloom-edit-text-runs (buffer-substring-no-properties (point-min) (point-max))
                          bloom-edit--keywords bloom-edit--done-keywords)))

;;;; The edit session

(defvar bloom-edit--buffer nil
  "The private buffer of the note open in edit mode, or nil.")

(defvar-local bloom-edit--notebook-id nil)
(defvar-local bloom-edit--heading-id nil
  "The note's heading id, followed across saves that rename it.")
(defvar-local bloom-edit--revision nil
  "SHA-256 of the subtree text last read from or written to the file.")
(defvar-local bloom-edit--document nil)
(defvar-local bloom-edit--editor-id nil)
(defvar-local bloom-edit--dirty nil
  "Non-nil while the buffer holds edits the file does not.")
(defvar-local bloom-edit--saved-at nil
  "When this session last saved, or nil before its first save.")
(defvar-local bloom-edit--timer nil)

(defun bloom-edit--session-p (buffer notebook-id heading-id)
  "Non-nil when BUFFER is the live edit session for NOTEBOOK-ID/HEADING-ID."
  (and (buffer-live-p buffer)
       (equal (buffer-local-value 'bloom-edit--notebook-id buffer) notebook-id)
       (equal (buffer-local-value 'bloom-edit--heading-id buffer) heading-id)))

(defun bloom-edit-sync-available-p ()
  "Non-nil when the Companion can host a synchronized editor."
  (when-let* ((client (jetpacs-client)))
    (and (jetpacs-granted-p "editor.sync" client)
         (jetpacs-node-advertised-p "editor" :app))))

(defun bloom-edit--keyword-lists (info)
  "Return (KEYWORDS . DONE) from document INFO's Org buffer."
  (let ((buffer (and (plist-get info :buffer) (get-buffer (plist-get info :buffer)))))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (cons (copy-sequence org-todo-keywords-1)
                (copy-sequence org-done-keywords)))
      (cons '("TODO" "DONE") '("DONE")))))

(defun bloom-edit--finish (buffer)
  "Save BUFFER's pending edits, detach it, and kill it."
  (when (buffer-live-p buffer)
    (bloom-edit-save buffer)
    (with-current-buffer buffer
      (when bloom-edit--timer (cancel-timer bloom-edit--timer))
      (ignore-errors (ebp-sync-detach buffer)))
    (kill-buffer buffer))
  (when (eq bloom-edit--buffer buffer) (setq bloom-edit--buffer nil)))

(defun bloom-edit-ensure (info)
  "Return the synchronized edit buffer for document INFO, attaching it once.
A screen rebuild finds the session attached and leaves it alone.  A
session the Companion closed is reattached, reseeded from the file unless
it still holds unsaved edits.  Another note's session is saved and ended."
  (let ((notebook-id (plist-get info :notebook-id))
        (heading-id (plist-get info :heading-id))
        (buffer bloom-edit--buffer))
    (unless (bloom-edit--session-p buffer notebook-id heading-id)
      (bloom-edit--finish buffer)
      (setq buffer (generate-new-buffer " *bloom-edit*"))
      (let ((key (format "%s\0%s\0%s" notebook-id heading-id (float-time)))
            (keywords (bloom-edit--keyword-lists info)))
        (with-current-buffer buffer
          (setq buffer-undo-list t
                bloom-edit--notebook-id notebook-id
                bloom-edit--heading-id heading-id
                bloom-edit--keywords (car keywords)
                bloom-edit--done-keywords (cdr keywords)
                bloom-edit--document
                (concat "doc:" (substring (sha1 key) 0 16) ".org")
                bloom-edit--editor-id (jetpacs-wire-id "bloom-edit" key))
          (setq-local buffer-auto-save-file-name nil)
          (setq-local ebp-sync-fontify-function #'bloom-edit-fontify-runs)
          (add-hook 'after-change-functions #'bloom-edit--changed nil t)))
      (setq bloom-edit--buffer buffer))
    (with-current-buffer buffer
      (unless (ebp-sync-attached-buffer bloom-edit--document bloom-edit--editor-id)
        (unless bloom-edit--dirty
          (let ((inhibit-modification-hooks t))
            (erase-buffer)
            (insert (or (plist-get info :content) "")))
          (setq bloom-edit--revision (plist-get info :revision)))
        (ebp-sync-attach (jetpacs-client) bloom-edit--document
                         bloom-edit--editor-id buffer)))
    buffer))

(defun bloom-edit--changed (&rest _)
  "Mark the session dirty and restart its autosave countdown."
  (let ((buffer (current-buffer)))
    (unless bloom-edit--dirty
      (setq bloom-edit--dirty t)
      (jetpacs-chrome-defer-current-view-refresh bloom-app-surface))
    (when bloom-edit--timer (cancel-timer bloom-edit--timer))
    (setq bloom-edit--timer
          (run-at-time bloom-edit-autosave-delay nil
                       (lambda ()
                         (when (bloom-edit-save buffer)
                           (jetpacs-chrome-defer-current-view-refresh
                            bloom-app-surface)))))))

(defun bloom-edit-save (&optional buffer)
  "Write BUFFER's subtree to its note now when it is dirty.
Return non-nil when a save happened.  A note changed on disk since the
session read it is left unsaved, as the app leaves it, with a notice."
  (let ((buffer (or buffer bloom-edit--buffer)))
    (when (and (buffer-live-p buffer)
               (buffer-local-value 'bloom-edit--dirty buffer))
      (with-current-buffer buffer
        (when bloom-edit--timer (cancel-timer bloom-edit--timer)
              (setq bloom-edit--timer nil))
        (let* ((text (save-restriction
                       (widen)
                       (buffer-substring-no-properties (point-min) (point-max))))
               (value (if (string-suffix-p "\n" text) text (concat text "\n")))
               (result (and (string-match-p "\\`\\*+ " value)
                            (condition-case err
                                (bloom-document-write-subtree
                                 bloom-edit--notebook-id bloom-edit--heading-id
                                 bloom-edit--revision value)
                              (error
                               (message "bloom: autosave failed (%s)" (car err))
                               nil)))))
          (pcase result
            ('stale
             (jetpacs-shell-notify "Note changed on disk; not saved"
                                   bloom-app-surface)
             nil)
            ((pred stringp)
             (setq bloom-edit--heading-id result
                   bloom-edit--revision (bloom-document-revision
                                         bloom-edit--notebook-id result)
                   bloom-edit--dirty nil
                   bloom-edit--saved-at (current-time))
             t)))))))

;;;; The screen

(defun bloom-edit--top-bar (info back buffer)
  "Return document INFO's edit bar: BACK, the save mark, and the switch.
BUFFER is the edit session, whose state colors the save mark."
  (let ((dirty (buffer-local-value 'bloom-edit--dirty buffer))
        (saved (buffer-local-value 'bloom-edit--saved-at buffer)))
    (jetpacs-with-attrs
     (apply #'jetpacs-row
            (append
             (list (jetpacs-icon-button "arrow_back" back :content-description "Back"))
             (when (or dirty saved)
               (list (jetpacs-icon-button
                      "save"
                      (jetpacs-action "bloom.edit.save")
                      :content-description
                      (if dirty "Unsaved changes, tap to save" "Saved")
                      :color (bloom-design-color (if dirty "success" "faint")))))
             (list (jetpacs-spacer :weight 1)
                   (bloom-read--toggle info t)
                   :align "center" :fill t)))
     :height 56 :pad '(:start 6 :end 8))))

(defun bloom-edit-screen (info back)
  "Return the parity edit screen for document INFO with BACK."
  (let* ((buffer (bloom-edit-ensure info))
         (text (with-current-buffer buffer
                 (save-restriction
                   (widen)
                   (buffer-substring-no-properties (point-min) (point-max))))))
    (jetpacs-design-profile-scope
     (jetpacs-design-active-profile bloom-design-profile-id)
     (list
      (jetpacs-with-semantics
       (jetpacs-scaffold
        :top-bar (bloom-edit--top-bar info back buffer)
        :body
        (jetpacs-design-scope
         nil nil
         (list
          (jetpacs-column
           (jetpacs-with-attrs
            (jetpacs-editor
             (jetpacs-claim-node-id
              (buffer-local-value 'bloom-edit--editor-id buffer))
             :document (buffer-local-value 'bloom-edit--document buffer)
             :value text
             :chromeless t
             :line-numbers :json-false)
            :weight 1)
           :fill t))
         :component-styles
         (list (jetpacs-design-component-style
                "editor.chromeless" '("bloom.edit-surface"))
               (jetpacs-design-component-style
                "editor.text" '("bloom.edit-text"))))
        :fab (bloom-read--fab
              (jetpacs-action "bloom.document.planning"
                              :args (list :notebook-id (plist-get info :notebook-id)
                                          :heading-id (plist-get info :heading-id)))
              "Open metadata"))
       :pane-title (or (plist-get info :headline) "Note"))))))

(defun bloom-edit-on-save (_args params)
  "Save the open note now for the save mark's event PARAMS."
  (if (bloom-edit-save)
      (progn (jetpacs-app-defer-refresh params) 'accepted)
    (let ((buffer bloom-edit--buffer))
      (jetpacs-shell-notify
       (if-let* ((saved (and (buffer-live-p buffer)
                             (buffer-local-value 'bloom-edit--saved-at buffer))))
           (format-time-string "The note was last saved at: %-I:%M %p" saved)
         "This note hasn't been saved yet")
       (plist-get params :surface))
      'accepted)))

(provide 'bloom-edit)
;;; bloom-edit.el ends here
