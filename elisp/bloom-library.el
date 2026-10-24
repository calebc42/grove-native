;;; bloom-library.el --- Bloom notebook and document model -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Bloom's private projection over the canonical Jetpacs Org scope.  Wire
;; records contain opaque ids and display strings only.  Paths, Org refs, and
;; live buffers remain in the maps below and can be reconstructed after an
;; Emacs restart by rescanning the bounded canonical scope.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'ebp-org)
(require 'jetpacs-files)
(require 'jetpacs-settings)
(require 'jetpacs-org-reminders)
(require 'bloom-core)

(defcustom bloom-library-notebook-limit 300
  "Maximum Org files retained in Bloom's notebook snapshot."
  :type '(integer 1 1000)
  :group 'bloom)

(defcustom bloom-library-tree-view t
  "Whether Notebooks initially shows folders instead of one flat file list."
  :type 'boolean
  :group 'bloom)

(defcustom bloom-library-favorite-ids nil
  "Opaque notebook ids pinned to the top of Bloom's library."
  :type '(repeat string)
  :group 'bloom)

(defcustom bloom-reader-show-drawers nil
  "Whether Bloom's Org reader initially expands drawers."
  :type 'boolean
  :group 'bloom)

(defcustom bloom-reader-default-mode 'read
  "Initial Bloom document presentation, `read' or `edit'."
  :type '(choice (const read) (const edit))
  :group 'bloom)

(defcustom bloom-search-result-limit 100
  "Maximum canonical Org search results Bloom retains."
  :type '(integer 1 512)
  :group 'bloom)

(defvar bloom-library-after-refresh-hook nil
  "Hook run after Bloom replaces a library, outline, or search snapshot.")

(defvar bloom-library--snapshot
  '(:notebooks nil :folders nil :total-count 0 :truncated nil :error nil)
  "Current presentation-safe notebook snapshot.")

(defvar bloom-library--search-snapshot
  '(:query "" :mode "text" :results nil :error nil)
  "Current presentation-safe search snapshot.")

(defvar bloom-library--notebook-paths (make-hash-table :test #'equal)
  "Opaque notebook id to canonical path.")

(defvar bloom-library--folder-paths (make-hash-table :test #'equal)
  "Opaque folder id to canonical directory path.")

(defvar bloom-library--heading-refs (make-hash-table :test #'equal)
  "Notebook/heading composite key to private Org ref.")

(defvar bloom-library--outlines (make-hash-table :test #'equal)
  "Opaque notebook id to presentation-safe outline snapshot.")

(defvar bloom-library--documents (make-hash-table :test #'equal)
  "Notebook/heading composite key to private document snapshot.")

(defun bloom-library-snapshot ()
  "Return a defensive copy of Bloom's notebook snapshot."
  (copy-tree bloom-library--snapshot))

(defun bloom-library-search-snapshot ()
  "Return a defensive copy of Bloom's search snapshot."
  (copy-tree bloom-library--search-snapshot))

(defun bloom-library--notebook-id (path)
  "Return PATH's stable opaque notebook id."
  (jetpacs-opaque-id "bloom-notebook-" (file-truename path)))

(defun bloom-library--folder-id (path)
  "Return PATH's stable opaque folder id."
  (jetpacs-opaque-id
   "bloom-folder-" (directory-file-name (file-truename path))))

(defun bloom-library--heading-id (ref)
  "Return a stable opaque heading id for private Org REF."
  (let ((file (or (plist-get ref :file) ""))
        (id (plist-get ref :id)))
    (jetpacs-opaque-id
     "bloom-heading-"
     (if (and (stringp id) (not (string-empty-p id)))
         (format "%s|id:%s" file id)
       (format "%s|pos:%s|headline:%s" file
               (or (plist-get ref :pos) 0)
               (or (plist-get ref :headline) ""))))))

(defun bloom-library-identify-heading (ref)
  "Retain private Org REF and return its opaque Bloom coordinates.
The return value is `(:notebook-id ID :heading-id ID)'.  REF and its path
remain only in Bloom's Emacs-side map; nil means the ref is outside the
canonical notebook library or cannot currently be resolved."
  (when-let* ((ref (and (listp ref) (copy-tree ref)))
              (file (plist-get ref :file))
              (canonical (ebp-org-file-allowed-p file)))
    (plist-put ref :file canonical)
    (let ((notebook-id (bloom-library--notebook-id canonical))
          (heading-id (bloom-library--heading-id ref)))
      (when (bloom-library-resolve-notebook notebook-id)
        (puthash (bloom-library--heading-key notebook-id heading-id)
                 ref bloom-library--heading-refs)
        (list :notebook-id notebook-id :heading-id heading-id)))))

(defun bloom-library--heading-key (notebook-id heading-id)
  "Return the private map key for NOTEBOOK-ID and HEADING-ID."
  (concat notebook-id "|" heading-id))

(defun bloom-library--org-root ()
  "Return Bloom's preferred canonical Org root, or nil."
  (let ((candidate (and (boundp 'org-directory)
                        (stringp org-directory)
                        (expand-file-name org-directory))))
    (when (and candidate (file-directory-p candidate)
               (not (file-remote-p candidate)))
      (file-name-as-directory (file-truename candidate)))))

(defun bloom-library--display-folder (directory root)
  "Return a safe display label for DIRECTORY relative to ROOT."
  (let ((relative
         (and root (file-in-directory-p directory root)
              (file-relative-name directory root))))
    (cond
     ((or (equal relative "./") (equal relative ".")) "Notebooks")
     (relative (directory-file-name relative))
     (t (file-name-nondirectory (directory-file-name directory))))))

(defun bloom-library--directory-chain (directory root)
  "Return DIRECTORY and its visible ancestors below ROOT.
When DIRECTORY is outside ROOT, return only DIRECTORY."
  (let ((current (directory-file-name directory))
        (root-clean (and root (directory-file-name root)))
        result)
    (if (and root-clean (file-in-directory-p current root-clean))
        (while (not (equal current root-clean))
          (push current result)
          (setq current
                (directory-file-name (file-name-directory current))))
      (push current result))
    (nreverse result)))

(defun bloom-library--folder-records (directories root)
  "Return safe folder records for canonical DIRECTORIES relative to ROOT."
  (let ((known (make-hash-table :test #'equal))
        records)
    (dolist (directory directories)
      (puthash (directory-file-name directory) t known))
    (dolist (directory directories)
      (let* ((clean (directory-file-name directory))
             (id (bloom-library--folder-id clean))
             (parent (directory-file-name (file-name-directory clean)))
             (root-clean (and root (directory-file-name root)))
             (parent-id (and (not (equal clean root-clean))
                             (gethash parent known)
                             (not (equal parent root-clean))
                             (bloom-library--folder-id parent))))
        (unless (and root-clean (equal clean root-clean))
          (puthash id (file-name-as-directory clean)
                   bloom-library--folder-paths)
          (push (list :id id :parent-id parent-id
                      :label (file-name-nondirectory clean)
                      :path-label (bloom-library--display-folder clean root))
                records))))
    (sort records
          (lambda (left right)
            (string-lessp (downcase (plist-get left :path-label))
                          (downcase (plist-get right :path-label)))))))

;;;; Facts the Android app shows on each Notebooks row

(defcustom bloom-library-expanded-folder-ids nil
  "Opaque folder ids whose subtree Notebooks shows expanded in place."
  :type '(repeat string)
  :group 'bloom)

(defcustom bloom-library-tree-defaults-applied nil
  "Non-nil once Notebooks has chosen its first-open folder expansion.
Like the app, the first refresh that finds notebooks expands every folder
holding a planned note, and later refreshes leave expansion to the user."
  :type 'boolean
  :group 'bloom)

(defconst bloom-library-drill-threshold 20
  "Folders holding more files than this open as their own screen.
The Android app's `FOLDER_DRILL_THRESHOLD': a smaller folder expands in place.")

(defconst bloom-library--palette-keys
  '("green" "blue" "amber" "red" "accent" "neutral")
  "Monogram palette keys in the order the app's name hash indexes them.")

(defconst bloom-library--note-scan-bytes 2000000
  "Bytes of a notebook read when counting its notes.")

(defun bloom-library--java-hash (string)
  "Return Java's `String.hashCode' of STRING as a non-negative integer.
The app keys a notebook's default monogram color on this hash of its
vault-relative path, so the same file gets the same color in both."
  (let ((bytes (encode-coding-string string 'utf-16be))
        (hash 0))
    (dotimes (index (/ (length bytes) 2))
      (setq hash (logand (+ (* 31 hash)
                            (* 256 (aref bytes (* 2 index)))
                            (aref bytes (1+ (* 2 index))))
                         #xFFFFFFFF)))
    (when (>= hash #x80000000) (setq hash (- hash #x100000000)))
    (if (= hash (- #x80000000)) 0 (abs hash))))

(defun bloom-library-palette-key (relative)
  "Return the default monogram palette key for vault-relative path RELATIVE."
  (nth (% (bloom-library--java-hash relative) 6) bloom-library--palette-keys))

(defun bloom-library--monogram (name)
  "Return the uppercase first character of NAME, or a bullet when blank."
  (let ((trimmed (string-trim (or name ""))))
    (if (string-empty-p trimmed) "•"
      (upcase (substring trimmed 0 1)))))

(defun bloom-library--note-count (file)
  "Return FILE's note count as the app computes it.
Top-level headings are notes; text before the first heading, other than
blank lines, keywords, and drawers, counts as one more."
  (with-temp-buffer
    (insert-file-contents file nil 0 bloom-library--note-scan-bytes)
    (goto-char (point-min))
    (let ((count 0) (intro nil) (in-drawer nil) (before-heading t))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (cond
           ((string-match-p "\\`\\* " line)
            (setq count (1+ count) before-heading nil))
           ((not before-heading))
           (in-drawer
            (when (string-match-p "\\`[ \t]*:END:[ \t]*\\'" line)
              (setq in-drawer nil)))
           ((string-match-p "\\`[ \t]*:[A-Za-z_-]+:[ \t]*\\'" line)
            (setq in-drawer t))
           ((or (string-match-p "\\`[ \t]*\\'" line)
                (string-match-p "\\`[ \t]*#\\+" line)))
           (t (setq intro t))))
        (forward-line 1))
      (+ count (if intro 1 0)))))

(defun bloom-library--plural (count singular)
  "Return COUNT followed by SINGULAR, pluralized with an s."
  (format "%d %s%s" count singular (if (= count 1) "" "s")))

(defun bloom-library-relative-time (time now)
  "Return TIME relative to NOW the way Android's `DateUtils' prints it.
Under a minute is zero minutes; then minutes, hours, and calendar days, with
yesterday by name; a week or more is the abbreviated date."
  (let* ((seconds (max 0 (float-time (time-subtract now time))))
         (days (- (time-to-days now) (time-to-days time))))
    (cond
     ((< seconds 3600)
      (format "%s ago" (bloom-library--plural (floor seconds 60) "minute")))
     ((< seconds 86400)
      (format "%s ago" (bloom-library--plural (floor seconds 3600) "hour")))
     ((< seconds 604800)
      (if (<= days 1) "Yesterday"
        (format "%s ago" (bloom-library--plural days "day"))))
     (t (format-time-string "%b %-d, %Y" time)))))

(defun bloom-library--relative-path (path root)
  "Return PATH relative to ROOT, or its plain name outside ROOT."
  (if (and root (file-in-directory-p path root))
      (directory-file-name (file-relative-name path root))
    (file-name-nondirectory (directory-file-name path))))

(defun bloom-library--planned-p (file)
  "Return non-nil when FILE holds a SCHEDULED or DEADLINE planning line."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file nil 0 bloom-library--note-scan-bytes)
        (goto-char (point-min))
        (re-search-forward "^[ \t]*\\(?:CLOSED:.*\\)?\\(?:SCHEDULED\\|DEADLINE\\):"
                           nil t))
    (error nil)))

(defun bloom-library--persist (symbol value)
  "Set SYMBOL to VALUE and save it with Customize when Emacs can.
A batch or `emacs -Q' session keeps the value for the session only."
  (set symbol value)
  (ignore-errors (jetpacs-settings-save-variable symbol value)))

(defun bloom-library--apply-tree-defaults (records)
  "Choose the first-open folder expansion from notebook RECORDS, once.
Every folder that, at any depth, holds a planned note opens; the rest stay
folded.  It runs only when RECORDS is non-empty, so a pre-sync scan of an
empty vault does not record an empty choice."
  (when (and records (not bloom-library-tree-defaults-applied))
    (let ((root (bloom-library--org-root)) open)
      (dolist (record records)
        (when (plist-get record :planned)
          (let ((file (gethash (plist-get record :id)
                               bloom-library--notebook-paths)))
            (when file
              (dolist (directory (bloom-library--directory-chain
                                  (file-name-directory file) root))
                (unless (and root (file-equal-p directory root))
                  (cl-pushnew (bloom-library--folder-id directory) open
                              :test #'equal)))))))
      (bloom-library--persist 'bloom-library-expanded-folder-ids
                              (sort open #'string<))
      (bloom-library--persist 'bloom-library-tree-defaults-applied t))))

(defun bloom-library--file-facts (file root now)
  "Return FILE's row facts relative to ROOT, with its age measured at NOW."
  (let* ((relative (bloom-library--relative-path file root))
         (modified (file-attribute-modification-time (file-attributes file)))
         (notes (condition-case nil (bloom-library--note-count file)
                  (error nil))))
    (list :relative relative
          :planned (bloom-library--planned-p file)
          :palette (bloom-library-palette-key relative)
          :monogram (bloom-library--monogram (file-name-nondirectory file))
          :note-count notes
          :modified-label (bloom-library-relative-time modified now)
          :directory-label
          (let ((dir (file-name-directory relative)))
            (and dir (not (string-empty-p dir)) dir)))))

(defun bloom-library--count-folders (folders notebooks)
  "Return FOLDERS with file, subfolder, depth, and palette facts.
Counts use NOTEBOOKS, so they reflect the bounded snapshot."
  (mapcar
   (lambda (folder)
     (let* ((id (plist-get folder :id))
            (path (gethash id bloom-library--folder-paths))
            (relative (plist-get folder :path-label)))
       (append
        folder
        (list
         :file-count
         (cl-count-if
          (lambda (notebook)
            (let ((file (gethash (plist-get notebook :id)
                                 bloom-library--notebook-paths)))
              (and path file (file-in-directory-p file path))))
          notebooks)
         :folder-count
         (cl-count id folders
                   :key (lambda (other) (plist-get other :parent-id))
                   :test #'equal)
         :depth (1+ (cl-count ?/ relative))
         :palette (bloom-library-palette-key relative)
         :expanded (and (member id bloom-library-expanded-folder-ids) t)))))
   folders))

(defun bloom-library-set-folder-expanded (folder-id expanded)
  "Record FOLDER-ID as EXPANDED or collapsed in the Notebooks tree.
The choice persists, as the app's does."
  (bloom-library--persist
   'bloom-library-expanded-folder-ids
   (if expanded
       (cl-adjoin folder-id bloom-library-expanded-folder-ids :test #'equal)
     (remove folder-id bloom-library-expanded-folder-ids)))
  (setq bloom-library--snapshot
        (plist-put (copy-tree bloom-library--snapshot) :folders
                   (mapcar (lambda (folder)
                             (if (equal (plist-get folder :id) folder-id)
                                 (plist-put (copy-sequence folder)
                                            :expanded (and expanded t))
                               folder))
                           (plist-get bloom-library--snapshot :folders)))))

(defun bloom-library--notebook-record (id)
  "Return the safe notebook record for ID, or nil."
  (cl-find id (plist-get bloom-library--snapshot :notebooks)
           :key (lambda (record) (plist-get record :id)) :test #'equal))

(defun bloom-library-refresh ()
  "Refresh Bloom's bounded notebook snapshot from the canonical Org scope."
  (interactive)
  ;; Ids stored under the Grove name carry its prefix; the hash is unchanged.
  (setq bloom-library-favorite-ids
        (mapcar #'bloom-core-migrate-id bloom-library-favorite-ids)
        bloom-library-expanded-folder-ids
        (mapcar #'bloom-core-migrate-id bloom-library-expanded-folder-ids))
  (condition-case err
      (let* ((scope (sort (delete-dups
                           (delq nil
                                 (mapcar
                                  (lambda (file)
                                    (when (and (stringp file)
                                               (file-exists-p file)
                                               (not (file-remote-p file)))
                                      (file-truename file)))
                                  (jetpacs-org-mode-agenda-scope))))
                          #'string-lessp))
             (now (current-time))
             (total (length scope))
             (files (seq-take scope bloom-library-notebook-limit))
             (root (bloom-library--org-root))
             (directories
              (sort
               (delete-dups
                (apply #'append
                       (mapcar
                        (lambda (file)
                          (bloom-library--directory-chain
                           (file-name-directory file) root))
                        files)))
               #'string-lessp))
             records)
        (clrhash bloom-library--notebook-paths)
        (clrhash bloom-library--folder-paths)
        (dolist (file files)
          (let* ((id (bloom-library--notebook-id file))
                 (directory (file-name-directory file))
                 (folder-id
                  (unless (and root (file-equal-p directory root))
                    (bloom-library--folder-id directory))))
            (puthash id file bloom-library--notebook-paths)
            (push (append
                   (list :id id :folder-id folder-id
                         :label (file-name-base file)
                         :file-label (file-name-nondirectory file)
                         :folder-label
                         (bloom-library--display-folder directory root)
                         :favorite (and (member id bloom-library-favorite-ids) t))
                   (bloom-library--file-facts file root now))
                  records)))
        (setq records
              (sort records
                    (lambda (left right)
                      (let ((lf (if (plist-get left :favorite) "0" "1"))
                            (rf (if (plist-get right :favorite) "0" "1")))
                        (string-lessp
                         (format "%s|%s|%s" lf
                                 (downcase (plist-get left :folder-label))
                                 (downcase (plist-get left :label)))
                         (format "%s|%s|%s" rf
                                 (downcase (plist-get right :folder-label))
                                 (downcase (plist-get right :label))))))))
        (bloom-library--apply-tree-defaults records)
        (setq bloom-library--snapshot
              (list :notebooks records
                    :folders (bloom-library--count-folders
                              (bloom-library--folder-records directories root)
                              records)
                    :total-count total
                    :truncated (> total (length files))
                    :error nil))
        (run-hooks 'bloom-library-after-refresh-hook)
        t)
    (error
     (message "bloom-library: refresh failed (%s)" (car err))
     (setq bloom-library--snapshot
           (plist-put (copy-tree bloom-library--snapshot)
                      :error "Notebook library unavailable"))
     (run-hooks 'bloom-library-after-refresh-hook)
     nil)))

(defun bloom-library-resolve-notebook (notebook-id)
  "Return NOTEBOOK-ID's canonical path, rebuilding the map once if needed."
  (when (stringp notebook-id)
    (or (gethash notebook-id bloom-library--notebook-paths)
        (and (bloom-library-refresh)
             (gethash notebook-id bloom-library--notebook-paths)))))

(defun bloom-library-resolve-folder (folder-id)
  "Return FOLDER-ID's canonical path; nil identifies the Org root."
  (if (null folder-id)
      (or (bloom-library--org-root)
          (when-let* ((file (car (sort
                                  (copy-sequence
                                   (hash-table-values
                                    bloom-library--notebook-paths))
                                  #'string-lessp))))
            (file-name-directory file)))
    (or (gethash folder-id bloom-library--folder-paths)
        (and (bloom-library-refresh)
             (gethash folder-id bloom-library--folder-paths)))))

(defun bloom-library-notebooks-in-folder (&optional folder-id flat)
  "Return safe notebooks directly in FOLDER-ID, or every notebook when FLAT."
  (let ((records (plist-get bloom-library--snapshot :notebooks)))
    (if flat records
      (seq-filter (lambda (record)
                    (equal (plist-get record :folder-id) folder-id))
                  records))))

(defun bloom-library-folders-in-folder (&optional folder-id)
  "Return safe folders directly below FOLDER-ID."
  (seq-filter (lambda (record)
                (equal (plist-get record :parent-id) folder-id))
              (plist-get bloom-library--snapshot :folders)))

(defun bloom-library--forget-heading-state (notebook-id)
  "Forget private heading and document state owned by NOTEBOOK-ID."
  (let ((prefix (concat notebook-id "|"))
        dead-refs dead-documents)
    (maphash (lambda (key _value)
               (when (string-prefix-p prefix key) (push key dead-refs)))
             bloom-library--heading-refs)
    (maphash (lambda (key _value)
               (when (string-prefix-p prefix key)
                 (push key dead-documents)))
             bloom-library--documents)
    (dolist (key dead-refs) (remhash key bloom-library--heading-refs))
    (dolist (key dead-documents) (remhash key bloom-library--documents))
    (remhash notebook-id bloom-library--outlines)))

(defconst bloom-library--timestamp-re
  (concat "[<[]\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
          "\\(?: +[^ 0-9>]+\\)?"
          "\\(?: +\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)"
          "\\(?:-\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)\\)?\\)?"
          "\\(?: +\\([.+]?\\+[0-9]+[hdwmy]\\)\\)?"
          "\\(?: +\\(--?[0-9]+[hdwmy]\\)\\)?")
  "An Org timestamp: date, optional time range, repeater, and warning.")

(defun bloom-library-human-timestamp (timestamp now)
  "Return Org TIMESTAMP as the app's chip prints it, relative to NOW's year.
`Sep 5 09:00-11:00 +1w': abbreviated month and day, the year only when it
is not NOW's, then any time range, repeater, and warning.  Nil for nil."
  (when (and (stringp timestamp)
             (string-match bloom-library--timestamp-re timestamp))
    (let* ((year (string-to-number (match-string 1 timestamp)))
           (month (string-to-number (match-string 2 timestamp)))
           (day (string-to-number (match-string 3 timestamp)))
           (start (match-string 4 timestamp))
           (end (match-string 5 timestamp))
           (repeat (match-string 6 timestamp))
           (warn (match-string 7 timestamp))
           (date (encode-time (list 0 0 12 day month year))))
      (concat (format-time-string "%b %-d" date)
              (unless (= year (decoded-time-year (decode-time now)))
                (format ", %d" year))
              (when start (concat " " start (when end (concat "-" end))))
              (when repeat (concat " " repeat))
              (when warn (concat " " warn))))))

(defun bloom-library--preview-lines (body)
  "Return BODY's first three non-blank lines outside drawers, joined.
The app previews a heading with these, two of them shown, so a third line
makes the second end in an ellipsis."
  (let (lines drawer)
    (dolist (line (split-string (or body "") "\n"))
      (let ((trimmed (string-trim line)))
        (cond
         (drawer (when (string-match-p "\\`:END:\\'" trimmed) (setq drawer nil)))
         ((string-match-p "\\`:[A-Za-z_-]+:\\'" trimmed) (setq drawer t))
         ((string-empty-p trimmed))
         ((< (length lines) 3) (push trimmed lines)))))
    (when lines
      (truncate-string-to-width (string-join (nreverse lines) "\n") 240))))

(defun bloom-library--preface-keywords (preface)
  "Return PREFACE's `#+KEY:' lines as (KEY . VALUE) pairs, in order."
  (let (pairs)
    (dolist (line (split-string (or preface "") "\n"))
      (when (string-match "\\`[ \t]*\\(#\\+[A-Za-z0-9_-]+:\\)[ \t]*\\(.*\\)" line)
        (push (cons (match-string 1 line) (string-trim (match-string 2 line)))
              pairs)))
    (nreverse pairs)))

(defun bloom-library--preface-intro (preface)
  "Return PREFACE's prose: lines other than keywords and drawers, trimmed."
  (let (lines drawer)
    (dolist (line (split-string (or preface "") "\n"))
      (let ((trimmed (string-trim line)))
        (cond
         (drawer (when (string-match-p "\\`:END:\\'" trimmed) (setq drawer nil)))
         ((string-match-p "\\`:[A-Za-z_-]+:\\'" trimmed) (setq drawer t))
         ((string-match-p "\\`#\\+" trimmed))
         (t (push line lines)))))
    (let ((text (string-trim (string-join (nreverse lines) "\n"))))
      (unless (string-empty-p text) text))))

(defconst bloom-library--active-timestamp-re
  "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^>\n]*>\\(?:--<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^>\n]*>\\)?"
  "An active Org timestamp, with an optional `--' range end.")

(defun bloom-library--active-timestamps (body)
  "Return the bare active timestamps in heading BODY, in order.
Planning lines do not count: their stamps are the heading's plan, and
the app treats only the other active stamps as agenda events."
  (let (stamps)
    (dolist (line (split-string (or body "") "\n"))
      (unless (string-match-p
               "\\`[ \t]*\\(?:SCHEDULED\\|DEADLINE\\|CLOSED\\):" line)
        (let ((start 0))
          (while (string-match bloom-library--active-timestamp-re line start)
            (push (match-string 0 line) stamps)
            (setq start (match-end 0))))))
    (nreverse stamps)))

(defun bloom-library--scan-outline (notebook-id)
  "Scan NOTEBOOK-ID and return a safe nested-outline snapshot."
  (let ((file (or (bloom-library-resolve-notebook notebook-id)
                  (signal 'ebp-org-unresolved nil))))
    (with-current-buffer (find-file-noselect file t)
      (unless (derived-mode-p 'org-mode) (org-mode))
      (org-with-wide-buffer
       (let* ((name (buffer-name))
              (first (save-excursion
                       (goto-char (point-min))
                       (and (re-search-forward org-heading-regexp nil t)
                            (line-beginning-position))))
              (preface (string-trim
                        (buffer-substring-no-properties
                         (point-min) (or first (point-max)))))
              (raw (ebp-org-outline-collect
                    (point-min) (point-max) nil nil
                    ebp-org-outline-max-headings))
              (stack nil)
              (now (current-time))
              records)
         (dolist (record raw)
           (goto-char (plist-get record :pos))
           (let* ((ref (ebp-org-ref-at-point))
                  (heading-id (bloom-library--heading-id ref))
                  (level (plist-get record :level))
                  (revision
                   (secure-hash
                    'sha256 (current-buffer) (point)
                    (save-excursion (org-end-of-subtree t t) (point)))))
             (while (and stack (>= (caar stack) level)) (pop stack))
             (let ((parent-id (cdar stack)))
               (puthash (bloom-library--heading-key notebook-id heading-id)
                        ref bloom-library--heading-refs)
               (push (list :id heading-id :parent-id parent-id :level level
                           :title (plist-get record :title)
                           :todo (plist-get record :todo)
                           :priority (plist-get record :priority)
                           :tags (copy-sequence (plist-get record :tags))
                           :deadline (plist-get record :deadline)
                           :deadline-label
                           (bloom-library-human-timestamp
                            (plist-get record :deadline) now)
                           :scheduled
                           (ignore-errors
                             (org-entry-get (plist-get record :pos) "SCHEDULED"))
                           :active (bloom-library--active-timestamps
                                    (plist-get record :body))
                           :scheduled-label
                           (bloom-library-human-timestamp
                            (ignore-errors
                              (org-entry-get (plist-get record :pos)
                                             "SCHEDULED"))
                            now)
                           :lines (bloom-library--preview-lines
                                   (plist-get record :body))
                           :done (plist-get record :done)
                           :revision revision
                           :preview
                           (let ((body (string-trim
                                        (or (plist-get record :body) ""))))
                             (and (not (string-empty-p body))
                                  (truncate-string-to-width body 120 nil nil t))))
                     records))
             (push (cons level heading-id) stack)))
         (list :notebook-id notebook-id
               :label (plist-get (bloom-library--notebook-record notebook-id)
                                 :label)
               :preface (truncate-string-to-width preface 8192 nil nil t)
               :keywords (bloom-library--preface-keywords preface)
               :todo-keywords (copy-sequence org-todo-keywords-1)
               :done-keywords (copy-sequence org-done-keywords)
               :file-tags (copy-sequence org-file-tags)
               :intro (bloom-library--preface-intro preface)
               :records (nreverse records)
               :buffer name
               :truncated (= (length raw) ebp-org-outline-max-headings)))))))

(defun bloom-library-outline-refresh (notebook-id)
  "Refresh and return NOTEBOOK-ID's safe outline snapshot."
  (bloom-library--forget-heading-state notebook-id)
  (let ((snapshot (bloom-library--scan-outline notebook-id)))
    (puthash notebook-id snapshot bloom-library--outlines)
    (run-hooks 'bloom-library-after-refresh-hook)
    (copy-tree snapshot)))

(defun bloom-library-outline-snapshot (notebook-id)
  "Return NOTEBOOK-ID's safe outline snapshot, producing it if absent."
  (copy-tree (or (gethash notebook-id bloom-library--outlines)
                 (bloom-library-outline-refresh notebook-id))))

(defun bloom-library-resolve-heading (notebook-id heading-id)
  "Return private Org ref for NOTEBOOK-ID and HEADING-ID, rebuilding once."
  (when (and (stringp notebook-id) (stringp heading-id))
    (let ((key (bloom-library--heading-key notebook-id heading-id)))
      (or (copy-tree (gethash key bloom-library--heading-refs))
          (and (ignore-errors (bloom-library-outline-refresh notebook-id))
               (copy-tree (gethash key bloom-library--heading-refs)))))))

;;;; Read-mode structure, as the app's OrgParser and BlockParser build it

(defconst bloom-library--list-item-re
  "\\`\\([ \t]*\\)\\([-+]\\|\\([0-9]+\\)[.)]\\)[ \t]+\\(?:\\[\\([ Xx-]\\)\\][ \t]+\\)?\\(.*\\)\\'"
  "A plain-list item: indent, bullet, ordinal, checkbox, and text.")

(defconst bloom-library--begin-re "\\`[ \t]*#\\+[Bb][Ee][Gg][Ii][Nn]_\\([^ \t]+\\)\\(.*\\)\\'"
  "A `#+BEGIN_TYPE' line: type and trailing text.")

(defconst bloom-library--end-re "\\`[ \t]*#\\+[Ee][Nn][Dd]_\\([^ \t]+\\)[ \t]*\\'"
  "A `#+END_TYPE' line.")

(defconst bloom-library--keyword-re "\\`[ \t]*#\\+\\([A-Za-z][A-Za-z0-9_-]*\\):.*\\'"
  "A standalone `#+KEYWORD:' line.")

(defun bloom-library--keyword-line-p (line)
  "Return non-nil when LINE is a keyword line other than a block marker."
  (and (string-match bloom-library--keyword-re line)
       (not (string-match-p "\\`[ \t]*#\\+\\(?:[Bb][Ee][Gg][Ii][Nn]\\|[Ee][Nn][Dd]\\)_"
                            line))))

(defun bloom-library--list-continuation-p (line indent)
  "Return non-nil when LINE continues a list item indented INDENT columns."
  (and (not (string-blank-p line))
       (> (- (length line) (length (string-trim-left line))) indent)
       (not (string-prefix-p "|" (string-trim-left line)))
       (not (bloom-library--keyword-line-p line))
       (not (string-match-p bloom-library--begin-re line))
       (not (string-match-p bloom-library--end-re line))))

(defun bloom-library-parse-blocks (lines)
  "Group body LINES into the app's read-mode blocks.
Each block is a plist with `:type' paragraph, list, block, or table.  A
`#+BEGIN' block keeps any keyword run directly above it as affiliated
lines; a keyword run on its own becomes a collapsed block of its own."
  (let ((lines (vconcat lines)) (i 0) blocks para)
    (cl-labels
        ((flush ()
           (when para
             (push (list :type 'paragraph :lines (nreverse para)) blocks)
             (setq para nil)))
         (parse-block (begin affiliated)
           (string-match bloom-library--begin-re (aref lines begin))
           (let* ((kind (upcase (match-string 1 (aref lines begin))))
                  (trailing (string-trim (match-string 2 (aref lines begin))))
                  (language (and (equal kind "SRC")
                                 (car (split-string trailing nil t))))
                  (j (1+ begin)) content)
             (catch 'done
               (while (< j (length lines))
                 (let ((line (aref lines j)))
                   (when (and (string-match bloom-library--end-re line)
                              (equal (upcase (match-string 1 line)) kind))
                     (setq j (1+ j))
                     (throw 'done nil))
                   (push line content)
                   (setq j (1+ j)))))
             (push (list :type 'block :kind kind :language language
                         :affiliated affiliated :lines (nreverse content)
                         :keyword-run nil :start begin)
                   blocks)
             (setq i j))))
      (while (< i (length lines))
        (let ((line (aref lines i)))
          (cond
           ((string-match-p bloom-library--begin-re line)
            (flush) (parse-block i nil))
           ((bloom-library--keyword-line-p line)
            (let ((k i) run)
              (while (and (< k (length lines))
                          (bloom-library--keyword-line-p (aref lines k)))
                (push (aref lines k) run)
                (setq k (1+ k)))
              (setq run (nreverse run))
              (flush)
              (if (and (< k (length lines))
                       (string-match-p bloom-library--begin-re (aref lines k)))
                  (parse-block k run)
                (string-match bloom-library--keyword-re (car run))
                (push (list :type 'block
                            :kind (if (cdr run) "KEYWORDS"
                                    (upcase (match-string 1 (car run))))
                            :affiliated nil :lines run :keyword-run t :start i)
                      blocks)
                (setq i k))))
           ((string-prefix-p "|" (string-trim-left line))
            (flush)
            (let (table)
              (while (and (< i (length lines))
                          (string-prefix-p "|" (string-trim-left (aref lines i))))
                (push (aref lines i) table)
                (setq i (1+ i)))
              (push (list :type 'table :lines (nreverse table)) blocks)))
           ((string-match bloom-library--list-item-re line)
            (flush)
            (let (items)
              (catch 'end
                (while (< i (length lines))
                  (let ((current (aref lines i)))
                    (cond
                     ((string-match bloom-library--list-item-re current)
                      (push (list :indent (length (match-string 1 current))
                                  :ordered (and (match-string 3 current) t)
                                  :checkbox (match-string 4 current)
                                  :text (match-string 5 current))
                            items)
                      (setq i (1+ i)))
                     ((and items (bloom-library--list-continuation-p
                                  current (plist-get (car items) :indent)))
                      (setcar items
                              (plist-put (car items) :text
                                         (concat (plist-get (car items) :text) " "
                                                 (string-trim current))))
                      (setq i (1+ i)))
                     (t (throw 'end nil))))))
              (push (list :type 'list
                          :items (bloom-library--number-items (nreverse items)))
                    blocks)))
           ((string-blank-p line) (flush) (setq i (1+ i)))
           (t (push line para) (setq i (1+ i))))))
      (flush)
      (nreverse blocks))))

(defun bloom-library--number-items (items)
  "Return list ITEMS, each ordered item numbered within its own nested run."
  (let (counts)
    (mapcar (lambda (item)
              (let ((indent (plist-get item :indent)))
                ;; A shallower item closes deeper runs; a bullet breaks a run.
                (setq counts (seq-filter (lambda (c) (<= (car c) indent)) counts))
                (if (plist-get item :ordered)
                    (let ((cell (assoc indent counts)))
                      (if cell (setcdr cell (1+ (cdr cell)))
                        (push (cons indent 1) counts))
                      (plist-put item :ordinal (cdr (assoc indent counts))))
                  (setq counts (assq-delete-all indent counts))
                  item)))
            items)))

(defun bloom-library-parse-subtree (content todo-keywords done-keywords now)
  "Parse subtree CONTENT into the app's read-mode headings, relative to NOW.
TODO-KEYWORDS and DONE-KEYWORDS are the buffer's keywords.  Each heading
plist carries its level, keyword, priority, title, own tags, planning
labels, PROPERTIES pairs, LOGBOOK lines, and body blocks."
  (let ((lines (vconcat (split-string content "\n")))
        (i 0) headings)
    (while (< i (length lines))
      (let ((line (aref lines i)))
        (if (not (string-match "\\`\\(\\*+\\)[ \t]+\\(.*\\)\\'" line))
            (setq i (1+ i))
          (let* ((level (length (match-string 1 line)))
                 (rest (match-string 2 line))
                 todo priority tags planning properties logbook body)
            (when (string-match "[ \t]+\\(:\\(?:[[:alnum:]_@#%]+:\\)+\\)[ \t]*\\'" rest)
              ;; Read the match before `split-string' runs its own.
              (let ((cut (match-beginning 0)) (field (match-string 1 rest)))
                (setq tags (split-string field ":" t)
                      rest (substring rest 0 cut))))
            (let ((word (car (split-string rest))))
              (when (member word todo-keywords)
                (setq todo word rest (string-trim-left (substring rest (length word))))))
            (when (string-match "\\`\\[#\\(.\\)\\][ \t]*" rest)
              (setq priority (match-string 1 rest)
                    rest (substring rest (match-end 0))))
            (setq i (1+ i))
            (when (and (< i (length lines))
                       (string-match-p "\\`[ \t]*\\(?:SCHEDULED\\|DEADLINE\\|CLOSED\\):"
                                       (aref lines i)))
              (let ((pl (aref lines i)) (start 0))
                (while (string-match "\\(SCHEDULED\\|DEADLINE\\|CLOSED\\):[ \t]*\\([<[][^]>]*[]>]\\)"
                                     pl start)
                  ;; Capture the match first: formatting runs its own match.
                  (let ((key (intern (concat ":" (downcase (match-string 1 pl)))))
                        (stamp (match-string 2 pl)))
                    (setq start (match-end 0))
                    (push (cons key (bloom-library-human-timestamp stamp now))
                          planning))))
              (setq i (1+ i)))
            ;; PROPERTIES and LOGBOOK directly after the planning line.
            (catch 'drawers
              (while (and (< i (length lines))
                          (string-match "\\`[ \t]*:\\(PROPERTIES\\|LOGBOOK\\):[ \t]*\\'"
                                        (aref lines i)))
                (let ((name (upcase (match-string 1 (aref lines i)))) (j (1+ i)) pending)
                  (while (and (< j (length lines))
                              (not (string-match-p "\\`[ \t]*:END:[ \t]*\\'" (aref lines j)))
                              (not (string-match-p "\\`\\*+[ \t]" (aref lines j))))
                    (push (aref lines j) pending)
                    (setq j (1+ j)))
                  (unless (and (< j (length lines))
                               (string-match-p "\\`[ \t]*:END:" (aref lines j)))
                    (throw 'drawers nil))  ; unclosed: body text
                  (setq pending (nreverse pending))
                  (if (equal name "PROPERTIES")
                      (dolist (prop pending)
                        (when (string-match "\\`[ \t]*:\\([^: \t]+\\):[ \t]*\\(.*\\)\\'" prop)
                          (push (cons (format ":%s:" (match-string 1 prop))
                                      (match-string 2 prop))
                                properties)))
                    (setq logbook (append logbook (mapcar #'string-trim pending))))
                  (setq i (1+ j)))))
            (while (and (< i (length lines))
                        (not (string-match-p "\\`\\*+[ \t]" (aref lines i))))
              (push (aref lines i) body)
              (setq i (1+ i)))
            (push (list :level level :todo todo
                        :done (and todo (member todo done-keywords) t)
                        :priority priority :title (string-trim rest) :tags tags
                        :scheduled (alist-get :scheduled planning)
                        :deadline (alist-get :deadline planning)
                        :closed (alist-get :closed planning)
                        :properties (nreverse properties) :logbook logbook
                        :blocks (bloom-library-parse-blocks (nreverse body)))
                  headings)))))
    (let ((ordered (nreverse headings)) (index 0))
      (mapcar (lambda (heading)
                (prog1 (append (list :index index) heading)
                  (setq index (1+ index))))
              ordered))))

(defun bloom-library--document-info (notebook-id heading-id)
  "Return private document information for NOTEBOOK-ID and HEADING-ID."
  (let ((ref (or (bloom-library-resolve-heading notebook-id heading-id)
                 (signal 'ebp-org-unresolved nil))))
    (let ((marker (ebp-org-resolve-ref ref)))
      (unwind-protect
          (with-current-buffer (marker-buffer marker)
            (org-with-wide-buffer
             (goto-char marker)
             (org-back-to-heading t)
             (let* ((beg (point))
                    (end (save-excursion (org-end-of-subtree t t) (point)))
                    (components (org-heading-components))
                    (content (buffer-substring-no-properties beg end))
                    ancestors)
               (save-excursion
                 (while (org-up-heading-safe)
                   (push (or (nth 4 (org-heading-components)) "Untitled")
                         ancestors)))
               (list :notebook-id notebook-id :heading-id heading-id
                     :headline (or (nth 4 components) "Untitled")
                     :todo (nth 2 components)
                     :priority (and (nth 3 components)
                                    (char-to-string (nth 3 components)))
                     :tags (org-get-tags nil t)
                     :scheduled (org-entry-get (point) "SCHEDULED")
                     :deadline (org-entry-get (point) "DEADLINE")
                     :properties (seq-take
                                  (org-entry-properties (point) 'standard) 50)
                     :ancestors ancestors :buffer (buffer-name)
                     :file-label (file-name-nondirectory
                                  (or (buffer-file-name) (buffer-name)))
                     :read (bloom-library-parse-subtree
                            content org-todo-keywords-1 org-done-keywords
                            (current-time))
                     :beg beg :end end :content content
                     :revision (secure-hash 'sha256 content)
                     :ref ref))))
        (set-marker marker nil)))))

(defun bloom-library-document-refresh (notebook-id heading-id)
  "Refresh and return NOTEBOOK-ID and HEADING-ID's private snapshot."
  (let ((snapshot (bloom-library--document-info notebook-id heading-id)))
    (puthash (bloom-library--heading-key notebook-id heading-id)
             snapshot bloom-library--documents)
    (run-hooks 'bloom-library-after-refresh-hook)
    (copy-tree snapshot)))

(defun bloom-library-document-snapshot (notebook-id heading-id)
  "Return NOTEBOOK-ID and HEADING-ID's snapshot, producing it if absent."
  (copy-tree
   (or (gethash (bloom-library--heading-key notebook-id heading-id)
                bloom-library--documents)
       (bloom-library-document-refresh notebook-id heading-id))))

(defun bloom-library--search-results (raw)
  "Convert canonical Org search RAW rows into Bloom-safe result records."
  (unless (plist-get bloom-library--snapshot :notebooks)
    (bloom-library-refresh))
  (let (results)
    (dolist (row raw)
      (let* ((ref (copy-tree (alist-get 'ref row)))
             (file (plist-get ref :file))
             (notebook-id (and file (bloom-library--notebook-id file)))
             (notebook (and notebook-id
                            (bloom-library--notebook-record notebook-id))))
        (when notebook
          (let ((heading-id (bloom-library--heading-id ref)))
            (puthash (bloom-library--heading-key notebook-id heading-id)
                     ref bloom-library--heading-refs)
            (push (list :notebook-id notebook-id :heading-id heading-id
                        :headline (or (alist-get 'headline row) "Untitled")
                        :todo (alist-get 'todo row)
                        :priority (alist-get 'priority row)
                        :tags (append (alist-get 'tags row) nil)
                        :notebook-label (plist-get notebook :label))
                  results)))))
    (nreverse results)))

(defun bloom-library--search-view-members (view)
  "Return presentation-safe active-view members from optional VIEW."
  (when view
    (list :view-id (plist-get view :id)
          :view-label (plist-get view :label)
          :view-source (plist-get view :source)
          :view-kind (plist-get view :kind))))

(defun bloom-library-org-ql-available-p ()
  "Non-nil when org-ql can be loaded for Lisp-form queries."
  (require 'org-ql nil t))

(defun bloom-library--notebook-files ()
  "Return every notebook file in Bloom's scope."
  (delq nil (mapcar (lambda (record)
                      (bloom-library-resolve-notebook (plist-get record :id)))
                    (plist-get bloom-library--snapshot :notebooks))))

(defun bloom-library--org-ql-rows (query limit)
  "Return up to LIMIT search rows for org-ql QUERY, a Lisp form as text.
Rows have the shape of `jetpacs-org-mode-search-items' rows, so the same
projection turns them into results."
  (unless (bloom-library-org-ql-available-p)
    (user-error "The org-ql package is not installed"))
  (let ((form (car (read-from-string query))))
    (unless (consp form) (user-error "An org-ql query is a Lisp form"))
    (seq-take
     (with-no-warnings
       (org-ql-select (bloom-library--notebook-files) form
         :action (lambda ()
                   (let ((components (org-heading-components)))
                     (list (cons 'ref (ebp-org-ref-at-point))
                           (cons 'headline (or (nth 4 components) ""))
                           (cons 'todo (nth 2 components))
                           (cons 'priority (and (nth 3 components)
                                                (char-to-string (nth 3 components))))
                           (cons 'tags (vconcat (org-get-tags nil t))))))))
     limit)))

(defun bloom-library-search-rows (query mode &optional limit)
  "Return native search rows for QUERY in MODE, up to LIMIT.
MODE is \"text\" (Org's agenda text search), \"match\" (Org's
tags/property/TODO match) or \"ql\" (an org-ql Lisp form)."
  (let ((limit (or limit bloom-search-result-limit)))
    (if (equal mode "ql")
        (bloom-library--org-ql-rows query limit)
      (jetpacs-org-mode-search-items
       query (if (equal mode "match") 'match 'text) limit))))

(defun bloom-library-search (query mode &optional view)
  "Replace Bloom's search snapshot using QUERY and MODE.
MODE is the string \"text\", \"match\" or \"ql\".  Optional VIEW identifies a saved
view that invoked the query.  A failed expression retains the last successful
result rows and records a safe validation error."
  (unless (and (stringp query) (member mode '("text" "match" "ql")))
    (user-error "Search requires text, match or ql mode and a string query"))
  (if (string-blank-p query)
      (progn
        (setq bloom-library--search-snapshot
              (list :query "" :mode mode :results nil :error nil))
        (run-hooks 'bloom-library-after-refresh-hook)
        t)
    (condition-case err
        (let ((raw (bloom-library-search-rows query mode)))
          (setq bloom-library--search-snapshot
                (append
                 (list :query query :mode mode
                       :results (bloom-library--search-results raw)
                       :error nil)
                 (bloom-library--search-view-members view)))
          (run-hooks 'bloom-library-after-refresh-hook)
          t)
      (error
       (message "bloom-library: search failed (%s)" (car err))
       (setq bloom-library--search-snapshot
             (append
              (list :query query :mode mode
                    :results
                    (plist-get bloom-library--search-snapshot :results)
                    :error "Invalid search expression")
              (bloom-library--search-view-members view)))
       (run-hooks 'bloom-library-after-refresh-hook)
       nil))))

(defun bloom-library-custom-view-search (view-id label kind)
  "Run opaque Org custom VIEW-ID, displaying LABEL and KIND.
Native Org settings and ordering come from Jetpacs' reusable custom-view
projection.  Failure retains the last successful rows behind a safe error."
  (unless (and (stringp view-id) (stringp label) (stringp kind))
    (user-error "Custom view identity is invalid"))
  (condition-case err
      (let ((raw (jetpacs-org-mode-custom-view-items
                  view-id bloom-search-result-limit)))
        (setq bloom-library--search-snapshot
              (list :query "" :mode "text"
                    :results (bloom-library--search-results raw)
                    :error nil :view-id view-id :view-label label
                    :view-source "emacs" :view-kind kind))
        (run-hooks 'bloom-library-after-refresh-hook)
        t)
    (ebp-org-unresolved (signal (car err) (cdr err)))
    (error
     (message "bloom-library: custom view failed (%s)" (car err))
     (setq bloom-library--search-snapshot
           (list :query "" :mode "text"
                 :results (plist-get bloom-library--search-snapshot :results)
                 :error "Saved view is unavailable"
                 :view-id view-id :view-label label
                 :view-source "emacs" :view-kind kind))
     (run-hooks 'bloom-library-after-refresh-hook)
     nil)))

(defun bloom-library-search-set-mode (mode)
  "Set Bloom search MODE and clear any saved-view attribution."
  (unless (member mode '("text" "match" "ql"))
    (user-error "Search mode must be text, match or ql"))
  (setq bloom-library--search-snapshot
        (list :query (or (plist-get bloom-library--search-snapshot :query) "")
              :mode mode
              :results (plist-get bloom-library--search-snapshot :results)
              :error (plist-get bloom-library--search-snapshot :error)))
  (run-hooks 'bloom-library-after-refresh-hook)
  t)

(defun bloom-library-clear-active-view (view-id)
  "Clear saved-view attribution when it currently names VIEW-ID."
  (when (equal view-id (plist-get bloom-library--search-snapshot :view-id))
    (setq bloom-library--search-snapshot
          (list :query (or (plist-get bloom-library--search-snapshot :query) "")
                :mode (or (plist-get bloom-library--search-snapshot :mode)
                          "text")
                :results (plist-get bloom-library--search-snapshot :results)
                :error (plist-get bloom-library--search-snapshot :error)))
    (run-hooks 'bloom-library-after-refresh-hook))
  t)

(defun bloom-library-toggle-favorite (notebook-id)
  "Persistently toggle NOTEBOOK-ID in Bloom's Customize-backed favorites."
  (unless (bloom-library-resolve-notebook notebook-id)
    (signal 'ebp-org-unresolved nil))
  (let* ((old (copy-sequence bloom-library-favorite-ids))
         (next (if (member notebook-id old)
                   (delete notebook-id (copy-sequence old))
                 (cons notebook-id old))))
    (condition-case err
        (progn
          (setq bloom-library-favorite-ids next)
          (jetpacs-settings-save-variable 'bloom-library-favorite-ids next)
          (bloom-library-refresh)
          (member notebook-id next))
      (error
       (setq bloom-library-favorite-ids old)
       (signal (car err) (cdr err))))))

(defun bloom-library--migrate-favorite (old-id new-id)
  "Replace favorite OLD-ID with NEW-ID and persist the new list."
  (when (member old-id bloom-library-favorite-ids)
    (setq bloom-library-favorite-ids
          (cons new-id
                (delete old-id
                        (copy-sequence bloom-library-favorite-ids))))
    ;; The file move/rename has already completed.  A broken optional
    ;; Customize destination must not revise that durable verdict.
    (condition-case err
        (jetpacs-settings-save-variable
         'bloom-library-favorite-ids bloom-library-favorite-ids)
      (error
       (message "bloom-library: favorite migration persistence failed (%s)"
                (car err))))))

(defun bloom-library--notebook-filename (name)
  "Validate NAME and return its single-segment Org filename."
  (unless (jetpacs-files-segment-p name)
    (user-error "Notebook name must be one printable path segment"))
  (let ((filename (if (string-match-p "\\.org\\'" name)
                      name (concat name ".org"))))
    (when (> (string-bytes filename) 255)
      (user-error "Notebook filename is too long"))
    filename))

(defun bloom-library-create-notebook (folder-id name)
  "Create NAME under FOLDER-ID and return its new opaque notebook id."
  (let* ((directory (or (bloom-library-resolve-folder folder-id)
                        (signal 'ebp-org-unavailable nil)))
         (filename (bloom-library--notebook-filename name))
         (path (jetpacs-files-create (expand-file-name filename directory))))
    (bloom-library-reset-projections)
    (bloom-library-refresh)
    (bloom-library--notebook-id path)))

(defun bloom-library-rename-notebook (notebook-id name)
  "Rename NOTEBOOK-ID to single-segment NAME and return its new id."
  (let* ((source (or (bloom-library-resolve-notebook notebook-id)
                     (signal 'ebp-org-unresolved nil)))
         (filename (bloom-library--notebook-filename name))
         (target (jetpacs-files-rename
                  source (expand-file-name filename
                                           (file-name-directory source))))
         (new-id (bloom-library--notebook-id target)))
    (bloom-library--migrate-favorite notebook-id new-id)
    (bloom-library-reset-projections)
    (bloom-library-refresh)
    new-id))

(defun bloom-library-move-notebook (notebook-id folder-id)
  "Move NOTEBOOK-ID under FOLDER-ID and return its new opaque id."
  (let* ((source (or (bloom-library-resolve-notebook notebook-id)
                     (signal 'ebp-org-unresolved nil)))
         (directory (or (bloom-library-resolve-folder folder-id)
                        (signal 'ebp-org-unavailable nil)))
         (target (jetpacs-files-move source directory))
         (new-id (bloom-library--notebook-id target)))
    (bloom-library--migrate-favorite notebook-id new-id)
    (bloom-library-reset-projections)
    (bloom-library-refresh)
    new-id))

(defun bloom-library-trash-notebook (notebook-id)
  "Move NOTEBOOK-ID to the recoverable platform trash."
  (let ((path (or (bloom-library-resolve-notebook notebook-id)
                  (signal 'ebp-org-unresolved nil))))
    (jetpacs-files-trash path)
    (setq bloom-library-favorite-ids
          (delete notebook-id (copy-sequence bloom-library-favorite-ids)))
    (condition-case err
        (jetpacs-settings-save-variable
         'bloom-library-favorite-ids bloom-library-favorite-ids)
      (error
       (message "bloom-library: favorite cleanup persistence failed (%s)"
                (car err))))
    (bloom-library-reset-projections)
    (bloom-library-refresh)
    t))

(defun bloom-library-reset-projections ()
  "Forget derived outline, document, heading, and search projections."
  (clrhash bloom-library--heading-refs)
  (clrhash bloom-library--outlines)
  (clrhash bloom-library--documents)
  (setq bloom-library--search-snapshot
        '(:query "" :mode "text" :results nil :error nil)))

(defun bloom-library-reset ()
  "Reset every private Bloom library map and snapshot."
  (clrhash bloom-library--notebook-paths)
  (clrhash bloom-library--folder-paths)
  (bloom-library-reset-projections)
  (setq bloom-library--snapshot
        '(:notebooks nil :folders nil :total-count 0 :truncated nil
                     :error nil))
  t)

(provide 'bloom-library)
;;; bloom-library.el ends here
