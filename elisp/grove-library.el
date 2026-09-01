;;; grove-library.el --- Grove notebook and document model -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Grove's private projection over the canonical Jetpacs Org scope.  Wire
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
(require 'jetpacs-org-reminders)
(require 'grove-core)

(defcustom grove-library-notebook-limit 300
  "Maximum Org files retained in Grove's notebook snapshot."
  :type '(integer 1 1000)
  :group 'grove)

(defcustom grove-library-tree-view t
  "Whether Notebooks initially shows folders instead of one flat file list."
  :type 'boolean
  :group 'grove)

(defcustom grove-library-favorite-ids nil
  "Opaque notebook ids pinned to the top of Grove's library."
  :type '(repeat string)
  :group 'grove)

(defcustom grove-reader-show-drawers nil
  "Whether Grove's Org reader initially expands drawers."
  :type 'boolean
  :group 'grove)

(defcustom grove-reader-default-mode 'read
  "Initial Grove document presentation, `read' or `edit'."
  :type '(choice (const read) (const edit))
  :group 'grove)

(defcustom grove-search-result-limit 100
  "Maximum canonical Org search results Grove retains."
  :type '(integer 1 512)
  :group 'grove)

(defvar grove-library-after-refresh-hook nil
  "Hook run after Grove replaces a library, outline, or search snapshot.")

(defvar grove-library--snapshot
  '(:notebooks nil :folders nil :total-count 0 :truncated nil :error nil)
  "Current presentation-safe notebook snapshot.")

(defvar grove-library--search-snapshot
  '(:query "" :mode "text" :results nil :error nil)
  "Current presentation-safe search snapshot.")

(defvar grove-library--notebook-paths (make-hash-table :test #'equal)
  "Opaque notebook id to canonical path.")

(defvar grove-library--folder-paths (make-hash-table :test #'equal)
  "Opaque folder id to canonical directory path.")

(defvar grove-library--heading-refs (make-hash-table :test #'equal)
  "Notebook/heading composite key to private Org ref.")

(defvar grove-library--outlines (make-hash-table :test #'equal)
  "Opaque notebook id to presentation-safe outline snapshot.")

(defvar grove-library--documents (make-hash-table :test #'equal)
  "Notebook/heading composite key to private document snapshot.")

(defun grove-library-snapshot ()
  "Return a defensive copy of Grove's notebook snapshot."
  (copy-tree grove-library--snapshot))

(defun grove-library-search-snapshot ()
  "Return a defensive copy of Grove's search snapshot."
  (copy-tree grove-library--search-snapshot))

(defun grove-library--opaque-id (prefix identity)
  "Return a stable opaque id using PREFIX and private IDENTITY."
  (concat prefix (substring (secure-hash 'sha256 identity) 0 32)))

(defun grove-library--notebook-id (path)
  "Return PATH's stable opaque notebook id."
  (grove-library--opaque-id "grove-notebook-" (file-truename path)))

(defun grove-library--folder-id (path)
  "Return PATH's stable opaque folder id."
  (grove-library--opaque-id
   "grove-folder-" (directory-file-name (file-truename path))))

(defun grove-library--heading-id (ref)
  "Return a stable opaque heading id for private Org REF."
  (let ((file (or (plist-get ref :file) ""))
        (id (plist-get ref :id)))
    (grove-library--opaque-id
     "grove-heading-"
     (if (and (stringp id) (not (string-empty-p id)))
         (format "%s|id:%s" file id)
       (format "%s|pos:%s|headline:%s" file
               (or (plist-get ref :pos) 0)
               (or (plist-get ref :headline) ""))))))

(defun grove-library-identify-heading (ref)
  "Retain private Org REF and return its opaque Grove coordinates.
The return value is `(:notebook-id ID :heading-id ID)'.  REF and its path
remain only in Grove's Emacs-side map; nil means the ref is outside the
canonical notebook library or cannot currently be resolved."
  (when-let* ((ref (and (listp ref) (copy-tree ref)))
              (file (plist-get ref :file))
              (canonical (ebp-org-file-allowed-p file)))
    (plist-put ref :file canonical)
    (let ((notebook-id (grove-library--notebook-id canonical))
          (heading-id (grove-library--heading-id ref)))
      (when (grove-library-resolve-notebook notebook-id)
        (puthash (grove-library--heading-key notebook-id heading-id)
                 ref grove-library--heading-refs)
        (list :notebook-id notebook-id :heading-id heading-id)))))

(defun grove-library--heading-key (notebook-id heading-id)
  "Return the private map key for NOTEBOOK-ID and HEADING-ID."
  (concat notebook-id "|" heading-id))

(defun grove-library--org-root ()
  "Return Grove's preferred canonical Org root, or nil."
  (let ((candidate (and (boundp 'org-directory)
                        (stringp org-directory)
                        (expand-file-name org-directory))))
    (when (and candidate (file-directory-p candidate)
               (not (file-remote-p candidate)))
      (file-name-as-directory (file-truename candidate)))))

(defun grove-library--display-folder (directory root)
  "Return a safe display label for DIRECTORY relative to ROOT."
  (let ((relative
         (and root (file-in-directory-p directory root)
              (file-relative-name directory root))))
    (cond
     ((or (equal relative "./") (equal relative ".")) "Notebooks")
     (relative (directory-file-name relative))
     (t (file-name-nondirectory (directory-file-name directory))))))

(defun grove-library--directory-chain (directory root)
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

(defun grove-library--folder-records (directories root)
  "Return safe folder records for canonical DIRECTORIES relative to ROOT."
  (let ((known (make-hash-table :test #'equal))
        records)
    (dolist (directory directories)
      (puthash (directory-file-name directory) t known))
    (dolist (directory directories)
      (let* ((clean (directory-file-name directory))
             (id (grove-library--folder-id clean))
             (parent (directory-file-name (file-name-directory clean)))
             (root-clean (and root (directory-file-name root)))
             (parent-id (and (not (equal clean root-clean))
                             (gethash parent known)
                             (not (equal parent root-clean))
                             (grove-library--folder-id parent))))
        (unless (and root-clean (equal clean root-clean))
          (puthash id (file-name-as-directory clean)
                   grove-library--folder-paths)
          (push (list :id id :parent-id parent-id
                      :label (file-name-nondirectory clean)
                      :path-label (grove-library--display-folder clean root))
                records))))
    (sort records
          (lambda (left right)
            (string-lessp (downcase (plist-get left :path-label))
                          (downcase (plist-get right :path-label)))))))

(defun grove-library--notebook-record (id)
  "Return the safe notebook record for ID, or nil."
  (cl-find id (plist-get grove-library--snapshot :notebooks)
           :key (lambda (record) (plist-get record :id)) :test #'equal))

(defun grove-library-refresh ()
  "Refresh Grove's bounded notebook snapshot from the canonical Org scope."
  (interactive)
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
             (total (length scope))
             (files (seq-take scope grove-library-notebook-limit))
             (root (grove-library--org-root))
             (directories
              (sort
               (delete-dups
                (apply #'append
                       (mapcar
                        (lambda (file)
                          (grove-library--directory-chain
                           (file-name-directory file) root))
                        files)))
               #'string-lessp))
             records)
        (clrhash grove-library--notebook-paths)
        (clrhash grove-library--folder-paths)
        (dolist (file files)
          (let* ((id (grove-library--notebook-id file))
                 (directory (file-name-directory file))
                 (folder-id
                  (unless (and root (file-equal-p directory root))
                    (grove-library--folder-id directory))))
            (puthash id file grove-library--notebook-paths)
            (push (list :id id :folder-id folder-id
                        :label (file-name-base file)
                        :file-label (file-name-nondirectory file)
                        :folder-label
                        (grove-library--display-folder directory root)
                        :favorite (and (member id grove-library-favorite-ids) t))
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
        (setq grove-library--snapshot
              (list :notebooks records
                    :folders (grove-library--folder-records directories root)
                    :total-count total
                    :truncated (> total (length files))
                    :error nil))
        (run-hooks 'grove-library-after-refresh-hook)
        t)
    (error
     (message "grove-library: refresh failed (%s)" (car err))
     (setq grove-library--snapshot
           (plist-put (copy-tree grove-library--snapshot)
                      :error "Notebook library unavailable"))
     (run-hooks 'grove-library-after-refresh-hook)
     nil)))

(defun grove-library-resolve-notebook (notebook-id)
  "Return NOTEBOOK-ID's canonical path, rebuilding the map once if needed."
  (when (stringp notebook-id)
    (or (gethash notebook-id grove-library--notebook-paths)
        (and (grove-library-refresh)
             (gethash notebook-id grove-library--notebook-paths)))))

(defun grove-library-resolve-folder (folder-id)
  "Return FOLDER-ID's canonical path; nil identifies the Org root."
  (if (null folder-id)
      (or (grove-library--org-root)
          (when-let* ((file (car (sort
                                  (copy-sequence
                                   (hash-table-values
                                    grove-library--notebook-paths))
                                  #'string-lessp))))
            (file-name-directory file)))
    (or (gethash folder-id grove-library--folder-paths)
        (and (grove-library-refresh)
             (gethash folder-id grove-library--folder-paths)))))

(defun grove-library-notebooks-in-folder (&optional folder-id flat)
  "Return safe notebooks directly in FOLDER-ID, or every notebook when FLAT."
  (let ((records (plist-get grove-library--snapshot :notebooks)))
    (if flat records
      (seq-filter (lambda (record)
                    (equal (plist-get record :folder-id) folder-id))
                  records))))

(defun grove-library-folders-in-folder (&optional folder-id)
  "Return safe folders directly below FOLDER-ID."
  (seq-filter (lambda (record)
                (equal (plist-get record :parent-id) folder-id))
              (plist-get grove-library--snapshot :folders)))

(defun grove-library--forget-heading-state (notebook-id)
  "Forget private heading and document state owned by NOTEBOOK-ID."
  (let ((prefix (concat notebook-id "|"))
        dead-refs dead-documents)
    (maphash (lambda (key _value)
               (when (string-prefix-p prefix key) (push key dead-refs)))
             grove-library--heading-refs)
    (maphash (lambda (key _value)
               (when (string-prefix-p prefix key)
                 (push key dead-documents)))
             grove-library--documents)
    (dolist (key dead-refs) (remhash key grove-library--heading-refs))
    (dolist (key dead-documents) (remhash key grove-library--documents))
    (remhash notebook-id grove-library--outlines)))

(defun grove-library--scan-outline (notebook-id)
  "Scan NOTEBOOK-ID and return a safe nested-outline snapshot."
  (let ((file (or (grove-library-resolve-notebook notebook-id)
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
              records)
         (dolist (record raw)
           (goto-char (plist-get record :pos))
           (let* ((ref (ebp-org-ref-at-point))
                  (heading-id (grove-library--heading-id ref))
                  (level (plist-get record :level))
                  (revision
                   (secure-hash
                    'sha256 (current-buffer) (point)
                    (save-excursion (org-end-of-subtree t t) (point)))))
             (while (and stack (>= (caar stack) level)) (pop stack))
             (let ((parent-id (cdar stack)))
               (puthash (grove-library--heading-key notebook-id heading-id)
                        ref grove-library--heading-refs)
               (push (list :id heading-id :parent-id parent-id :level level
                           :title (plist-get record :title)
                           :todo (plist-get record :todo)
                           :priority (plist-get record :priority)
                           :tags (copy-sequence (plist-get record :tags))
                           :deadline (plist-get record :deadline)
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
               :label (plist-get (grove-library--notebook-record notebook-id)
                                 :label)
               :preface (truncate-string-to-width preface 8192 nil nil t)
               :records (nreverse records)
               :buffer name
               :truncated (= (length raw) ebp-org-outline-max-headings)))))))

(defun grove-library-outline-refresh (notebook-id)
  "Refresh and return NOTEBOOK-ID's safe outline snapshot."
  (grove-library--forget-heading-state notebook-id)
  (let ((snapshot (grove-library--scan-outline notebook-id)))
    (puthash notebook-id snapshot grove-library--outlines)
    (run-hooks 'grove-library-after-refresh-hook)
    (copy-tree snapshot)))

(defun grove-library-outline-snapshot (notebook-id)
  "Return NOTEBOOK-ID's safe outline snapshot, producing it if absent."
  (copy-tree (or (gethash notebook-id grove-library--outlines)
                 (grove-library-outline-refresh notebook-id))))

(defun grove-library-resolve-heading (notebook-id heading-id)
  "Return private Org ref for NOTEBOOK-ID and HEADING-ID, rebuilding once."
  (when (and (stringp notebook-id) (stringp heading-id))
    (let ((key (grove-library--heading-key notebook-id heading-id)))
      (or (copy-tree (gethash key grove-library--heading-refs))
          (and (ignore-errors (grove-library-outline-refresh notebook-id))
               (copy-tree (gethash key grove-library--heading-refs)))))))

(defun grove-library--document-info (notebook-id heading-id)
  "Return private document information for NOTEBOOK-ID and HEADING-ID."
  (let ((ref (or (grove-library-resolve-heading notebook-id heading-id)
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
                     :beg beg :end end :content content
                     :revision (secure-hash 'sha256 content)
                     :ref ref))))
        (set-marker marker nil)))))

(defun grove-library-document-refresh (notebook-id heading-id)
  "Refresh and return NOTEBOOK-ID and HEADING-ID's private snapshot."
  (let ((snapshot (grove-library--document-info notebook-id heading-id)))
    (puthash (grove-library--heading-key notebook-id heading-id)
             snapshot grove-library--documents)
    (run-hooks 'grove-library-after-refresh-hook)
    (copy-tree snapshot)))

(defun grove-library-document-snapshot (notebook-id heading-id)
  "Return NOTEBOOK-ID and HEADING-ID's snapshot, producing it if absent."
  (copy-tree
   (or (gethash (grove-library--heading-key notebook-id heading-id)
                grove-library--documents)
       (grove-library-document-refresh notebook-id heading-id))))

(defun grove-library--search-results (raw)
  "Convert canonical Org search RAW rows into Grove-safe result records."
  (unless (plist-get grove-library--snapshot :notebooks)
    (grove-library-refresh))
  (let (results)
    (dolist (row raw)
      (let* ((ref (copy-tree (alist-get 'ref row)))
             (file (plist-get ref :file))
             (notebook-id (and file (grove-library--notebook-id file)))
             (notebook (and notebook-id
                            (grove-library--notebook-record notebook-id))))
        (when notebook
          (let ((heading-id (grove-library--heading-id ref)))
            (puthash (grove-library--heading-key notebook-id heading-id)
                     ref grove-library--heading-refs)
            (push (list :notebook-id notebook-id :heading-id heading-id
                        :headline (or (alist-get 'headline row) "Untitled")
                        :todo (alist-get 'todo row)
                        :priority (alist-get 'priority row)
                        :tags (append (alist-get 'tags row) nil)
                        :notebook-label (plist-get notebook :label))
                  results)))))
    (nreverse results)))

(defun grove-library--search-view-members (view)
  "Return presentation-safe active-view members from optional VIEW."
  (when view
    (list :view-id (plist-get view :id)
          :view-label (plist-get view :label)
          :view-source (plist-get view :source)
          :view-kind (plist-get view :kind))))

(defun grove-library-search (query mode &optional view)
  "Replace Grove's search snapshot using QUERY and MODE.
MODE is the string \"text\" or \"match\".  Optional VIEW identifies a saved
view that invoked the query.  A failed expression retains the last successful
result rows and records a safe validation error."
  (unless (and (stringp query) (member mode '("text" "match")))
    (user-error "Search requires text or match mode and a string query"))
  (if (string-blank-p query)
      (progn
        (setq grove-library--search-snapshot
              (list :query "" :mode mode :results nil :error nil))
        (run-hooks 'grove-library-after-refresh-hook)
        t)
    (condition-case err
        (let ((raw (jetpacs-org-mode-search-items
                    query (if (equal mode "match") 'match 'text)
                    grove-search-result-limit)))
          (setq grove-library--search-snapshot
                (append
                 (list :query query :mode mode
                       :results (grove-library--search-results raw)
                       :error nil)
                 (grove-library--search-view-members view)))
          (run-hooks 'grove-library-after-refresh-hook)
          t)
      (error
       (message "grove-library: search failed (%s)" (car err))
       (setq grove-library--search-snapshot
             (append
              (list :query query :mode mode
                    :results
                    (plist-get grove-library--search-snapshot :results)
                    :error "Invalid search expression")
              (grove-library--search-view-members view)))
       (run-hooks 'grove-library-after-refresh-hook)
       nil))))

(defun grove-library-custom-view-search (view-id label kind)
  "Run opaque Org custom VIEW-ID, displaying LABEL and KIND.
Native Org settings and ordering come from Jetpacs' reusable custom-view
projection.  Failure retains the last successful rows behind a safe error."
  (unless (and (stringp view-id) (stringp label) (stringp kind))
    (user-error "Custom view identity is invalid"))
  (condition-case err
      (let ((raw (jetpacs-org-mode-custom-view-items
                  view-id grove-search-result-limit)))
        (setq grove-library--search-snapshot
              (list :query "" :mode "text"
                    :results (grove-library--search-results raw)
                    :error nil :view-id view-id :view-label label
                    :view-source "emacs" :view-kind kind))
        (run-hooks 'grove-library-after-refresh-hook)
        t)
    (ebp-org-unresolved (signal (car err) (cdr err)))
    (error
     (message "grove-library: custom view failed (%s)" (car err))
     (setq grove-library--search-snapshot
           (list :query "" :mode "text"
                 :results (plist-get grove-library--search-snapshot :results)
                 :error "Saved view is unavailable"
                 :view-id view-id :view-label label
                 :view-source "emacs" :view-kind kind))
     (run-hooks 'grove-library-after-refresh-hook)
     nil)))

(defun grove-library-search-set-mode (mode)
  "Set Grove search MODE and clear any saved-view attribution."
  (unless (member mode '("text" "match"))
    (user-error "Search mode must be text or match"))
  (setq grove-library--search-snapshot
        (list :query (or (plist-get grove-library--search-snapshot :query) "")
              :mode mode
              :results (plist-get grove-library--search-snapshot :results)
              :error (plist-get grove-library--search-snapshot :error)))
  (run-hooks 'grove-library-after-refresh-hook)
  t)

(defun grove-library-clear-active-view (view-id)
  "Clear saved-view attribution when it currently names VIEW-ID."
  (when (equal view-id (plist-get grove-library--search-snapshot :view-id))
    (setq grove-library--search-snapshot
          (list :query (or (plist-get grove-library--search-snapshot :query) "")
                :mode (or (plist-get grove-library--search-snapshot :mode)
                          "text")
                :results (plist-get grove-library--search-snapshot :results)
                :error (plist-get grove-library--search-snapshot :error)))
    (run-hooks 'grove-library-after-refresh-hook))
  t)

(defun grove-library-toggle-favorite (notebook-id)
  "Persistently toggle NOTEBOOK-ID in Grove's Customize-backed favorites."
  (unless (grove-library-resolve-notebook notebook-id)
    (signal 'ebp-org-unresolved nil))
  (let* ((old (copy-sequence grove-library-favorite-ids))
         (next (if (member notebook-id old)
                   (delete notebook-id (copy-sequence old))
                 (cons notebook-id old))))
    (condition-case err
        (progn
          (setq grove-library-favorite-ids next)
          (customize-save-variable 'grove-library-favorite-ids next)
          (grove-library-refresh)
          (member notebook-id next))
      (error
       (setq grove-library-favorite-ids old)
       (signal (car err) (cdr err))))))

(defun grove-library--migrate-favorite (old-id new-id)
  "Replace favorite OLD-ID with NEW-ID and persist the new list."
  (when (member old-id grove-library-favorite-ids)
    (setq grove-library-favorite-ids
          (cons new-id
                (delete old-id
                        (copy-sequence grove-library-favorite-ids))))
    ;; The file move/rename has already completed.  A broken optional
    ;; Customize destination must not revise that durable verdict.
    (condition-case err
        (customize-save-variable
         'grove-library-favorite-ids grove-library-favorite-ids)
      (error
       (message "grove-library: favorite migration persistence failed (%s)"
                (car err))))))

(defun grove-library--notebook-filename (name)
  "Validate NAME and return its single-segment Org filename."
  (unless (and (stringp name)
               (not (string-blank-p name))
               (equal name (file-name-nondirectory name))
               (not (member name '("." "..")))
               (not (string-match-p "[[:cntrl:]]" name)))
    (user-error "Notebook name must be one printable path segment"))
  (let ((filename (if (string-match-p "\\.org\\'" name)
                      name (concat name ".org"))))
    (when (> (string-bytes filename) 255)
      (user-error "Notebook filename is too long"))
    filename))

(defun grove-library-create-notebook (folder-id name)
  "Create NAME under FOLDER-ID and return its new opaque notebook id."
  (let* ((directory (or (grove-library-resolve-folder folder-id)
                        (signal 'ebp-org-unavailable nil)))
         (filename (grove-library--notebook-filename name))
         (path (jetpacs-files-create (expand-file-name filename directory))))
    (grove-library-reset-projections)
    (grove-library-refresh)
    (grove-library--notebook-id path)))

(defun grove-library-rename-notebook (notebook-id name)
  "Rename NOTEBOOK-ID to single-segment NAME and return its new id."
  (let* ((source (or (grove-library-resolve-notebook notebook-id)
                     (signal 'ebp-org-unresolved nil)))
         (filename (grove-library--notebook-filename name))
         (target (jetpacs-files-rename
                  source (expand-file-name filename
                                           (file-name-directory source))))
         (new-id (grove-library--notebook-id target)))
    (grove-library--migrate-favorite notebook-id new-id)
    (grove-library-reset-projections)
    (grove-library-refresh)
    new-id))

(defun grove-library-move-notebook (notebook-id folder-id)
  "Move NOTEBOOK-ID under FOLDER-ID and return its new opaque id."
  (let* ((source (or (grove-library-resolve-notebook notebook-id)
                     (signal 'ebp-org-unresolved nil)))
         (directory (or (grove-library-resolve-folder folder-id)
                        (signal 'ebp-org-unavailable nil)))
         (target (jetpacs-files-move source directory))
         (new-id (grove-library--notebook-id target)))
    (grove-library--migrate-favorite notebook-id new-id)
    (grove-library-reset-projections)
    (grove-library-refresh)
    new-id))

(defun grove-library-trash-notebook (notebook-id)
  "Move NOTEBOOK-ID to the recoverable platform trash."
  (let ((path (or (grove-library-resolve-notebook notebook-id)
                  (signal 'ebp-org-unresolved nil))))
    (jetpacs-files-trash path)
    (setq grove-library-favorite-ids
          (delete notebook-id (copy-sequence grove-library-favorite-ids)))
    (condition-case err
        (customize-save-variable
         'grove-library-favorite-ids grove-library-favorite-ids)
      (error
       (message "grove-library: favorite cleanup persistence failed (%s)"
                (car err))))
    (grove-library-reset-projections)
    (grove-library-refresh)
    t))

(defun grove-library-reset-projections ()
  "Forget derived outline, document, heading, and search projections."
  (clrhash grove-library--heading-refs)
  (clrhash grove-library--outlines)
  (clrhash grove-library--documents)
  (setq grove-library--search-snapshot
        '(:query "" :mode "text" :results nil :error nil)))

(defun grove-library-reset ()
  "Reset every private Grove library map and snapshot."
  (clrhash grove-library--notebook-paths)
  (clrhash grove-library--folder-paths)
  (grove-library-reset-projections)
  (setq grove-library--snapshot
        '(:notebooks nil :folders nil :total-count 0 :truncated nil
                     :error nil))
  t)

(provide 'grove-library)
;;; grove-library.el ends here
