;;; grove-test.el --- ERT for the Grove Jetpacs applet -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'grove)

(defconst grove-test--now (encode-time 0 30 9 1 9 2026)
  "Stable local time used by Grove snapshot tests.")

(defun grove-test--ref (file pos headline &optional id)
  "Return a private Org reference for FILE, POS, HEADLINE, and optional ID."
  (list :id id :file file :pos pos :headline headline))

(defun grove-test--agenda-item
    (file pos headline date &optional time type todo priority tags id)
  "Return one canonical agenda fixture row."
  `((headline . ,headline)
    (todo . ,todo)
    (priority . ,priority)
    (tags . ,(vconcat tags))
    (file . ,file)
    (pos . ,pos)
    (time . ,time)
    (date . ,date)
    (type . ,type)
    (extra . nil)
    (ts-date . nil)
    (ref . ,(grove-test--ref file pos headline id))))

(defun grove-test--snapshot ()
  "Return a populated explicit snapshot without private paths."
  '(:day-label "Tuesday" :date-label "September 1"
               :updated-label "Updated 09:30" :today-count 1 :total-count 2
               :error nil
               :items
               ((:id "grove-item-a" :headline "Write project brief" :todo "TODO"
                     :priority "A" :tags ("work") :file-label "work.org" :time "09:00"
                     :date "2026-09-01" :type "scheduled" :section "TODAY")
                (:id "grove-item-b" :headline "Book the train" :todo "NEXT"
                     :priority nil :tags nil :file-label "travel.org" :time nil
                     :date "2026-09-02" :type "deadline" :section "TOMORROW"))))

(ert-deftest grove-org-refresh-builds-stable-private-snapshot ()
  "The producer deduplicates, sorts, and keeps Org refs out of rendered IR."
  (let* ((file "/tmp/private-vault/work.org")
         (today (grove-test--agenda-item
                 file 20 "Write project brief" "2026-09-01"
                 " 9:00 " "scheduled" "TODO" "A" '("work") "task-1"))
         (duplicate (copy-tree today))
         (tomorrow (grove-test--agenda-item
                    file 50 "Book the train" "2026-09-02"
                    nil "deadline" "NEXT" nil nil nil))
         first-id second-id)
    (let ((grove-org--snapshot (copy-tree grove-org--snapshot)))
      (cl-letf (((symbol-function 'jetpacs-org-mode-agenda-items)
                 (lambda (&rest _) (list tomorrow duplicate today))))
        (should (grove-org-refresh grove-test--now))
        (let ((snapshot (grove-org-snapshot)))
          (should (= (plist-get snapshot :total-count) 2))
          (should (= (plist-get snapshot :today-count) 1))
          (should (equal (mapcar (lambda (row) (plist-get row :section))
                                 (plist-get snapshot :items))
                         '("TODAY" "TOMORROW")))
          (setq first-id (plist-get (car (plist-get snapshot :items)) :id)))
        (should (grove-org-refresh grove-test--now))
        (setq second-id
              (plist-get (car (plist-get (grove-org-snapshot) :items)) :id))
        (should (equal first-id second-id))
        (should-not
         (string-search
          "/tmp/private-vault"
          (jetpacs-node->canonical-json (grove-ui-screen nil))))))))

(ert-deftest grove-agenda-builder-is-repeatable-over-explicit-state ()
  "The same snapshot and range produce byte-identical canonical screens."
  (let ((grove-org--snapshot (grove-test--snapshot))
        (grove-agenda-range "today"))
    (should
     (equal (jetpacs-node->canonical-json (grove-ui-screen nil))
            (jetpacs-node->canonical-json (grove-ui-screen nil))))))

(ert-deftest grove-agenda-row-uses-reveal-first-deep-commit-swipe ()
  "A normal swipe reveals two actions; only the deeper gesture commits Done."
  (cl-letf (((symbol-function 'jetpacs-feature-advertised-p)
             (lambda (&rest _) t)))
    (let* ((item (car (plist-get (grove-test--snapshot) :items)))
           (row (grove-ui-agenda-row item nil t t))
           (side (plist-get row :swipe_end))
           (actions (plist-get side :actions)))
      (should (eq (plist-get side :commit) t))
      (should (= (length actions) 2))
      (should (equal (plist-get (aref actions 0) :label) "Done"))
      (should (equal (plist-get
                      (plist-get (aref actions 0) :on_trigger) :action)
                     "grove.agenda.done"))
      (should (equal (plist-get (aref actions 1) :label) "Open")))))

(ert-deftest grove-widgets-are-live-profile-valid-snapshot-projections ()
  "Capture and Agenda widgets stay inside the negotiated Glance vocabulary."
  (let ((grove-org--snapshot (grove-test--snapshot)))
    (should (jetpacs-check-profile (grove-widgets-capture-surface) 'widget))
    (should (jetpacs-check-profile (grove-widgets-agenda-surface) 'widget))
    (should (jetpacs-check-profile
             (grove-widgets-agenda-stale-surface) 'widget))
    ;; Rich swipe is an application-only interaction.  Glance rows expose
    ;; the same durable actions through their tap targets without advertising
    ;; a renderer node the widget profile cannot honor.
    (dolist (surface (list (grove-widgets-agenda-surface)
                           (grove-widgets-agenda-stale-surface)))
      (should-not
       (string-search "\"swipe_end\""
                      (jetpacs-node->canonical-json surface))))
    (let* ((capture (grove-widgets-capture-surface))
           (tap (plist-get (plist-get capture :body) :on_tap)))
      (should (equal (plist-get tap :action) "org.capture.show"))
      (should (equal (plist-get tap :when_offline) "wake"))
      (should (= (plist-get tap :ttl_s) 86400)))))

(ert-deftest grove-item-actions-resolve-current-opaque-identities ()
  "Open and Done resolve private refs in Emacs and reject vanished rows."
  (let* ((ref (grove-test--ref "/tmp/work.org" 1
                               "Write project brief" "task-1"))
         opened toggled notified refreshed continuation)
    (cl-letf (((symbol-function 'grove-org-ref-for-id)
               (lambda (id) (and (equal id "grove-item-a") ref)))
              ((symbol-function 'grove-library--notebook-id)
               (lambda (_file) "grove-notebook-a"))
              ((symbol-function 'grove-library--heading-id)
               (lambda (_ref) "grove-heading-a"))
              ((symbol-function 'grove-library-resolve-notebook)
               (lambda (_id) "/tmp/work.org"))
              ((symbol-function 'grove-document-open)
               (lambda (&rest args) (setq opened args) 'accepted))
              ((symbol-function 'jetpacs-flow-continue)
               (lambda (fn) (setq continuation fn)))
              ((symbol-function 'ebp-org-toggle-todo)
               (lambda (&rest args) (setq toggled args)))
              ((symbol-function 'grove-org-refresh)
               (lambda (&optional _now) (setq refreshed t)))
              ((symbol-function 'jetpacs-shell-notify)
               (lambda (&rest args) (setq notified args))))
      (should (eq (grove--on-item-open
                   '(:item-id "missing") '(:surface "widget:grove.agenda"))
                  'stale))
      (should (eq (grove--on-item-open
                   '(:item-id "grove-item-a")
                   '(:surface "widget:grove.agenda"))
                  'accepted))
      (should
       (equal opened
              '("grove-notebook-a" "grove-heading-a"
                (:surface "widget:grove.agenda"))))
      (should-not continuation)
      (should (eq (grove--on-item-done
                   '(:item-id "grove-item-a")
                   '(:surface "app:grove"))
                  'accepted))
      (should (equal toggled (list ref 'grove 'done)))
      (should refreshed)
      (should (equal notified '("Completed" "app:grove"))))))

(ert-deftest grove-registration-is-symmetric ()
  "Register and unregister cover the app, actions, and both widget roots."
  (unwind-protect
      (progn
        (grove-register)
        (should (assoc grove-owner jetpacs-apps--registry))
        (dolist (name (append '("grove.agenda.refresh" "grove.agenda.select"
                                "grove.agenda.item.open" "grove.agenda.done")
                              grove-navigation--verbs
                              grove-structure--verbs
                              grove-document--verbs))
          (should (gethash name jetpacs-action-handlers)))
        (should (assoc grove-capture-widget-surface jetpacs-shell--roots))
        (should (assoc grove-agenda-widget-surface jetpacs-shell--roots))
        (grove-unregister)
        (should-not (assoc grove-owner jetpacs-apps--registry))
        (should-not (assoc grove-capture-widget-surface jetpacs-shell--roots))
        (should-not (assoc grove-agenda-widget-surface jetpacs-shell--roots))
        (dolist (name (append '("grove.agenda.refresh" "grove.agenda.select"
                                "grove.agenda.item.open" "grove.agenda.done")
                              grove-navigation--verbs
                              grove-structure--verbs
                              grove-document--verbs))
          (should-not (gethash name jetpacs-action-handlers))))
    (grove-register)))

(ert-deftest grove-library-refresh-builds-a-private-nested-tree ()
  "Nested notebook paths become opaque, restart-rebuildable tree records."
  (should (equal (grove-library--notebook-filename "journal")
                 "journal.org"))
  (should-error (grove-library--notebook-filename "../escape")
                :type 'user-error)
  (should-error (grove-library--notebook-filename "bad\nname")
                :type 'user-error)
  (let* ((root (make-temp-file "grove-library" t))
         (projects (expand-file-name "projects" root))
         (client (expand-file-name "client" projects))
         (first (expand-file-name "inbox.org" root))
         (second (expand-file-name "work.org" client))
         (org-directory root)
         (ebp-org-roots (list root))
         (grove-library--snapshot
          '(:notebooks nil :folders nil :total-count 0 :truncated nil
                       :error nil))
         (grove-library--notebook-paths (make-hash-table :test #'equal))
         (grove-library--folder-paths (make-hash-table :test #'equal))
         (grove-library--heading-refs (make-hash-table :test #'equal))
         (grove-library--outlines (make-hash-table :test #'equal))
         (grove-library--documents (make-hash-table :test #'equal))
         (grove-library-favorite-ids nil))
    (unwind-protect
        (progn
          (make-directory client t)
          (write-region "* Inbox\n" nil first nil 'silent)
          (write-region "* Work\n" nil second nil 'silent)
          (cl-letf (((symbol-function 'jetpacs-org-mode-agenda-scope)
                     (lambda () (list second first second))))
            (should (grove-library-refresh))
            (let* ((snapshot (grove-library-snapshot))
                   (notebooks (plist-get snapshot :notebooks))
                   (folders (plist-get snapshot :folders))
                   (first-id (plist-get
                              (cl-find "inbox" notebooks
                                       :key (lambda (row)
                                              (plist-get row :label))
                                       :test #'equal)
                              :id))
                   (project-row
                    (cl-find "projects" folders
                             :key (lambda (row) (plist-get row :label))
                             :test #'equal))
                   (client-row
                    (cl-find "client" folders
                             :key (lambda (row) (plist-get row :label))
                             :test #'equal)))
              (should (= (plist-get snapshot :total-count) 2))
              (should (= (length notebooks) 2))
              (should project-row)
              (should client-row)
              (should (equal (plist-get client-row :parent-id)
                             (plist-get project-row :id)))
              (should-not
               (string-search root
                              (jetpacs-node->canonical-json
                               (grove-navigation-notebooks-screen nil))))
              ;; Simulate Emacs process-state loss.  The opaque id resolves
              ;; by rebuilding the canonical scope, not by trusting wire data.
              (clrhash grove-library--notebook-paths)
              (should (file-equal-p
                       (grove-library-resolve-notebook first-id) first))
              (let ((ids (mapcar (lambda (row) (plist-get row :id))
                                 notebooks)))
                (should (grove-library-refresh))
                (should (equal ids
                               (mapcar
                                (lambda (row) (plist-get row :id))
                                (plist-get (grove-library-snapshot)
                                           :notebooks))))))))
      (dolist (buffer (buffer-list))
        (when-let* ((file (buffer-local-value 'buffer-file-name buffer))
                    ((file-in-directory-p file root)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory root t))))

(ert-deftest grove-outline-preserves-hierarchy-and-rebuilds-heading-refs ()
  "The native outline is nested and opaque heading ids survive state loss."
  (let* ((root (make-temp-file "grove-outline" t))
         (file (expand-file-name "garden.org" root))
         (org-directory root)
         (org-agenda-files (list file))
         (ebp-org-roots (list root))
         (grove-library--snapshot
          '(:notebooks nil :folders nil :total-count 0 :truncated nil
                       :error nil))
         (grove-library--notebook-paths (make-hash-table :test #'equal))
         (grove-library--folder-paths (make-hash-table :test #'equal))
         (grove-library--heading-refs (make-hash-table :test #'equal))
         (grove-library--outlines (make-hash-table :test #'equal))
         (grove-library--documents (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (write-region
           "#+title: Garden\nA short preface.\n* TODO Parent\n:PROPERTIES:\n:ID: parent-id\n:END:\nParent body.\n** TODO Child\nChild body.\n* Sibling\n"
           nil file nil 'silent)
          (cl-letf (((symbol-function 'jetpacs-org-mode-agenda-scope)
                     (lambda () (list file))))
            (should (grove-library-refresh))
            (let* ((notebook-id
                    (plist-get
                     (car (plist-get (grove-library-snapshot) :notebooks))
                     :id))
                   (outline (grove-library-outline-refresh notebook-id))
                   (records (plist-get outline :records))
                   (parent (cl-find "Parent" records
                                    :key (lambda (row)
                                           (plist-get row :title))
                                    :test #'equal))
                   (child (cl-find "Child" records
                                   :key (lambda (row)
                                          (plist-get row :title))
                                   :test #'equal))
                   (child-id (plist-get child :id)))
              (should (equal (plist-get child :parent-id)
                             (plist-get parent :id)))
              (should (string-search "short preface"
                                     (plist-get outline :preface)))
              (clrhash grove-library--heading-refs)
              (clrhash grove-library--outlines)
              (should (equal
                       (plist-get (grove-library-resolve-heading
                                   notebook-id child-id)
                                  :headline)
                       "Child"))
              (let ((grove-navigation--notebook-id notebook-id))
                (should-not
                 (string-search root
                                (jetpacs-node->canonical-json
                                 (grove-navigation-outline-screen nil)))))
              (let ((grove-document--current
                     (list :notebook-id notebook-id :heading-id child-id))
                    (grove-document--modes (make-hash-table :test #'equal)))
                (let ((reader (grove-document-screen nil)))
                  (should (jetpacs-check-profile reader 'app))
                  (should-not
                   (string-search root
                                  (jetpacs-node->canonical-json reader))))
                (puthash (grove-document--key notebook-id child-id)
                         'edit grove-document--modes)
                (should (jetpacs-check-profile
                         (grove-document-screen nil) 'app))))))
      (when-let* ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest grove-native-search-retains-results-on-expression-error ()
  "Search projects native Org rows and an invalid query cannot erase results."
  (let* ((root (make-temp-file "grove-search" t))
         (file (expand-file-name "notes.org" root))
         (ref (grove-test--ref file 1 "Alpha" "alpha-id"))
         (grove-library--snapshot
          '(:notebooks nil :folders nil :total-count 0 :truncated nil
                       :error nil))
         (grove-library--search-snapshot
          '(:query "" :mode "text" :results nil :error nil))
         (grove-library--notebook-paths (make-hash-table :test #'equal))
         (grove-library--folder-paths (make-hash-table :test #'equal))
         (grove-library--heading-refs (make-hash-table :test #'equal))
         (grove-library--outlines (make-hash-table :test #'equal))
         (grove-library--documents (make-hash-table :test #'equal))
         (org-directory root))
    (unwind-protect
        (progn
          (write-region "* TODO Alpha :work:\nNeedle.\n"
                        nil file nil 'silent)
          (cl-letf (((symbol-function 'jetpacs-org-mode-agenda-scope)
                     (lambda () (list file)))
                    ((symbol-function 'jetpacs-org-mode-search-items)
                     (lambda (&rest _)
                       `(((headline . "Alpha") (todo . "TODO")
                          (tags . ["work"]) (summary . "notes: Alpha")
                          (ref . ,ref))))))
            (should (grove-library-refresh))
            (should (grove-library-search "needle" "text"))
            (let* ((good (grove-library-search-snapshot))
                   (results (plist-get good :results)))
              (should (= (length results) 1))
              (should-not
               (string-search root
                              (jetpacs-node->canonical-json
                               (grove-navigation-search-screen nil))))
              (cl-letf (((symbol-function 'jetpacs-org-mode-search-items)
                         (lambda (&rest _) (error "bad private expression"))))
                (should-not (grove-library-search "bad" "match"))
                (let ((failed (grove-library-search-snapshot)))
                  (should (equal (plist-get failed :results) results))
                  (should (equal (plist-get failed :query) "bad"))
                  (should (equal (plist-get failed :mode) "match"))
                  (should (equal (plist-get failed :error)
                                 "Invalid search expression")))))))
      (delete-directory root t))))

(ert-deftest grove-document-subtree-save-rolls-back-pre-durability-failure ()
  "A failed prewrite restores the entire live Org buffer exactly."
  (let* ((file (make-temp-file "grove-document" nil ".org"
                               "* TODO Original\nBody.\n* Keep\nSafe.\n"))
         (buffer (find-file-noselect file t)))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (set-buffer-modified-p nil)
          (let* ((before (buffer-string))
                 (beg (point-min))
                 (end (save-excursion
                        (goto-char beg)
                        (org-end-of-subtree t t)
                        (point))))
            (cl-letf (((symbol-function 'jetpacs-editor-org-save-policy)
                       (lambda (_buffer)
                         (goto-char (point-max))
                         (insert "CORRUPT")
                         (error "prewrite failed"))))
              (should-error
               (grove-document--replace-subtree
                buffer beg end "* TODO Changed\nNew.\n"))
              (should (equal (buffer-string) before))
              (should-not (buffer-modified-p)))
            (cl-letf (((symbol-function 'jetpacs-editor-org-save-policy)
                       (lambda (_buffer) t)))
              (let ((new-ref (grove-document--replace-subtree
                              buffer beg end "* TODO Changed\nNew.\n")))
                (should (equal (plist-get new-ref :headline) "Changed"))
                (should (string-prefix-p "* TODO Changed\nNew.\n* Keep"
                                         (buffer-string)))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest grove-planning-commits-the-captured-pair-and-checks-revision ()
  "Planning forwards both fields atomically and rejects a stale document."
  (let* ((content "* TODO Alpha\n")
         (revision (secure-hash 'sha256 content))
         ;; A queued save must remain self-describing after Emacs loses the
         ;; transient planning canvas.
         (grove-document--planning nil)
         (ref (grove-test--ref "/tmp/alpha.org" 1 "Alpha" "alpha"))
         called refreshed continued)
    (let ((grove-document--planning
           (list :notebook-id "notebook" :heading-id "heading"
                 :revision revision :scheduled "" :deadline "")))
      (let ((screen (grove-document-planning-screen nil)))
        (should (jetpacs-check-profile screen 'app))
        (let ((json (jetpacs-node->canonical-json screen)))
          (should (string-search "notebook" json))
          (should (string-search revision json)))))
    (cl-letf (((symbol-function 'grove-library-resolve-heading)
               (lambda (&rest _) ref))
              ((symbol-function 'grove-document--live-subtree)
               (lambda (_ref) (list :content content)))
              ((symbol-function 'ebp-org-set-planning-pair)
               (lambda (&rest args) (setq called args)))
              ((symbol-function 'grove-document--after-mutation)
               (lambda (&rest _) (setq refreshed t)))
              ((symbol-function 'jetpacs-flow-continue)
               (lambda (fn) (setq continued fn))))
      (should
       (eq (grove-document--on-planning-save
            `(:notebook-id "notebook" :heading-id "heading"
                           :revision ,revision)
            '(:surface "app:grove"
                       :fields (:grove-scheduled "<2026-09-04 Fri 10:00 +1w>"
                                                 :grove-deadline "<2026-09-05 Sat>")))
           'accepted))
      (should (equal called
                     (list ref 'grove "<2026-09-04 Fri 10:00 +1w>"
                           "<2026-09-05 Sat>")))
      (should refreshed)
      (should (functionp continued))
      (should-not grove-document--planning))
    (let ((grove-document--planning
           (list :notebook-id "notebook" :heading-id "heading"
                 :revision revision :scheduled "" :deadline ""))
          called)
      (cl-letf (((symbol-function 'grove-library-resolve-heading)
                 (lambda (&rest _) ref))
                ((symbol-function 'grove-document--live-subtree)
                 (lambda (_ref) (list :content "* TODO Changed\n")))
                ((symbol-function 'ebp-org-set-planning-pair)
                 (lambda (&rest args) (setq called args))))
        (should
         (eq (grove-document--on-planning-save
              `(:notebook-id "notebook" :heading-id "heading"
                             :revision ,revision)
              nil)
             'stale))
        (should-not called)))))

(ert-deftest grove-notebooks-use-reveal-only-swipe-and-folder-local-builders ()
  "Notebook swipes never commit and each stacked folder keeps its own model."
  (let* ((folder-id "grove-folder-projects")
         (grove-library-tree-view t)
         (grove-library--snapshot
          `(:notebooks
            ((:id "root-note" :folder-id nil :label "Inbox"
                  :file-label "inbox.org" :folder-label "Notebooks"
                  :favorite nil)
             (:id "child-note" :folder-id ,folder-id :label "Client"
                  :file-label "client.org" :folder-label "projects"
                  :favorite t))
            :folders ((:id ,folder-id :parent-id nil :label "projects"
                           :path-label "projects"))
            :total-count 2 :truncated nil :error nil)))
    (cl-letf (((symbol-function 'jetpacs-feature-advertised-p)
               (lambda (&rest _) t)))
      (let* ((row (grove-navigation-notebook-row
                   (car (plist-get grove-library--snapshot :notebooks))))
             (swipe (plist-get row :swipe_end)))
        (should swipe)
        (should-not (plist-get swipe :commit)))
      (let* ((root-json
              (jetpacs-node->canonical-json
               (grove-navigation-notebooks-screen nil)))
             (child-json
              (jetpacs-node->canonical-json
               (grove-navigation--notebooks-screen-for-folder
                folder-id nil)))
             (root-again
              (jetpacs-node->canonical-json
               (grove-navigation-notebooks-screen nil))))
        (should (string-search "Inbox" root-json))
        (should-not (string-search "Client" root-json))
        (should (string-search "Client" child-json))
        (should-not (string-search "Inbox" child-json))
        (should (equal root-json root-again))))))

(ert-deftest grove-outline-rows-expose-safe-rich-structure-controls ()
  "Leading swipes reveal four actions without commit; only Done deep-commits."
  (let* ((grove-navigation--notebook-id "grove-notebook-test")
         (record '(:id "grove-heading-test" :parent-id nil :level 1
                       :title "Plan project" :todo "TODO" :done nil
                       :revision
                       "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
         (row (car (grove-navigation--outline-children (list record) nil)))
         (start (plist-get row :swipe_start))
         (end (plist-get row :swipe_end))
         (json (jetpacs-node->canonical-json row)))
    (should (= 4 (length (plist-get start :actions))))
    (should-not (plist-get start :commit))
    (should (equal (mapcar (lambda (action) (plist-get action :label))
                           (append (plist-get start :actions) nil))
                   '("Above" "Below" "Child" "Refile")))
    (should (eq (plist-get end :commit) t))
    (should (equal (mapcar (lambda (action) (plist-get action :label))
                           (append (plist-get end :actions) nil))
                   '("Done" "Plan")))
    (should (string-search "Insert above" json))
    (should (string-search "Paste under" json))
    (should (string-search "Narrow to subtree" json))
    (should (string-search "Archive this heading" json))
    (should-not (string-search ":file" json))
    (should-not (string-search "/home/" json))))

(ert-deftest grove-outline-mutation-is-revision-bound-and-undoable ()
  "A live opaque row can move and undo; a later local edit makes it stale."
  (let* ((root (make-temp-file "grove-structure" t))
         (file (expand-file-name "notes.org" root))
         (org-directory root)
         (org-agenda-files (list file))
         (ebp-org-roots (list root))
         (grove-library--snapshot
          '(:notebooks nil :folders nil :total-count 0 :truncated nil
                       :error nil))
         (grove-library--notebook-paths (make-hash-table :test #'equal))
         (grove-library--folder-paths (make-hash-table :test #'equal))
         (grove-library--heading-refs (make-hash-table :test #'equal))
         (grove-library--outlines (make-hash-table :test #'equal))
         (grove-library--documents (make-hash-table :test #'equal))
         (grove-structure--expanded (make-hash-table :test #'equal))
         (grove-structure--undo-notebooks nil)
         (ebp-org-file-save-function
          (lambda (buffer)
            (with-current-buffer buffer (save-buffer))
            t)))
    (unwind-protect
        (progn
          (write-region
           "* Alpha\n:PROPERTIES:\n:ID: alpha-id\n:END:\n** Child\n* Beta\n:PROPERTIES:\n:ID: beta-id\n:END:\n"
           nil file nil 'silent)
          (cl-letf (((symbol-function 'jetpacs-org-mode-agenda-scope)
                     (lambda () (list file)))
                    ((symbol-function 'jetpacs-event-stale-p)
                     (lambda (_params) nil))
                    ((symbol-function 'jetpacs-app-defer-refresh)
                     (lambda (&rest _) t))
                    ((symbol-function 'jetpacs-shell-notify)
                     (lambda (&rest _) t))
                    ((symbol-function 'jetpacs-shell-push)
                     (lambda (&rest _) t)))
            (grove-library-refresh)
            (let* ((notebook-id
                    (plist-get
                     (car (plist-get (grove-library-snapshot) :notebooks))
                     :id))
                   (records
                    (plist-get (grove-library-outline-refresh notebook-id)
                               :records))
                   (alpha (cl-find "Alpha" records
                                   :key (lambda (record)
                                          (plist-get record :title))
                                   :test #'equal))
                   (args (grove-structure--identity-args
                          notebook-id alpha "move-down"))
                   (params '(:surface "app:grove")))
              (should (eq (grove-structure--on-mutate args params)
                          'accepted))
              (with-current-buffer (find-file-noselect file t)
                (org-with-wide-buffer
                 (goto-char (point-min))
                 (should (looking-at-p "\\* Beta"))))
              (should (jetpacs-org-structure-undo-summary grove-owner))
              (should (eq (grove-structure--on-undo nil params) 'accepted))
              (with-current-buffer (find-file-noselect file t)
                (org-with-wide-buffer
                 (goto-char (point-min))
                 (should (looking-at-p "\\* Alpha"))
                 (search-forward ":END:\n")
                 (insert "Local drift\n")))
              (should (eq (grove-structure--on-mutate args params) 'stale))
              (let* ((fresh-records
                      (plist-get
                       (grove-library-outline-refresh notebook-id) :records))
                     (fresh-alpha
                      (cl-find "Alpha" fresh-records
                               :key (lambda (record)
                                      (plist-get record :title))
                               :test #'equal))
                     (narrow-args
                      (grove-structure--identity-args
                       notebook-id fresh-alpha "narrow")))
                (should (eq (grove-structure--on-mutate
                             narrow-args params)
                            'accepted))
                (let ((slice
                       (grove-structure-outline-slice
                        notebook-id fresh-records)))
                  (should (plist-get slice :focused))
                  (should (equal
                           (mapcar (lambda (record)
                                     (plist-get record :title))
                                   (plist-get slice :records))
                           '("Alpha" "Child"))))
                (should (eq (grove-structure--on-widen
                             (list :notebook-id notebook-id) params)
                            'accepted))))))
      (jetpacs-org-structure-reset grove-owner)
      (when-let* ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest grove-primary-shell-and-settings-are-valid-and-whitelisted ()
  "Grove exposes four primary destinations and only accepts safe settings."
  (should (equal (mapcar (lambda (row) (plist-get row :key))
                         grove-navigation-destinations)
                 '("notebooks" "agenda" "search" "settings")))
  (let ((entry (cdr (assoc grove-owner jetpacs-apps--registry))))
    (should (eq (plist-get entry :chrome) 'primary))
    (should (equal (plist-get entry :home-route) "notebooks")))
  (should (jetpacs-check-profile
           (grove-navigation-settings-screen nil) 'app))
  (let ((grove-library-tree-view t)
        saved deferred)
    (cl-letf (((symbol-function 'grove-navigation--save-setting)
               (lambda (symbol value) (setq saved (list symbol value))))
              ((symbol-function 'jetpacs-app-defer-refresh)
               (lambda (_params) (setq deferred t))))
      (should (eq (grove-navigation--on-setting
                   '(:field "tree" :value :json-false) nil)
                  'accepted))
      (should (equal saved '(grove-library-tree-view nil)))
      (should deferred)
      (setq saved nil deferred nil)
      (should (eq (grove-navigation--on-setting
                   '(:field "agenda-days" :value 0) nil)
                  'rejected))
      (should-not saved)
      (should-not deferred)
      (should (eq (grove-navigation--on-setting
                   '(:field "unknown" :value t) nil)
                  'rejected)))))

(provide 'grove-test)
;;; grove-test.el ends here
