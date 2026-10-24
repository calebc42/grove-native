;;; bloom-test.el --- ERT for the Bloom Jetpacs applet -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'bloom)
(bloom-register)  ; `require' only loads

(defconst bloom-test--root
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "This test file's directory, for fixtures next to it.")

(defconst bloom-test--now (encode-time 0 30 9 1 9 2026)
  "Stable local time used by Bloom snapshot tests.")

(defun bloom-test--ref (file pos headline &optional id)
  "Return a private Org reference for FILE, POS, HEADLINE, and optional ID."
  (list :id id :file file :pos pos :headline headline))

(defun bloom-test--agenda-item
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
    (ref . ,(bloom-test--ref file pos headline id))))

(defun bloom-test--snapshot ()
  "Return a populated explicit snapshot without private paths."
  '(:day-label "Tuesday" :date-label "September 1"
               :updated-label "Updated 09:30" :today-count 1 :total-count 2
               :error nil
               :items
               ((:id "bloom-item-a" :headline "Write project brief" :todo "TODO"
                     :priority "A" :tags ("work") :file-label "work.org" :time "09:00"
                     :date "2026-09-01" :type "scheduled" :section "TODAY")
                (:id "bloom-item-b" :headline "Book the train" :todo "NEXT"
                     :priority nil :tags nil :file-label "travel.org" :time nil
                     :date "2026-09-02" :type "deadline" :section "TOMORROW"))))

(defun bloom-test--library-snapshot ()
  "Return a populated Notebooks fixture without private paths."
  '(:notebooks
    ((:id "root-note" :folder-id nil :label "Inbox"
          :file-label "inbox.org" :folder-label "Notebooks"
          :favorite nil))
    :folders nil :total-count 1 :truncated nil :error nil))

(ert-deftest bloom-org-refresh-builds-stable-private-snapshot ()
  "The producer deduplicates, sorts, and keeps Org refs out of rendered IR."
  (let* ((file "/tmp/private-vault/work.org")
         (today (bloom-test--agenda-item
                 file 20 "Write project brief" "2026-09-01"
                 " 9:00 " "scheduled" "TODO" "A" '("work") "task-1"))
         (duplicate (copy-tree today))
         (tomorrow (bloom-test--agenda-item
                    file 50 "Book the train" "2026-09-02"
                    nil "deadline" "NEXT" nil nil nil))
         first-id second-id)
    (let ((bloom-org--snapshot (copy-tree bloom-org--snapshot)))
      (cl-letf (((symbol-function 'jetpacs-org-mode-agenda-items)
                 (lambda (&rest _) (list tomorrow duplicate today))))
        (should (bloom-org-refresh bloom-test--now))
        (let ((snapshot (bloom-org-snapshot)))
          (should (= (plist-get snapshot :total-count) 2))
          (should (= (plist-get snapshot :today-count) 1))
          (should (equal (mapcar (lambda (row) (plist-get row :section))
                                 (plist-get snapshot :items))
                         '("TODAY" "TOMORROW")))
          (setq first-id (plist-get (car (plist-get snapshot :items)) :id)))
        (should (bloom-org-refresh bloom-test--now))
        (setq second-id
              (plist-get (car (plist-get (bloom-org-snapshot) :items)) :id))
        (should (equal first-id second-id))
        (should-not
         (string-search
          "/tmp/private-vault"
          (jetpacs-node->canonical-json (bloom-ui-screen nil))))))))

(ert-deftest bloom-org-planning-marks-are-bounded-date-only-counts ()
  "Planning scans one month but retains no Org identity or headline text."
  (let* ((private "/tmp/private-vault/secret.org")
         (first (bloom-test--agenda-item
                 private 1 "Private A" "2026-09-03"))
         (second (bloom-test--agenda-item
                  private 2 "Private B" "2026-09-08"))
         (outside (bloom-test--agenda-item
                   private 3 "Outside" "2026-10-01"))
         called)
    (cl-letf (((symbol-function 'jetpacs-org-mode-agenda-items)
               (lambda (span start)
                 (setq called (list span start))
                 (list first (copy-tree first) (copy-tree first)
                       (copy-tree first) second outside))))
      (let ((marks (bloom-org-planning-marks "2026-09")))
        (should (equal called '(month "2026-09-01")))
        (should (equal marks '(("2026-09-03" . 3)
                               ("2026-09-08" . 1))))
        (should-not (string-search private (format "%S" marks)))
        (should-not (string-search "Private" (format "%S" marks)))))))

(ert-deftest bloom-agenda-builder-is-repeatable-over-explicit-state ()
  "The same snapshot and range produce byte-identical canonical screens."
  (let ((bloom-org--snapshot (bloom-test--snapshot))
        (bloom-agenda-range "today"))
    (should
     (equal (jetpacs-node->canonical-json (bloom-ui-screen nil))
            (jetpacs-node->canonical-json (bloom-ui-screen nil))))))

(ert-deftest bloom-refresh-pushes-widgets-only-when-granted ()
  "Snapshot refresh never attempts widget traffic the session declined."
  (let (pushed)
    (cl-letf (((symbol-function 'jetpacs-connected-p) (lambda () t))
              ((symbol-function 'jetpacs-shell-push)
               (lambda (surface &rest _)
                 (push surface pushed))))
      (cl-letf (((symbol-function 'jetpacs-granted-p) (lambda (&rest _) nil)))
        (bloom--push-current-surfaces)
        (should (equal (nreverse pushed) (list bloom-app-surface))))
      (setq pushed nil)
      (cl-letf (((symbol-function 'jetpacs-granted-p)
                 (lambda (capability &rest _)
                   (equal capability "surfaces.widget"))))
        (bloom--push-current-surfaces)
        (should (equal (nreverse pushed)
                       (list bloom-app-surface
                             bloom-capture-widget-surface
                             bloom-agenda-widget-surface)))))))

(ert-deftest bloom-agenda-selects-deterministic-design-or-exact-material ()
  "Agenda follows the app extension profile without forking its data model."
  (let ((bloom-org--snapshot (bloom-test--snapshot))
        (bloom-agenda-range "today")
        calls)
    (cl-letf (((symbol-function 'jetpacs-extension-advertised-p)
               (lambda (&rest args)
                 (push args calls)
                 nil)))
      (let ((first (bloom-ui-presented-screen nil))
            (second (bloom-ui-presented-screen nil))
            (material (bloom-ui-screen nil)))
        (should (equal first material))
        (should (equal (jetpacs-node->canonical-json first)
                       (jetpacs-node->canonical-json second)))
        (should-not (string-search
                     "jetpacs.design_scope"
                     (jetpacs-node->canonical-json first)))))
    (should (member '("jetpacs.design" :app) calls))
    (cl-letf (((symbol-function 'jetpacs-extension-advertised-p)
               (lambda (extension target)
                 (and (equal extension "jetpacs.design")
                      (eq target :app))))
              ;; A portable profile without Bloom's parity faces.
              ((symbol-function 'bloom-design-parity-available-p) #'ignore))
      (let* ((first (bloom-ui-presented-screen nil))
             (second (bloom-ui-presented-screen nil))
             (json (jetpacs-node->canonical-json first)))
        (should (equal (plist-get first :t) "jetpacs.design_scope"))
        (should (string-search "jetpacs.tabs" json))
        (should (string-search "\"t\":\"button\"" json))
        (should-not (string-search "jetpacs.action" json))
        (should-not (string-search "jetpacs.pressable" json))
        (should (string-search "\"kind\":\"theme-role\"" json))
        (should-not (string-match-p "#[[:xdigit:]]\\{6,8\\}" json))
        (should (string-search "bloom.agenda.select" json))
        (should (string-search "bloom-item-a" json))
        (should (jetpacs-check-profile first 'app))
        (should (equal json (jetpacs-node->canonical-json second)))
        (should (<= (jetpacs-node-wire-bytes first) 262144))))))

(ert-deftest bloom-notebooks-selects-deterministic-design-or-exact-material ()
  "Notebooks keeps its Material tree as the exact unadvertised fallback."
  (let ((bloom-library-tree-view t)
        (bloom-library--snapshot (bloom-test--library-snapshot))
        calls)
    (cl-letf (((symbol-function 'jetpacs-extension-advertised-p)
               (lambda (&rest args)
                 (push args calls)
                 nil)))
      (let ((first (bloom-navigation-presented-notebooks-screen nil))
            (second (bloom-navigation-presented-notebooks-screen nil))
            (material (bloom-navigation-notebooks-screen nil)))
        (should (equal first material))
        (should (equal (jetpacs-node->canonical-json first)
                       (jetpacs-node->canonical-json second)))))
    (should (member '("jetpacs.design" :app) calls))
    (cl-letf (((symbol-function 'jetpacs-extension-advertised-p)
               (lambda (extension target)
                 (and (equal extension "jetpacs.design")
                      (eq target :app)))))
      (let* ((first (bloom-navigation-presented-notebooks-screen nil))
             (second (bloom-navigation-presented-notebooks-screen nil))
             (json (jetpacs-node->canonical-json first)))
        (should (equal (plist-get first :t) "jetpacs.design_scope"))
        ;; The parity screen: Bloom's exact faces, its own dock and drawer.
        (should (string-search "\"t\":\"jetpacs.styled\"" json))
        (should (string-search "bloom.t.row-title" json))
        (should (string-search "bloom.library.notebook.menu" json))
        (should (string-search "\"fab_position\":\"center\"" json))
        (should (string-search "\"drawer\"" json))
        (should (string-search "\"kind\":\"theme-role\"" json))
        (should-not (string-match-p "#[[:xdigit:]]\\{6,8\\}" json))
        (should (string-search "bloom.notebook.open" json))
        (should (string-search "root-note" json))
        (should (jetpacs-check-profile first 'app))
        (should (equal json (jetpacs-node->canonical-json second)))
        (should (<= (jetpacs-node-wire-bytes first) 262144))))))

(ert-deftest bloom-design-preset-is-complete-palette-neutral-and-bounded ()
  "Bloom binds every semantic slot without replacing live theme colors."
  (let* ((profile (bloom-design-profile))
         (tokens (plist-get profile :tokens))
         (bindings (plist-get profile :component-styles))
         (scope (jetpacs-design-profile-scope
                 profile (list (jetpacs-text "Bloom")))))
    (should
     (equal
      (sort (mapcar #'car bindings) #'string<)
      (sort (copy-sequence jetpacs-design-component-style-slots) #'string<)))
    (dolist (name '("color.primary" "color.on-primary" "color.secondary"
                    "color.on-secondary" "color.error" "color.on-error"
                    "color.background" "color.on-background" "color.surface"
                    "color.on-surface" "color.outline" "color.success"
                    "color.warning"))
      (should (equal (plist-get (cdr (assoc name tokens)) :kind)
                     "theme-role")))
    (should
     (equal (mapcar #'car (plist-get profile :typography))
            '("display" "screen-title" "section-title" "item-title"
              "reading-body" "body" "supporting" "label-large" "label"
              "caption" "code" "metadata")))
    (should-not
     (string-match-p "#[[:xdigit:]]\\{6,8\\}"
                     (jetpacs-node->canonical-json scope)))
    ;; Palette-neutral: no chrome roles, so the Emacs theme stays in charge.
    (should-not (plist-get profile :theme-roles))
    (should-not (string-search "theme_roles" (jetpacs-node->canonical-json scope)))
    (should (<= (jetpacs-node-wire-bytes scope) 262144))))

(ert-deftest bloom-design-light-and-dark-presets-carry-the-literal-palette ()
  "Every literal preset binds the app's verbatim colors and every slot.
Bloom Light and Bloom Dark are the app's rosewater defaults; Grove Light and
Grove Dark are the original palettes it still offers."
  (dolist (pair `((,(bloom-design-light-profile) "#C06A6E" "#F8F3EE")
                  (,(bloom-design-dark-profile) "#EBA8A2" "#25223A")
                  (,(bloom-design-grove-light-profile) "#8A5A2B" "#F3EDE1")
                  (,(bloom-design-grove-dark-profile) "#CB9D62" "#16130E")))
    (let* ((profile (car pair))
           (tokens (plist-get profile :tokens))
           (scope (jetpacs-design-profile-scope
                   profile (list (jetpacs-text "Bloom"))))
           (json (jetpacs-node->canonical-json scope)))
      (should (equal (plist-get (cdr (assoc "color.primary" tokens)) :kind)
                     "color"))
      (should (equal (plist-get (cdr (assoc "color.primary" tokens)) :value)
                     (nth 1 pair)))
      (should (equal (plist-get (cdr (assoc "color.background" tokens)) :value)
                     (nth 2 pair)))
      ;; Soft fills keep their real alpha instead of an opaque tint.
      (should (string-match-p "#[[:xdigit:]]\\{8\\}"
                              (plist-get (cdr (assoc "color.accent-soft" tokens))
                                         :value)))
      (should-not (string-search "theme-role" json))
      ;; Literal presets re-declare all 13 roles, so the receiver's chrome
      ;; follows the palette; each role points at the preset's own token.
      (should (= (length (plist-get profile :theme-roles))
                 (length jetpacs-theme-roles)))
      (should (string-search "\"theme_roles\":{\"background\":{\"kind\":\"token\",\"value\":\"color.background\"}"
                             json))
      (should (string-search "\"on_primary\":{\"kind\":\"token\",\"value\":\"color.on-primary\"}"
                             json))
      (should
       (equal
        (sort (mapcar #'car (plist-get profile :component-styles)) #'string<)
        (sort (copy-sequence jetpacs-design-component-style-slots) #'string<)))
      (should (equal (plist-get profile :typography)
                     (plist-get (bloom-design-profile) :typography)))
      (should (<= (jetpacs-node-wire-bytes scope) 262144))))
  (should (equal (mapcar #'car
                         (seq-filter
                          (lambda (binding)
                            (string-prefix-p "text." (car binding)))
                          bloom-design-component-styles))
                 '("text.body" "text.title" "text.headline" "text.caption"
                   "text.label" "text.mono"))))

(ert-deftest bloom-design-registers-and-removes-all-presets ()
  "Registration exposes every preset to Design Lab and is fully reversible."
  (bloom-design-register)
  (should (= (length bloom-design-preset-ids) 5))
  (dolist (id bloom-design-preset-ids)
    (should (jetpacs-design-profile-get id)))
  (should (equal (plist-get (jetpacs-design-profile-get "bloom.light") :label)
                 "Bloom Light"))
  (should (equal (plist-get (jetpacs-design-profile-get "bloom.grove-light") :label)
                 "Grove Light"))
  (bloom-design-unregister)
  (dolist (id bloom-design-preset-ids)
    (should-not (jetpacs-design-profile-get id)))
  (bloom-design-register))

(ert-deftest bloom-design-reader-rebinds-body-text-to-serif-prose ()
  "The document screen nests a scope that rebinds only `text.body'."
  (let ((bloom-document--current nil))
    (cl-letf (((symbol-function 'jetpacs-extension-advertised-p)
               (lambda (extension target)
                 (and (equal extension "jetpacs.design") (eq target :app)))))
      (let* ((screen (bloom-document-presented-screen nil))
             (scaffold (aref (plist-get screen :children) 0))
             (body (plist-get scaffold :body))
             (json (jetpacs-node->canonical-json screen)))
        (should (equal (plist-get scaffold :t) "scaffold"))
        (should (equal (plist-get body :t) "jetpacs.design_scope"))
        (should (string-search "\"tokens\":{}" (jetpacs-node->canonical-json body)))
        (should (equal (plist-get (aref (plist-get body :component_styles) 0)
                                  :slot)
                       "text.body"))
        (should (string-search "typography.reading-body" json))
        (should (jetpacs-check-profile screen 'app))
        ;; Only the document kind gets the nested prose scope.
        (let ((settings (bloom-navigation-presented-settings-screen nil)))
          (should-not (equal (plist-get (plist-get (aref (plist-get settings :children) 0)
                                                   :body)
                                        :t)
                             "jetpacs.design_scope"))))
      ;; A portable profile without reading prose leaves the body untouched.
      (let* ((id "bloom-test-no-prose")
             (profile (jetpacs-design-make-profile
                       id "No prose" :tokens nil :typography nil
                       :styles nil :motions nil :component-styles nil)))
        (unwind-protect
            (progn
              (jetpacs-design-register-profile profile)
              (let* ((jetpacs-design-active-profile-id id)
                     (screen (bloom-document-presented-screen nil))
                     (scaffold (aref (plist-get screen :children) 0)))
                (should-not (equal (plist-get (plist-get scaffold :body) :t)
                                   "jetpacs.design_scope"))))
          (jetpacs-design-unregister-profile id))))))

(ert-deftest bloom-design-covers-every-app-screen-and-preserves-fallbacks ()
  "All seven Bloom screen kinds share the runtime gate and exact fallback."
  (let ((bloom-navigation--notebook-id nil)
        (bloom-document--current nil)
        (bloom-document--planning nil)
        (pairs
         '((bloom-navigation-outline-screen
            bloom-navigation-presented-outline-screen)
           (bloom-navigation-search-screen
            bloom-navigation-presented-search-screen)
           (bloom-navigation-settings-screen
            bloom-navigation-presented-settings-screen)
           (bloom-document-screen bloom-document-presented-screen)
           (bloom-document-planning-screen
            bloom-document-presented-planning-screen))))
    (dolist (pair pairs)
      (cl-letf (((symbol-function 'jetpacs-extension-advertised-p)
                 (lambda (&rest _) nil)))
        (should (equal (funcall (cadr pair) nil)
                       (funcall (car pair) nil))))
      (cl-letf (((symbol-function 'jetpacs-extension-advertised-p)
                 (lambda (extension target)
                   (and (equal extension "jetpacs.design")
                        (eq target :app)))))
        (let ((screen (funcall (cadr pair) nil)))
          (should (equal (plist-get screen :t) "jetpacs.design_scope"))
          (should (jetpacs-check-profile screen 'app)))))))

(ert-deftest bloom-design-accepts-a-portable-active-profile ()
  "A global profile need not define Bloom-private style names."
  (let* ((id "bloom-test-portable")
         (profile
          (jetpacs-design-make-profile
           id "Portable test"
           :tokens `(("accent" . ,(jetpacs-design-theme-role "primary")))
           :typography nil
           :styles
           `(("portable.action"
              . ,(jetpacs-design-style
                  `(("content_color" . ,(jetpacs-design-token "accent"))))))
           :motions nil
           :component-styles
           '(("action.label" . ("portable.action"))))))
    (unwind-protect
        (progn
          (jetpacs-design-register-profile profile)
          (let ((jetpacs-design-active-profile-id id))
            (cl-letf (((symbol-function 'jetpacs-extension-advertised-p)
                       (lambda (&rest _) t)))
              (let ((json
                     (jetpacs-node->canonical-json
                      (bloom-navigation-presented-settings-screen nil))))
                (should (string-search "portable.action" json))
                (should-not (string-search "radius.panel" json))
                (should-not (string-search "jetpacs.pressable" json))
                (should (string-search "\"t\":\"button\"" json))))))
      (jetpacs-design-unregister-profile id))))

(ert-deftest bloom-agenda-semantic-tabs-inject-the-selected-value ()
  "The design Tabs event uses the same Agenda action as Material buttons."
  (let ((bloom-agenda-range "today") deferred)
    (cl-letf (((symbol-function 'jetpacs-app-defer-refresh)
               (lambda (_params) (setq deferred t))))
      (should (eq (bloom--on-select-range
                   '(:value "upcoming") '(:surface "app:bloom"))
                  'accepted))
      (should (equal bloom-agenda-range "upcoming"))
      (should deferred))))

(ert-deftest bloom-agenda-row-uses-reveal-first-deep-commit-swipe ()
  "A normal swipe reveals two actions; only the deeper gesture commits Done."
  (cl-letf (((symbol-function 'jetpacs-feature-advertised-p)
             (lambda (&rest _) t)))
    (let* ((item (car (plist-get (bloom-test--snapshot) :items)))
           (row (bloom-ui-agenda-row item nil t t))
           (side (plist-get row :swipe_end))
           (actions (plist-get side :actions)))
      (should (eq (plist-get side :commit) t))
      (should (= (length actions) 2))
      (should (equal (plist-get (aref actions 0) :label) "Done"))
      (should (equal (plist-get
                      (plist-get (aref actions 0) :on_trigger) :action)
                     "bloom.agenda.done"))
      (should (equal (plist-get (aref actions 1) :label) "Open")))))

(ert-deftest bloom-rows-use-the-list-item-node-only-where-it-is-drawable ()
  "App rows take the Jetpacs row; widget rows keep the canonical one.
The node is declared for the app target only, so the widget projection
must degrade even offline, where the profile question has no answer."
  (let ((bloom-org--snapshot (bloom-test--snapshot))
        (bloom-library--snapshot (bloom-test--library-snapshot))
        (bloom-agenda-range "today"))
    (let ((app (jetpacs-node->canonical-json (bloom-ui-screen nil)))
          (widget (jetpacs-node->canonical-json
                   (bloom-widgets-agenda-surface))))
      (should (string-search "jetpacs.list_item" app))
      (should-not (string-search "jetpacs.list_item" widget))
      ;; Both still carry the row's identity and its durable action.
      (should (string-search "bloom-item-a" app))
      (should (string-search "bloom-item-a" widget)))
    (should (string-search
             "jetpacs.list_item"
             (jetpacs-node->canonical-json
              (bloom-navigation-notebooks-screen nil))))))

(ert-deftest bloom-menus-are-named-and-bound-to-the-popup-face ()
  "Every Bloom menu announces itself, and the menu slots reach a profile.

An un-annotated menu falls back to its icon identifier for an accessible
name, which announces as \"more_vert\"; the authored name is what stops
that.  The bindings are asserted separately because a menu drawn by the
Foundation renderer takes its whole appearance from them."
  (let ((menus (list (bloom-structure-menu
                      "nb" '(:id "heading-1" :revision 3))
                     (bloom-navigation--notebook-menu
                      (car (plist-get (bloom-test--library-snapshot)
                                      :notebooks)))
                     (plist-get (bloom-views-row
                                 '(:id "v1" :label "Today" :source "bloom"))
                                :trailing))))
    (dolist (menu menus)
      (should (equal (plist-get menu :t) "menu"))
      (should (stringp (plist-get (plist-get menu :semantics) :name)))
      (should-not (equal (plist-get (plist-get menu :semantics) :name)
                         "more_vert"))))
  (dolist (slot '("menu.trigger" "menu.container" "menu.item"
                  "menu.item-label" "menu.item-supporting"
                  "menu.group-label"))
    (should (assoc slot bloom-design-component-styles)))
  ;; The popup is a surface, not a row: it must not inherit the flat row.
  (should (equal (cdr (assoc "menu.container" bloom-design-component-styles))
                 '("bloom.popup")))
  ;; Menu labels match the app's own DropdownMenuItem scale.
  (should (equal (cdr (assoc "menu.item-label" bloom-design-component-styles))
                 '("typography.label-large"))))

(ert-deftest bloom-widgets-are-live-profile-valid-snapshot-projections ()
  "Capture and Agenda widgets stay inside the negotiated Glance vocabulary."
  (let ((bloom-org--snapshot (bloom-test--snapshot)))
    (should (jetpacs-check-profile (bloom-widgets-capture-surface) 'widget))
    (should (jetpacs-check-profile (bloom-widgets-agenda-surface) 'widget))
    (should (jetpacs-check-profile
             (bloom-widgets-agenda-stale-surface) 'widget))
    ;; Rich swipe is an application-only interaction.  Glance rows expose
    ;; the same durable actions through their tap targets without advertising
    ;; a renderer node the widget profile cannot honor.
    (dolist (surface (list (bloom-widgets-agenda-surface)
                           (bloom-widgets-agenda-stale-surface)))
      (should-not
       (string-search "\"swipe_end\""
                      (jetpacs-node->canonical-json surface))))
    (let* ((capture (bloom-widgets-capture-surface))
           (tap (plist-get (plist-get capture :body) :on_tap)))
      (should (equal (plist-get tap :action) "org.capture.show"))
      ;; Hermetic tests have no live grant, so durable actions must queue.
      (should (equal (plist-get tap :when_offline) "queue"))
      (should (= (plist-get tap :ttl_s) 86400)))))

(ert-deftest bloom-durable-actions-wake-only-when-granted ()
  "Bloom remains profile-valid when a Companion cannot wake Emacs."
  (cl-letf (((symbol-function 'jetpacs-granted-p)
             (lambda (&rest _) nil)))
    (let ((action (bloom-navigation--notebook-action "notebook-a")))
      (should (equal (plist-get action :when_offline) "queue"))
      (should-not (jetpacs-gate-descriptor-policy action))))
  (cl-letf (((symbol-function 'jetpacs-granted-p)
             (lambda (capability &optional _client)
               (equal capability "offline.wake"))))
    (let ((action (bloom-navigation--notebook-action "notebook-a")))
      (should (equal (plist-get action :when_offline) "wake"))
      (should-not (jetpacs-gate-descriptor-policy action)))))

(ert-deftest bloom-item-actions-resolve-current-opaque-identities ()
  "Open and Done resolve private refs in Emacs and reject vanished rows."
  (let* ((ref (bloom-test--ref "/tmp/work.org" 1
                               "Write project brief" "task-1"))
         opened toggled notified refreshed continuation)
    (cl-letf (((symbol-function 'bloom-org-ref-for-id)
               (lambda (id) (and (equal id "bloom-item-a") ref)))
              ((symbol-function 'bloom-library--notebook-id)
               (lambda (_file) "bloom-notebook-a"))
              ((symbol-function 'bloom-library--heading-id)
               (lambda (_ref) "bloom-heading-a"))
              ((symbol-function 'bloom-library-resolve-notebook)
               (lambda (_id) "/tmp/work.org"))
              ((symbol-function 'bloom-document-open)
               (lambda (&rest args) (setq opened args) 'accepted))
              ((symbol-function 'jetpacs-flow-continue)
               (lambda (fn) (setq continuation fn)))
              ((symbol-function 'ebp-org-toggle-todo)
               (lambda (&rest args) (setq toggled args)))
              ((symbol-function 'bloom-org-refresh)
               (lambda (&optional _now) (setq refreshed t)))
              ((symbol-function 'jetpacs-shell-notify)
               (lambda (&rest args) (setq notified args))))
      (should (eq (bloom--on-item-open
                   '(:item-id "missing") '(:surface "widget:bloom.agenda"))
                  'stale))
      (should (eq (bloom--on-item-open
                   '(:item-id "bloom-item-a")
                   '(:surface "widget:bloom.agenda"))
                  'accepted))
      (should
       (equal opened
              '("bloom-notebook-a" "bloom-heading-a"
                (:surface "widget:bloom.agenda"))))
      (should-not continuation)
      (should (eq (bloom--on-item-done
                   '(:item-id "bloom-item-a")
                   '(:surface "app:bloom"))
                  'accepted))
      (should (equal toggled (list ref 'bloom 'done)))
      (should refreshed)
      (should (equal notified '("Completed" "app:bloom"))))))

(ert-deftest bloom-registration-is-symmetric ()
  "Register and unregister cover the app, actions, hooks, and widget roots."
  (unwind-protect
      (progn
        (bloom-register)
        (should (assoc bloom-owner jetpacs-apps--registry))
        (dolist (name (append '("bloom.agenda.refresh" "bloom.agenda.select"
                                "bloom.agenda.item.open" "bloom.agenda.done")
                              bloom-navigation--verbs
                              bloom-structure--verbs
                              bloom-views--verbs
                              bloom-document--verbs
                              bloom-reminders--verbs))
          (should (gethash name jetpacs-action-handlers)))
        (should (assoc bloom-capture-widget-surface jetpacs-shell--roots))
        (should (assoc bloom-agenda-widget-surface jetpacs-shell--roots))
        (should (jetpacs--owned-names "hook" bloom-owner))
        (bloom-unregister)
        (should-not (jetpacs--owned-names "hook" bloom-owner))
        (dolist (entry '((after-save-hook . bloom-org--after-save)
                         (bloom-org-after-refresh-hook . bloom--schedule-pushes)
                         (bloom-org-after-refresh-hook
                          . bloom-reminders-schedule-sync)
                         (jetpacs-shell-after-push-hook
                          . bloom-reminders-schedule-sync)))
          (should-not (memq (cdr entry) (default-value (car entry)))))
        (should-not (assoc bloom-owner jetpacs-apps--registry))
        (should-not (assoc bloom-capture-widget-surface jetpacs-shell--roots))
        (should-not (assoc bloom-agenda-widget-surface jetpacs-shell--roots))
        (dolist (name (append '("bloom.agenda.refresh" "bloom.agenda.select"
                                "bloom.agenda.item.open" "bloom.agenda.done")
                              bloom-navigation--verbs
                              bloom-structure--verbs
                              bloom-views--verbs
                              bloom-document--verbs
                              bloom-reminders--verbs))
          (should-not (gethash name jetpacs-action-handlers))))
    (bloom-register)))

(ert-deftest bloom-reminders-project-opaque-ordered-elisp-actions ()
  "Bloom authors Complete then Reschedule without leaking an Org path."
  (let* ((private "/tmp/private-vault/work plan.org")
         (candidate
          (list :ref (bloom-test--ref private 42 "Ship it" "task-1")
                :headline "Ship it" :type "scheduled" :time "14:30"
                :event_at_ms 1788294600000 :at_ms 1788293700000
                :identity (concat private "|task-1|2026-09-02T14:30")))
         (bloom-reminder-lead-minutes 15))
    (cl-letf (((symbol-function 'jetpacs-granted-p)
               (lambda (capability &optional _client)
                 (member capability '("reminders.actions" "offline.wake"))))
              ((symbol-function 'jetpacs-org-mode-reminder-candidates)
               (lambda (&rest _) (list candidate)))
              ((symbol-function 'bloom-library-identify-heading)
               (lambda (_ref)
                 '(:notebook-id "bloom-notebook-a"
                   :heading-id "bloom-heading-a"))))
      (let* ((reminder (car (bloom-reminders--projection)))
             (actions (plist-get reminder :actions))
             (complete (aref actions 0))
             (reschedule (aref actions 1))
             (json (jetpacs-node->canonical-json reminder)))
        (should (= (length actions) 2))
        (should (equal (plist-get complete :label) "Complete"))
        (should (equal (plist-get reschedule :label) "Reschedule"))
        (should (equal
                 (plist-get (plist-get reschedule :on_tap) :open_surface)
                 bloom-app-surface))
        (should (equal
                 (plist-get (plist-get (plist-get reschedule :on_tap) :args)
                            :focus)
                 "scheduled"))
        (should (equal (plist-get (plist-get complete :on_tap) :action)
                       "bloom.reminder.complete"))
        (should-not (plist-get complete :dismiss))
        (should-not (string-search private json))
        (should-not (string-search "work plan.org" json))))))

(ert-deftest bloom-reminders-degrade-without-interaction-capability ()
  "An older peer receives the basic reminder schema and no app launch adjunct."
  (let ((candidate
         '(:ref (:file "/tmp/work.org" :pos 1 :headline "Task")
           :headline "Task" :type "deadline" :time "10:00"
           :event_at_ms 100000 :at_ms 100000 :identity "private")))
    (cl-letf (((symbol-function 'jetpacs-granted-p)
               (lambda (_capability &optional _client) nil))
              ((symbol-function 'jetpacs-org-mode-reminder-candidates)
               (lambda (&rest _) (list candidate)))
              ((symbol-function 'bloom-library-identify-heading)
               (lambda (_ref)
                 '(:notebook-id "bloom-notebook-a"
                   :heading-id "bloom-heading-a"))))
      (let* ((reminder (car (bloom-reminders--projection)))
             (tap (plist-get reminder :on_tap)))
        (should-not (plist-member reminder :actions))
        (should-not (plist-member tap :open_surface))
        (should (equal (plist-get tap :when_offline) "queue"))))))

(ert-deftest bloom-reminder-handlers-own-org-semantics ()
  "Complete mutates Org; Reschedule reuses Bloom's normal planning canvas."
  (let ((ref '(:file "/tmp/work.org" :pos 1 :headline "Task"))
        toggled refreshed planned)
    (cl-letf (((symbol-function 'bloom-library-resolve-heading)
               (lambda (notebook heading)
                 (and (equal notebook "bloom-notebook-a")
                      (equal heading "bloom-heading-a") ref)))
              ((symbol-function 'ebp-org-toggle-todo)
               (lambda (&rest args) (setq toggled args)))
              ((symbol-function 'bloom-library-reset-projections) #'ignore)
              ((symbol-function 'bloom-library-refresh) #'ignore)
              ((symbol-function 'bloom-org-refresh)
               (lambda (&optional _now) (setq refreshed t)))
              ((symbol-function 'bloom-document-open-planning)
               (lambda (&rest args) (setq planned args) 'accepted)))
      (let ((args '(:owner "bloom"
                    :reminder_id "bloom-reminder-1234"
                    :notebook-id "bloom-notebook-a"
                    :heading-id "bloom-heading-a"
                    :focus "deadline")))
        (should (eq (bloom-reminders--on-complete args nil) 'accepted))
        (should (equal toggled (list ref 'bloom 'done)))
        (should refreshed)
        (should (eq (bloom-reminders--on-reschedule args nil) 'accepted))
        (should (equal planned
                       '("bloom-notebook-a" "bloom-heading-a" nil
                         "deadline")))
        (should (eq
                 (bloom-reminders--on-complete
                  (plist-put (copy-sequence args) :owner "other") nil)
                 'stale))))))

(ert-deftest bloom-reminder-sync-preserves-confirmed-set-on-scan-failure ()
  "A failed Org scan never becomes authority to clear Room's durable set."
  (let ((bloom-reminders-enabled t)
        (bloom-reminders--last-reminders '((:id "bloom-reminder-old")))
        (calls 0))
    (cl-letf (((symbol-function 'jetpacs-connected-p) (lambda () t))
              ((symbol-function 'jetpacs-granted-p) (lambda (&rest _) t))
              ((symbol-function 'jetpacs-error-label) (lambda (_) "error"))
              ((symbol-function 'bloom-reminders--projection)
               (lambda () (error "private path")))
              ((symbol-function 'jetpacs-reminders-set)
               (lambda (&rest _) (cl-incf calls))))
      (bloom-reminders--sync-now)
      (should (zerop calls))
      (should (equal bloom-reminders--last-reminders
                     '((:id "bloom-reminder-old")))))))

(ert-deftest bloom-library-refresh-builds-a-private-nested-tree ()
  "Nested notebook paths become opaque, restart-rebuildable tree records."
  (should (equal (bloom-library--notebook-filename "journal")
                 "journal.org"))
  (should-error (bloom-library--notebook-filename "../escape")
                :type 'user-error)
  (should-error (bloom-library--notebook-filename "bad\nname")
                :type 'user-error)
  (let* ((root (make-temp-file "bloom-library" t))
         (projects (expand-file-name "projects" root))
         (client (expand-file-name "client" projects))
         (first (expand-file-name "inbox.org" root))
         (second (expand-file-name "work.org" client))
         (org-directory root)
         (ebp-org-roots (list root))
         (bloom-library--snapshot
          '(:notebooks nil :folders nil :total-count 0 :truncated nil
                       :error nil))
         (bloom-library--notebook-paths (make-hash-table :test #'equal))
         (bloom-library--folder-paths (make-hash-table :test #'equal))
         (bloom-library--heading-refs (make-hash-table :test #'equal))
         (bloom-library--outlines (make-hash-table :test #'equal))
         (bloom-library--documents (make-hash-table :test #'equal))
         (bloom-library-favorite-ids nil))
    (unwind-protect
        (progn
          (make-directory client t)
          (write-region "* Inbox\n" nil first nil 'silent)
          (write-region "* Work\n" nil second nil 'silent)
          (cl-letf (((symbol-function 'jetpacs-org-mode-agenda-scope)
                     (lambda () (list second first second))))
            (should (bloom-library-refresh))
            (let* ((snapshot (bloom-library-snapshot))
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
                               (bloom-navigation-notebooks-screen nil))))
              ;; Simulate Emacs process-state loss.  The opaque id resolves
              ;; by rebuilding the canonical scope, not by trusting wire data.
              (clrhash bloom-library--notebook-paths)
              (should (file-equal-p
                       (bloom-library-resolve-notebook first-id) first))
              (let ((ids (mapcar (lambda (row) (plist-get row :id))
                                 notebooks)))
                (should (bloom-library-refresh))
                (should (equal ids
                               (mapcar
                                (lambda (row) (plist-get row :id))
                                (plist-get (bloom-library-snapshot)
                                           :notebooks))))))))
      (dolist (buffer (buffer-list))
        (when-let* ((file (buffer-local-value 'buffer-file-name buffer))
                    ((file-in-directory-p file root)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory root t))))

(ert-deftest bloom-outline-preserves-hierarchy-and-rebuilds-heading-refs ()
  "The native outline is nested and opaque heading ids survive state loss."
  (let* ((root (make-temp-file "bloom-outline" t))
         (file (expand-file-name "garden.org" root))
         (org-directory root)
         (org-agenda-files (list file))
         (ebp-org-roots (list root))
         (bloom-library--snapshot
          '(:notebooks nil :folders nil :total-count 0 :truncated nil
                       :error nil))
         (bloom-library--notebook-paths (make-hash-table :test #'equal))
         (bloom-library--folder-paths (make-hash-table :test #'equal))
         (bloom-library--heading-refs (make-hash-table :test #'equal))
         (bloom-library--outlines (make-hash-table :test #'equal))
         (bloom-library--documents (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (write-region
           "#+title: Garden\nA short preface.\n* TODO Parent\n:PROPERTIES:\n:ID: parent-id\n:END:\nParent body.\n** TODO Child\nChild body.\n* Sibling\n"
           nil file nil 'silent)
          (cl-letf (((symbol-function 'jetpacs-org-mode-agenda-scope)
                     (lambda () (list file))))
            (should (bloom-library-refresh))
            (let* ((notebook-id
                    (plist-get
                     (car (plist-get (bloom-library-snapshot) :notebooks))
                     :id))
                   (outline (bloom-library-outline-refresh notebook-id))
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
              (clrhash bloom-library--heading-refs)
              (clrhash bloom-library--outlines)
              (should (equal
                       (plist-get (bloom-library-resolve-heading
                                   notebook-id child-id)
                                  :headline)
                       "Child"))
              (let ((bloom-navigation--notebook-id notebook-id))
                (should-not
                 (string-search root
                                (jetpacs-node->canonical-json
                                 (bloom-navigation-outline-screen nil)))))
              (let ((bloom-document--current
                     (list :notebook-id notebook-id :heading-id child-id))
                    (bloom-document--modes (make-hash-table :test #'equal)))
                (let ((reader (bloom-document-screen nil)))
                  (should (jetpacs-check-profile reader 'app))
                  (should-not
                   (string-search root
                                  (jetpacs-node->canonical-json reader))))
                (puthash (bloom-document--key notebook-id child-id)
                         'edit bloom-document--modes)
                (should (jetpacs-check-profile
                         (bloom-document-screen nil) 'app))))))
      (when-let* ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest bloom-native-search-retains-results-on-expression-error ()
  "Search projects native Org rows and an invalid query cannot erase results."
  (let* ((root (make-temp-file "bloom-search" t))
         (file (expand-file-name "notes.org" root))
         (ref (bloom-test--ref file 1 "Alpha" "alpha-id"))
         (bloom-library--snapshot
          '(:notebooks nil :folders nil :total-count 0 :truncated nil
                       :error nil))
         (bloom-library--search-snapshot
          '(:query "" :mode "text" :results nil :error nil))
         (bloom-library--notebook-paths (make-hash-table :test #'equal))
         (bloom-library--folder-paths (make-hash-table :test #'equal))
         (bloom-library--heading-refs (make-hash-table :test #'equal))
         (bloom-library--outlines (make-hash-table :test #'equal))
         (bloom-library--documents (make-hash-table :test #'equal))
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
            (should (bloom-library-refresh))
            (should (bloom-library-search "needle" "text"))
            (let* ((good (bloom-library-search-snapshot))
                   (results (plist-get good :results)))
              (should (= (length results) 1))
              (should-not
               (string-search root
                              (jetpacs-node->canonical-json
                               (bloom-navigation-search-screen nil))))
              (cl-letf (((symbol-function 'jetpacs-org-mode-search-items)
                         (lambda (&rest _) (error "bad private expression"))))
                (should-not (bloom-library-search "bad" "match"))
                (let ((failed (bloom-library-search-snapshot)))
                  (should (equal (plist-get failed :results) results))
                  (should (equal (plist-get failed :query) "bad"))
                  (should (equal (plist-get failed :mode) "match"))
                  (should (equal (plist-get failed :error)
                                 "Invalid search expression")))))))
      (delete-directory root t))))

(ert-deftest bloom-saved-views-combine-customize-and-read-only-emacs-views ()
  "Hybrid view rows stay bounded and custom agenda internals remain private."
  (let ((bloom-saved-views '(("Needles" "text" "private phone query")))
        (bloom-saved-view-limit 8)
        (bloom-library--search-snapshot
         '(:query "private phone query" :mode "text" :results nil
                  :error nil)))
    (cl-letf (((symbol-function 'jetpacs-org-mode-custom-views)
               (lambda ()
                 '((:id "org-custom-view-safe" :label "Work agenda"
                        :kind "tags-todo")))))
      (let* ((snapshot (bloom-views-snapshot))
             (views (plist-get snapshot :views))
             (phone (car views))
             (emacs (cadr views))
             (run (bloom-views-run-action emacs))
             (json (jetpacs-node->canonical-json
                    (bloom-navigation-search-screen nil))))
        (should (= (length views) 2))
        (should (equal (plist-get phone :source) "bloom"))
        (should (equal (plist-get emacs :source) "emacs"))
        (should (equal (plist-get run :args)
                       '(:view-id "org-custom-view-safe")))
        (should (string-search "Work agenda" json))
        (should (string-search "Read-only" json))
        (should-not (string-search "org-agenda-custom-commands" json))
        (should (jetpacs-check-profile
                 (bloom-navigation-search-screen nil) 'app))))))

(ert-deftest bloom-saved-view-actions-resolve-live-config-and-persist ()
  "Phone views save/delete locally and both sources run by opaque identity."
  (let ((bloom-saved-views nil)
        (bloom-saved-view-limit 8)
        persisted phone-run custom-run deferred notified)
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (_symbol value) (setq persisted (copy-tree value))))
              ((symbol-function 'jetpacs-org-mode-custom-views)
               (lambda ()
                 '((:id "org-custom-view-work" :label "Work"
                        :kind "tags"))))
              ((symbol-function 'bloom-library-search)
               (lambda (&rest args) (setq phone-run args) t))
              ((symbol-function 'bloom-library-custom-view-search)
               (lambda (&rest args) (setq custom-run args) t))
              ((symbol-function 'bloom-library-clear-active-view)
               (lambda (_id) t))
              ((symbol-function 'jetpacs-app-defer-refresh)
               (lambda (_params) (setq deferred t)))
              ((symbol-function 'jetpacs-shell-notify)
               (lambda (&rest args) (setq notified args))))
      (let* ((phone (bloom-views--add "Inbox" "match" "+inbox"))
             (phone-id (plist-get phone :id)))
        (should (equal persisted '(("Inbox" "match" "+inbox"))))
        (should-error (bloom-views--add "inbox" "text" "duplicate")
                      :type 'user-error)
        (should (eq (bloom-views--on-run
                     (list :view-id phone-id) '(:surface "app:bloom"))
                    'accepted))
        (should (equal (seq-take phone-run 2) '("+inbox" "match")))
        (should (eq (bloom-views--on-run
                     '(:view-id "org-custom-view-work")
                     '(:surface "app:bloom"))
                    'accepted))
        (should (equal custom-run
                       '("org-custom-view-work" "Work" "tags")))
        (should (eq (bloom-views--on-delete
                     (list :view-id phone-id) '(:surface "app:bloom"))
                    'accepted))
        (should-not bloom-saved-views)
        (should deferred)
        (should (equal notified '("Saved view deleted" "app:bloom")))
        (should (eq (bloom-views--on-delete
                     '(:view-id "org-custom-view-work")
                     '(:surface "app:bloom"))
                    'rejected))))))

(ert-deftest bloom-save-view-flow-captures-query-but-never-authors-it-in-action ()
  "Save uses field capture, validates natively, and prompts outside dispatch."
  (let ((bloom-saved-views nil)
        continuation validated persisted searched)
    (cl-letf (((symbol-function 'jetpacs-flow-continue)
               (lambda (function) (setq continuation function)))
              ((symbol-function 'jetpacs-dialog-can-bridge-p) (lambda () t))
              ((symbol-function 'read-string) (lambda (&rest _) "My view"))
              ((symbol-function 'jetpacs-org-mode-search-items)
               (lambda (&rest args) (setq validated args) nil))
              ((symbol-function 'customize-save-variable)
               (lambda (_symbol value) (setq persisted (copy-tree value))))
              ((symbol-function 'bloom-library-search)
               (lambda (&rest args) (setq searched args) t))
              ((symbol-function 'jetpacs-app-defer-refresh) #'ignore)
              ((symbol-function 'jetpacs-shell-notify) #'ignore))
      (let ((action (bloom-views-save-action "text")))
        (should (equal (plist-get action :args) '(:mode "text")))
        (should-not (string-search "needle" (prin1-to-string action))))
      (should (eq (bloom-views--on-save
                   '(:mode "text")
                   '(:surface "app:bloom"
                     :fields (:bloom-search-query "needle")))
                  'accepted))
      (should continuation)
      (funcall continuation)
      (should (equal validated '("needle" text 100)))
      (should (equal persisted '(("My view" "text" "needle"))))
      (should (equal (seq-take searched 2) '("needle" "text"))))))

(ert-deftest bloom-document-subtree-save-rolls-back-pre-durability-failure ()
  "A failed prewrite restores the entire live Org buffer exactly."
  (let* ((file (make-temp-file "bloom-document" nil ".org"
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
            (cl-letf (((symbol-function 'jetpacs-org-save-policy)
                       (lambda (_buffer)
                         (goto-char (point-max))
                         (insert "CORRUPT")
                         (error "prewrite failed"))))
              (should-error
               (bloom-document--replace-subtree
                buffer beg end "* TODO Changed\nNew.\n"))
              (should (equal (buffer-string) before))
              (should-not (buffer-modified-p)))
            (cl-letf (((symbol-function 'jetpacs-org-save-policy)
                       (lambda (_buffer) t)))
              (let ((new-ref (bloom-document--replace-subtree
                              buffer beg end "* TODO Changed\nNew.\n")))
                (should (equal (plist-get new-ref :headline) "Changed"))
                (should (string-prefix-p "* TODO Changed\nNew.\n* Keep"
                                         (buffer-string)))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest bloom-planning-commits-the-captured-pair-and-checks-revision ()
  "Planning forwards both fields atomically and rejects a stale document."
  (let* ((content "* TODO Alpha\n")
         (revision (secure-hash 'sha256 content))
         ;; A queued save must remain self-describing after Emacs loses the
         ;; transient planning canvas.
         (bloom-document--planning nil)
         (ref (bloom-test--ref "/tmp/alpha.org" 1 "Alpha" "alpha"))
         called refreshed continued)
    (let ((bloom-document--planning
           (list :notebook-id "notebook" :heading-id "heading"
                 :revision revision :scheduled "" :deadline "")))
      (let ((screen (bloom-document-planning-screen nil)))
        (should (jetpacs-check-profile screen 'app))
        (let ((json (jetpacs-node->canonical-json screen)))
          (should (string-search "notebook" json))
          (should (string-search revision json)))))
    (cl-letf (((symbol-function 'bloom-library-resolve-heading)
               (lambda (&rest _) ref))
              ((symbol-function 'bloom-document--live-subtree)
               (lambda (_ref) (list :content content)))
              ((symbol-function 'ebp-org-set-planning-pair)
               (lambda (&rest args) (setq called args)))
              ((symbol-function 'bloom-document--after-mutation)
               (lambda (&rest _) (setq refreshed t)))
              ((symbol-function 'jetpacs-flow-continue)
               (lambda (fn) (setq continued fn))))
      (should
       (eq (bloom-document--on-planning-save
            `(:notebook-id "notebook" :heading-id "heading"
                           :revision ,revision)
            '(:surface "app:bloom"
                       :fields (:bloom-scheduled "<2026-09-04 Fri 10:00 +1w>"
                                                 :bloom-deadline "<2026-09-05 Sat>")))
           'accepted))
      (should (equal called
                     (list ref 'bloom "<2026-09-04 Fri 10:00 +1w>"
                           "<2026-09-05 Sat>")))
      (should refreshed)
      (should (functionp continued))
      (should-not bloom-document--planning))
    (let ((bloom-document--planning
           (list :notebook-id "notebook" :heading-id "heading"
                 :revision revision :scheduled "" :deadline ""))
          called)
      (cl-letf (((symbol-function 'bloom-library-resolve-heading)
                 (lambda (&rest _) ref))
                ((symbol-function 'bloom-document--live-subtree)
                 (lambda (_ref) (list :content "* TODO Changed\n")))
                ((symbol-function 'ebp-org-set-planning-pair)
                 (lambda (&rest args) (setq called args))))
        (should
         (eq (bloom-document--on-planning-save
              `(:notebook-id "notebook" :heading-id "heading"
                             :revision ,revision)
              nil)
             'stale))
        (should-not called)))))

(defun bloom-test--json (value)
  "VALUE's canonical JSON as a multibyte string, searchable for any text."
  (let ((json (jetpacs-node->canonical-json value)))
    (if (multibyte-string-p json) json (decode-coding-string json 'utf-8))))

(defun bloom-test--materialized-planning-json ()
  "The planning screen as the Companion renders it: bound members resolved."
  (bloom-test--json
   (jetpacs-data-materialize (bloom-document-planning-screen nil)
                             (bloom-document-surface-data)
                             :strip t)))

(ert-deftest bloom-planning-calendar-is-pure-styled-and-profile-valid ()
  "The combined calendar is a pure projection with a styled planning range.
Its values travel in the surface data document (SPEC 13.8): the authored
screen binds them, the materialized screen shows them."
  (let ((bloom-document--planning
         '(:notebook-id "notebook" :heading-id "heading" :revision "rev"
           :scheduled "<2026-09-04 Fri 09:00 +1w>"
           :deadline "<2026-09-08 Tue>" :focus "scheduled"
           :today "2026-09-01" :month "2026-09"
           :marks (("2026-09-04" . 2) ("2026-09-06" . 1)))))
    (cl-letf (((symbol-function 'jetpacs-org-mode-agenda-items)
               (lambda (&rest _) (error "builder performed Org I/O")))
              ((symbol-function 'jetpacs-node-member-advertised-p)
               (lambda (type member &optional _target)
                 (and (equal type "month_grid")
                      (equal member "day_styles")))))
      (let* ((screen (bloom-document-planning-screen nil))
             (authored (bloom-test--json screen))
             (json (bloom-test--materialized-planning-json)))
        (should (jetpacs-check-profile screen 'app))
        ;; Authored: bindings in place of every draft-dependent value.
        (should (string-search "\"range_start\":\"/planning/range_start\""
                               authored))
        (should (string-search "\"value\":\"/planning/scheduled/value\""
                               authored))
        (should (string-search "\"variant\":\"/planning/deadline/target_variant\""
                               authored))
        (should-not (string-search "2026-09-04" authored))
        (should-not (string-search "09:00" authored))
        ;; Materialized: what the literal screen carried.
        (should-not (string-search "\"bind\"" json))
        (should (string-search "\"range_start\":\"2026-09-04\"" json))
        (should (string-search "\"range_end\":\"2026-09-08\"" json))
        (should (string-search "\"day_styles\"" json))
        (should (string-search "\"background\":\"primary\"" json))
        (should (string-search "\"background\":\"error\"" json))
        (should (string-search "\"2026-09-06\":{\"dots\":1}" json))
        (should (string-search "\"value\":\"<2026-09-04 Fri 09:00 +1w>\"" json))
        (should (string-search "\"label\":\"2026-09-04\"" json))
        (should (string-search "\"label\":\"09:00\"" json))
        (should (string-search "\"label\":\"Add time\"" json))
        (should (string-search "Scheduled · 2026-09-04" json))
        (should (string-search "\"variant\":\"filled\"" json))
        (should (string-search "bloom.planning.focus" json))
        (should (string-search "bloom.planning.month" json))
        (should (string-search "Clear both" json))
        (should (string-search "4 days between scheduled and deadline" json))
        ;; The document is the handler's patch, byte for byte.
        (should (equal (bloom-test--json
                        (jetpacs-data-set "/planning" (bloom-document-planning-data)))
                       (bloom-test--json
                        (jetpacs-data-set "/planning"
                                          (plist-get (bloom-document-surface-data)
                                                     :planning)))))))))

(ert-deftest bloom-planning-calendar-degrades-day-styles-to-colored-marks ()
  "A constrained member profile gets marks instead of day_styles."
  (let ((bloom-document--planning
         '(:notebook-id "notebook" :heading-id "heading" :revision "rev"
           :scheduled "2026-09-04" :deadline "2026-09-08"
           :focus "deadline" :today "2026-09-01" :month "2026-09"
           :marks nil)))
    (cl-letf (((symbol-function 'jetpacs-node-member-advertised-p)
               (lambda (&rest _) nil)))
      (let ((json (bloom-test--materialized-planning-json)))
        (should-not (string-search "\"day_styles\"" json))
        (should (string-search "\"color\":\"primary\"" json))
        (should (string-search "\"color\":\"error\"" json))))))

(ert-deftest bloom-planning-actions-preserve-captured-pair-and-validate-dates ()
  "Calendar transitions preserve local edits and reject malformed injection.
Every change patches the surface data document: since SPEC 13.8.6 the
feedback cards are conditional nodes, so even a change of feedback kind
is a patch, never a snapshot."
  (let ((bloom-document--planning
         '(:notebook-id "notebook" :heading-id "heading" :revision "rev"
           :scheduled "<2026-09-04 Fri 09:00 +1w>"
           :deadline "" :focus "scheduled" :today "2026-09-01"
           :month "2026-09" :marks nil))
        mark-months published)
    (cl-letf (((symbol-function 'bloom-org-planning-marks)
               (lambda (month)
                 (push month mark-months)
                 (list (cons (concat month "-02") 1))))
              ((symbol-function 'jetpacs-app-defer-refresh)
               (lambda (&rest _) (push 'refresh published)))
              ((symbol-function 'jetpacs-app-defer-patch)
               (lambda (ops &rest _) (push (cons 'patch ops) published))))
      ;; A lone scheduled date becomes a pair: the lead-time card appears
      ;; through the document alone (SPEC 13.8.6), so this too is a patch.
      (should
       (eq (bloom-document--on-planning-pick
            '(:field "scheduled" :kind "date" :value "2026-10-07")
            '(:fields (:bloom-scheduled "<2026-09-04 Fri 09:00 +1w>"
                       :bloom-deadline "<2026-10-11 Sun>")))
           'accepted))
      (should (equal (plist-get bloom-document--planning :scheduled)
                     "<2026-10-07 09:00 +1w>"))
      (should (equal (plist-get bloom-document--planning :deadline)
                     "<2026-10-11 Sun>"))
      (should (equal (plist-get bloom-document--planning :month) "2026-10"))
      (should (equal mark-months '("2026-10")))
      (should (eq (car (car published)) 'patch))
      (should (equal (plist-get (plist-get (car (cdr (car published))) :value)
                                :feedback_kind)
                     "lead-time"))
      ;; Moving focus changes values only: one patch of the whole object.
      (should
       (eq (bloom-document--on-planning-focus
            '(:field "deadline")
            '(:fields (:bloom-scheduled "<2026-10-07 09:00 +1w>"
                       :bloom-deadline "<2026-10-11 Sun>")))
           'accepted))
      (should (eq (car (car published)) 'patch))
      (let* ((ops (cdr (car published)))
             (planning (plist-get (car ops) :value)))
        (should (= (length ops) 1))
        (should (equal (plist-get (car ops) :path) "/planning"))
        (should (equal (bloom-test--json (car ops))
                       (bloom-test--json
                        (jetpacs-data-set "/planning"
                                          (bloom-document-planning-data)))))
        (should (equal (plist-get planning :month) "2026-10"))
        (should (equal (plist-get planning :range_start) "2026-10-07"))
        (should (equal (plist-get planning :range_end) "2026-10-11"))
        (should-not (plist-member planning :selected))
        (should (equal (plist-get (plist-get planning :deadline) :target_variant)
                       "filled"))
        (should (equal (plist-get (plist-get planning :scheduled) :target_variant)
                       "outlined"))
        (should (equal (plist-get (plist-get planning :scheduled) :time) "09:00"))
        (should (equal (plist-get (plist-get planning :feedback) :text)
                       "4 days between scheduled and deadline."))
        (should (equal (gethash "2026-10-02" (plist-get planning :marks))
                       '(:dots 1))))
      ;; Shorthand drafts dissolve the pair: the card goes — by patch.
      (should
       (eq (bloom-document--on-planning-month
            '(:value "2026-11")
            '(:fields (:bloom-scheduled "nov 2 09:00"
                       :bloom-deadline "nov 9")))
           'accepted))
      (should (equal (plist-get bloom-document--planning :scheduled)
                     "nov 2 09:00"))
      (should (equal (plist-get bloom-document--planning :deadline) "nov 9"))
      (should (equal mark-months '("2026-11" "2026-10")))
      (should (eq (car (car published)) 'patch))
      (should-not (plist-member (plist-get (car (cdr (car published))) :value)
                                :feedback_kind))
      (let ((before (copy-tree bloom-document--planning))
            (shown (length published)))
        (should
         (eq (bloom-document--on-planning-pick
              '(:field "deadline" :kind "date" :value "2026-02-30") nil)
             'rejected))
        (should (equal bloom-document--planning before))
        (should (= (length published) shown)))
      (should
       (eq (bloom-document--on-planning-clear
            '(:field "both")
            '(:fields (:bloom-scheduled "keep?" :bloom-deadline "keep?")))
           'accepted))
      (should (equal (plist-get bloom-document--planning :scheduled) ""))
      (should (equal (plist-get bloom-document--planning :deadline) ""))
      (should (eq (car (car published)) 'patch)))))

(ert-deftest bloom-planning-focus-serves-calendar-taps-and-suggestions ()
  "A calendar tap names no target: the focused one, which only the document
shows, receives it.  The runway suggestion is computed when its button
is tapped, from the captured deadline, never baked into the tree."
  (let ((bloom-document--planning
         '(:notebook-id "notebook" :heading-id "heading" :revision "rev"
           :scheduled "" :deadline "<2026-09-08 Tue>" :focus "deadline"
           :today "2026-09-01" :month "2026-09" :marks nil))
        published)
    (cl-letf (((symbol-function 'bloom-org-planning-marks)
               (lambda (_month) nil))
              ((symbol-function 'jetpacs-app-defer-refresh)
               (lambda (&rest _) (push 'refresh published)))
              ((symbol-function 'jetpacs-app-defer-patch)
               (lambda (ops &rest _) (push (cons 'patch ops) published))))
      (let ((authored (bloom-test--json (bloom-document-planning-screen nil))))
        (should (string-search "\"args\":{\"kind\":\"date\"}" authored))
        (should (string-search "bloom.planning.suggest" authored))
        (should-not (string-search "Schedule this for" authored))
        (should (string-search "Schedule this for 2026-09-05"
                               (bloom-test--materialized-planning-json))))
      ;; A day tap with no field lands on the focused deadline.
      (should
       (eq (bloom-document--on-planning-pick
            '(:kind "date" :value "2026-09-10")
            '(:fields (:bloom-scheduled "" :bloom-deadline "<2026-09-08 Tue>")))
           'accepted))
      (should (equal (plist-get bloom-document--planning :deadline) "<2026-09-10>"))
      (should (equal (plist-get bloom-document--planning :scheduled) ""))
      ;; Still a runway: a patch, whose suggestion follows the deadline.
      (should (eq (car (car published)) 'patch))
      (let ((planning (plist-get (car (cdr (car published))) :value)))
        (should (equal (plist-get (plist-get planning :feedback) :text)
                       "Schedule this for 2026-09-07, three days before the deadline."))
        (should (equal (plist-get planning :selected) "2026-09-10"))
        (should (equal (plist-get (plist-get planning :deadline) :target_label)
                       "Deadline · 2026-09-10")))
      ;; The suggestion applies the captured deadline, not the draft's.
      (should
       (eq (bloom-document--on-planning-suggest
            nil
            '(:fields (:bloom-scheduled "" :bloom-deadline "<2026-09-20 Sun>")))
           'accepted))
      (should (equal (plist-get bloom-document--planning :scheduled) "2026-09-17"))
      (should (equal (plist-get bloom-document--planning :deadline)
                     "<2026-09-20 Sun>"))
      (should (equal (plist-get bloom-document--planning :focus) "scheduled"))
      ;; Now a pair: the lead-time card shows, and that too is a patch.
      (should (eq (car (car published)) 'patch))
      (should (equal (plist-get (plist-get (car (cdr (car published))) :value)
                                :feedback_kind)
                     "lead-time"))
      (setq bloom-document--planning
            (plist-put bloom-document--planning :deadline ""))
      (should (eq (bloom-document--on-planning-suggest nil nil) 'rejected)))))

(ert-deftest bloom-root-carries-the-planning-data-document ()
  "The app root's data builder is the planning projection (SPEC 13.8)."
  (let ((bloom-document--planning nil))
    (should-not (bloom-document-surface-data)))
  (let ((bloom-document--planning
         '(:notebook-id "n" :heading-id "h" :revision "r"
           :scheduled "" :deadline "" :focus "scheduled"
           :today "2026-09-01" :month "2026-09" :marks nil)))
    (should (equal (plist-get (plist-get (bloom-document-surface-data) :planning)
                              :month)
                   "2026-09"))
    (should (equal (bloom-document-surface-data) (bloom-document-surface-data)))))

(ert-deftest bloom-planning-presets-and-colliding-styles-are-deterministic ()
  "Quick dates use calendar arithmetic and a same-day deadline wins color."
  (should (equal (bloom-document--planning-preset-dates "2026-09-01")
                 '(("Today" . "2026-09-01")
                   ("Tomorrow" . "2026-09-02")
                   ("This weekend" . "2026-09-05")
                   ("Next week" . "2026-09-07"))))
  (let ((styles (bloom-document--planning-day-styles
                 "2026-09-04" "2026-09-04")))
    (should (= (length styles) 1))
    (should (equal (plist-get (cdar styles) :background) "error"))))

(ert-deftest bloom-notebooks-use-reveal-only-swipe-and-folder-local-builders ()
  "Notebook swipes never commit and each stacked folder keeps its own model."
  (let* ((folder-id "bloom-folder-projects")
         (bloom-library-tree-view t)
         (bloom-library--snapshot
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
      (let* ((row (bloom-navigation-notebook-row
                   (car (plist-get bloom-library--snapshot :notebooks))))
             (swipe (plist-get row :swipe_end)))
        (should swipe)
        (should-not (plist-get swipe :commit)))
      (let* ((root-json
              (jetpacs-node->canonical-json
               (bloom-navigation-notebooks-screen nil)))
             (child-json
              (jetpacs-node->canonical-json
               (bloom-navigation--notebooks-screen-for-folder
                folder-id nil)))
             (root-again
              (jetpacs-node->canonical-json
               (bloom-navigation-notebooks-screen nil))))
        (should (string-search "Inbox" root-json))
        (should-not (string-search "Client" root-json))
        (should (string-search "Client" child-json))
        (should-not (string-search "Inbox" child-json))
        (should (equal root-json root-again))))))

(ert-deftest bloom-outline-rows-expose-safe-rich-structure-controls ()
  "Leading swipes reveal four actions without commit; only Done deep-commits."
  (let* ((bloom-navigation--notebook-id "bloom-notebook-test")
         (record '(:id "bloom-heading-test" :parent-id nil :level 1
                       :title "Plan project" :todo "TODO" :done nil
                       :revision
                       "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
         (row (car (bloom-navigation--outline-children (list record) nil)))
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

(ert-deftest bloom-outline-disclosure-and-reading-have-distinct-targets ()
  "Only parents disclose children; every heading retains a reading action."
  (let* ((bloom-navigation--notebook-id "bloom-notebook-test")
         (records '((:id "parent" :parent-id nil :title "Parent" :todo "TODO")
                    (:id "child" :parent-id "parent" :title "Child")
                    (:id "leaf" :parent-id nil :title "Leaf"))))
    (dolist (advertised '(nil t))
      (cl-letf (((symbol-function 'jetpacs-node-advertised-p)
                 (lambda (&rest _) advertised)))
        (let* ((rows (bloom-navigation--outline-children records nil))
               (parent (car rows))
               (leaf (cadr rows))
               (header (plist-get parent :header))
               (json (jetpacs-node->canonical-json
                      (apply #'jetpacs-column rows))))
          (should (equal (plist-get parent :t) "collapsible"))
          (should-not (plist-get header :on_tap))
          (should (equal (plist-get (plist-get leaf :on_tap) :action)
                         "bloom.document.open"))
          (should-not (string-search "chevron_right" json))
          (should-not (string-search "account_tree" json))
          (should (string-search "TODO · 1 section" (decode-coding-string json 'utf-8)))
          (should (string-search "Read" json))
          (should (equal json
                         (jetpacs-node->canonical-json
                          (apply #'jetpacs-column
                                 (bloom-navigation--outline-children
                                  records nil))))))))))

(ert-deftest bloom-outline-mutation-is-revision-bound-and-undoable ()
  "A live opaque row can move and undo; a later local edit makes it stale."
  (let* ((root (make-temp-file "bloom-structure" t))
         (file (expand-file-name "notes.org" root))
         (org-directory root)
         (org-agenda-files (list file))
         (ebp-org-roots (list root))
         (bloom-library--snapshot
          '(:notebooks nil :folders nil :total-count 0 :truncated nil
                       :error nil))
         (bloom-library--notebook-paths (make-hash-table :test #'equal))
         (bloom-library--folder-paths (make-hash-table :test #'equal))
         (bloom-library--heading-refs (make-hash-table :test #'equal))
         (bloom-library--outlines (make-hash-table :test #'equal))
         (bloom-library--documents (make-hash-table :test #'equal))
         (bloom-structure--expanded (make-hash-table :test #'equal))
         (bloom-structure--undo-notebooks nil)
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
            (bloom-library-refresh)
            (let* ((notebook-id
                    (plist-get
                     (car (plist-get (bloom-library-snapshot) :notebooks))
                     :id))
                   (records
                    (plist-get (bloom-library-outline-refresh notebook-id)
                               :records))
                   (alpha (cl-find "Alpha" records
                                   :key (lambda (record)
                                          (plist-get record :title))
                                   :test #'equal))
                   (args (bloom-structure--identity-args
                          notebook-id alpha "move-down"))
                   (params '(:surface "app:bloom")))
              (should (eq (bloom-structure--on-mutate args params)
                          'accepted))
              (with-current-buffer (find-file-noselect file t)
                (org-with-wide-buffer
                 (goto-char (point-min))
                 (should (looking-at-p "\\* Beta"))))
              (should (jetpacs-org-structure-undo-summary bloom-owner))
              (should (eq (bloom-structure--on-undo nil params) 'accepted))
              (with-current-buffer (find-file-noselect file t)
                (org-with-wide-buffer
                 (goto-char (point-min))
                 (should (looking-at-p "\\* Alpha"))
                 (search-forward ":END:\n")
                 (insert "Local drift\n")))
              (should (eq (bloom-structure--on-mutate args params) 'stale))
              (let* ((fresh-records
                      (plist-get
                       (bloom-library-outline-refresh notebook-id) :records))
                     (fresh-alpha
                      (cl-find "Alpha" fresh-records
                               :key (lambda (record)
                                      (plist-get record :title))
                               :test #'equal))
                     (narrow-args
                      (bloom-structure--identity-args
                       notebook-id fresh-alpha "narrow")))
                (should (eq (bloom-structure--on-mutate
                             narrow-args params)
                            'accepted))
                (let ((slice
                       (bloom-structure-outline-slice
                        notebook-id fresh-records)))
                  (should (plist-get slice :focused))
                  (should (equal
                           (mapcar (lambda (record)
                                     (plist-get record :title))
                                   (plist-get slice :records))
                           '("Alpha" "Child"))))
                (should (eq (bloom-structure--on-widen
                             (list :notebook-id notebook-id) params)
                            'accepted))))))
      (jetpacs-org-structure-reset bloom-owner)
      (when-let* ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest bloom-primary-shell-and-settings-are-valid-and-whitelisted ()
  "Bloom is a standalone app with four destinations and safe settings."
  (should (equal (mapcar (lambda (row) (plist-get row :key))
                         bloom-navigation-destinations)
                 '("notebooks" "agenda" "search" "settings")))
  (let ((entry (cdr (assoc bloom-owner jetpacs-apps--registry))))
    (should (eq (plist-get entry :chrome) 'standalone))
    (should (equal (plist-get entry :home-route) "notebooks"))
    (should-not (plist-get entry :requires-extensions)))
  (should (jetpacs-check-profile
           (bloom-navigation-settings-screen nil) 'app))
  (let ((bloom-library-tree-view t)
        saved deferred reminder-sync)
    (cl-letf (((symbol-function 'bloom-navigation--save-setting)
               (lambda (symbol value) (setq saved (list symbol value))))
              ((symbol-function 'jetpacs-app-defer-refresh)
               (lambda (_params) (setq deferred t)))
              ((symbol-function 'bloom-reminders-schedule-sync)
               (lambda (&rest _) (setq reminder-sync t))))
      (should (eq (bloom-navigation--on-setting
                   '(:field "tree" :value :json-false) nil)
                  'accepted))
      (should (equal saved '(bloom-library-tree-view nil)))
      (should deferred)
      (setq saved nil deferred nil)
      (should (eq (bloom-navigation--on-setting
                   '(:field "agenda-days" :value 0) nil)
                  'rejected))
      (should-not saved)
      (should-not deferred)
      (should (eq (bloom-navigation--on-setting
                   '(:field "reminder-lead" :value 60) nil)
                  'accepted))
      (should (equal saved '(bloom-reminder-lead-minutes 60)))
      (should reminder-sync)
      (setq saved nil reminder-sync nil)
      (should (eq (bloom-navigation--on-setting
                   '(:field "reminder-lead" :value 61) nil)
                  'rejected))
      (should-not saved)
      (should-not reminder-sync)
      (should (eq (bloom-navigation--on-setting
                   '(:field "unknown" :value t) nil)
                  'rejected)))))

(ert-deftest bloom-org-item-time-is-the-foundation-formatter ()
  "Bloom's agenda clock text is the foundation's, so its cards and the
reminder projection can never disagree."
  (should (eq (symbol-function 'bloom-org--item-time)
              'jetpacs-org-reminders-item-hm))
  (should (equal (bloom-org--item-time " 9:05...... ") "09:05"))
  (should-not (bloom-org--item-time "all day")))

(ert-deftest bloom-lifecycle-is-symmetric ()
  "Loading bloom.el registers nothing, and bloom-unregister undoes
everything bloom-register did: the trusted launcher's two lifecycle
checks, run from the foundation's applet tooling."
  (require 'jetpacs-devtools)
  (load (expand-file-name "../jetpacs-applet-mcp/jetpacs-applet-tooling.el"
                          (file-name-directory
                           (locate-library "jetpacs-surfaces")))
        nil 'nomessage)
  (should-not (jetpacs-applet-tooling-load-recording-registrations
               (locate-library "bloom.el" t)))
  (let ((report (jetpacs-applet-tooling-check-lifecycle-symmetry "bloom")))
    (should (string-match-p "Status: SYMMETRIC" report))
    (should (string-match-p "Registered on entry: yes" report)))
  (should (assoc "bloom" jetpacs-apps--registry)))

(defun bloom-test--agenda-note (title &rest props)
  "Return an agenda note titled TITLE with PROPS over safe defaults."
  (append props
          (list :notebook-id "n" :heading-id (concat "h-" title) :file "tasks.org"
                :title title :tags nil :active nil
                :active-keywords '("TODO"))))

(ert-deftest bloom-agenda-timestamps-and-day-labels-follow-the-app ()
  "Stamps keep time, range and repeater; days read as the app names them."
  (should (equal (bloom-agenda-parse-timestamp "<2026-09-05 Sat 09:00-11:00 +1w>")
                 '(:date "2026-09-05" :time "09:00" :end "11:00" :repeater "+1w"
                   :range-end nil)))
  (should (equal (plist-get (bloom-agenda-parse-timestamp "<2026-10-27 Tue 9:05>") :time)
                 "09:05"))
  (should-not (bloom-agenda-parse-timestamp "no stamp"))
  (should (equal (bloom-agenda-day-label "2026-10-23" "2026-10-23") "Today"))
  (should (equal (bloom-agenda-day-label "2026-10-24" "2026-10-23") "Tomorrow"))
  (should (equal (bloom-agenda-day-label "2026-10-22" "2026-10-23") "Yesterday"))
  (should (equal (bloom-agenda-day-label "2026-10-27" "2026-10-23") "Tuesday"))
  (should (equal (bloom-agenda-day-label "2026-11-01" "2026-10-23")
                 "Sunday, Nov 1")))

(ert-deftest bloom-agenda-state-buckets-today-overdue-and-upcoming ()
  "One day per plan, overdue oldest first, the window, and the app's meta."
  (let* ((bloom-agenda--tab 'today)
         (bloom-agenda-grouping-today "date") (bloom-agenda-grouping-upcoming "date")
         (bloom-agenda-filter-today "open") (bloom-agenda-filter-upcoming "open")
         (bloom-agenda-show-tags t) (bloom-agenda-show-file nil)
         (bloom-agenda-show-timestamps nil)
         (notes
          (list
           (bloom-test--agenda-note
            "Deck" :keyword "TODO" :priority "A" :tags '("work" "planning")
            :scheduled (bloom-agenda-parse-timestamp "<2026-10-23 Fri>"))
           (bloom-test--agenda-note
            "Vendor" :deadline (bloom-agenda-parse-timestamp "<2026-10-21 Wed>"))
           (bloom-test--agenda-note
            "Belcanto" :keyword "TODO"
            :scheduled (bloom-agenda-parse-timestamp "<2026-09-05 Sat 09:00-11:00 +1w>")
            :deadline (bloom-agenda-parse-timestamp "<2026-09-26 Sat>"))
           (bloom-test--agenda-note
            "Cabin" :keyword "TODO"
            :scheduled (bloom-agenda-parse-timestamp "<2026-10-24 Sat>"))
           (bloom-test--agenda-note
            "Far" :keyword "TODO"
            :scheduled (bloom-agenda-parse-timestamp "<2026-12-24 Thu>"))
           (bloom-test--agenda-note
            "Expenses" :keyword "DONE" :done t
            :scheduled (bloom-agenda-parse-timestamp "<2026-10-23 Fri>"))))
         (state (bloom-agenda-state notes "2026-10-23")))
    (should (equal (plist-get state :day) "Friday"))
    (should (equal (plist-get state :date) "October 23"))
    (should (= (plist-get state :today-count) 1))
    (should (equal (mapcar (lambda (n) (plist-get n :title)) (plist-get state :overdue))
                   '("Belcanto" "Vendor")))
    (let ((section (car (plist-get state :sections))))
      (should (equal (plist-get section :label) "Scheduled today"))
      (should (equal (plist-get (car (plist-get section :rows)) :meta)
                     '((":work:planning:" tag nil)))))
    (should (equal (bloom-agenda--meta (nth 2 notes) "2026-10-23" t)
                   '(("Sep 5 · 48d late" danger nil) ("09:00–11:00" normal nil)
                     ("⚑ Sep 26" danger nil) ("↻ +1w" muted nil))))
    (should (equal (bloom-agenda--meta (nth 1 notes) "2026-10-23" t)
                   '(("⚑ Oct 21 · 2d late" danger nil))))
    (let* ((bloom-agenda--tab 'upcoming)
           (upcoming (bloom-agenda-state notes "2026-10-23")))
      (should (equal (mapcar (lambda (s) (plist-get s :label))
                             (plist-get upcoming :sections))
                     '("Tomorrow"))))))

(ert-deftest bloom-agenda-screen-is-deterministic-and-profile-valid ()
  "The parity Agenda renders the same tree twice and carries its parts."
  (let ((bloom-agenda--tab 'today) (bloom-agenda--levers-open t)
        (bloom-agenda--overdue-open t))
    (cl-letf (((symbol-function 'bloom-agenda-notes)
               (lambda ()
                 (list (bloom-test--agenda-note
                        "Deck" :keyword "TODO" :priority "A"
                        :scheduled (list :date (format-time-string "%F")))
                       (bloom-test--agenda-note
                        "Old" :keyword "TODO"
                        :scheduled (list :date "2020-01-01"))))))
      (let ((first (jetpacs-node->canonical-json
                    (bloom-agenda-screen (jetpacs-view-switch "notebooks")))))
        (should (string-search "bloom.agenda.overdue" first))
        (should (string-search "Move to today" first))
        (should (string-search "SCHEDULED TODAY" first))
        (should (string-search "GROUP BY" first))
        (should (string-search "bloom.agenda.toggle" first))
        (should (equal first (jetpacs-node->canonical-json
                              (bloom-agenda-screen
                               (jetpacs-view-switch "notebooks")))))))))

(ert-deftest bloom-search-picks-the-native-engine-by-query-shape ()
  "Lisp forms go to org-ql, Org match syntax to the matcher, words to text."
  (cl-letf (((symbol-function 'bloom-library-org-ql-available-p) (lambda () t)))
    (should (equal (bloom-search-mode-for "(and (todo) (tags \"work\"))") "ql")))
  (cl-letf (((symbol-function 'bloom-library-org-ql-available-p) #'ignore))
    (should (equal (bloom-search-mode-for "(and (todo))") "text")))
  (dolist (query '("+work/!" "/!" "SCHEDULED<\"<today>\"" "work+urgent" "-home"
                   "PRIORITY=\"A\""))
    (should (equal (bloom-search-mode-for query) "match")))
  (dolist (query '("roadmap" "phone call" "{reg.*exp}"))
    (should (equal (bloom-search-mode-for query) "text"))))

(ert-deftest bloom-search-quick-cards-are-org-matches-with-the-apps-counts ()
  "On the parity vault the four cards count as the Android app does."
  (let* ((vault (expand-file-name "../fixtures/parity-vault" bloom-test--root))
         (org-agenda-files (list vault (expand-file-name "projects" vault))))
    (cl-letf (((symbol-function 'bloom-search--done-regexp)
               (lambda () "^\\(DONE\\)$"))
              ((symbol-function 'current-time)
               (lambda () (encode-time 0 0 12 23 10 2026))))
      (dolist (card (bloom-search-quick-cards))
        (should (equal (bloom-search-mode-for (plist-get card :query)) "match"))
        (let ((hits (org-map-entries (lambda () t) (plist-get card :query) 'agenda)))
          (should (= (length hits)
                     (pcase (plist-get card :key)
                       ("overdue" 4) ("today" 1) ("open" 20) ("unscheduled" 13)))))))))

(ert-deftest bloom-search-screen-is-deterministic-and-profile-valid ()
  "The parity Search screen renders twice alike, blank and with results."
  (cl-letf (((symbol-function 'bloom-search--count) (lambda (_) 3))
            ((symbol-function 'bloom-views-snapshot)
             (lambda () (list :views (list (list :id "v1" :label "All TODO"
                                                 :mode "match" :query "/!"
                                                 :source "bloom")))))
            ((symbol-function 'bloom-search--records)
             (lambda () (make-hash-table :test #'equal))))
    (let ((bloom-library--search-snapshot (list :query "" :mode "text")))
      (let ((first (jetpacs-node->canonical-json
                    (bloom-search-screen (jetpacs-view-switch "notebooks")))))
        (should (string-search "QUICK START" first))
        (should (string-search "3 past their date" first))
        (should (string-search "SAVED SEARCHES" first))
        (should (equal first (jetpacs-node->canonical-json
                              (bloom-search-screen
                               (jetpacs-view-switch "notebooks")))))))
    (let ((bloom-library--search-snapshot
           (list :query "roadmap" :mode "text"
                 :results (list (list :notebook-id "n1" :heading-id "h1"
                                      :headline "Finish Q3 roadmap deck"
                                      :todo "TODO" :priority "A" :tags '("work")
                                      :notebook-label "tasks")))))
      (let* ((screen (bloom-search-screen (jetpacs-view-switch "notebooks")))
             (json (jetpacs-node->canonical-json screen)))
        (should (string-search "1 result " json))
        (should (string-search " 1 notebook" json))
        ;; The query word is its own highlighted span inside the title.
        (should (string-search "\"text\":\"Finish Q3 \"" json))
        (should (string-search "\"text\":\"roadmap\"" json))
        (should (string-search "rich_text" json))
        (should (string-search "[#A]" json))
        (should (jetpacs-check-profile screen 'app))))))

(ert-deftest bloom-screen-scopes-stay-within-the-receivers-style-cap ()
  "Profile styles plus a screen's own stay at or under 256 in effect."
  (let ((profile (length (plist-get (bloom-design-light-profile) :styles))))
    (should (<= (+ profile (length (bloom-design-agenda-styles))) 256))
    (should (<= (+ profile (length (bloom-design-search-styles)) 2) 256))))

(ert-deftest bloom-settings-register-beside-the-jetpacs-settings-hub ()
  "Bloom joins the settings hub and registry, and leaves both on teardown."
  (let ((jetpacs-settings-hub-entries nil) (jetpacs-settings-registry nil))
    (bloom-settings-register)
    (should (assoc "bloom" jetpacs-settings-hub-entries))
    (should (equal (plist-get (cdr (assoc "bloom" jetpacs-settings-hub-entries)) :title)
                   "Bloom"))
    (let ((section (cdr (assoc "Bloom" jetpacs-settings-registry))))
      (should (assq 'bloom-agenda-show-tags section))
      (should (assq 'bloom-reminders-enabled section))
      (should-not (assq 'jetpacs-design-active-profile-id section)))
    (bloom-settings-unregister)
    (should-not (assoc "bloom" jetpacs-settings-hub-entries))
    (should-not (assoc "Bloom" jetpacs-settings-registry))))

(ert-deftest bloom-settings-change-only-touches-blooms-options ()
  "The change action applies Bloom's options and refuses everything else."
  (let ((bloom-agenda-show-file nil) applied)
    (cl-letf (((symbol-function 'jetpacs-settings-apply)
               (lambda (symbol value &optional _after)
                 (push (cons symbol value) applied) (set symbol value) t))
              ((symbol-function 'jetpacs-app-defer-refresh) #'ignore))
      (should (eq (bloom-settings-on-change
                   '(:name "bloom-agenda-show-file" :value "t") nil)
                  'accepted))
      (should (equal applied '((bloom-agenda-show-file . t))))
      (should (eq (bloom-settings-on-change
                   '(:name "org-directory" :value "\"/tmp\"") nil)
                  'rejected))
      (should (eq (bloom-settings-on-change
                   '(:name "jetpacs-design-active-profile-id" :value "\"nope\"") nil)
                  'rejected))
      (should (= (length applied) 1)))))

(ert-deftest bloom-settings-screens-are-deterministic-and-profile-valid ()
  "The root and every page render twice alike and pass the app profile."
  (let ((root (bloom-settings-root-screen (jetpacs-view-switch "notebooks"))))
    (should (jetpacs-check-profile root 'app))
    (let ((json (jetpacs-node->canonical-json root)))
      (should (string-search "Look and Feel" json))
      (should (string-search "bloom.settings.page" json))
      (should (equal json (jetpacs-node->canonical-json
                           (bloom-settings-root-screen
                            (jetpacs-view-switch "notebooks")))))))
  (dolist (page (bloom-settings-pages))
    (let ((screen (bloom-settings-page-screen (plist-get page :key)
                                              (jetpacs-view-switch "notebooks"))))
      (should (jetpacs-check-profile screen 'app))
      (should (string-search (plist-get page :title)
                             (jetpacs-node->canonical-json screen))))))

(ert-deftest bloom-edit-inline-tokens-follow-the-apps-claiming-order ()
  "Links and timestamps claim first; emphasis needs Org's boundaries."
  (should (equal (bloom-edit-inline-tokens "see *bold* and /it/ now")
                 '((4 10 bold) (15 19 italic))))
  (should (equal (bloom-edit-inline-tokens "a [[https://x.org][*x*]] b")
                 '((2 24 link))))
  (should (equal (bloom-edit-inline-tokens "on <2026-08-30 Sun> ~c~ =v=")
                 '((3 19 timestamp) (20 23 code) (24 27 code))))
  ;; No boundary before, whitespace inside the marker, or a word after.
  (should-not (bloom-edit-inline-tokens "a*b* * c * *d*e"))
  (should (equal (bloom-edit-inline-tokens "call 833-806-1627.")
                 '((5 17 link))))
  (should-not (bloom-edit-inline-tokens "id 1833-806-16278")))

(ert-deftest bloom-edit-line-styles-port-the-apps-highlighter ()
  "Headline, planning, drawer, and property lines style as the app's do."
  (let ((keywords '("TODO" "NEXT" "DONE")) (done '("DONE")))
    (let ((styles (bloom-edit-line-styles "** TODO Book it  :a:b:" keywords done)))
      (should (equal (aref styles 0) '(star . 2)))
      (should (equal (aref styles 1) '(star . 2)))
      (should-not (aref styles 2))
      (should (eq (aref styles 3) 'todo))
      (should (eq (aref styles 8) 'headline))
      (should (eq (aref styles 15) 'tag))
      (should (eq (aref styles 21) 'tag)))
    (let ((styles (bloom-edit-line-styles "* DONE x" keywords done)))
      (should (eq (aref styles 2) 'done)))
    (let ((styles (bloom-edit-line-styles
                   "SCHEDULED: <2026-09-05 Sat>" keywords done)))
      (should (eq (aref styles 0) 'planning-keyword))
      (should (eq (aref styles 10) 'planning))
      (should (eq (aref styles 11) 'timestamp)))
    (should (cl-every (lambda (key) (eq key 'property))
                      (bloom-edit-line-styles ":PROPERTIES:" keywords done)))
    (let ((styles (bloom-edit-line-styles ":CREATED: [2026-08-02 Sun]"
                                          keywords done)))
      (should (eq (aref styles 8) 'property-key))
      (should (eq (aref styles 9) 'property)))))

(ert-deftest bloom-edit-runs-are-sorted-roles-with-heading-levels ()
  "Runs merge per role, carry star depth, and skip plain text."
  (let ((runs (bloom-edit-text-runs "* A\nplain *b*\n" '("TODO") nil)))
    (should (equal runs
                   '((:start 0 :end 1 :role "heading" :level 1)
                     (:start 2 :end 3 :role "function")
                     (:start 10 :end 13 :role "constant"))))
    (let ((last -1))
      (dolist (run runs)
        (should (>= (plist-get run :start) last))
        (setq last (plist-get run :end))))))

(ert-deftest bloom-edit-profile-binds-every-syntax-slot ()
  "Each preset styles all fifteen roles and eight heading depths."
  (let* ((profile (bloom-design-light-profile))
         (slots (mapcar #'car (plist-get profile :component-styles))))
    (dolist (slot jetpacs-design-component-style-slots)
      (when (string-prefix-p "editor.syntax." slot)
        (should (member slot slots))))
    (should (assoc "bloom.syn.heading-7" (plist-get profile :styles)))))

(ert-deftest bloom-edit-screen-attaches-once-and-autosaves ()
  "A rebuild reuses the attached session; a change saves after the delay."
  (let* ((bloom-edit--buffer nil)
         (attached nil) (writes nil) (refreshes 0)
         (info (list :notebook-id "n" :heading-id "h" :headline "Trip"
                     :content "* Trip\nBody.\n"
                     :revision (secure-hash 'sha256 "* Trip\nBody.\n"))))
    (cl-letf (((symbol-function 'jetpacs-client) (lambda () 'client))
              ((symbol-function 'ebp-sync-attached-buffer)
               (lambda (&rest _) (car attached)))
              ((symbol-function 'ebp-sync-attach)
               (lambda (_client _doc _eid buffer) (push buffer attached)))
              ((symbol-function 'ebp-sync-detach) #'ignore)
              ((symbol-function 'jetpacs-chrome-defer-current-view-refresh)
               (lambda (&rest _) (cl-incf refreshes)))
              ((symbol-function 'bloom-document-write-subtree)
               (lambda (_n _h revision value) (push (cons revision value) writes) "h2")))
      (unwind-protect
          (let ((first (jetpacs-node->canonical-json
                        (bloom-edit-screen info (jetpacs-view-switch "outline")))))
            (should (= (length attached) 1))
            (should (string-search "editor.syntax" first))
            (should (string-search "\"chromeless\":true" first))
            (should (string-search "bloom.edit-surface" first))
            (should-not (string-search "\"on_save\"" first))
            (should (equal first (jetpacs-node->canonical-json
                                  (bloom-edit-screen
                                   info (jetpacs-view-switch "outline")))))
            (should (= (length attached) 1))
            (with-current-buffer bloom-edit--buffer
              (should (equal (bloom-edit-fontify-runs)
                             '((:start 0 :end 1 :role "heading" :level 1)
                               (:start 2 :end 6 :role "function"))))
              (goto-char (point-max))
              (insert "More.\n"))
            (should (= refreshes 1))
            (should (bloom-edit-save))
            (should (equal writes
                           (list (cons (plist-get info :revision)
                                       "* Trip\nBody.\nMore.\n"))))
            (should-not (bloom-edit-save))
            (should (equal (buffer-local-value 'bloom-edit--heading-id
                                               bloom-edit--buffer)
                           "h2")))
        (when (buffer-live-p bloom-edit--buffer)
          (kill-buffer bloom-edit--buffer))))))


(provide 'bloom-test)
;;; bloom-test.el ends here

(ert-deftest bloom-library-row-facts-match-the-android-app ()
  "Monogram colors, note counts, and ages are the app's own derivations."
  ;; Java `String.hashCode' of the vault-relative path, modulo six.
  (should (equal (mapcar #'bloom-library-palette-key
                         '("archive.org" "inbox.org" "journal.org"
                           "reading-list.org" "tasks.org" "travel.org"
                           "projects" "projects/blog.org" "projects/life.org"))
                 '("amber" "green" "red" "neutral" "accent" "green"
                   "green" "red" "blue")))
  (let ((file (make-temp-file "bloom-notes" nil ".org"
                              (concat "#+TITLE: T\n\n"
                                      ":PROPERTIES:\n:ID: x\n:END:\n"
                                      "An intro line.\n* One\n** Child\n* Two\n"))))
    (unwind-protect
        (should (= (bloom-library--note-count file) 3))
      (delete-file file)))
  (let ((now bloom-test--now))
    (should (equal (bloom-library-relative-time (time-subtract now 30) now)
                   "0 minutes ago"))
    (should (equal (bloom-library-relative-time (time-subtract now 60) now)
                   "1 minute ago"))
    (should (equal (bloom-library-relative-time (time-subtract now 7200) now)
                   "2 hours ago"))
    (should (equal (bloom-library-relative-time (time-subtract now 90000) now)
                   "Yesterday"))
    (should (equal (bloom-library-relative-time
                    (time-subtract now (* 3 86400)) now)
                   "3 days ago"))
    (should (equal (bloom-library-relative-time
                    (time-subtract now (* 20 86400)) now)
                   "Aug 12, 2026"))))

(ert-deftest bloom-parity-faces-carry-the-apps-exact-metrics ()
  "Every preset declares the parity faces with the app's line heights."
  (dolist (profile (list (bloom-design-profile) (bloom-design-light-profile)
                         (bloom-design-dark-profile)))
    (should (bloom-design-parity-available-p profile)))
  (let* ((styles (plist-get (bloom-design-light-profile) :styles))
         (title (cdr (assoc "bloom.t.row-title" styles)))
         (props (plist-get title :properties)))
    ;; 15 sp at the app's inherited 1.65 em: half the trimmed leading
    ;; restored above and below, and one line box as a minimum height.
    (should (equal (plist-get (plist-get props :line_height) :value) "24.75"))
    (should (equal (plist-get (plist-get props :padding_top) :value) "2.625"))
    (should (equal (plist-get (plist-get props :min_height) :value) "24.75"))
    (should (equal (plist-get (plist-get props :padding_bottom) :value)
                   "2.625")))
  ;; Folder-tile hairlines carry 40 percent alpha in the literal presets.
  (let ((tokens (plist-get (bloom-design-light-profile) :tokens)))
    (should (equal (plist-get (cdr (assoc "color.edge-green" tokens)) :value)
                   "#663F8A8F"))))

(ert-deftest bloom-notebook-tree-expands-in-place-and-stays-deterministic ()
  "Folders expand under their row; the parity root is repeatable."
  (let* ((folder "bloom-folder-projects")
         (bloom-library-expanded-folder-ids nil)
         (bloom-library--snapshot
          `(:notebooks
            ((:id "root-note" :folder-id nil :label "inbox" :file-label "inbox.org"
                  :folder-label "Notebooks" :favorite nil :palette "green"
                  :monogram "I" :note-count 5 :modified-label "1 minute ago")
             (:id "child-note" :folder-id ,folder :label "blog"
                  :file-label "blog.org" :folder-label "projects" :favorite nil
                  :palette "red" :monogram "B" :note-count 3
                  :modified-label "2 days ago"))
            :folders ((:id ,folder :parent-id nil :label "projects"
                           :path-label "projects" :file-count 1 :folder-count 0
                           :depth 1 :palette "green" :expanded nil))
            :total-count 2 :truncated nil :error nil)))
    (cl-letf (((symbol-function 'jetpacs-extension-advertised-p)
               (lambda (extension target)
                 (and (equal extension "jetpacs.design") (eq target :app))))
              ((symbol-function 'bloom-library-resolve-folder)
               (lambda (id) (and (equal id folder) "/tmp/projects/")))
              ((symbol-function 'jetpacs-app-defer-refresh) #'ignore))
      (let ((collapsed (jetpacs-node->canonical-json
                        (bloom-navigation-presented-notebooks-screen nil))))
        (should (string-search "inbox.org" collapsed))
        (should-not (string-search "blog.org" collapsed))
        (should (string-search "5 notes " collapsed))
        (should (string-search " 1 minute ago" collapsed))
        (should (equal collapsed
                       (jetpacs-node->canonical-json
                        (bloom-navigation-presented-notebooks-screen nil)))))
      (should (eq (bloom-navigation--on-folder-toggle
                   (list :folder-id folder) nil)
                  'accepted))
      (let ((expanded (jetpacs-node->canonical-json
                       (bloom-navigation-presented-notebooks-screen nil))))
        (should (string-search "blog.org" expanded))
        (should (string-search "bloom.rail" expanded)))
      (should (eq (bloom-navigation--on-folder-toggle
                   (list :folder-id "bloom-folder-gone") nil)
                  'stale)))))

(ert-deftest bloom-parity-outline-folds-in-place-and-stays-deterministic ()
  "The parity outline starts folded, unfolds by heading, and is repeatable."
  (let* ((notebook "bloom-notebook-travel")
         (bloom-navigation--notebook-id notebook)
         (bloom-navigation--outline-expanded (make-hash-table :test #'equal))
         (bloom-navigation--preface-open (make-hash-table :test #'equal))
         (bloom-library--outlines (make-hash-table :test #'equal))
         (bloom-library--snapshot
          `(:notebooks ((:id ,notebook :folder-id nil :label "travel"
                             :file-label "travel.org" :folder-label "Notebooks"))
            :folders nil :total-count 1 :truncated nil :error nil)))
    (puthash notebook
             `(:notebook-id ,notebook :label "travel"
               :keywords (("#+TITLE:" . "Travel"))
               :intro "Trips, bookings, and packing lists."
               :records
               ((:id "h1" :parent-id nil :level 1 :title "Portugal trip"
                     :tags ("travel") :lines "Two weeks in September.")
                (:id "h2" :parent-id "h1" :level 2 :title "Book Lisbon apartment"
                     :todo "TODO" :priority "A" :tags ("travel" "lodging")
                     :deadline-label "Aug 30")))
             bloom-library--outlines)
    (cl-letf (((symbol-function 'jetpacs-extension-advertised-p)
               (lambda (extension target)
                 (and (equal extension "jetpacs.design") (eq target :app))))
              ((symbol-function 'jetpacs-app-defer-refresh) #'ignore))
      (let ((folded (jetpacs-node->canonical-json
                     (bloom-navigation-presented-outline-screen
                      (jetpacs-view-switch "notebooks")))))
        (should (string-search "Portugal trip" folded))
        (should-not (string-search "Book Lisbon apartment" folded))
        (should (string-search "PREFACE" folded))
        (should (string-search "\"icon\":\"arrow_back\"" folded))
        (should (string-search "bloom.outline.menu" folded))
        (should (equal folded
                       (jetpacs-node->canonical-json
                        (bloom-navigation-presented-outline-screen
                         (jetpacs-view-switch "notebooks"))))))
      (should (eq (bloom-navigation--on-outline-toggle '(:heading-id "h1") nil)
                  'accepted))
      (let ((open (jetpacs-node->canonical-json
                   (bloom-navigation-presented-outline-screen
                    (jetpacs-view-switch "notebooks")))))
        (should (string-search "Book Lisbon apartment" open))
        (should (string-search "[#A]" open))
        (should (string-search "bloom.chip.warning" open))
        (should (string-search "Aug 30" open))))))

(ert-deftest bloom-grove-names-migrate ()
  "Former Grove options alias their Bloom names and stored ids migrate."
  (should (eq (indirect-variable 'grove-saved-views) 'bloom-saved-views))
  (should (eq (indirect-variable 'grove-library-favorite-ids)
              'bloom-library-favorite-ids))
  (should (equal (bloom-core-migrate-id "grove-notebook-0123") "bloom-notebook-0123"))
  (should (equal (bloom-core-migrate-id "bloom-folder-ab") "bloom-folder-ab"))
  (should-not (bloom-core-migrate-id nil)))

(ert-deftest bloom-library-first-open-expands-planned-folders-once ()
  "The first refresh with notebooks opens folders holding planned notes."
  (let* ((root (make-temp-file "bloom-tree" t))
         (org-directory root)
         (bloom-library-tree-defaults-applied nil)
         (bloom-library-expanded-folder-ids nil))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "plans" root))
          (make-directory (expand-file-name "notes" root))
          (with-temp-file (expand-file-name "plans/work.org" root)
            (insert "* TODO Ship\nSCHEDULED: <2026-10-24 Sat>\n"))
          (with-temp-file (expand-file-name "notes/idea.org" root)
            (insert "* Idea\n"))
          (cl-letf (((symbol-function 'jetpacs-org-mode-agenda-scope)
                     (lambda ()
                       (list (expand-file-name "plans/work.org" root)
                             (expand-file-name "notes/idea.org" root))))
                    ((symbol-function 'customize-save-variable) #'ignore))
            (should (bloom-library-refresh))
            (let ((folders (plist-get bloom-library--snapshot :folders)))
              (should (plist-get (cl-find "plans" folders
                                          :key (lambda (f) (plist-get f :label))
                                          :test #'equal)
                                 :expanded))
              (should-not (plist-get (cl-find "notes" folders
                                              :key (lambda (f) (plist-get f :label))
                                              :test #'equal)
                                     :expanded)))
            (should bloom-library-tree-defaults-applied)
            ;; Later refreshes leave the user's choice alone.
            (setq bloom-library-expanded-folder-ids nil)
            (bloom-library-refresh)
            (should-not bloom-library-expanded-folder-ids)))
      (delete-directory root t)
      (bloom-library-reset))))

(ert-deftest bloom-read-parser-matches-the-apps-structure ()
  "Headings keep planning, drawers and blocks as the app's parser does."
  (let* ((content (concat "* Portugal trip :travel:\n"
                          ":PROPERTIES:\n:CREATED: [2026-08-02 Sun]\n:END:\n"
                          "Two weeks away.\nStill planning.\n\n"
                          "** TODO [#A] Book flat :travel:lodging:\n"
                          "DEADLINE: <2026-08-30 Sun> SCHEDULED: <2026-08-20 Thu 09:00>\n"
                          ":LOGBOOK:\n- Note taken\n:END:\n"
                          "- [ ] Passport\n- [X] Sunscreen\n  in the bag\n1. one\n2. two\n"
                          "#+CAPTION: Fig\n#+BEGIN_SRC sh\necho hi\n#+END_SRC\n"
                          "#+TBLFM: x\n| a | b |\n"
                          "** NEXT Rent a car\n"))
         (headings (bloom-library-parse-subtree
                    content '("TODO" "DONE") '("DONE")
                    (encode-time (list 0 0 12 23 10 2026))))
         (own (nth 0 headings)) (flat (nth 1 headings)) (car-row (nth 2 headings)))
    (should (= (length headings) 3))
    (should (equal (plist-get own :tags) '("travel")))
    (should (equal (plist-get own :properties) '((":CREATED:" . "[2026-08-02 Sun]"))))
    (should (equal (plist-get (car (plist-get own :blocks)) :lines)
                   '("Two weeks away." "Still planning.")))
    (should (equal (plist-get flat :todo) "TODO"))
    (should (equal (plist-get flat :priority) "A"))
    (should (equal (plist-get flat :title) "Book flat"))
    (should (equal (plist-get flat :deadline) "Aug 30"))
    (should (equal (plist-get flat :scheduled) "Aug 20 09:00"))
    (should (equal (plist-get flat :logbook) '("- Note taken")))
    (let ((blocks (plist-get flat :blocks)))
      (should (equal (mapcar (lambda (b) (plist-get b :type)) blocks)
                     '(list block block table)))
      (let ((items (plist-get (nth 0 blocks) :items)))
        (should (equal (mapcar (lambda (i) (plist-get i :checkbox)) items)
                       '(" " "X" nil nil)))
        (should (equal (plist-get (nth 1 items) :text) "Sunscreen in the bag"))
        (should (equal (plist-get (nth 3 items) :ordinal) 2)))
      (should (equal (plist-get (nth 1 blocks) :kind) "SRC"))
      (should (equal (plist-get (nth 1 blocks) :language) "sh"))
      (should (equal (plist-get (nth 1 blocks) :affiliated) '("#+CAPTION: Fig")))
      (should (plist-get (nth 2 blocks) :keyword-run))
      (should (equal (plist-get (nth 2 blocks) :kind) "TBLFM")))
    ;; NEXT is not a configured keyword, so it stays in the title.
    (should-not (plist-get car-row :todo))
    (should (equal (plist-get car-row :title) "NEXT Rent a car"))))

(ert-deftest bloom-read-inline-markup-becomes-spans ()
  "Inline markup maps onto the app's span styles."
  (let ((spans (bloom-read-spans
                "A *bold* /it/ =code= see [[https://x.org][site]] <2026-10-23 Fri>.")))
    (should (cl-find 600 spans :key (lambda (s) (plist-get s :font_weight))
                     :test #'equal))
    (should (cl-find t spans :key (lambda (s) (plist-get s :italic))))
    (should (cl-find "site" spans :key (lambda (s) (plist-get s :text)) :test #'equal))
    (should (cl-find "<2026-10-23 Fri>" spans :key (lambda (s) (plist-get s :text))
                     :test #'equal))
    (should (equal (mapconcat (lambda (s) (plist-get s :text)) spans "")
                   "A bold it code see site <2026-10-23 Fri>."))))

(ert-deftest bloom-read-screen-is-deterministic-and-folds ()
  "The parity reader is repeatable and folds a heading's subtree."
  (let* ((bloom-read--folded (make-hash-table :test #'equal))
         (bloom-read--open (make-hash-table :test #'equal))
         (info (list :notebook-id "n" :heading-id "h" :headline "Trip"
                     :file-label "travel.org" :ancestors nil
                     :read (bloom-library-parse-subtree
                            "* Trip\nBody.\n** Child\nChild body.\n*** Leaf\n"
                            '("TODO" "DONE") '("DONE") (current-time)))))
    (cl-letf (((symbol-function 'jetpacs-app-defer-refresh) #'ignore))
      (let ((first (jetpacs-node->canonical-json
                    (bloom-read-screen info (jetpacs-view-switch "outline")))))
        (should (string-search "Child body." first))
        (should (string-search "Leaf" first))
        (should (string-search "bloom.t.read-title" first))
        (should (equal first (jetpacs-node->canonical-json
                              (bloom-read-screen info (jetpacs-view-switch "outline"))))))
      (should (eq (bloom-read-on-fold info '(:index 1) nil) 'accepted))
      (let ((folded (jetpacs-node->canonical-json
                     (bloom-read-screen info (jetpacs-view-switch "outline")))))
        (should-not (string-search "Child body." folded))
        (should-not (string-search "Leaf" folded))
        (should (string-search " 1" folded))))))
