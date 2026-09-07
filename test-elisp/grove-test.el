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

(defun grove-test--library-snapshot ()
  "Return a populated Notebooks fixture without private paths."
  '(:notebooks
    ((:id "root-note" :folder-id nil :label "Inbox"
          :file-label "inbox.org" :folder-label "Notebooks"
          :favorite nil))
    :folders nil :total-count 1 :truncated nil :error nil))

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

(ert-deftest grove-org-planning-marks-are-bounded-date-only-counts ()
  "Planning scans one month but retains no Org identity or headline text."
  (let* ((private "/tmp/private-vault/secret.org")
         (first (grove-test--agenda-item
                 private 1 "Private A" "2026-09-03"))
         (second (grove-test--agenda-item
                  private 2 "Private B" "2026-09-08"))
         (outside (grove-test--agenda-item
                   private 3 "Outside" "2026-10-01"))
         called)
    (cl-letf (((symbol-function 'jetpacs-org-mode-agenda-items)
               (lambda (span start)
                 (setq called (list span start))
                 (list first (copy-tree first) (copy-tree first)
                       (copy-tree first) second outside))))
      (let ((marks (grove-org-planning-marks "2026-09")))
        (should (equal called '(month "2026-09-01")))
        (should (equal marks '(("2026-09-03" . 3)
                               ("2026-09-08" . 1))))
        (should-not (string-search private (format "%S" marks)))
        (should-not (string-search "Private" (format "%S" marks)))))))

(ert-deftest grove-agenda-builder-is-repeatable-over-explicit-state ()
  "The same snapshot and range produce byte-identical canonical screens."
  (let ((grove-org--snapshot (grove-test--snapshot))
        (grove-agenda-range "today"))
    (should
     (equal (jetpacs-node->canonical-json (grove-ui-screen nil))
            (jetpacs-node->canonical-json (grove-ui-screen nil))))))

(ert-deftest grove-refresh-pushes-widgets-only-when-granted ()
  "Snapshot refresh never attempts widget traffic the session declined."
  (let (pushed)
    (cl-letf (((symbol-function 'jetpacs-connected-p) (lambda () t))
              ((symbol-function 'jetpacs-shell-push)
               (lambda (surface &rest _)
                 (push surface pushed))))
      (cl-letf (((symbol-function 'jetpacs-granted-p) (lambda (&rest _) nil)))
        (grove--push-current-surfaces)
        (should (equal (nreverse pushed) (list grove-app-surface))))
      (setq pushed nil)
      (cl-letf (((symbol-function 'jetpacs-granted-p)
                 (lambda (capability &rest _)
                   (equal capability "surfaces.widget"))))
        (grove--push-current-surfaces)
        (should (equal (nreverse pushed)
                       (list grove-app-surface
                             grove-capture-widget-surface
                             grove-agenda-widget-surface)))))))

(ert-deftest grove-agenda-selects-deterministic-design-or-exact-material ()
  "Agenda follows the app extension profile without forking its data model."
  (let ((grove-org--snapshot (grove-test--snapshot))
        (grove-agenda-range "today")
        calls)
    (cl-letf (((symbol-function 'jetpacs-extension-advertised-p)
               (lambda (&rest args)
                 (push args calls)
                 nil)))
      (let ((first (grove-ui-presented-screen nil))
            (second (grove-ui-presented-screen nil))
            (material (grove-ui-screen nil)))
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
                      (eq target :app)))))
      (let* ((first (grove-ui-presented-screen nil))
             (second (grove-ui-presented-screen nil))
             (json (jetpacs-node->canonical-json first)))
        (should (equal (plist-get first :t) "jetpacs.design_scope"))
        (should (string-search "jetpacs.tabs" json))
        (should (string-search "\"t\":\"button\"" json))
        (should-not (string-search "jetpacs.action" json))
        (should-not (string-search "jetpacs.pressable" json))
        (should (string-search "\"kind\":\"theme-role\"" json))
        (should-not (string-match-p "#[[:xdigit:]]\\{6,8\\}" json))
        (should (string-search "grove.agenda.select" json))
        (should (string-search "grove-item-a" json))
        (should (jetpacs-check-profile first 'app))
        (should (equal json (jetpacs-node->canonical-json second)))
        (should (<= (jetpacs-node-wire-bytes first) 262144))))))

(ert-deftest grove-notebooks-selects-deterministic-design-or-exact-material ()
  "Notebooks keeps its Material tree as the exact unadvertised fallback."
  (let ((grove-library-tree-view t)
        (grove-library--snapshot (grove-test--library-snapshot))
        calls)
    (cl-letf (((symbol-function 'jetpacs-extension-advertised-p)
               (lambda (&rest args)
                 (push args calls)
                 nil)))
      (let ((first (grove-navigation-presented-notebooks-screen nil))
            (second (grove-navigation-presented-notebooks-screen nil))
            (material (grove-navigation-notebooks-screen nil)))
        (should (equal first material))
        (should (equal (jetpacs-node->canonical-json first)
                       (jetpacs-node->canonical-json second)))))
    (should (member '("jetpacs.design" :app) calls))
    (cl-letf (((symbol-function 'jetpacs-extension-advertised-p)
               (lambda (extension target)
                 (and (equal extension "jetpacs.design")
                      (eq target :app)))))
      (let* ((first (grove-navigation-presented-notebooks-screen nil))
             (second (grove-navigation-presented-notebooks-screen nil))
             (json (jetpacs-node->canonical-json first)))
        (should (equal (plist-get first :t) "jetpacs.design_scope"))
        (should (string-search "\"t\":\"button\"" json))
        (should (string-search "\"kind\":\"theme-role\"" json))
        (should-not (string-match-p "#[[:xdigit:]]\\{6,8\\}" json))
        (should (string-search "grove.notebook.open" json))
        (should (string-search "root-note" json))
        (should (jetpacs-check-profile first 'app))
        (should (equal json (jetpacs-node->canonical-json second)))
        (should (<= (jetpacs-node-wire-bytes first) 262144))))))

(ert-deftest grove-design-preset-is-complete-palette-neutral-and-bounded ()
  "Grove binds every semantic slot without replacing live theme colors."
  (let* ((profile (grove-design-profile))
         (tokens (plist-get profile :tokens))
         (bindings (plist-get profile :component-styles))
         (scope (jetpacs-design-profile-scope
                 profile (list (jetpacs-text "Grove")))))
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

(ert-deftest grove-design-light-and-dark-presets-carry-the-literal-palette ()
  "Grove Light and Grove Dark bind the app's verbatim colors and every slot."
  (dolist (pair `((,(grove-design-light-profile) "#8A5A2B" "#F3EDE1")
                  (,(grove-design-dark-profile) "#CB9D62" "#16130E")))
    (let* ((profile (car pair))
           (tokens (plist-get profile :tokens))
           (scope (jetpacs-design-profile-scope
                   profile (list (jetpacs-text "Grove"))))
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
                     (plist-get (grove-design-profile) :typography)))
      (should (<= (jetpacs-node-wire-bytes scope) 262144))))
  (should (equal (mapcar #'car
                         (seq-filter
                          (lambda (binding)
                            (string-prefix-p "text." (car binding)))
                          grove-design-component-styles))
                 '("text.body" "text.title" "text.headline" "text.caption"
                   "text.label" "text.mono"))))

(ert-deftest grove-design-registers-and-removes-all-three-presets ()
  "Registration exposes every preset to Design Lab and is fully reversible."
  (grove-design-register)
  (dolist (id grove-design-preset-ids)
    (should (jetpacs-design-profile-get id)))
  (should (equal (plist-get (jetpacs-design-profile-get "grove.light") :label)
                 "Grove Light"))
  (grove-design-unregister)
  (dolist (id grove-design-preset-ids)
    (should-not (jetpacs-design-profile-get id)))
  (grove-design-register))

(ert-deftest grove-design-reader-rebinds-body-text-to-serif-prose ()
  "The document screen nests a scope that rebinds only `text.body'."
  (let ((grove-document--current nil))
    (cl-letf (((symbol-function 'jetpacs-extension-advertised-p)
               (lambda (extension target)
                 (and (equal extension "jetpacs.design") (eq target :app)))))
      (let* ((screen (grove-document-presented-screen nil))
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
        (let ((settings (grove-navigation-presented-settings-screen nil)))
          (should-not (equal (plist-get (plist-get (aref (plist-get settings :children) 0)
                                                   :body)
                                        :t)
                             "jetpacs.design_scope"))))
      ;; A portable profile without reading prose leaves the body untouched.
      (let* ((id "grove-test-no-prose")
             (profile (jetpacs-design-make-profile
                       id "No prose" :tokens nil :typography nil
                       :styles nil :motions nil :component-styles nil)))
        (unwind-protect
            (progn
              (jetpacs-design-register-profile profile)
              (let* ((jetpacs-design-active-profile-id id)
                     (screen (grove-document-presented-screen nil))
                     (scaffold (aref (plist-get screen :children) 0)))
                (should-not (equal (plist-get (plist-get scaffold :body) :t)
                                   "jetpacs.design_scope"))))
          (jetpacs-design-unregister-profile id))))))

(ert-deftest grove-design-covers-every-app-screen-and-preserves-fallbacks ()
  "All seven Grove screen kinds share the runtime gate and exact fallback."
  (let ((grove-navigation--notebook-id nil)
        (grove-document--current nil)
        (grove-document--planning nil)
        (pairs
         '((grove-navigation-outline-screen
            grove-navigation-presented-outline-screen)
           (grove-navigation-search-screen
            grove-navigation-presented-search-screen)
           (grove-navigation-settings-screen
            grove-navigation-presented-settings-screen)
           (grove-document-screen grove-document-presented-screen)
           (grove-document-planning-screen
            grove-document-presented-planning-screen))))
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

(ert-deftest grove-design-accepts-a-portable-active-profile ()
  "A global profile need not define Grove-private style names."
  (let* ((id "grove-test-portable")
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
                      (grove-navigation-presented-settings-screen nil))))
                (should (string-search "portable.action" json))
                (should-not (string-search "radius.panel" json))
                (should-not (string-search "jetpacs.pressable" json))
                (should (string-search "\"t\":\"button\"" json))))))
      (jetpacs-design-unregister-profile id))))

(ert-deftest grove-agenda-semantic-tabs-inject-the-selected-value ()
  "The design Tabs event uses the same Agenda action as Material buttons."
  (let ((grove-agenda-range "today") deferred)
    (cl-letf (((symbol-function 'jetpacs-app-defer-refresh)
               (lambda (_params) (setq deferred t))))
      (should (eq (grove--on-select-range
                   '(:value "upcoming") '(:surface "app:grove"))
                  'accepted))
      (should (equal grove-agenda-range "upcoming"))
      (should deferred))))

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

(ert-deftest grove-rows-use-the-list-item-node-only-where-it-is-drawable ()
  "App rows take the Jetpacs row; widget rows keep the canonical one.
The node is declared for the app target only, so the widget projection
must degrade even offline, where the profile question has no answer."
  (let ((grove-org--snapshot (grove-test--snapshot))
        (grove-library--snapshot (grove-test--library-snapshot))
        (grove-agenda-range "today"))
    (let ((app (jetpacs-node->canonical-json (grove-ui-screen nil)))
          (widget (jetpacs-node->canonical-json
                   (grove-widgets-agenda-surface))))
      (should (string-search "jetpacs.list_item" app))
      (should-not (string-search "jetpacs.list_item" widget))
      ;; Both still carry the row's identity and its durable action.
      (should (string-search "grove-item-a" app))
      (should (string-search "grove-item-a" widget)))
    (should (string-search
             "jetpacs.list_item"
             (jetpacs-node->canonical-json
              (grove-navigation-notebooks-screen nil))))))

(ert-deftest grove-menus-are-named-and-bound-to-the-popup-face ()
  "Every Grove menu announces itself, and the menu slots reach a profile.

An un-annotated menu falls back to its icon identifier for an accessible
name, which announces as \"more_vert\"; the authored name is what stops
that.  The bindings are asserted separately because a menu drawn by the
Foundation renderer takes its whole appearance from them."
  (let ((menus (list (grove-structure-menu
                      "nb" '(:id "heading-1" :revision 3))
                     (grove-navigation--notebook-menu
                      (car (plist-get (grove-test--library-snapshot)
                                      :notebooks)))
                     (plist-get (grove-views-row
                                 '(:id "v1" :label "Today" :source "grove"))
                                :trailing))))
    (dolist (menu menus)
      (should (equal (plist-get menu :t) "menu"))
      (should (stringp (plist-get (plist-get menu :semantics) :name)))
      (should-not (equal (plist-get (plist-get menu :semantics) :name)
                         "more_vert"))))
  (dolist (slot '("menu.trigger" "menu.container" "menu.item"
                  "menu.item-label" "menu.item-supporting"
                  "menu.group-label"))
    (should (assoc slot grove-design-component-styles)))
  ;; The popup is a surface, not a row: it must not inherit the flat row.
  (should (equal (cdr (assoc "menu.container" grove-design-component-styles))
                 '("grove.popup")))
  ;; Menu labels match the app's own DropdownMenuItem scale.
  (should (equal (cdr (assoc "menu.item-label" grove-design-component-styles))
                 '("typography.label-large"))))

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
      ;; Hermetic tests have no live grant, so durable actions must queue.
      (should (equal (plist-get tap :when_offline) "queue"))
      (should (= (plist-get tap :ttl_s) 86400)))))

(ert-deftest grove-durable-actions-wake-only-when-granted ()
  "Grove remains profile-valid when a Companion cannot wake Emacs."
  (cl-letf (((symbol-function 'jetpacs-granted-p)
             (lambda (&rest _) nil)))
    (let ((action (grove-navigation--notebook-action "notebook-a")))
      (should (equal (plist-get action :when_offline) "queue"))
      (should-not (jetpacs-gate-descriptor-policy action))))
  (cl-letf (((symbol-function 'jetpacs-granted-p)
             (lambda (capability &optional _client)
               (equal capability "offline.wake"))))
    (let ((action (grove-navigation--notebook-action "notebook-a")))
      (should (equal (plist-get action :when_offline) "wake"))
      (should-not (jetpacs-gate-descriptor-policy action)))))

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
                              grove-views--verbs
                              grove-document--verbs
                              grove-reminders--verbs))
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
                              grove-views--verbs
                              grove-document--verbs
                              grove-reminders--verbs))
          (should-not (gethash name jetpacs-action-handlers))))
    (grove-register)))

(ert-deftest grove-reminders-project-opaque-ordered-elisp-actions ()
  "Grove authors Complete then Reschedule without leaking an Org path."
  (let* ((private "/tmp/private-vault/work plan.org")
         (candidate
          (list :ref (grove-test--ref private 42 "Ship it" "task-1")
                :headline "Ship it" :type "scheduled" :time "14:30"
                :event_at_ms 1788294600000 :at_ms 1788293700000
                :identity (concat private "|task-1|2026-09-02T14:30")))
         (grove-reminder-lead-minutes 15))
    (cl-letf (((symbol-function 'jetpacs-granted-p)
               (lambda (capability &optional _client)
                 (member capability '("reminders.actions" "offline.wake"))))
              ((symbol-function 'jetpacs-org-mode-reminder-candidates)
               (lambda (&rest _) (list candidate)))
              ((symbol-function 'grove-library-identify-heading)
               (lambda (_ref)
                 '(:notebook-id "grove-notebook-a"
                   :heading-id "grove-heading-a"))))
      (let* ((reminder (car (grove-reminders--projection)))
             (actions (plist-get reminder :actions))
             (complete (aref actions 0))
             (reschedule (aref actions 1))
             (json (jetpacs-node->canonical-json reminder)))
        (should (= (length actions) 2))
        (should (equal (plist-get complete :label) "Complete"))
        (should (equal (plist-get reschedule :label) "Reschedule"))
        (should (equal
                 (plist-get (plist-get reschedule :on_tap) :open_surface)
                 grove-app-surface))
        (should (equal
                 (plist-get (plist-get (plist-get reschedule :on_tap) :args)
                            :focus)
                 "scheduled"))
        (should (equal (plist-get (plist-get complete :on_tap) :action)
                       "grove.reminder.complete"))
        (should-not (plist-get complete :dismiss))
        (should-not (string-search private json))
        (should-not (string-search "work plan.org" json))))))

(ert-deftest grove-reminders-degrade-without-interaction-capability ()
  "An older peer receives the basic reminder schema and no app launch adjunct."
  (let ((candidate
         '(:ref (:file "/tmp/work.org" :pos 1 :headline "Task")
           :headline "Task" :type "deadline" :time "10:00"
           :event_at_ms 100000 :at_ms 100000 :identity "private")))
    (cl-letf (((symbol-function 'jetpacs-granted-p)
               (lambda (_capability &optional _client) nil))
              ((symbol-function 'jetpacs-org-mode-reminder-candidates)
               (lambda (&rest _) (list candidate)))
              ((symbol-function 'grove-library-identify-heading)
               (lambda (_ref)
                 '(:notebook-id "grove-notebook-a"
                   :heading-id "grove-heading-a"))))
      (let* ((reminder (car (grove-reminders--projection)))
             (tap (plist-get reminder :on_tap)))
        (should-not (plist-member reminder :actions))
        (should-not (plist-member tap :open_surface))
        (should (equal (plist-get tap :when_offline) "queue"))))))

(ert-deftest grove-reminder-handlers-own-org-semantics ()
  "Complete mutates Org; Reschedule reuses Grove's normal planning canvas."
  (let ((ref '(:file "/tmp/work.org" :pos 1 :headline "Task"))
        toggled refreshed planned)
    (cl-letf (((symbol-function 'grove-library-resolve-heading)
               (lambda (notebook heading)
                 (and (equal notebook "grove-notebook-a")
                      (equal heading "grove-heading-a") ref)))
              ((symbol-function 'ebp-org-toggle-todo)
               (lambda (&rest args) (setq toggled args)))
              ((symbol-function 'grove-library-reset-projections) #'ignore)
              ((symbol-function 'grove-library-refresh) #'ignore)
              ((symbol-function 'grove-org-refresh)
               (lambda (&optional _now) (setq refreshed t)))
              ((symbol-function 'grove-document-open-planning)
               (lambda (&rest args) (setq planned args) 'accepted)))
      (let ((args '(:owner "grove"
                    :reminder_id "grove-reminder-1234"
                    :notebook-id "grove-notebook-a"
                    :heading-id "grove-heading-a"
                    :focus "deadline")))
        (should (eq (grove-reminders--on-complete args nil) 'accepted))
        (should (equal toggled (list ref 'grove 'done)))
        (should refreshed)
        (should (eq (grove-reminders--on-reschedule args nil) 'accepted))
        (should (equal planned
                       '("grove-notebook-a" "grove-heading-a" nil
                         "deadline")))
        (should (eq
                 (grove-reminders--on-complete
                  (plist-put (copy-sequence args) :owner "other") nil)
                 'stale))))))

(ert-deftest grove-reminder-sync-preserves-confirmed-set-on-scan-failure ()
  "A failed Org scan never becomes authority to clear Room's durable set."
  (let ((grove-reminders-enabled t)
        (grove-reminders--last-reminders '((:id "grove-reminder-old")))
        (calls 0))
    (cl-letf (((symbol-function 'jetpacs-connected-p) (lambda () t))
              ((symbol-function 'jetpacs-granted-p) (lambda (&rest _) t))
              ((symbol-function 'jetpacs-error-label) (lambda (_) "error"))
              ((symbol-function 'grove-reminders--projection)
               (lambda () (error "private path")))
              ((symbol-function 'jetpacs-reminders-set)
               (lambda (&rest _) (cl-incf calls))))
      (grove-reminders--sync-now)
      (should (zerop calls))
      (should (equal grove-reminders--last-reminders
                     '((:id "grove-reminder-old")))))))

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

(ert-deftest grove-saved-views-combine-customize-and-read-only-emacs-views ()
  "Hybrid view rows stay bounded and custom agenda internals remain private."
  (let ((grove-saved-views '(("Needles" "text" "private phone query")))
        (grove-saved-view-limit 8)
        (grove-library--search-snapshot
         '(:query "private phone query" :mode "text" :results nil
                  :error nil)))
    (cl-letf (((symbol-function 'jetpacs-org-mode-custom-views)
               (lambda ()
                 '((:id "org-custom-view-safe" :label "Work agenda"
                        :kind "tags-todo")))))
      (let* ((snapshot (grove-views-snapshot))
             (views (plist-get snapshot :views))
             (phone (car views))
             (emacs (cadr views))
             (run (grove-views-run-action emacs))
             (json (jetpacs-node->canonical-json
                    (grove-navigation-search-screen nil))))
        (should (= (length views) 2))
        (should (equal (plist-get phone :source) "grove"))
        (should (equal (plist-get emacs :source) "emacs"))
        (should (equal (plist-get run :args)
                       '(:view-id "org-custom-view-safe")))
        (should (string-search "Work agenda" json))
        (should (string-search "Read-only" json))
        (should-not (string-search "org-agenda-custom-commands" json))
        (should (jetpacs-check-profile
                 (grove-navigation-search-screen nil) 'app))))))

(ert-deftest grove-saved-view-actions-resolve-live-config-and-persist ()
  "Phone views save/delete locally and both sources run by opaque identity."
  (let ((grove-saved-views nil)
        (grove-saved-view-limit 8)
        persisted phone-run custom-run deferred notified)
    (cl-letf (((symbol-function 'customize-save-variable)
               (lambda (_symbol value) (setq persisted (copy-tree value))))
              ((symbol-function 'jetpacs-org-mode-custom-views)
               (lambda ()
                 '((:id "org-custom-view-work" :label "Work"
                        :kind "tags"))))
              ((symbol-function 'grove-library-search)
               (lambda (&rest args) (setq phone-run args) t))
              ((symbol-function 'grove-library-custom-view-search)
               (lambda (&rest args) (setq custom-run args) t))
              ((symbol-function 'grove-library-clear-active-view)
               (lambda (_id) t))
              ((symbol-function 'jetpacs-app-defer-refresh)
               (lambda (_params) (setq deferred t)))
              ((symbol-function 'jetpacs-shell-notify)
               (lambda (&rest args) (setq notified args))))
      (let* ((phone (grove-views--add "Inbox" "match" "+inbox"))
             (phone-id (plist-get phone :id)))
        (should (equal persisted '(("Inbox" "match" "+inbox"))))
        (should-error (grove-views--add "inbox" "text" "duplicate")
                      :type 'user-error)
        (should (eq (grove-views--on-run
                     (list :view-id phone-id) '(:surface "app:grove"))
                    'accepted))
        (should (equal (seq-take phone-run 2) '("+inbox" "match")))
        (should (eq (grove-views--on-run
                     '(:view-id "org-custom-view-work")
                     '(:surface "app:grove"))
                    'accepted))
        (should (equal custom-run
                       '("org-custom-view-work" "Work" "tags")))
        (should (eq (grove-views--on-delete
                     (list :view-id phone-id) '(:surface "app:grove"))
                    'accepted))
        (should-not grove-saved-views)
        (should deferred)
        (should (equal notified '("Saved view deleted" "app:grove")))
        (should (eq (grove-views--on-delete
                     '(:view-id "org-custom-view-work")
                     '(:surface "app:grove"))
                    'rejected))))))

(ert-deftest grove-save-view-flow-captures-query-but-never-authors-it-in-action ()
  "Save uses field capture, validates natively, and prompts outside dispatch."
  (let ((grove-saved-views nil)
        continuation validated persisted searched)
    (cl-letf (((symbol-function 'jetpacs-flow-continue)
               (lambda (function) (setq continuation function)))
              ((symbol-function 'jetpacs-dialog-can-bridge-p) (lambda () t))
              ((symbol-function 'read-string) (lambda (&rest _) "My view"))
              ((symbol-function 'jetpacs-org-mode-search-items)
               (lambda (&rest args) (setq validated args) nil))
              ((symbol-function 'customize-save-variable)
               (lambda (_symbol value) (setq persisted (copy-tree value))))
              ((symbol-function 'grove-library-search)
               (lambda (&rest args) (setq searched args) t))
              ((symbol-function 'jetpacs-app-defer-refresh) #'ignore)
              ((symbol-function 'jetpacs-shell-notify) #'ignore))
      (let ((action (grove-views-save-action "text")))
        (should (equal (plist-get action :args) '(:mode "text")))
        (should-not (string-search "needle" (prin1-to-string action))))
      (should (eq (grove-views--on-save
                   '(:mode "text")
                   '(:surface "app:grove"
                     :fields (:grove-search-query "needle")))
                  'accepted))
      (should continuation)
      (funcall continuation)
      (should (equal validated '("needle" text 100)))
      (should (equal persisted '(("My view" "text" "needle"))))
      (should (equal (seq-take searched 2) '("needle" "text"))))))

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

(ert-deftest grove-planning-calendar-is-pure-styled-and-profile-valid ()
  "The combined calendar is a pure projection with a styled planning range."
  (let ((grove-document--planning
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
      (let* ((screen (grove-document-planning-screen nil))
             (json (jetpacs-node->canonical-json screen)))
        (should (jetpacs-check-profile screen 'app))
        (should (string-search "\"range_start\":\"2026-09-04\"" json))
        (should (string-search "\"range_end\":\"2026-09-08\"" json))
        (should (string-search "\"day_styles\"" json))
        (should (string-search "\"background\":\"primary\"" json))
        (should (string-search "\"background\":\"error\"" json))
        (should (string-search "grove.planning.focus" json))
        (should (string-search "grove.planning.month" json))
        (should (string-search "Clear both" json))
        (should (string-search "4 days between scheduled and deadline" json))))))

(ert-deftest grove-planning-calendar-degrades-day-styles-to-colored-marks ()
  "A constrained member profile gets marks instead of day_styles."
  (let ((grove-document--planning
         '(:notebook-id "notebook" :heading-id "heading" :revision "rev"
           :scheduled "2026-09-04" :deadline "2026-09-08"
           :focus "deadline" :today "2026-09-01" :month "2026-09"
           :marks nil)))
    (cl-letf (((symbol-function 'jetpacs-node-member-advertised-p)
               (lambda (&rest _) nil)))
      (let ((json (jetpacs-node->canonical-json
                   (grove-document-planning-screen nil))))
        (should-not (string-search "\"day_styles\"" json))
        (should (string-search "\"color\":\"primary\"" json))
        (should (string-search "\"color\":\"error\"" json))))))

(ert-deftest grove-planning-actions-preserve-captured-pair-and-validate-dates ()
  "Calendar transitions preserve local edits and reject malformed injection."
  (let ((grove-document--planning
         '(:notebook-id "notebook" :heading-id "heading" :revision "rev"
           :scheduled "<2026-09-04 Fri 09:00 +1w>"
           :deadline "" :focus "scheduled" :today "2026-09-01"
           :month "2026-09" :marks nil))
        mark-months refreshed)
    (cl-letf (((symbol-function 'grove-org-planning-marks)
               (lambda (month)
                 (push month mark-months)
                 (list (cons (concat month "-02") 1))))
              ((symbol-function 'jetpacs-app-defer-refresh)
               (lambda (&rest _) (setq refreshed t))))
      (should
       (eq (grove-document--on-planning-pick
            '(:field "scheduled" :kind "date" :value "2026-10-07")
            '(:fields (:grove-scheduled "<2026-09-04 Fri 09:00 +1w>"
                       :grove-deadline "<2026-10-11 Sun>")))
           'accepted))
      (should (equal (plist-get grove-document--planning :scheduled)
                     "<2026-10-07 09:00 +1w>"))
      (should (equal (plist-get grove-document--planning :deadline)
                     "<2026-10-11 Sun>"))
      (should (equal (plist-get grove-document--planning :month) "2026-10"))
      (should (equal mark-months '("2026-10")))
      (should refreshed)
      (setq refreshed nil)
      (should
       (eq (grove-document--on-planning-month
            '(:value "2026-11")
            '(:fields (:grove-scheduled "nov 2 09:00"
                       :grove-deadline "nov 9")))
           'accepted))
      (should (equal (plist-get grove-document--planning :scheduled)
                     "nov 2 09:00"))
      (should (equal (plist-get grove-document--planning :deadline) "nov 9"))
      (should (equal mark-months '("2026-11" "2026-10")))
      (let ((before (copy-tree grove-document--planning)))
        (should
         (eq (grove-document--on-planning-pick
              '(:field "deadline" :kind "date" :value "2026-02-30") nil)
             'rejected))
        (should (equal grove-document--planning before)))
      (should
       (eq (grove-document--on-planning-clear
            '(:field "both")
            '(:fields (:grove-scheduled "keep?" :grove-deadline "keep?")))
           'accepted))
      (should (equal (plist-get grove-document--planning :scheduled) ""))
      (should (equal (plist-get grove-document--planning :deadline) "")))))

(ert-deftest grove-planning-presets-and-colliding-styles-are-deterministic ()
  "Quick dates use calendar arithmetic and a same-day deadline wins color."
  (should (equal (grove-document--planning-preset-dates "2026-09-01")
                 '(("Today" . "2026-09-01")
                   ("Tomorrow" . "2026-09-02")
                   ("This weekend" . "2026-09-05")
                   ("Next week" . "2026-09-07"))))
  (let ((styles (grove-document--planning-day-styles
                 "2026-09-04" "2026-09-04")))
    (should (= (length styles) 1))
    (should (equal (plist-get (cdar styles) :background) "error"))))

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

(ert-deftest grove-outline-disclosure-and-reading-have-distinct-targets ()
  "Only parents disclose children; every heading retains a reading action."
  (let* ((grove-navigation--notebook-id "grove-notebook-test")
         (records '((:id "parent" :parent-id nil :title "Parent" :todo "TODO")
                    (:id "child" :parent-id "parent" :title "Child")
                    (:id "leaf" :parent-id nil :title "Leaf"))))
    (dolist (advertised '(nil t))
      (cl-letf (((symbol-function 'jetpacs-node-advertised-p)
                 (lambda (&rest _) advertised)))
        (let* ((rows (grove-navigation--outline-children records nil))
               (parent (car rows))
               (leaf (cadr rows))
               (header (plist-get parent :header))
               (json (jetpacs-node->canonical-json
                      (apply #'jetpacs-column rows))))
          (should (equal (plist-get parent :t) "collapsible"))
          (should-not (plist-get header :on_tap))
          (should (equal (plist-get (plist-get leaf :on_tap) :action)
                         "grove.document.open"))
          (should-not (string-search "chevron_right" json))
          (should-not (string-search "account_tree" json))
          (should (string-search "TODO · 1 section" (decode-coding-string json 'utf-8)))
          (should (string-search "Read" json))
          (should (equal json
                         (jetpacs-node->canonical-json
                          (apply #'jetpacs-column
                                 (grove-navigation--outline-children
                                  records nil))))))))))

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
    (should (equal (plist-get entry :home-route) "notebooks"))
    (should-not (plist-get entry :requires-extensions)))
  (should (jetpacs-check-profile
           (grove-navigation-settings-screen nil) 'app))
  (let ((grove-library-tree-view t)
        saved deferred reminder-sync)
    (cl-letf (((symbol-function 'grove-navigation--save-setting)
               (lambda (symbol value) (setq saved (list symbol value))))
              ((symbol-function 'jetpacs-app-defer-refresh)
               (lambda (_params) (setq deferred t)))
              ((symbol-function 'grove-reminders-schedule-sync)
               (lambda (&rest _) (setq reminder-sync t))))
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
                   '(:field "reminder-lead" :value 60) nil)
                  'accepted))
      (should (equal saved '(grove-reminder-lead-minutes 60)))
      (should reminder-sync)
      (setq saved nil reminder-sync nil)
      (should (eq (grove-navigation--on-setting
                   '(:field "reminder-lead" :value 61) nil)
                  'rejected))
      (should-not saved)
      (should-not reminder-sync)
      (should (eq (grove-navigation--on-setting
                   '(:field "unknown" :value t) nil)
                  'rejected)))))

(provide 'grove-test)
;;; grove-test.el ends here
