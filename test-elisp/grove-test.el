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
  (let* ((buffer (generate-new-buffer " *grove-open-test*"))
         (marker (with-current-buffer buffer
                   (insert "* TODO Write project brief\n")
                   (copy-marker (point-min))))
         (ref (grove-test--ref "/tmp/work.org" 1
                               "Write project brief" "task-1"))
         opened toggled notified refreshed continuation)
    (unwind-protect
        (cl-letf (((symbol-function 'grove-org-ref-for-id)
                   (lambda (id) (and (equal id "grove-item-a") ref)))
                  ((symbol-function 'ebp-org-resolve-ref)
                   (lambda (_ref) (copy-marker marker)))
                  ((symbol-function 'jetpacs-flow-continue)
                   (lambda (fn) (setq continuation fn)))
                  ((symbol-function 'jetpacs-navigate-buffer)
                   (lambda (&rest args) (setq opened args) grove-app-surface))
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
          (should (functionp continuation))
          (funcall continuation)
          (should (equal (nth 1 opened) grove-app-surface))
          (should (= (nth 3 opened) 1))
          (should (eq (grove--on-item-done
                       '(:item-id "grove-item-a")
                       '(:surface "app:grove"))
                      'accepted))
          (should (equal toggled (list ref 'grove 'done)))
          (should refreshed)
          (should (equal notified '("Completed" "app:grove"))))
      (set-marker marker nil)
      (kill-buffer buffer))))

(ert-deftest grove-registration-is-symmetric ()
  "Register and unregister cover the app, actions, and both widget roots."
  (unwind-protect
      (progn
        (grove-register)
        (should (assoc grove-owner jetpacs-apps--registry))
        (dolist (name '("grove.agenda.refresh" "grove.agenda.select"
                        "grove.agenda.item.open" "grove.agenda.done"))
          (should (gethash name jetpacs-action-handlers)))
        (should (assoc grove-capture-widget-surface jetpacs-shell--roots))
        (should (assoc grove-agenda-widget-surface jetpacs-shell--roots))
        (grove-unregister)
        (should-not (assoc grove-owner jetpacs-apps--registry))
        (should-not (assoc grove-capture-widget-surface jetpacs-shell--roots))
        (should-not (assoc grove-agenda-widget-surface jetpacs-shell--roots))
        (dolist (name '("grove.agenda.refresh" "grove.agenda.select"
                        "grove.agenda.item.open" "grove.agenda.done"))
          (should-not (gethash name jetpacs-action-handlers))))
    (grove-register)))

(provide 'grove-test)
;;; grove-test.el ends here
