;;; ~/.config/doom/+org.el -*- lexical-binding: t; -*-

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(setq org-directory "~/org/")

;; add the project TODO files to the agenda as well
;; FIXME: this is an old relic before doom and I don't think it works anymore
;; (with-eval-after-load 'org-agenda
;;   (require 'org-projectile)
;;   (mapcar #'(lambda (file)
;;              (when (file-exists-p file)
;;                (push file org-agenda-files)))
;;           (org-projectile-todo-files)))

;; org-journal. move TODOs from previous days to the current
;; (defun my-old-carryover (old_carryover)
;;   (save-excursion
;;     (let ((matcher (cdr (org-make-tags-matcher org-journal-carryover-items))))
;;       (dolist (entry (reverse old_carryover))
;;         (save-restriction
;;           (narrow-to-region (car entry) (cadr entry))
;;           (goto-char (point-min))
;;           (org-scan-tags '(lambda ()
;;                             (org-set-tags ":carried:"))
;;                          matcher org--matcher-tags-todo-only))))))
;; (setq org-journal-handle-old-carryover 'my-old-carryover)

(add-to-list 'org-modules 'org-habit)

;; I.. don't know what this comes from
(setq org-agenda-text-search-extra-files '(agenda-archives))

(setq org-deadline-warning-days 14
      ;; show tasks scheduled or due in next fortnight
      org-agenda-span 14
      ;;don't show tasks as scheduled if they are already shown as a deadline
      ;; org-agenda-skip-scheduled-if-deadline-is-shown t

      ;; timestamp when we set something to done
      ;; org-log-done nil
      org-agenda-start-with-log-mode t
      org-log-into-drawer t


      org-journal-date-format "%B %d, %Y - %A"
      org-journal-file-format "%Y%m.org"
      org-journal-file-type 'monthly
      org-journal-enable-agenda-integration t
      ;; org-journal-dir "~/org/journal/"

      ;; org-default-notes-file "~/org/inbox.org"
      ;; org-agenda-start-on-weekday 1 ;; start on monday instead of current day
      ;; org-use-fast-todo-selection t

      ;; org-refile-targets '((nil :maxlevel . 4)
      ;;                      (org-agenda-files :maxlevel . 4))

      ;; include the file in the refile search
      org-refile-use-outline-path 'file
      ;; org-deadline-warning-days 7
      ;; org-agenda-compact-blocks nil ;; don't compact the agenda
      ;; give me all the possible completions at once so helm can present them
      ;; org-outline-path-complete-in-steps nil)
      ;; org-agenda-default-appointment-duration 60
      ;; org-agenda-skip-scheduled-if-done t

      ;; I don't think all are added if not
      org-agenda-files (directory-files-recursively "~/org/" "\\.org$")

      ;; org-agenda-clockreport-parameter-plist
      ;; (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80))

      org-habit-graph-column 60

      +org-capture-todo-file "inbox.org"
      +org-capture-mail-file (concat org-directory "mail.org")
      +org-capture-work-file (concat org-directory "work.org")
      +org-capture-personal-file (concat org-directory "personal.org")

      ;; include tags from all agenda files
      org-complete-tags-always-offer-all-agenda-tags t

      ;; persist agendas and don't bury them when you hit q (gr to update)
      ;; (setq org-agenda-sticky t)


      ;; default all open files
      ;; org-mru-clock-files #'org-agenda-files

      ;; keep history between sessions
      org-clock-persist 'history
      org-mru-clock-how-many 100
      ;; TODO: check out org-clock-persistence-insinuate
)

(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-todo-keywords
      '((sequence "TODO(t)"
                  "INPROGRESS(i!)"
                  "NEXT(n)"
                  "WAITING(w@/!)"
                  "SOMEDAY(s!)"
                  "|" "DONE(d@)" "CANCELLED(c@/!)")
        (sequence "BUG(b)" "|" "FIXED(f!)" "IGNORED(x@/!)")))


;; thanks tecosaur
(defun +doct-icon-declaration-to-icon (declaration)
  "Convert :icon declaration to icon"
  (let ((name (pop declaration))
        (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
        (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
        (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
    (apply set `(,name :face ,face :v-adjust ,v-adjust))))

(defun +doct-iconify-capture-templates (groups)
  "Add declaration's :icon to each template group in GROUPS."
  (let ((templates (doct-flatten-lists-in groups)))
    (setq doct-templates (mapcar (lambda (template)
                                   (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                               (spec (plist-get (plist-get props :doct) :icon)))
                                     (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                    "\t"
                                                                    (nth 1 template))))
                                   template)
                                 templates))))

(setq doct-after-conversion-functions '(+doct-iconify-capture-templates))

(setq org-capture-templates
      (doct `(("Personal"
               :icon ("person" :set "material" :color "green")
               :keys "p"
               :prepend true
               :file +org-capture-personal-file
               :type entry
               :default-tags "@life"
               :children (("Tasks"
                           :icon ("inbox" :set "octicon" :color "yellow")
                           :keys "t"
                           :extra ""
                           :clock-in t
                           :clock-resume: t
                           :headline "Tasks"
                           :template ("* TODO %^{Task description} %^G:%{default-tags}:%{extra}"
                                      ":PROPERTIES:"
                                      ":Created: %U"
                                      ":END:"
                                      ""
                                      "%?"
                                      ""
                                      "%a")
                           :children (("General Task" :keys "t"
                                       :icon ("inbox" :set "octicon" :color "yellow")
                                       :extra "")
                                      ("Task with deadline" :keys "d"
                                       :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                                       :extra "\nDEADLINE: %^{Deadline:}t")
                                      ("Scheduled Task" :keys "s"
                                       :icon ("calendar" :set "octicon" :color "orange")
                                       :extra "\nSCHEDULED: %^{Start time:}t")))

                          ("Enter a note"
                           :icon ("sticky-note-o" :set "faicon" :color "green")
                           :keys "n"
                           :headline "Notes"
                           :template ("* %?"
                                      "%i %a"))

                          ("Meeting"
                           :icon ("repo" :set "octicon" :color "silver")
                           :keys "m"
                           :headline "Meetings"
                           :clock-in t
                           :clock-resume: t
                           :template ("* [%(org-read-date nil nil org-read-date-final-answer)] %^{Meeting description}} %^G:%{default-tags}:meeting:"
                                      ":PROPERTIES:"
                                      ":Created: %U"
                                      ":END:"
                                      "SCHEDULED: %^t"
                                      ""
                                      "%?"
                                      ""
                                      "%a"))))
              ("Work"
               :keys "w"
               :icon ("work" :set "material" :color "yellow")
               :prepend t
               :file +org-capture-work-file
               :type entry
               :clock-in t
               :clock-resume: t
               :default-tags "@work"
               :children (("Tasks"
                           :keys "t"
                           :icon ("inbox" :set "octicon" :color "yellow")
                           :headline "Tasks"
                           :template ("* TODO %^{Task description} %^G:%{default-tags}:%{extra}"
                                      ":PROPERTIES:"
                                      ":Created: %U"
                                      ":END:"
                                      ""
                                      "%?"
                                      ""
                                      "%a")
                           :children (("General Task" :keys "t"
                                       :icon ("inbox" :set "octicon" :color "yellow")
                                       :extra "")
                                      ("Task with deadline" :keys "d"
                                       :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                                       :extra "\nDEADLINE: %^{Deadline:}t")
                                      ("Scheduled Task" :keys "s"
                                       :icon ("calendar" :set "octicon" :color "orange")
                                       :extra "\nSCHEDULED: %^{Start time:}t")))
                          ("Bug"
                           :icon ("bug" :set "octicon" :color "green")
                           :keys "b"
                           :headline "Bugs"
                           :template ("* TODO [#%^{Priority|B|A|C}] %^{Bug description} %^G:%{default-tags}:planet9:bug:%{extra}"
                                      ":PROPERTIES:"
                                      ":Created: %U"
                                      ":END:"
                                      ""
                                      "%?"
                                      ""
                                      "%a")
                           :children (("General bug"
                                       :icon ("inbox" :set "octicon" :color "yellow")
                                       :keys "b"
                                       :extra "")
                                      ("Bug with deadline"
                                       :icon ("timer" :set "material" :color "orange")
                                       :keys "d"
                                       :extra "\nDEADLINE: %^{Deadline:}t")
                                      ("Scheduled bug"
                                       :icon ("calendar" :set "octicon" :color "orange")
                                       :keys "s"
                                       :extra "\nSCHEDULED: %^{Start time:}t")))
                          ("Meeting"
                           :icon ("repo" :set "octicon" :color "silver")
                           :keys "m"
                           :headline "Meetings"
                           :template ("* [%(org-read-date nil nil org-read-date-final-answer)] %^{Meeting description} %^G:planet9:meeting:"
                                      ":PROPERTIES:"
                                      ":Created: %U"
                                      ":END:"
                                      "SCHEDULED: %^t"
                                      ""
                                      "%?"
                                      ""
                                      "%a"))))

              ("Active project" :keys "a"
               :icon ("repo" :set "octicon" :color "silver")
               :prepend t
               :type entry
               :headline "Inbox"
               :template ("* %{time-or-todo} %?"
                          "%i"
                          "%a")
               :file ""
               :custom (:time-or-todo "")
               :children (("Project-local todo" :keys "t"
                           :icon ("checklist" :set "octicon" :color "green")
                           :time-or-todo "TODO"
                           :file +org-capture-project-todo-file)
                          ("Project-local note" :keys "n"
                           :icon ("sticky-note" :set "faicon" :color "yellow")
                           :time-or-todo "%U"
                           :file +org-capture-project-notes-file)
                          ("Project-local changelog" :keys "c"
                           :icon ("list" :set "faicon" :color "blue")
                           :time-or-todo "%U"
                           :heading "Unreleased"
                           :file +org-capture-project-changelog-file)))
              )))

;; TODO: why is this changed?
(setq org-protocol-default-template-key "p")


(defun bergheim/org-clock-status ()
  "Return the org time status - including any pomodoro activity"
  (if (and (featurep! 'org-pomodoro) (org-pomodoro-active-p))
      (cl-case org-pomodoro-state
        (:pomodoro
         (format "Pomo: %d minutes - %s" (/ (org-pomodoro-remaining-seconds) 60) org-clock-heading))
        (:short-break
         (format "Short break time: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
        (:long-break
         (format "Long break time: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
        (:overtime
         (format "Overtime! %d minutes" (/ (org-pomodoro-remaining-seconds) 60))))
    (if (org-clocking-p)
        (format "%s - %s" (org-duration-from-minutes (org-clock-get-clocked-time)) org-clock-heading)
      "")))
