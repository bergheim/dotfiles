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

      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      ;; I had this but not really sure where the difference is
      org-agenda-block-separator nil
      ;; this either
      org-agenda-compact-blocks t

      ;; start on monday instead of current day
      ;; org-agenda-start-on-weekday 1

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

      org-protocol-default-template-key "z"
)

(advice-add 'org-refile :after 'org-save-all-org-buffers)
(advice-add #'org-todo :after (lambda (&rest _)
                                  (org-save-all-org-buffers)))
(add-hook 'auto-save-hook 'org-save-all-org-buffers)

(setq org-todo-keywords
      '((sequence "TODO(t)"
                  "INPROGRESS(i!)"
                  "NEXT(n)"
                  "WAITING(w@/!)"
                  "SOMEDAY(s!)"
                  "|" "DONE(d@)" "CANCELLED(c@/!)")
        (sequence "BUG(b)" "|" "FIXED(f!)" "IGNORED(x@/!)")))

;; (use-package! org-super-agenda
;;   :after org-agenda
;;   :commands org-super-agenda-mode
;;   :config
;;   (setq org-super-agenda-header-map (make-sparse-keymap)))

(org-super-agenda-mode)
;; don't break evil on org-super-agenda headings, see
;; https://github.com/alphapapa/org-super-agenda/issues/50
(setq org-super-agenda-header-map (make-sparse-keymap))

;; (setq org-agenda-prefix-format
;;       (quote
;;        ((agenda . "%-12c%?-18t% s")
;;         (timeline . "% s")
;;         (todo . "%-20c")
;;         (tags . "%-22c")
;;         (search . "%-12c"))))

(setq org-agenda-custom-commands
      '(("d" "Dashboard for today"
         ((agenda "" ((org-agenda-overriding-header "Dashboard")
                      (org-agenda-span 'day)
                      ;; (org-agenda-current-span 'day)
                      (org-agenda-start-day (org-today))
                      (org-super-agenda-groups
                       '((:name "And make sure you keep up these :)"
                          :habit t
                          :order 3)
                         (:name "This is how your day looks"
                          ;; :habit nil
                          ;; :discard (:habit t)
                          :time-grid t
                          :order 1)
                         (:name "First, do one of these"
                          ;; :discard (:not (:tag "@work"))
                          :and (:deadline today :priority "A")
                          :deadline today
                          :and (:deadline past :priority "A")
                          :and (:scheduled t :priority "A")
                          :and (:scheduled past :priority "A")
                          :deadline past
                          :scheduled past
                          :order 2)

                         (:name "Scheduled for today"
                          :scheduled today
                          :order 3)

                         (:name "Upcoming deadlines"
                          :deadline future
                          :order 4)
                         (:name "Do you still need to do these?"
                          :discard (:anything t)
                          :scheduled past
                          :order 10)))))))

        ("w" "Work related tasks"
         ((tags-todo "@work|planet9" (
                                      (org-super-agenda-groups
                                       '(
                                         ;; (:discard (:not (:and (:tag ("@work" "planet9")))))
                                         (:name "Important tasks"
                                          :priority ("A" "B")
                                          :order 1)
                                         (:name "Needs refiling"
                                          :tag "REFILE"
                                          :order 1)))))))

        ("c" "Done and clocked items"
         ((agenda "" ((org-agenda-overriding-header "")
                      (org-agenda-span 'day)
                      (org-agenda-current-span 'week)
                      (org-agenda-start-day (org-today))
                      (org-super-agenda-groups
                       '((:name "Done today"
                          :and (:regexp "State \"DONE\""
                                :log t))
                         (:name "Clocked today"
                          :log t)
                         (:discard (:anything t))))))))

        ("i" "In progress" tags-todo "TODO=\"INPROGRESS\"")
        ("l" "Low effort tasks" tags-todo "EFFORT>=\"0:01\"&EFFORT<=\"0:15\"")

        ("p" "Projects" tags "+project-someday-TODO=\"DONE\"-TODO=\"SOMEDAY\""
         ((org-tags-exclude-from-inheritance '("project"))
          (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))

        ("O" "Timeline for today" ((agenda "" ))
         ((org-agenda-ndays 1)
          (org-agenda-show-log t)
          (org-agenda-log-mode-items '(clock closed))
          (org-agenda-clockreport-mode t)
          (org-agenda-entry-types '())))

        ("gc" "Coding" tags-todo "@coding"
         ((org-agenda-view-columns-initially t)))
        ("gh" "Home" tags-todo "@home"
         ((org-agenda-view-columns-initially t)))
        ("ge" "Errands" tags-todo "@errands"
         ((org-agenda-view-columns-initially t)))
        ("gw" "Waiting for" todo "WAITING")
        ("gs" "Someday" tags-todo "TODO=\"SOMEDAY\""
         ((org-agenda-view-columns-initially nil)
          (org-tags-exclude-from-inheritance '("project"))
          (org-agenda-overriding-header "Someday: ")
          (org-columns-default-format "%50ITEM %TODO %3PRIORITY %Effort{:} %TAGS")
          (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up tag-up category-keep))))
        ("gi" "In progress" tags-todo "TODO=\"INPROGRESS\"")
        ("gP" "By priority"
         ((tags-todo "+PRIORITY=\"A\"")
          (tags-todo "+PRIORITY=\"B\"")
          (tags-todo "+PRIORITY=\"\"")
          (tags-todo "+PRIORITY=\"C\""))
         ((org-agenda-prefix-format "%-10c %-10T %e ")
          (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))

        ("o" "Overview"
         ((agenda "Agenda today" ((org-agenda-span 'day)
                                  (org-super-agenda-groups
                                   '((:name "Today"
                                      :time-grid t
                                      :date today
                                      :todo "TODAY"
                                      :scheduled today
                                      :order 1)))))
          (alltodo "All todos" ((org-agenda-overriding-header "")
                                (org-super-agenda-groups
                                 '((:name "Next to do"
                                    :todo "NEXT"
                                    :order 1)
                                   (:name "Important"
                                    :tag "Important"
                                    :priority "A"
                                    :order 6)
                                   (:name "Due Today"
                                    :deadline today
                                    :order 2)
                                   (:name "Due Soon"
                                    :deadline future
                                    :order 8)
                                   (:name "Overdue"
                                    :deadline past
                                    :face error
                                    :order 7)
                                   (:name "Projects"
                                    :tag "Project"
                                    :order 14)
                                   (:name "Emacs"
                                    :tag "emacs"
                                    :order 13)
                                   (:name "To read"
                                    :tag "toread"
                                    :order 30)
                                   (:name "Waiting"
                                    :todo "WAITING"
                                    :order 20)
                                   (:discard (:tag ("Chore" "Routine" "Daily")))))))))

        ("D" "Playground"
         ((tags "test"
                ((org-agenda-overriding-header "Work things\n")))
          (agenda ""
                  ((org-agenda-overriding-header "Todays agenda")
                   (org-agenda-block-separator ?*)
                   (org-deadline-warning-days 0)
                   (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                   ;; (org-super-agenda-date-format "%A %-e %B %Y")
                   (org-agenda-span 1))
                  )))))

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
               :clock-in t
               :clock-resume t
               :children (("Tasks"
                           :icon ("inbox" :set "octicon" :color "yellow")
                           :keys "t"
                           :extra ""
                           :headline "Tasks"
                           :template ("* TODO %^{Task description} %^G:%{default-tags}:%{extra}"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
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
                           :template ("* )] %^{Meeting description} %^G:%{default-tags}:meeting:"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":END:"
                                      "%^T"
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
               :clock-resume t
               :default-tags "@work"
               :children (("Tasks"
                           :keys "t"
                           :icon ("inbox" :set "octicon" :color "yellow")
                           :headline "Tasks"
                           :template ("* TODO %^{Task description} %^G:%{default-tags}:%{extra}"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
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
                                      ":CREATED: %U"
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
                                      ":CREATED: %U"
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


              ("Protocol Link Marked" :keys "z"
               :icon ("bookmark" :set "octicon" :color "silver")
               :type entry
               :prepent t
               :headline "Protocol"
               :file +org-capture-todo-file
               :immediate-finish t
               :template ("* [[%:link][%:description]] \nCaptured: %U\n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE%?"))

              ("Protocol Link Unmarked" :keys "Z"
               :icon ("bookmark" :set "octicon" :color "silver")
               :type entry
               :prepent t
               :headline "Protocol"
               :file +org-capture-todo-file
               :immediate-finish t
               :template ("* [[%:link][%:description]] \nCaptured: %U"))


(defun bergheim/org-clock-status ()
  "Return the org time status - including any pomodoro activity"
  (if (and (featurep 'org-pomodoro) (org-pomodoro-active-p))
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
