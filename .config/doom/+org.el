;;; ~/.config/doom/+org.el -*- lexical-binding: t; -*-

(add-to-list 'org-modules 'org-habit)

;; I.. don't know what this comes from
(setq org-agenda-text-search-extra-files '(agenda-archives))

;; TODO: verify this works (from https://github.com/unhammer/org-mru-clock/)
;; (add-hook 'minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook)

(defun bergheim/org-agenda-mark-done-and-add-followup ()
  "Mark the current TODO as done and add another task after it.
       Creates it at the same level as the previous task, so it's better to use
       this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-todo "DONE")
  (org-agenda-switch-to)
  (if (member "@work" (org-get-tags))
      (org-capture 0 "wtt")
    (org-capture 0 "ptt")))

(defun bergheim/org-agenda-toggle-work ()
  (interactive)
  (pcase (get 'work 'state)
      ('show-work (progn (org-agenda-redo)
        (org-agenda-filter-apply '("+@work") 'tag)
        (put 'work 'state 'show-private)))
    ('show-private (progn (org-agenda-redo)
        (org-agenda-filter-apply '("-@work") 'tag)
        (put 'work 'state 'show-everything)))
    (_ (progn (org-agenda-redo)
        (org-agenda-filter-apply '() 'tag)
        (put 'work 'state 'show-work)))))

(defun bergheim/vertico--without-orderless (fn &rest args)
  (let ((completion-styles '(partial-completion)))
    (apply fn args)))

(defun bergheim/vertico--without-sorting (fn &rest args)
  (let ((vertico-sort-function 'nil))
    (apply fn args)))

;; You can advise completion-metadata-get and return identity for
;; display-sort-function/cycle-sort-function
(defun bergheim/testus (&rest prop)
  (let ((existing-dsf
   (completion-metadata-get prop 'display-sort-function)))
  ;; (if (eq prop 'display-sort-function)
  ;;     (identity))
  (message "testus %s" existing-dsf)))
;; (advice-remove 'completion-metadata-get #'bergheim/testus)
;; (advice-add 'completion-metadata-get :around #'bergheim/testus)

(defun bergheim/org-mru-goto ()
  (interactive)
  (bergheim/vertico--without-sorting
   (call-interactively 'org-mru-clock-goto)))

(defun bergheim/org-mru-clock-in ()
  (interactive)
  (bergheim/vertico--without-sorting 'org-mru-clock-in))

(setq org-deadline-warning-days 14
      ;; show tasks scheduled or due in next fortnight
      org-agenda-span 14
      ;;don't show tasks as scheduled if they are already shown as a deadline
      org-agenda-skip-scheduled-if-deadline-is-shown t

      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      ;; I had this but not really sure where the difference is
      org-agenda-block-separator nil
      ;; this either
      org-agenda-compact-blocks t

      org-agenda-breadcrumbs-separator " ❱ "

      ;; start on monday instead of current day
      ;; org-agenda-start-on-weekday 1

      ;; stamp a CLOSED: [X] on DONE items
      org-log-done 'time

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

      ;; once you've used org for a while, you start to chose which files go to
      ;; the agenda
      org-agenda-files (append (file-expand-wildcards "~/org/*.org")
                               (file-expand-wildcards "~/org/roam/*.org")
                               (directory-files-recursively "~/org/neptune" "\\.org$")
                               (directory-files-recursively "~/org/projects" "\\.org$")
                               (directory-files-recursively "~/org/journal" "\\.org$"))

      ;; org-agenda-clockreport-parameter-plist
      ;; (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80))

      org-habit-graph-column 60

      +org-capture-todo-file "inbox.org"
      +org-capture-mail-file (concat org-directory "mail.org")
      +org-capture-work-file (concat org-directory "work.org")
      +org-capture-personal-file (concat org-directory "personal.org")
      org-capture-custom-template-directory (concat org-directory "templates/capture/")

      ;; include tags from all agenda files
      org-complete-tags-always-offer-all-agenda-tags t

      ;; persist agendas and don't bury them when you hit q (gr to update)
      ;; (setq org-agenda-sticky t)

      ;; this might be handly if I find project TODOs start to litter things
      ;; org-tags-exclude-from-inheritance (quote ("crypt" "project"))

      ;; default all open files
      ;; org-mru-clock-files #'org-agenda-files

      org-clock-history-length 20

      ;; auto-resolve a clock after 30 minutes of idle time
      org-clock-idle-time 30
      ;; keep clocks - makes it easier to see a list of recent tasks
      org-clock-out-remove-zero-time-clocks nil
      ;; keep history between sessions
      org-clock-persist 'history
      ;; TODO: check out org-clock-persistence-insinuate

      ;; org-archive-location "archive/%s_archive::datetree/"

      org-protocol-default-template-key "z")

(advice-add 'org-archive-subtree :after #'org-save-all-org-buffers)
(add-hook! '(org-clock-out-hook org-clock-in-hook) #'org-save-all-org-buffers)
;; this is handled by my/org-roam-copy-todo-to-today at the moment
;; (advice-add #'org-todo :after (lambda (&rest _) (org-save-all-org-buffers)))
;; (advice-add 'org-refile :after 'org-save-all-org-buffers)

;; open new notes etc in insert mode
(add-hook 'org-log-buffer-setup-hook #'evil-insert-state)

(use-package! org-mru-clock
  :init
  (setq org-mru-clock-files #'org-agenda-files
      org-mru-clock-how-many 100))

(setq org-todo-keywords
      '((sequence "TODO(t)"
                  "INPROGRESS(i!)"
                  "NEXT(n)"
                  "WAITING(w@/!)"
                  "SOMEDAY(s!)"
                  "|" "DONE(d@)" "CANCELLED(c@/!)")
        (sequence "BUG(b)" "|" "FIXED(f!)" "IGNORED(x@/!)")))

;; this should work.. not that it matters much
;; (use-package! org-super-agenda
;;   :after org-agenda
;;   :commands org-super-agenda-mode
;;   ;; :hook (org-agenda-mode . org-super-agenda-mode)
;;   :config
;;   (setq org-super-agenda-header-map (make-sparse-keymap))
;;   ;; (org-agenda-super-mode)
;;   )
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
                      ;; (org-agenda-remove-tags t)
                      ;; (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now")
                      ;; (org-agenda-show-log nil)
                      (org-super-agenda-groups
                       '((:name "Keep your habits up 🔥"
                          :habit t
                          :order 3)
                         (:name "Logged 📅" :log t :order 15)
                         ;; (:name "Done today"
                         ;;         :discard (:and (:log t)))
                         (:name "This is how your day looks 🌞"
                          ;; :habit nil
                          ;; :discard (:habit t)
                          ;; :log '(closed clock)

                          ;; :date today
                          :time-grid t
                          :order 1)
                         (:name "First, do one of these 🐸"
                          ;; :discard (:not (:tag "@work"))
                          :and (:deadline today :priority "A")
                          :deadline today
                          :and (:deadline past :priority "A")
                          :and (:scheduled t :priority "A")
                          :and (:scheduled past :priority "A")
                          :deadline past
                          :scheduled past
                          :order 2)

                         (:name "Scheduled for today ⏰"
                          :scheduled today
                          :order 3)

                         (:name "Upcoming deadlines 🚌"
                          :deadline future
                          :order 4)
                         (:name "Do you still need to do these? 🤔"
                          :discard (:anything t)
                          :scheduled past
                          :order 10)))))))

        ("W" "Dashboard for the week"
         ((agenda "" ((org-agenda-overriding-header "Dashboard")
                      (org-agenda-span 'week)
                      ;; (org-agenda-current-span 'day)
                      (org-agenda-start-day "-1w")
                      ;; (org-agenda-clockreport-mode nil)
                      (org-agenda-log-mode-items '(closed))
                      (org-super-agenda-groups
                       '((:time-grid t
                          :order 1)
                         (:discard (:anything t))))))))

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

        ("c" "Todays done and clocked items"
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
        ("gd" "Done items" todo "DONE"
         ((org-agenda-view-columns-initially t)))
        ("ge" "Errands" tags-todo "errands"
         ((org-agenda-view-columns-initially t)))
        ("gh" "Home" tags-todo "@home"
         ((org-agenda-view-columns-initially t)))
        ("gi" "In progress" tags-todo "TODO=\"INPROGRESS\"")
        ("gs" "Someday" tags-todo "TODO=\"SOMEDAY\""
         ((org-agenda-view-columns-initially nil)
          (org-tags-exclude-from-inheritance '("project"))
          (org-agenda-overriding-header "Someday: ")
          (org-columns-default-format "%50ITEM %TODO %3PRIORITY %Effort{:} %TAGS")
          (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up tag-up category-keep))))
        ("gw" "Waiting for" todo "WAITING")
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

(defun bergheim/org-recent-tasks-in-buffer ()
  "Lists the recently worked on tasks for this buffer"
  (interactive)
  (let ((vertico-sort-function nil)
        (data (seq-uniq
               (mapcar
                (lambda (elem)
                  (cons (org-element-property :raw-value elem) elem))
                (org-ql-query
                  :select #'element-with-markers
                  :from (org-agenda-files)
                  :where '(ts :from -100 :to today)
                  :order-by '(reverse date)))
               ;; only take the most recently clocked item
               (lambda (a b) (string-equal
                              (car a)
                              (car b))))))
    (org-goto-marker-or-bmk
     (org-element-property
      :org-marker
      (cdr
       (assoc
        (completing-read "TODO: " data)
        data
        #'string-equal))))))

(defun bergheim/org-agenda-recent-changes (&optional tags)
  (interactive)
  (let (filter from)
    (if current-prefix-arg
        (setq from (org-read-date nil))
      (setq from -7))
    (when tags
      (setq filter `(:discard (:not (:tag ,tags)))))
    (org-ql-search (org-agenda-files)
      `(ts :from ,from :to today)
      :title "Recent Items"
      :super-groups `(,filter
                      (:name "Work" :auto-ts t)))))

;; TODO maybe just bin this
(defun bergheim/org-agenda-new-items ()
  (interactive)
  (org-ql-search (org-agenda-files)
    '(ts :from -7 :to today)
    :title "Recent Items"
    :sort '(todo priority date)
    :where '(and (property "CREATED")
    ;;            (string-match "issue" (org-entry-get (point) "CUSTOM_ID"))))
    :super-groups '((:auto-ts t)))))

(defun bergheim/org-agenda-work-items ()
  (interactive)
  (org-ql-search (org-agenda-files)
    '(not (done))
    :title "Work related tasks"
    :super-groups '((:name "Important tasks"
                     :discard (:not (:tag ("@work" "planet9")))
                     :priority ("A" "B"))
                    (:name "Needs refiling"
                     :tag "REFILE"
                     :order 7)
                    (:name "Habits"
                     :habit t
                     :order 3)
                    (:todo "WAITING"
                     :order 6)
                    (:priority "A" :order 1)
                    (:priority "B" :order 2)
                    (:priority "C" :order 2))))

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

(set-popup-rule! "^ \\*Agenda Com"
  :slot -1 :vslot -1 :size #'+popup-shrink-to-fit :side 'right :ttl 0)

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
                           :template-file ,(expand-file-name "task.org" org-capture-custom-template-directory)
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
                           :template-file ,(expand-file-name "note.org" org-capture-custom-template-directory))

                          ("Meeting"
                           :icon ("repo" :set "octicon" :color "silver")
                           :keys "m"
                           :headline "Meetings"
                           :template-file ,(expand-file-name "meeting.org" org-capture-custom-template-directory))))

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
                           :template-file ,(expand-file-name "task.org" org-capture-custom-template-directory)
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
                           :template-file ,(expand-file-name "bug.org" org-capture-custom-template-directory)
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
                           :template-file ,(expand-file-name "meeting.org" org-capture-custom-template-directory))))

              ("Capture to clocked in task" :keys "c"
               :icon ("email" :set "material" :color "green")
               :type entry
               :prepend t
               :clock t
               :template-file ,(expand-file-name "clocked.org" org-capture-custom-template-directory))

              ("Interrupted" :keys "I"
               :icon ("stop-circle" :set "faicon" :color "red")
               :file +org-capture-work-file
               :type entry
               :clock-in t
               :clock-resume t
               :headline "Interruptions"
               :default-tags "@work:interrupted"
               :template-file ,(expand-file-name "interrupted.org" org-capture-custom-template-directory))

              ("Review"
               :keys "r"
               :icon ("rate_review" :set "material" :color "yellow")
               :type entry
               :clock-in t
               :clock-keep t
               :file "~/org/review.org"
               :jump-to-captured t
               :default-tags "review"
               :children (("Daily review"
                            :icon ("today" :set "material" :color "green")
                            :keys "r"
                            :headline "Daily"
                            :default-tags "@work:daily:review"
                            :template-file ,(expand-file-name "review-daily.org" org-capture-custom-template-directory))
                           ("Weekly review"
                            :icon ("weekend" :set "material" :color "green")
                            :keys "w"
                            :headline "Weekly"
                            :default-tags "@work:weekly:review"
                            :template-file ,(expand-file-name "review-weekly.org" org-capture-custom-template-directory))
                           ("Monthly review"
                            :icon ("lightbulb-o" :set "faicon" :color "green")
                            :keys "m"
                            :headline "Monthly"
                            :default-tags "@work:monthly:review"
                            :template-file ,(expand-file-name "review-monthly.org" org-capture-custom-template-directory))))

              ("Add contact" :keys "C"
               :icon ("person" :set "octicon" :color "green")
               :type entry
               :headline "People"
               :file "~/org/contacts.org"
               :template ("* %(org-contacts-template-name)
:PROPERTIES:
:ADDRESS: %^{Address}
:BIRTHDAY: %^{Birthday (yyyy-mm-dd)}
:EMAIL: %(org-contacts-template-email)
:NOTE: %^{NOTE}
:END:"))

              ("Active project" :keys "a"
               :icon ("repo" :set "octicon" :color "green")
               :prepend t
               :type entry
               :headline "Inbox"
               :template-file ,(expand-file-name "active-project.org" org-capture-custom-template-directory)
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
                           :headline "Unreleased"
                           :file +org-capture-project-changelog-file)))

              ("Interesting"
               :keys "i"
               :icon ("eye" :set "faicon" :color "lcyan")
               :file +org-capture-todo-file
               :prepend t
               :headline "Interesting"
               :type entry
               :template-file ,(expand-file-name "interesting.org" org-capture-custom-template-directory)
               :children (("Webpage" :keys "w"
                           :icon ("globe" :set "faicon" :color "green")
                           :desc "%(org-cliplink-capture) "
                           :i-type "read:web")
                          ("Article" :keys "a"
                           :icon ("file-text" :set "octicon" :color "yellow")
                           :desc ""
                           :i-type "read:reaserch")
                          ("Information" :keys "i"
                           :icon ("info-circle" :set "faicon" :color "blue")
                           :desc ""
                           :i-type "read:info")
                          ("Idea" :keys "I"
                           :icon ("bubble_chart" :set "material" :color "silver")
                           :desc ""
                           :i-type "idea")))

              ("Protocol Link Marked" :keys "z"
               :icon ("stop" :set "octicon" :color "red")
               :type entry
               :prepend t
               :headline "Protocol"
               :file +org-capture-todo-file
               :immediate-finish t
               :template-file ,(expand-file-name "protocol-marked.org" org-capture-custom-template-directory))

              ("Protocol Link Unmarked" :keys "Z"
               :icon ("stop" :set "octicon" :color "red")
               :type entry
               :prepend t
               :headline "Protocol"
               :file +org-capture-todo-file
               :immediate-finish t
               :template-file ,(expand-file-name "protocol-unmarked.org" org-capture-custom-template-directory))

              ("Protocol Link Active Task" :keys "o"
               :icon ("stop" :set "octicon" :color "red")
               :type entry
               :prepend t
               :clock t
               :immediate-finish t
               :template-file ,(expand-file-name "protocol-active-task.org" org-capture-custom-template-directory))

              ("Email Workflow" :keys "e"
               :icon ("stop" :set "octicon" :color "red")
               :type entry
               :file +org-capture-mail-file
               :immediate-finish t
               :children (("Follow Up" :keys "f"
                           :headline "Follow Up"
                           :file +org-capture-mail-file
                           :template-file ,(expand-file-name "email-follow-up.org" org-capture-custom-template-directory)
                           )
                          ("Read Later" :keys "l"
                           :template-file ,(expand-file-name "email-read-later.org" org-capture-custom-template-directory)
                           :headline "Read Later"))))))

(defun org-capture-select-template-prettier (&optional keys)
  "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
  (let ((org-capture-templates
         (or (org-contextualize-keys
              (org-capture-upgrade-templates org-capture-templates)
              org-capture-templates-contexts)
             '(("t" "Task" entry (file+headline "" "Tasks")
                "* TODO %?\n  %u\n  %a")))))
    (if keys
        (or (assoc keys org-capture-templates)
            (error "No capture template referred to by \"%s\" keys" keys))
      (org-mks org-capture-templates
               "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
               "Template key: "
               `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
(advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

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


;; this will create unique ids that are easy to read as well. works great.
;; nicked from https://github.com/novoid/dot-emacs/blob/master/config.org
(defun my-generate-sanitized-alnum-dash-string(str)
"Returns a string which contains only a-zA-Z0-9 with single dashes
 replacing all other characters in-between them.

 Some parts were copied and adapted from org-hugo-slug
 from https://github.com/kaushalmodi/ox-hugo (GPLv3)."
(let* (;; Remove "<FOO>..</FOO>" HTML tags if present.
       (str (replace-regexp-in-string "<\\(?1:[a-z]+\\)[^>]*>.*</\\1>" "" str))
       ;; Remove URLs if present in the string.  The ")" in the
       ;; below regexp is the closing parenthesis of a Markdown
       ;; link: [Desc](Link).
       (str (replace-regexp-in-string (concat "\\](" ffap-url-regexp "[^)]+)") "]" str))
       ;; Replace "&" with " and ", "." with " dot ", "+" with
       ;; " plus ".
       (str (replace-regexp-in-string
             "&" " and "
             (replace-regexp-in-string
              "\\." " dot "
              (replace-regexp-in-string
               "\\+" " plus " str))))
       (str (replace-regexp-in-string "æ" "ae" str nil))
       (str (replace-regexp-in-string "ø" "ue" str nil))
       (str (replace-regexp-in-string "å" "oe" str nil))
       ;; Replace all characters except alphabets, numbers and
       ;; parentheses with spaces.
       (str (replace-regexp-in-string "[^[:alnum:]()]" " " str))
       ;; Remove leading and trailing whitespace.
       (str (replace-regexp-in-string "\\(^[[:space:]]*\\|[[:space:]]*$\\)" "" str))
       ;; Replace 2 or more spaces with a single space.
       (str (replace-regexp-in-string "[[:space:]]\\{2,\\}" " " str))
       ;; Replace parentheses with double-hyphens.
       (str (replace-regexp-in-string "\\s-*([[:space:]]*\\([^)]+?\\)[[:space:]]*)\\s-*" " -\\1- " str))
       ;; Remove any remaining parentheses character.
       (str (replace-regexp-in-string "[()]" "" str))
       ;; Replace spaces with hyphens.
       (str (replace-regexp-in-string " " "-" str))
       ;; Remove leading and trailing hyphens.
       (str (replace-regexp-in-string "\\(^[-]*\\|[-]*$\\)" "" str)))
  str)
)

(defun my-id-get-or-generate()
"Returns the ID property if set or generates and returns a new one if not set.
 The generated ID is stripped off potential progress indicator cookies and
 sanitized to get a slug. Furthermore, it is prepended with an ISO date-stamp
 if none was found before."
    (interactive)
        (when (not (org-id-get))
            (progn
               (let* (
                      (my-heading-text (nth 4 (org-heading-components)));; retrieve heading string
                      (my-heading-text (replace-regexp-in-string "\\(\\[[0-9]+%\\]\\)" "" my-heading-text));; remove progress indicators like "[25%]"
                      (my-heading-text (replace-regexp-in-string "\\(\\[[0-9]+/[0-9]+\\]\\)" "" my-heading-text));; remove progress indicators like "[2/7]"
                      (my-heading-text (replace-regexp-in-string "\\(\\[#[ABC]\\]\\)" "" my-heading-text));; remove priority indicators like "[#A]"
                      (my-heading-text (replace-regexp-in-string "\\[\\[\\(.+?\\)\\]\\[" "" my-heading-text t));; removes links, keeps their description and ending brackets
;;                      (my-heading-text (replace-regexp-in-string "[<\\[][12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)[>\\]]" "" my-heading-text t));; removes day of week and time from date- and time-stamps (doesn't work somehow)
                      (my-heading-text (replace-regexp-in-string "<[12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)>" "" my-heading-text t));; removes day of week and time from active date- and time-stamps
                      (my-heading-text (replace-regexp-in-string "\\[[12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)\\]" "" my-heading-text t));; removes day of week and time from inactive date- and time-stamps
                      (new-id (my-generate-sanitized-alnum-dash-string my-heading-text));; get slug from heading text
                      (my-created-property (assoc "CREATED" (org-entry-properties))) ;; nil or content of CREATED time-stamp
                     )
                   (when (not (string-match "[12][0-9][0-9][0-9]-[01][0-9]-[0123][0-9]-.+" new-id))
                           ;; only if no ISO date-stamp is found at the beginning of the new id:
                           (if my-created-property (progn
                               ;; prefer date-stamp of CREATED property (if found):
                               (setq my-created-datestamp (substring (org-entry-get nil "CREATED" nil) 1 11)) ;; returns "2021-12-16" or nil (if no CREATED property)
                               (setq new-id (concat my-created-datestamp "-" new-id))
                           )
                           ;; use today's date-stamp if no CREATED property is found:
                           (setq new-id (concat (format-time-string "%Y-%m-%d-") new-id))))
                   (org-set-property "ID" new-id)
                   )
                 )
        )
        (kill-new (concat "id:" (org-id-get)));; put ID in kill-ring
        (org-id-get);; retrieve the current ID in any case as return value
)

(defun bergheim/org-id-advice (&rest args)
  "Add unique and clear IDs to everything, except modes where it does not make sense"
  ;; (unless (string-match "^\\(magit\\|mu4e\\)-.*" (format "%s" major-mode))
  (if (string-prefix-p "org-" (format "%s" major-mode))
    (my-id-get-or-generate)
    (org-id-update-id-locations))
  args)
(advice-add 'org-store-link :before #'bergheim/org-id-advice)


(org-add-link-type
 "org-agenda"
 (defun bergheim/org--open-dashboard (dashboard)
   (org-agenda current-prefix-arg dashboard)))

(org-add-link-type
 "tag"
 (defun endless/follow-tag-link (tag)
   "Display a list of TODO headlines with tag TAG.
With prefix argument, also display headlines without a TODO keyword."
   (org-tags-view (null current-prefix-arg) tag)))


;; org roam stuff


(use-package! org-roam-dailies
  :init
  (setq org-roam-dailies-directory "daily/"
        org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %<%H:%M> %?\n%T"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n"))))

  (defun my/org-id-remove-entry ()
    "Remove/delete the ID entry and update the databases.
Update the `org-id-locations' global hash-table, and update the
`org-id-locations-file'.  `org-id-track-globally' must be `t`."
    (interactive)
    (save-excursion
      (org-back-to-heading t)
      (when (org-entry-delete (point) "ID")
        ;; TODO if we start seeing errors here maybe just use current file
        (org-id-update-id-locations nil 'silent))))

  (defun my/org-roam-copy-todo-to-today ()
    (interactive)
    (let ((org-refile-keep t) ;; Set this to nil to delete the original!
          (org-roam-dailies-capture-templates
           '(("t" "tasks" entry "%?"
              :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
          (org-after-refile-insert-hook #'save-buffer)
          today-file
          pos)
      (save-window-excursion
        (org-roam-dailies--capture (current-time) t)
        (setq today-file (buffer-file-name))
        (setq pos (point)))

      ;; Only refile if the target file is different than the current file
      (unless (equal (file-truename today-file)
                     (file-truename (buffer-file-name)))
        (org-refile nil nil (list "Tasks" today-file nil pos))

        ;; (save-window-excursion
        ;;   (switch-to-buffer today-file)
        ;;   (goto-char pos)
        ;;   (my/org-id-remove-entry))
        )))

  :config
  (add-to-list 'org-after-todo-state-change-hook
               (lambda ()
                 (if (equal org-state "DONE")
                     (my/org-roam-copy-todo-to-today)
                   (org-save-all-org-buffers)))))





;; (after! org-roam-dailies
;;   (add-to-list 'org-after-todo-state-change-hook
;;                (lambda ()
;;                  (when (equal org-state "DONE")
;;                    (my/org-roam-copy-todo-to-today)))))


(use-package! org-contacts
  :init (setq org-contacts-files '("~/org/contacts.org")))

;; (use-package! org-capture
;;   :ensure nil
;;   :after org
;;   :preface
;;   (defvar my/org-contacts-template "* %(org-contacts-template-name)
;; :PROPERTIES:
;; :ADDRESS: %^{289 Cleveland St. Brooklyn, 11206 NY, USA}
;; :BIRTHDAY: %^{yyyy-mm-dd}
;; :EMAIL: %(org-contacts-template-email)
;; :NOTE: %^{NOTE}
;; :END:" "Template for org-contacts.")
;;   :custom
;;   (org-capture-templates
;;    `(("c" "Contact" entry (file+headline "~/.personal/agenda/contacts.org" "Friends"),
;;       my/org-contacts-template
;;       :empty-lines 1))))
