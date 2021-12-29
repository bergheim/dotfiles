;;; ~/.config/doom/+org.el -*- lexical-binding: t; -*-

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(setq org-directory "~/org/")

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

      ;; include tags from all agenda files
      org-complete-tags-always-offer-all-agenda-tags t
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

 (setq org-capture-templates
          (doct `((,(format "%s\tBug" (all-the-icons-octicon "bug" :face 'all-the-icons-green :v-adjust 0.01))
               :keys "b"
               :file +org-capture-todo-file
               ;; :prepend t
               ;; :headline "Bug"
               :type entry
               :clock-in t
               :clock-resume t
               :template ("* TODO [#%^{Priority|A|B|C}] %?  %^G:bug:%{extra}"
                          ;; "\nFrom: %a\n\n%i")
                          "%U %i %a")
               :children ((,(format "%s\tGeneral bug" (all-the-icons-octicon "inbox" :face 'all-the-icons-yellow :v-adjust 0.01))
                           :keys "b"
                           :extra ""
                           )
                          (,(format "%s\tBug with deadline" (all-the-icons-material "timer" :face 'all-the-icons-orange :v-adjust -0.1))
                           :keys "d"
                           :extra "\nDEADLINE: %^{Deadline:}t"
                           )
                          (,(format "%s\tScheduled bug" (all-the-icons-octicon "calendar" :face 'all-the-icons-orange :v-adjust 0.01))
                           :keys "s"
                           :extra "\nSCHEDULED: %^{Start time:}t"
                           )
                          ))

                  (,(format "%s\tPersonal todo" (all-the-icons-octicon "checklist" :face 'all-the-icons-green :v-adjust 0.01))
                   :keys "t"
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%U %i %a")
                   :clock-in t
                   :clock-resume: t
                   )
                  (,(format "%s\tPersonal note" (all-the-icons-faicon "sticky-note-o" :face 'all-the-icons-green :v-adjust 0.01))
                   :keys "n"
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* %?"
                              "%i %a")
                   )

                  (,(format "%s\tEmail" (all-the-icons-faicon "envelope" :face 'all-the-icons-blue :v-adjust 0.01))
                   :keys "e"
                   :file +org-capture-mail-file
                   :prepend t
                   :headline "Mail"
                   :type entry
                   :template ("* TODO %{email-action} with %:fromname on %a"
                              "SCHEDULED:%t"
                              "DEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))"
                              "%i")
                   :children (
                              ;; TODO can use date or date-timestamp to get the msg date
                              (,(format "%s\tFollow up" (all-the-icons-octicon "file-text" :face 'all-the-icons-yellow :v-adjust 0.01))
                               :keys "f"
                               :template ("* TODO Follow up with %:fromname on %a"
                                          "SCHEDULED:%t"
                                          "DEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))"
                                          "%U %i")
                               :desc ""
                               :headline "Follow Up"
                               :immediate-finish t
                               )
                              (,(format "%s\tRead Later" (all-the-icons-octicon "file-text" :face 'all-the-icons-yellow :v-adjust 0.01))
                               :template ("* TODO Read %a"
                                          "SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+6d\"))"
                                          "%a"
                                          "%U %i")
                               :keys "l"
                               :desc ""
                               :headline "Read Later"
                               :immediate-finish t
                               )
                              )
                   )

                  (,(format "%s\tInteresting" (all-the-icons-faicon "eye" :face 'all-the-icons-lcyan :v-adjust 0.01))
                   :keys "i"
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Interesting"
                   :type entry
                   :template ("* [ ] %{desc}%? :%{i-type}:"
                              "%i %a")
                   :children ((,(format "%s\tWebpage" (all-the-icons-faicon "globe" :face 'all-the-icons-green :v-adjust 0.01))
                               :keys "w"
                               :desc "%(org-cliplink-capture) "
                               :i-type "read:web"
                               )
                              (,(format "%s\tArticle" (all-the-icons-octicon "file-text" :face 'all-the-icons-yellow :v-adjust 0.01))
                               :keys "a"
                               :desc ""
                               :i-type "read:reaserch"
                               )
                              (,(format "%s\tInformation" (all-the-icons-faicon "info-circle" :face 'all-the-icons-blue :v-adjust 0.01))
                               :keys "i"
                               :desc ""
                               :i-type "read:info"
                               )
                              (,(format "%s\tIdea" (all-the-icons-material "bubble_chart" :face 'all-the-icons-silver :v-adjust 0.01))
                               :keys "I"
                               :desc ""
                               :i-type "idea"
                               )))
                  (,(format "%s\tTasks" (all-the-icons-octicon "inbox" :face 'all-the-icons-yellow :v-adjust 0.01))
                   :keys "k"
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Tasks"
                   :type entry
                   :template ("* TODO %? %^G%{extra}"
                              "%i")
                   :children ((,(format "%s\tGeneral Task" (all-the-icons-octicon "inbox" :face 'all-the-icons-yellow :v-adjust 0.01))
                               :keys "k"
                               :extra ""
                               )
                              (,(format "%s\tTask with deadline" (all-the-icons-material "timer" :face 'all-the-icons-orange :v-adjust -0.1))
                               :keys "d"
                               :extra "\nDEADLINE: %^{Deadline:}t"
                               )
                              (,(format "%s\tScheduled Task" (all-the-icons-octicon "calendar" :face 'all-the-icons-orange :v-adjust 0.01))
                               :keys "s"
                               :extra "\nSCHEDULED: %^{Start time:}t"
                               )
                              ))
                  (,(format "%s\tMeeting" (all-the-icons-octicon "repo" :face 'all-the-icons-silver :v-adjust 0.01))
                   :keys "m"
                   :file +org-capture-todo-file
                   ;; :prepend t
                   ;; :headline "Meetings"
                   :type entry
                   :clock-in t
                   :clock-resume: t
                   :template ("* [%(org-read-date nil nil org-read-date-final-answer)] %? %^G:meeting:"
                              "%U %i (scheduled for %^t)")
                   )
                  (,(format "%s\tProject" (all-the-icons-octicon "repo" :face 'all-the-icons-silver :v-adjust 0.01))
                   :keys "p"
                   :prepend t
                   :type entry
                   :headline "Inbox"
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :file ""
                   :custom (:time-or-todo "")
                   :children ((,(format "%s\tProject-local todo" (all-the-icons-octicon "checklist" :face 'all-the-icons-green :v-adjust 0.01))
                               :keys "t"
                               :time-or-todo "TODO"
                               :file +org-capture-project-todo-file)
                              (,(format "%s\tProject-local note" (all-the-icons-faicon "sticky-note" :face 'all-the-icons-yellow :v-adjust 0.01))
                               :keys "n"
                               :time-or-todo "%U"
                               :file +org-capture-project-notes-file)
                              (,(format "%s\tProject-local changelog" (all-the-icons-faicon "list" :face 'all-the-icons-blue :v-adjust 0.01))
                               :keys "c"
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-project-changelog-file))
                   )
                  ("\tCentralised project templates"
                   :keys "o"
                   :type entry
                   :prepend t
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :children (("Project todo"
                               :keys "t"
                               :prepend nil
                               :time-or-todo "TODO"
                               :heading "Tasks"
                               :file +org-capture-central-project-todo-file)
                              ("Project note"
                               :keys "n"
                               :time-or-todo "%U"
                               :heading "Notes"
                               :file +org-capture-central-project-notes-file)
                              ("Project changelog"
                               :keys "c"
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-central-project-changelog-file))
                   ))))

;; TODO: why is this changed?
(setq org-protocol-default-template-key "p")

