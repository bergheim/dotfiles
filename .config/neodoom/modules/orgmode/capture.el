;;; capture.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim
;;
;; Author: Thomas Bergheim
;; Maintainer: Thomas Bergheim
;; Created: September 18, 2023
;; Modified: September 18, 2023
;; Version: 0.0.1

;; declarative capture templates
(use-package doct
  :ensure t
  ;;recommended: defer until calling doct
  :commands (doct))

(use-package org-capture
  :elpaca nil
  :after org
  :config
  (setq org-capture-templates
        (doct `(("Personal"
                 :icon ("nf-cod-person" :set "codicon" :color "green")
                 :keys "p"
                 :prepend true
                 :file +org-capture-personal-file
                 :type entry
                 :default-tags "@life"
                 :clock-in t
                 :clock-resume t
                 :children (("Tasks"
                             :icon ("nf-oct-inbox" :set "octicon" :color "yellow")
                             :keys "t"
                             :extra ""
                             :headline "Tasks"
                             :template-file ,(expand-file-name "task.org" org-capture-custom-template-directory)
                             :children (("General Task" :keys "t"
                                         :icon ("nf-oct-inbox" :set "octicon" :color "yellow")
                                         :extra "")
                                        ("Task with deadline" :keys "d"
                                         :icon ("nf-md-timer" :set "mdicon" :color "orange" :v-adjust -0.1)
                                         :extra "\nDEADLINE: %^{Deadline:}t")
                                        ("Scheduled Task" :keys "s"
                                         :icon ("nf-oct-calendar" :set "octicon" :color "orange")
                                         :extra "\nSCHEDULED: %^{Start time:}t")))

                            ("Enter a note"
                             :icon ("nf-fa-sticky_note_o" :set "faicon" :color "green")
                             :keys "n"
                             :headline "Notes"
                             :template-file ,(expand-file-name "note.org" org-capture-custom-template-directory))

                            ("Meeting"
                             :icon ("nf-oct-repo" :set "octicon" :color "silver")
                             :keys "m"
                             :headline "Meetings"
                             :template-file ,(expand-file-name "meeting.org" org-capture-custom-template-directory))))

                ("Work"
                 :keys "w"
                 :icon ("nf-fae-planet" :set "faicon" :color "yellow")
                 :prepend t
                 :file +org-capture-work-file
                 :type entry
                 :clock-in t
                 :clock-resume t
                 :default-tags "@work"
                 :children (("Tasks"
                             :keys "t"
                             :icon ("nf-oct-inbox" :set "octicon" :color "yellow")
                             :headline "Tasks"
                             :template-file ,(expand-file-name "task.org" org-capture-custom-template-directory)
                             :children (("General Task" :keys "t"
                                         :icon ("nf-oct-inbox" :set "octicon" :color "yellow")
                                         :extra "")
                                        ("Task with deadline" :keys "d"
                                         :icon ("nf-md-timer" :set "mdicon" :color "orange" :v-adjust -0.1)
                                         :extra "\nDEADLINE: %^{Deadline:}t")
                                        ("Scheduled Task" :keys "s"
                                         :icon ("nf-oct-calendar" :set "octicon" :color "orange")
                                         :extra "\nSCHEDULED: %^{Start time:}t")))
                            ("Bug"
                             :icon ("nf-oct-bug" :set "octicon" :color "green")
                             :keys "b"
                             :headline "Bugs"
                             :template-file ,(expand-file-name "bug.org" org-capture-custom-template-directory)
                             :children (("General bug"
                                         :icon ("nf-oct-inbox" :set "octicon" :color "yellow")
                                         :keys "b"
                                         :extra "")
                                        ("Bug with deadline"
                                         :icon ("nf-md-timer" :set "mdicon" :color "orange")
                                         :keys "d"
                                         :extra "\nDEADLINE: %^{Deadline:}t")
                                        ("Scheduled bug"
                                         :icon ("nf-oct-calendar" :set "octicon" :color "orange")
                                         :keys "s"
                                         :extra "\nSCHEDULED: %^{Start time:}t")))
                            ("Meeting"
                             :icon ("nf-oct-repo" :set "octicon" :color "silver")
                             :keys "m"
                             :jump-to-captured t
                             :file "~/org/roam/work/meetings.org"
                             :headline "Meetings"
                             :template-file ,(expand-file-name "meeting.org" org-capture-custom-template-directory))))

                ("Capture to clocked in task" :keys "c"
                 :icon ("nf-md-email" :set "mdicon" :color "green")
                 :type entry
                 :prepend t
                 :clock t
                 :template-file ,(expand-file-name "clocked.org" org-capture-custom-template-directory))

                ("Interrupted" :keys "i"
                 :icon ("nf-fa-stop_circle" :set "faicon" :color "red")
                 :file +org-capture-work-file
                 :type entry
                 :clock-in t
                 :clock-resume t
                 :headline "Interruptions"
                 :default-tags "@work:interrupted"
                 :template-file ,(expand-file-name "interrupted.org" org-capture-custom-template-directory))

                ("Review"
                 :keys "r"
                 :icon ("nf-oct-code_review" :set "octicon" :color "yellow")
                 :type entry
                 :clock-in t
                 :clock-keep t
                 :file "~/org/review.org"
                 :jump-to-captured t
                 :default-tags "review"
                 :children (("Daily review"
                             :icon ("nf-md-calendar_today" :set "mdicon" :color "green")
                             :keys "r"
                             :headline "Daily"
                             :default-tags "@work:daily:review"
                             :template-file ,(expand-file-name "review-daily.org" org-capture-custom-template-directory))
                            ("Weekly review"
                             :icon ("nf-md-calendar_weekend" :set "mdicon" :color "green")
                             :keys "w"
                             :headline "Weekly"
                             :default-tags "@work:weekly:review"
                             :template-file ,(expand-file-name "review-weekly.org" org-capture-custom-template-directory))
                            ("Monthly review"
                             :icon ("nf-fa-lightbulb_o" :set "faicon" :color "green")
                             :keys "m"
                             :headline "Monthly"
                             :default-tags "@work:monthly:review"
                             :template-file ,(expand-file-name "review-monthly.org" org-capture-custom-template-directory))
                            ("Yearly review"
                             :icon ("nf-md-calendar_today" :set "mdicon" :color "green")
                             :keys "y"
                             :headline "Yearly"
                             :default-tags "yearly:review"
                             :template-file ,(expand-file-name "review-yearly.org" org-capture-custom-template-directory))))

                ("Add contact" :keys "C"
                 :icon ("nf-oct-person" :set "octicon" :color "green")
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

                ;; TODO: add back once we have a working setup
                ;; ("Active project" :keys "a"
                ;;  :icon ("nf-oct-repo" :set "octicon" :color "green")
                ;;  :prepend t
                ;;  :type entry
                ;;  :headline "Inbox"
                ;;  :template-file ,(expand-file-name "active-project.org" org-capture-custom-template-directory)
                ;;  :custom (:time-or-todo "")
                ;;  :children (("Project-local todo" :keys "t"
                ;;              :icon ("nf-oct-checklist" :set "octicon" :color "green")
                ;;              :time-or-todo "TODO"
                ;;              :file +org-capture-project-todo-file)
                ;;             ("Project-local note" :keys "n"
                ;;              :icon ("nf-fa-sticky_note" :set "faicon" :color "yellow")
                ;;              :time-or-todo "%U"
                ;;              :file +org-capture-project-notes-file)
                ;;             ("Project-local changelog" :keys "c"
                ;;              :icon ("nf-fa-list" :set "faicon" :color "blue")
                ;;              :time-or-todo "%U"
                ;;              :headline "Unreleased"
                ;;              :file +org-capture-project-changelog-file)))

                ("Interesting"
                 :keys "I"
                 :icon ("nf-fa-eye" :set "faicon" :color "lcyan")
                 :file +org-capture-todo-file
                 :prepend t
                 :headline "Interesting"
                 :type entry
                 :template-file ,(expand-file-name "interesting.org" org-capture-custom-template-directory)
                 :children (("Webpage" :keys "w"
                             :icon ("nf-fa-globe" :set "faicon" :color "green")
                             :desc "%(org-cliplink-capture) "
                             :i-type "read:web")
                            ("Article" :keys "a"
                             :icon ("nf-fa-file_text" :set "faicon" :color "yellow")
                             :desc ""
                             :i-type "read:research")
                            ("Information" :keys "i"
                             :icon ("nf-fa-info_circle" :set "faicon" :color "blue")
                             :desc ""
                             :i-type "read:info")
                            ("Idea" :keys "I"
                             :icon ("nf-md-chart_bubble" :set "mdicon" :color "silver")
                             :desc ""
                             :i-type "idea")))

                ("Protocol Link Marked" :keys "z"
                 :icon ("nf-oct-stop" :set "octicon" :color "red")
                 :type entry
                 :prepend t
                 :headline "Protocol"
                 :file +org-capture-todo-file
                 :immediate-finish t
                 :template-file ,(expand-file-name "protocol-marked.org" org-capture-custom-template-directory))

                ("Protocol Link Unmarked" :keys "Z"
                 :icon ("nf-oct-stop" :set "octicon" :color "red")
                 :type entry
                 :prepend t
                 :headline "Protocol"
                 :file +org-capture-todo-file
                 :immediate-finish t
                 :template-file ,(expand-file-name "protocol-unmarked.org" org-capture-custom-template-directory))

                ("Protocol Link Active Task" :keys "o"
                 :icon ("nf-oct-stop" :set "octicon" :color "red")
                 :type entry
                 :prepend t
                 :clock t
                 :immediate-finish t
                 :template-file ,(expand-file-name "protocol-active-task.org" org-capture-custom-template-directory))

                ("Email Workflow" :keys "e"
                 :icon ("nf-oct-stop" :set "octicon" :color "red")
                 :type entry
                 :file +org-capture-mail-file
                 :children (("Follow Up" :keys "f"
                             :headline "Follow Up"
                             :clock-in t
                             :file +org-capture-mail-file
                             :template-file ,(expand-file-name "email-follow-up.org" org-capture-custom-template-directory))
                            ("Read Later" :keys "l"
                             :template-file ,(expand-file-name "email-read-later.org" org-capture-custom-template-directory)
                             :immediate-finish t
                             :headline "Read Later")))))))


;; TODO: add proper capture templates for persons. use this as a baseline..?
;; (use-package org-capture
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


;;; capture.el ends here
