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

(defun bergheim/org-capture-quote-or-nothing ()
  (let ((selected-text (plist-get org-store-link-plist :initial)))
    (if (equal selected-text "")
        ""
      (concat "#+BEGIN_QUOTE\n" (s-trim selected-text) "\n#+END_QUOTE\n"))))

(use-package org-capture
  :ensure nil
  :after org
  :config
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  (setq org-capture-custom-template-directory (concat org-directory "templates/capture/")
        +org-capture-contacts-file (expand-file-name "contacts.org" org-directory)
        +org-capture-habits-file (expand-file-name "habits.org" org-directory)
        +org-capture-mail-followup-file (expand-file-name "email/followup.org" org-directory)
        +org-capture-mail-later-file (expand-file-name "email/later.org" org-directory)
        +org-capture-personal-file (concat org-directory "personal.org")
        +org-capture-review-file (expand-file-name "review.org" org-directory)
        +org-capture-protocol-file (expand-file-name "inbox.org" org-directory)
        +org-capture-quotes-file (expand-file-name "quotes.org" org-directory)
        +org-capture-work-file (concat org-directory "work.org")
        +org-capture-work-meeting (concat org-directory "roam/work/meetings.org")
        org-capture-templates
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

                            ("Appointment"
                             :icon ("nf-fa-calendar" :set "faicon" :color "green")
                             :keys "a"
                             :prepend nil
                             :file bergheim/calendar/nextcloud/local
                             :template-file ,(expand-file-name "appointment.org" org-capture-custom-template-directory)
                             :after-finalize org-caldav-sync)

                            ("Meeting minutes"
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
                             :file +org-capture-work-meeting
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

                ("Spaced repetition" :keys "s"
                 :icon ("nf-fa-repeat" :set "faicon" :color "red")
                 :file +org-capture-habits-file
                 :olp ("Skill honing")
                 :template-file ,(expand-file-name "recall.org" org-capture-custom-template-directory)
                 :immediate-finish nil)

                ("Review"
                 :keys "r"
                 :icon ("nf-oct-code_review" :set "octicon" :color "yellow")
                 :type entry
                 :clock-in t
                 :clock-keep t
                 :file +org-capture-review-file
                 :jump-to-captured t
                 :default-tags "review"
                 :children (("Daily review"
                             :icon ("nf-md-calendar_today" :set "mdicon" :color "green")
                             :keys "r"
                             :headline "Daily"
                             :default-tags "daily:review"
                             :hook org-caldav-sync
                             :template-file ,(expand-file-name "review-daily.org" org-capture-custom-template-directory))
                            ("Weekly review"
                             :icon ("nf-md-calendar_weekend" :set "mdicon" :color "green")
                             :keys "w"
                             :headline "Weekly"
                             :default-tags "weekly:review"
                             :template-file ,(expand-file-name "review-weekly.org" org-capture-custom-template-directory))
                            ("Monthly review"
                             :icon ("nf-fa-lightbulb_o" :set "faicon" :color "green")
                             :keys "m"
                             :headline "Monthly"
                             :default-tags "monthly:review"
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
                 :file +org-capture-contacts-file
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
                 :file +org-capture-personal-file
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

                ("Protocol Capture" :keys "z"
                 :icon ("nf-oct-stop" :set "octicon" :color "red")
                 :type entry
                 :prepend nil
                 :headline "Protocol"
                 :file +org-capture-protocol-file
                 :immediate-finish t
                 :template-file ,(expand-file-name "protocol-capture.org" org-capture-custom-template-directory))

                ("Protocol Capture Active Task" :keys "o"
                 :icon ("nf-oct-stop" :set "octicon" :color "red")
                 :type entry
                 :prepend t
                 :clock t
                 :immediate-finish t
                 :template-file ,(expand-file-name "protocol-active-task.org" org-capture-custom-template-directory))

                ("Add quote" :keys "Q"
                 :icon ("nf-md-email" :set "mdicon" :color "green")
                 :type entry
                 :prepend t
                 ;; :clock t
                 :file +org-capture-quotes-file
                 :template-file ,(expand-file-name "quote.org" org-capture-custom-template-directory))

                ("Email Workflow" :keys "e"
                 :icon ("nf-oct-stop" :set "octicon" :color "red")
                 :type entry
                 :children (("Follow Up" :keys "f"
                             :clock-in t
                             :file +org-capture-mail-followup-file
                             :function bergheim/org-email-follow-up
                             :template-file ,(expand-file-name "email-follow-up.org" org-capture-custom-template-directory))
                            ("Read Later" :keys "l"
                             :file +org-capture-mail-later-file
                             :template-file ,(expand-file-name "email-read-later.org" org-capture-custom-template-directory)
                             :immediate-finish t
                             :headline "Read Later")))))))

(defun bergheim//delete-frame-after-capture ()
  "Delete frame after capturing."
  (delete-frame)
  (remove-hook 'org-capture-after-finalize-hook 'bergheim//delete-frame-after-capture))

(defun bergheim/capture ()
  "Capture externally"
  (interactive)
  (delete-other-windows)

  (add-hook 'org-capture-after-finalize-hook #'bergheim//delete-frame-after-capture)

  ;; HACK to make the capture fullscreen
  (cl-letf (((symbol-function 'switch-to-buffer-other-window)
             (symbol-function 'switch-to-buffer)))
    (org-capture)))

;; (defun bergheim/capture ()
;;   "Create a new frame and run `org-capture'."
;;   (interactive)
;;   (make-frame '((name . "floating emacs-capture")
;;                 (top . 300)
;;                 (left . 700)
;;                 (width . 80)
;;                 (height . 25)))
;;   (select-frame-by-name "floating emacs-capture")
;;   (delete-other-windows)
;;   (add-hook 'org-capture-after-finalize-hook #'bergheim//delete-frame-after-capture)
;;   (cl-letf (((symbol-function 'switch-to-buffer-other-window)
;;              (symbol-function 'switch-to-buffer)))
;;     (org-capture)))

(defun bergheim/org-email-follow-up ()
  "Select a follow-up email category"
  (save-window-excursion
    (let* ((file +org-capture-mail-followup-file  )
           (headings (org-ql-select +org-capture-mail-followup-file  '(level 2)
                       :action (lambda ()
                                 (org-get-heading t t t t))))
           (choice (completing-read "Category: " headings)))
      (find-file file)
      (goto-char (org-find-exact-headline-in-buffer choice)))))

;;; capture.el ends here
