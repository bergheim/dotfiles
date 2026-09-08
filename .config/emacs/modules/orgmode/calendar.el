;;; calendar.el --- Calendar: org-caldav sync + calfw view -*- lexical-binding: t; -*-

(use-package org-caldav
  :ensure t
  :defer t
  :config
  ;; TODO: remove once https://github.com/dengste/org-caldav/pull/348 lands
  ;; (Emacs 31 warns about the missing lexical-binding cookie in the sync
  ;; state file on every sync)
  (advice-add
   'org-caldav-save-sync-state :after
   (lambda (&rest _)
     (let ((f (org-caldav-sync-state-filename org-caldav-calendar-id)))
       (when (file-exists-p f)
         (with-temp-file f
           (insert ";; -*- lexical-binding: t -*-\n")
           (insert-file-contents f))))))

  ;; TODO: remove once https://github.com/dengste/org-caldav/pull/349 lands
  ;; (issue #323: description lines starting with `*' become headings and
  ;; corrupt the inbox)
  (advice-add
   'org-caldav--insert-description :override
   (lambda (description)
     (when (> (length description) 0)
       (when org-caldav-description-blank-line-before (newline))
       (let ((beg (point)))
         (insert description)
         (org-indent-region beg (point))
         (let ((end (point-marker)))
           (save-excursion
             (goto-char beg)
             (while (re-search-forward "^\\*" end t)
               (replace-match " *" t t)))))
       (when org-caldav-description-blank-line-after (newline))
       (newline))))
  ;; apparently these are experimental
  (setq org-icalendar-include-todo 'all
        org-caldav-sync-todo t
        org-caldav-todo-percent-states
        '((0 "TODO") (1 "NEXT") (2 "INPROGRESS") (100 "DONE")))
  (setq org-caldav-url bergheim/calendar/nextcloud
        org-caldav-delete-calendar-entries 'ask
        org-caldav-save-directory (expand-file-name "caldav" org-directory)
        org-caldav-calendar-id "personal"
        org-caldav-files `(,(expand-file-name "caldav/caldav-appointments.org" org-directory))
        org-caldav-inbox bergheim/calendar/nextcloud/local
        org-caldav-calendars `((:calendar-id "personal"
                                :inbox ,bergheim/calendar/nextcloud/local))))

(use-package calfw
  :after org
  :demand t
  :init
  (defun bergheim/ask-time (prompt)
    "Ask for time using PROMPT."
    (let ((time (read-string prompt)))
      (unless (string= time "")
        (let* ((parts (split-string time ":"))
               (hours (car parts))
               (minutes (or (cadr parts) "00")))
          (format "%s:%02d" hours (string-to-number minutes))))))

  (cl-defun bergheim/format-scheduled-time (&key (start nil) (end nil) (date nil))
    "Format START and END time to return a proper org-mode timestamp."
    (let* ((date (or date (substring (calfw-org-capture-day) 1 11))) ; extract "2024-01-25"
           (end (or end
                    (when start
                      (format-time-string "%H:%M"
                                          (time-add (date-to-time (concat date " " start))
                                                    (seconds-to-time 3600))))))
           (time (format " %s%s" (or start "") (when end (concat "-" end)))))
      (if start
          (concat date time)
        date)))

  (defun bergheim/open-calendar ()
    (interactive)
    (calfw-open-calendar-buffer
     :contents-sources
     (list
      (calfw-org-create-file-source "personal" bergheim/calendar/nextcloud/local "DarkGreen"))
     ;; :view 'block-5-day
     :view 'two-weeks)
    (calfw-navi-goto-today-command))
  :custom
  (calfw-org-capture-template
   '("k" "Calendar capture" entry (file bergheim/calendar/nextcloud/local)
     "* %^{Title}\n<%(bergheim/format-scheduled-time :start (bergheim/ask-time \"Start Time: \") :end (bergheim/ask-time \"End Time: \"))>\n\n%?"))
  :config
  (defun bergheim/calfw-sync-and-refresh ()
    "Run `org-caldav-sync' and redraw the calendar."
    (interactive)
    (org-caldav-sync)
    (calfw-refresh-calendar-buffer))
  ;; autosync after capture from calfw
  (defun bergheim//caldav-sync-hook ()
    (when (string= (org-capture-get :key) "k")
      (org-caldav-sync)))
  (add-hook 'org-capture-after-finalize-hook #'bergheim//caldav-sync-hook)
  :general
  (general-define-key
   :states '(normal insert emacs motion visual)
   :keymaps 'calfw-calendar-mode-map
   "RET" #'calfw-show-details-command
   "h" #'calfw-navi-previous-day-command
   "l" #'calfw-navi-next-day-command
   "j" #'calfw-navi-next-week-command
   "k" #'calfw-navi-previous-week-command
   "0" #'calfw-navi-goto-week-begin-command
   "$" #'calfw-navi-goto-week-end-command
   "gb" #'calfw-navi-goto-first-date-command
   "gB" #'calfw-navi-goto-last-date-command
   "J" #'calfw-org-goto-date
   "[" #'calfw-navi-prev-view
   "]" #'calfw-navi-next-view
   "A" #'calfw-org-open-agenda-day
   "C" #'calfw-org-capture
   "q" #'calfw-org-clean-exit
   "gr" #'calfw-refresh-calendar-buffer
   "gR" #'bergheim/calfw-sync-and-refresh
   "T" #'calfw-navi-goto-today-command
   "gt" #'calfw-navi-goto-today-command
   "d" #'calfw-change-view-day
   "w" #'calfw-change-view-week
   "m" #'calfw-change-view-month)

  (bergheim/localleader-keys
   :states '(normal motion)
   :keymaps 'calfw-calendar-mode-map
   "" '(:ignore t :which-key "calendar")
   "c" '(calfw-org-capture :which-key "create event")
   "r" '(calfw-refresh-calendar-buffer :which-key "refresh")
   "s" '(bergheim/calfw-sync-and-refresh :which-key "sync caldav")
   "g" '(calfw-org-goto-date :which-key "goto date")
   "t" '(calfw-navi-goto-today-command :which-key "today")
   "a" '(calfw-org-open-agenda-day :which-key "agenda day")
   "d" '(calfw-change-view-day :which-key "day view")
   "w" '(calfw-change-view-week :which-key "week view")
   "W" '(calfw-change-view-two-weeks :which-key "two weeks view")
   "m" '(calfw-change-view-month :which-key "month view")
   "q" '(calfw-org-clean-exit :which-key "quit"))

  (general-define-key
   :states '(normal insert emacs motion visual)
   :keymaps 'calfw-details-mode-map
   "q" #'calfw-details-kill-buffer-command
   "M-n" #'calfw-details-navi-next-command
   "M-p" #'calfw-details-navi-prev-command))

(use-package calfw-org
  :after calfw
  :demand)

(use-package calfw-ical
  :after calfw)
