;;; calendar.el --- Calendar: org-caldav sync + calfw view -*- lexical-binding: t; -*-

(use-package org-caldav
  :ensure t
  :defer t
  :config
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
     :view 'two-weeks))
  :custom
  (calfw-org-capture-template
   '("k" "Calendar capture" entry (file bergheim/calendar/nextcloud/local)
     "* %^{Title}\n<%(bergheim/format-scheduled-time :start (bergheim/ask-time \"Start Time: \") :end (bergheim/ask-time \"End Time: \"))>\n\n%?"))
  :config
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
   "gb" #'calfw-navi-goto-first-date-command
   "gB" #'calfw-navi-goto-last-date-command
   "J" #'calfw-org-goto-date
   "[" #'calfw-navi-prev-view
   "]" #'calfw-navi-next-view
   "A" #'calfw-org-open-agenda-day
   "C" #'calfw-org-capture
   "q" #'calfw-org-clean-exit
   "gr" #'calfw-refresh-calendar-buffer
   "gR" #'(lambda ()
            (interactive)
            (org-caldav-sync)
            (calfw-refresh-calendar-buffer))
   "T" #'calfw-navi-goto-today-command
   "gt" #'calfw-navi-goto-today-command
   "d" #'calfw-change-view-day
   "w" #'calfw-change-view-week
   "m" #'calfw-change-view-month)

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
