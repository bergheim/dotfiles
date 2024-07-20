;;; settings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

(setq org-directory (concat bergheim/home-directory "org/")
      org-deadline-warning-days 7

      org-confirm-babel-evaluate nil

      ;; t means adapt indentation to outline node level
      org-adapt-indentation t

      ;; continue on https://hugocisneros.com/org-config/
      calendar-date-style 'european
      org-icalendar-timezone "Europe/Oslo"
      org-icalendar-alarm-time 30
      org-extend-today-until 4

      ;; show tasks scheduled or due in next fortnight
      org-agenda-span 14
      ;;don't show tasks as scheduled if they are already shown as a deadline
      org-agenda-skip-scheduled-if-deadline-is-shown t

      ;; include things from Emacs' calendar diary
      org-agenda-include-diary t
      org-agenda-insert-diary-extract-time t

      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      ;; I had this but not really sure where the difference is
      org-agenda-block-separator nil
      ;; this either
      org-agenda-compact-blocks t

      org-agenda-breadcrumbs-separator " ❱ "

      ;; start on monday instead of current day
      org-agenda-start-on-weekday 1

      ;; https://github.com/correl/dotfiles/blob/master/.doom.d/config.org
      ;; https://github.com/PRDeltoid/doom-dotfiles/blob/master/config.el
      ;; this is by default a level. do I really want a project tag?
      ;; org-stuck-projects '("+project/-DONE" ("TODO" "NEXT") nil "")

      ;; stamp a CLOSED: [X] on DONE items
      org-log-done 'time
      org-log-into-drawer t

      org-log-reschedule 'note
      org-log-redeadline 'note

      org-agenda-start-with-log-mode t

      ;; TODO: remove this if it results in too much slowdown. Time spc n S for instance
      org-use-property-inheritance t

      ;; org-default-notes-file "~/org/inbox.org"
      ;; org-use-fast-todo-selection t
      ;; org-export-with-todo-keywords nil

      org-refile-targets '((nil :maxlevel . 4)
                           (org-agenda-files :maxlevel . 4))

      ;; include the file in the refile search
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil

      ;; org-agenda-compact-blocks nil ;; don't compact the agenda
      ;; give me all the possible completions at once so helm can present them
      ;; org-outline-path-complete-in-steps nil)
      ;; org-agenda-default-appointment-duration 60

      org-agenda-files (append (file-expand-wildcards (concat org-directory "*.org"))
                               (list bergheim/calendar/nextcloud/local)
                               (directory-files-recursively (expand-file-name "projects" org-directory) "\\.org$"))

      ;; org-agenda-clockreport-parameter-plist
      ;; (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80))

      org-habit-graph-column 60

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

      org-attach-id-dir "data/"

      org-protocol-default-template-key "z"

      org-todo-keywords
      '((sequence "TODO(t)"
                  "INPROGRESS(i!)"
                  "NEXT(n)"
                  "WAITING(w@/!)"
                  "SOMEDAY(s!)"
                  "|" "DONE(d@)" "CANCELLED(c@/!)")
        (sequence "BUG(b)" "|" "FIXED(f!)" "IGNORED(x@/!)")
        (sequence "DRAFT(D)" "POST(p)" "|" "PUBLISH(b)")))

(add-to-list 'org-modules 'org-habit)

(advice-add 'org-archive-subtree :after #'org-save-all-org-buffers)
(advice-add 'org-refile :after #'org-save-all-org-buffers)
;; (advice-add #'org-todo :after (lambda (&rest _) (org-save-all-org-buffers)))
;; (add-hook 'org-clock-in-hook #'org-save-all-org-buffers)
;; (add-hook 'org-clock-out-hook #'org-save-all-org-buffers)

;; open new notes etc in insert mode
(add-hook 'org-log-buffer-setup-hook #'evil-insert-state)

(unless (file-exists-p org-directory)
  (make-directory org-directory))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (calc . t)
   (js .t)))

;; TODO: what is this again
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

;;; settings.el ends here
