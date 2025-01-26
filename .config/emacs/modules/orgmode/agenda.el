;;; agenda.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

;; This drove me nuts! Unbind SPC in org-agenda-mode
(defun bergheim/org-agenda-setup-keys ()
  (evil-define-key 'motion org-agenda-mode-map (kbd "SPC") nil)
  (evil-define-key 'motion evil-org-agenda-mode-map (kbd "SPC") nil)
  (bergheim/localleader-keys
    :keymaps '(org-agenda-mode-map evil-org-agenda-mode-map)
    :states '(normal motion)
    "e" '(org-agenda-set-effort :which-key "Set effort")
    "o" 'org-agenda-goto
    "q" 'org-agenda-set-tags
    "t" 'org-agenda-todo))

(use-package org-agenda
  :ensure nil
  :commands (org-agenda)
  :after org
  ;; overwrite whatever is overwriting the binds
  :hook (org-agenda-mode . bergheim/org-agenda-setup-keys)
  :config

  ;; tags in the agenda is just clutter
  (setq org-agenda-remove-tags t)

  ;; agenda commands search archives as well
  (setq org-agenda-text-search-extra-files '(agenda-archives))

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
                         '((:name "Happy birthday 🎂"
                            :property "BIRTHDAY"
                            :order 2)
                           (:name "Keep your habits up 🔥"
                            :habit t
                            :order 3)

                           (:name "Currently working on 🧑‍🏭"
                            :todo "INPROGRESS"
                            :order 2)

                           (:name "Logged 📑" :log t :order 15)

                           (:discard (:todo "SOMEDAY"))
                           ;; (:name "Done today" :discard (:log t))

                           (:name "This is how your day looks 🌞"
                            :time-grid t
                            :order 1)

                           (:name "Waiting.. 😴"
                            :todo "WAITING"
                            :order 5)

                           (:name "First, do one of these 🐸"
                            :and (:deadline today :priority "A")
                            :deadline today
                            :and (:deadline past :priority "A")
                            :and (:scheduled t :priority "A")
                            :and (:scheduled past :priority "A")
                            :deadline past
                            :order 3)

                           (:name "Scheduled for today ⏰"
                            :scheduled today
                            :order 2)

                           (:name "Upcoming deadlines 🚌"
                            :deadline future
                            :order 2)

                           (:name "Follow up 📆"
                            :tag "email"
                            :order 4)

                           (:name "Do you still need to do these? 🤔"
                            :scheduled past
                            :order 5)
                           ))))))

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
                    ))))))

(use-package org-super-agenda
  :after org
  :demand
  ;; :commands (bergheim/org-super-agenda)
  :init
  (defun bergheim/org-super-agenda (&rest args)
    (interactive)
    (apply #'org-agenda args))
  :config
  ;; don't break evil on org-super-agenda headings, see
  ;; https://github.com/alphapapa/org-super-agenda/issues/50
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (org-super-agenda-mode 1)

  (org-super-agenda--def-auto-group
    bergheim/clocked-or-created
    "Group items based on the latest CLOCK or CREATED timestamp in the entry.
The date is formatted according to `org-super-agenda-date-format'."
    ;; :keyword :auto-
    :key-form
    (cl-labels ((latest-ts-up-to
                 (limit) ;; FIXME: What if the logbook is empty?
                 (-some->> (cl-loop for next-ts = (when (re-search-forward org-element--timestamp-regexp limit t)
                                                    (ts-parse-org (match-string 1)))
                                    while next-ts
                                    collect next-ts)
                   (-sort #'ts>)
                   car)))
      (org-super-agenda--when-with-marker-buffer
        (org-super-agenda--get-marker item)
        (let* ((created (when (org-entry-get (point) "CREATED")
                          (ts-parse-org (org-entry-get (point) "CREATED"))))
               (clocked (let ((drawer-name (org-clock-drawer-name))
                              (entry-end (org-entry-end-position))
                              ts)
                          (when (and drawer-name
                                     (re-search-forward (rx-to-string `(seq bol (0+ blank) ":" ,drawer-name ":"))
                                                        entry-end 'noerror)
                                     (org-at-drawer-p))
                            (latest-ts-up-to (save-excursion
                                               ;; End of drawer.
                                               (re-search-forward (rx bol (0+ blank) ":END:") entry-end))))))
               (latest-ts (car (sort (remq nil (list created clocked)) #'ts>))))
          (when latest-ts
            (propertize (ts-format org-super-agenda-date-format latest-ts)
                        'org-super-agenda-ts latest-ts)))))
    :key-sort-fn (lambda (a b)
                   (ts> (get-text-property 0 'org-super-agenda-ts a)
                        (get-text-property 0 'org-super-agenda-ts b)))))

;; TODO: verify this works (from https://github.com/unhammer/org-mru-clock/)
;; (add-hook 'minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook)

;;; agenda.el ends here
