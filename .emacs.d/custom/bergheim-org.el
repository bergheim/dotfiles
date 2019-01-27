(global-set-key (kbd "C-c c") 'org-capture)

(setq spaceline-org-clock-p t
      org-deadline-warning-days 14
      ;; show tasks scheduled or due in next fortnight
      org-agenda-span (quote fortnight)
      ;;don't show tasks as scheduled if they are already shown as a deadline
      org-agenda-skip-scheduled-if-deadline-is-shown t

      org-directory (file-truename "~/org")
      org-journal-dir "~/org/journal/"
      org-default-notes-file "~/org/inbox.org"
      org-agenda-start-on-weekday 1 ;; start on monday
      org-use-fast-todo-selection t

      ;; include this file and any file contributing to the agenda
      org-refile-targets '((nil :maxlevel . 4)
                           (org-agenda-files :maxlevel . 4))
      ;; include the file in the refile search
      org-refile-use-outline-path 'file
      org-deadline-warning-days 7
      org-enable-bootstrap-support t ;; prettier html exports
      org-enable-reveal-js t ;; hipster html presentation exports
      org-journal-enable-agenda-integration t
      org-agenda-compact-blocks nil ;; don't compact the agenda
      ;; carry over _all_ items except done
      org-journal-carryover-items "-TODO=\"DONE\"-TODO=\"CANCELLED\""
      ;; give me all the possible completions at once so helm can present them
      org-outline-path-complete-in-steps nil)
      ;; (setq org-agenda-default-appointment-duration 60)
      ;; (setq org-agenda-skip-scheduled-if-done t)

;; This is a much better "see last clocked in tasks package"
(require 'org-mru-clock)
(setq org-mru-clock-how-many 100)
(setq org-mru-clock-completing-read #'ivy-completing-read)

(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

;; add all org files recursively
;; (setq org-agenda-clockreport-parameter-plist
;;       (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80 :formula %)))
;; (setq org-agenda-time-grid
;;       (quote
;;        ((daily today remove-match)
;;         (900 1100 1300 1500 1700)
;;         "......" "----------------")))

(setq org-agenda-files (apply 'append
                              (mapcar
                               (lambda (directory)
                                 (directory-files-recursively
                                  directory org-agenda-file-regexp))
                               '("~/org")))) ;; TODO set this to org-directory

;; add the project TODO files to the agenda as well
(with-eval-after-load 'org-agenda
  (require 'org-projectile)
  (mapcar #'(lambda (file)
             (when (file-exists-p file)
               (push file org-agenda-files)))
          (org-projectile-todo-files)))

(setq org-todo-keywords
      '((sequence "TODO(t)"
                  "INPROGRESS(i)"
                  "WAITING(w)"
                  "SOMEDAY(.)"
                  "|" "DONE(x!)" "CANCELLED(c@)")
        (sequence "NEXT(n)"
                  "|" "DONE(x!)" "CANCLELLED(c@)")
        (sequence "BUG(b)" "|" "FIXED(f)")
        (sequence "READ(r)" "|" "DONE(x!)")
        ))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "yellow" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              )))

(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))

(setq org-tag-alist '(("@work" . ?w) ("@life" . ?l)))

;; The triggers break down to the following rules:
;;
;; Moving a task to CANCELLED adds a CANCELLED tag
;; Moving a task to WAITING adds a WAITING tag
;; Moving a task to HOLD adds WAITING and HOLD tags
;; Moving a task to a done state removes WAITING and HOLD tags
;; Moving a task to TODO removes WAITING, CANCELLED, and HOLD tags
;; Moving a task to NEXT removes WAITING, CANCELLED, and HOLD tags
;; Moving a task to DONE removes WAITING, CANCELLED, and HOLD tags
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-capture-templates
      (quote (("t" "todo" entry (file org-default-notes-file)
               ;; "* TODO %?\n%T\n" :clock-in t :clock-resume t)
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file org-default-notes-file)
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file org-default-notes-file)
               "* %? :NOTE:\n%T\n" :clock-in t :clock-resume t)
              ("b" "bug" entry (file org-default-notes-file)
               "* %? :BUG:\n%T\n" :clock-in t :clock-resume t)
              ("j" "journal" entry (file+datetree (concat org-directory "journal.org"))
               "* %?\n%T\n" :clock-in t :clock-resume t)
              ("m" "meeting" entry (file org-default-notes-file)
               "* MEETING with %? :MEETING:\n%T" :clock-in t :clock-resume t))))

;; this saves on refile, but I really should add something else as well. Maybe a
;; timer each hour or something. This is used for git ssync issues
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(require 'org-habit)
(load-file (expand-file-name "~/.emacs.d/custom/bh-org.el"))

;; (load-file (expand-file-name "~/.emacs.d/cadair-org-mode.el"))

;;;; Nicked from
;;;; http://stackoverflow.com/questions/23517372/hook-or-advice-when-aborting-org-capture-before-template-selection
(defadvice org-capture
    (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup"
  (if (equal "floating emacs-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "floating emacs-capture" (frame-parameter nil 'name))
      (delete-frame)))

;; Exclude DONE state tasks from refile targets
(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-CANCELLED+WAITING|HOLD/!"
                           ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil))))
