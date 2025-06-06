;;; org.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

(use-package org
  :defer t
  :ensure nil
  :after general

  ;; LOL @this loading
  :config
  (bergheim/load-file "modules/orgmode/base.el")
  (bergheim/load-file "modules/orgmode/helpers.el")
  (bergheim/load-file "modules/orgmode/commands.el")
  (bergheim/load-file "modules/orgmode/keybindings.el")
  (bergheim/load-file "modules/orgmode/capture.el")
  (bergheim/load-file "modules/orgmode/agenda.el")
  (bergheim/load-file "modules/orgmode/roam.el")
  (bergheim/load-file "modules/orgmode/attachments.el")
  (bergheim/load-file "modules/orgmode/commands.el")
  (bergheim/load-file "modules/orgmode/style.el")

  :general
  (general-define-key
   :states '(normal insert visual)
   :keymaps 'org-mode-map
   "C-M-<return>" #'bergheim/org-insert-indented-todo-subheading

   "H-a" #'org-archive-subtree
   "H-r" #'org-refile
   "H-t" #'org-todo
   "H-q" #'org-set-tags-command

   "C-M-h" #'org-metaleft
   "C-M-j" #'org-metadown
   "C-M-k" #'org-metaup
   "C-M-l" #'org-metaright

   "C-M-S-h" #'org-shiftmetaleft
   "C-M-S-j" #'org-shiftmetadown
   "C-M-S-k" #'org-shiftmetaup
   "C-M-S-l" #'org-shiftmetaright)

  (general-define-key
   :states '(normal visual)
   :keymaps 'org-mode-map
   "RET" '+org/dwim-at-point
   "C-h" 'bergheim/org-move-up-header
   "C-l" 'bergheim/org-move-down-header
   "gh" 'bergheim/org-move-up-header
   "gl" 'bergheim/org-move-down-header)

  (bergheim/localleader-keys
    :keymaps 'org-mode-map
    "," 'org-insert-structure-template
    "a" '(org-attach :which-key "attach")
    "b" '(bergheim/org-open-attachments :which-key "open attachments")
    "c" '(bergheim/org-block :which-key "block")
    "C" '(bergheim/org-src-block :which-key "source block")
    "d" '(nil :which-key "Dates")
    "ds" 'org-schedule
    "dd" 'org-deadline
    "di" 'org-time-stamp-inactive
    "e" 'org-edit-special
    "l" '(nil :which-key "Links")
    "ld" '(org-super-links-quick-insert-drawer-link :which-key "drawer search")
    "li" '(org-insert-link :which-key "insert manually")
    "ll" '(org-super-links-quick-insert-inline-link :which-key "inline search")
    "lP" '((lambda () (interactive) (let ((org-super-links-related-into-drawer nil)) (org-super-links-insert-link))) :which-key "paste here")
    "lp" '(org-super-links-insert-link :which-key "paste to drawer")
    "ls" '(org-super-links-quick-insert-inline-link :which-key "inline search")
    "ly" '(org-super-links-store-link :which-key "copy")
    "i" 'org-toggle-item
    "n" 'bergheim/org-roam-create-node
    "h" 'org-toggle-heading
    "p" 'org-priority
    "q" 'org-set-tags-command
    "u" '(bergheim/org-copy-url-only :which-key "copy URL")
    "s" (cons "Subtree" (make-sparse-keymap))
    ;; "sa" 'bergheim/org-archive-subtree
    "sa" 'org-archive-subtree
    "sA" 'org-toggle-archive-tag
    "sb" 'org-tree-to-indirect-buffer
    "sc" 'org-clone-subtree-with-time-shift
    "sn" 'org-narrow-to-subtree
    "sN" 'widen
    "so" 'org-sort
    "sr" 'org-refile
    "ss" 'org-sparse-tree
    "x" 'org-table-blank-field
    "t" 'org-todo))

(elpaca-wait)

(use-package org-protocol
  :demand t
  :ensure nil)

;; see also https://github.com/akhramov/org-wild-notifier.el
(use-package org-alert
  :after org
  :demand t
  :config
  (setq org-alert-interval 300
        org-alert-notify-cutoff 10
        org-alert-notify-after-event-cutoff 10))

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
    (let* ((date (or date (substring (cfw:org-capture-day) 1 11))) ; extract "2024-01-25"
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
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-file-source "personal" bergheim/calendar/nextcloud/local "DarkGreen"))
     ;; :view 'block-5-day
     :view 'transpose-two-weeks))
  :custom
  (cfw:org-capture-template
   '("k" "Calendar capture" entry (file bergheim/calendar/nextcloud/local)
     "* %^{Title}\n<%(bergheim/format-scheduled-time :start (bergheim/ask-time \"Start Time: \") :end (bergheim/ask-time \"End Time: \"))>\n\n%?"))
  :config

  (defun bergheim//caldav-sync-hook ()
    (when (string= (org-capture-get :key) "k")
      (org-caldav-sync)))
  (add-hook 'org-capture-after-finalize-hook #'bergheim//caldav-sync-hook)

  ;; maybe just sync this every time the files changes instead?
  ;; (defun bergheim//caldav-sync-hook ()
  ;;   (when (equal (expand-file-name (buffer-file-name))
  ;;                (expand-file-name bergheim/calendar/nextcloud/local))
  ;;     (org-caldav-sync)))
  ;; (add-hook 'after-save-hook 'bergheim//caldav-sync-hook)
  :general
  (general-define-key
   :states '(normal insert emacs motion visual)
   :keymaps 'cfw:calendar-mode-map
   "RET" #'cfw:show-details-command
   "g" #'cfw:navi-goto-first-date-command
   "G" #'cfw:navi-goto-last-date-command
   "J" #'cfw:org-goto-date
   ;; "g" #'cfw:org-goto-date
   ;; "G" #'cfw:navi-goto-date-command
   "[" #'cfw:navi-previous-month-command
   "]" #'cfw:navi-next-month-command
   "J" #'cfw:org-goto-date
   "d" #'cfw:change-view-day
   "w" #'cfw:change-view-week
   "m" #'cfw:change-view-month)

  (general-define-key
   :states '(normal insert emacs motion visual)
   :keymaps 'cfw:details-mode-map
   "q" #'cfw:details-kill-buffer-command
   "M-n" #'cfw:details-navi-next-command
   "M-p" #'cfw:details-navi-prev-command))

(use-package calfw-org    :after calfw :demand t)
(use-package calfw-ical   :after calfw :demand t)
(use-package calfw-blocks
  :after calfw
  :demand t
  :ensure (:host github :repo "ml729/calfw-blocks")
  :config
  (setq cfw:org-overwrite-default-keybinding t
        calfw-blocks-earliest-visible-time '(2 0)
        calfw-blocks-lines-per-hour 3))

(use-package org-clock
  :ensure nil
  :after org
  :commands (org-clock-drawer-name))

(use-package org-mru-clock
  :after org
  :init
  (setq org-mru-clock-files #'org-agenda-files
        org-mru-clock-how-many 100))

(use-package org-ql
  :after org
  :init
  (defun bergheim/org-ql-search-for-tag (&optional arg)
    "Search for an Org tag in `org-agenda-files'.
With universal arg ARG, search all .org files under `org-directory`."
    (interactive "P")
    (let* ((files (if arg
                      (directory-files-recursively org-directory "\\.org$")
                    (org-agenda-files)))
           (all-tags (org-global-tags-completion-table files))
           (tag (completing-read "Tag: " all-tags)))
      (org-ql-search files
        `(tags ,tag)
        :title "Items"
        :sort 'todo
        :super-groups '((:auto-todo t)))))

  ;; TODO: this sort of feels like it probably is in org-ql already, but I couldn't find it
  (defun bergheim/org-ql-find-in-org-directory-recursively ()
    "Search Org files in `org-directory` recursively using `org-ql-find`."
    (interactive)
    (let* ((org-files (directory-files-recursively org-directory "\\.org$"))
           (filtered-files (seq-remove (lambda (file)
                                         (string-prefix-p denote-directory file))
                                       org-files)))
      (org-ql-find filtered-files)))
  :general
  (:keymaps '(org-ql-view-map org-ql-view-list-map)
   :states '(normal motion emacs)
   "RET" 'org-ql-view-switch
   "?" 'org-ql-view-dispatch
   "q" 'kill-current-buffer))

;; FIXME: remove that pesky line length sorting in vertico
(use-package org-recent-headings
  :after org
  :defer t
  :config
  (org-recent-headings-mode))

(use-package frecency
  :after org)

(use-package org-sticky-header
  :ensure t
  :after org
  :custom
  (org-sticky-header-full-path 'full)
  ;; (org-sticky-header-always-show-header 'nil)
  :hook
  (org-mode . org-sticky-header-mode))

(use-package org-contacts
  :after org
  :demand
  :commands (org-contacts-anniversaries)
  :init
  (setq org-contacts-files (list (expand-file-name "contacts.org" org-directory))))

(use-package org-journal
  :ensure t
  :defer t
  :custom
  (org-journal-dir (expand-file-name "journal" org-directory))
  (org-journal-file-format "%Y%m.org")
  (org-journal-date-prefix "* ")
  (org-journal-time-prefix "** ")
  (org-journal-date-format "%B %d, %Y - %A")
  (org-journal-file-type 'monthly)
  (org-journal-find-file #'find-file)
  (org-journal-enable-agenda-integration t)

  :general
  (general-define-key
   :states 'normal
   :keymaps 'org-journal-mode-map
   "]f"  #'org-journal-next-entry
   "[f"  #'org-journal-previous-entry
   "C-n" #'org-journal-next-entry
   "C-p" #'org-journal-previous-entry)

  (general-define-key
   :keymaps 'org-journal-search-mode-map
   "C-n" #'org-journal-search-next
   "C-p" #'org-journal-search-previous)

  ;; TODO: need a working localleader mode
  ;; (general-define-key
  ;;  :prefix ","
  ;;  :states 'normal
  ;;  :keymaps 'org-journal-mode-map
  ;;  "jc" #'org-journal-new-entry
  ;;  "jd" #'org-journal-new-date-entry
  ;;  "jn" #'org-journal-next-entry
  ;;  "jp" #'org-journal-previous-entry
  ;;  "ss" #'org-journal-search
  ;;  "sf" #'org-journal-search-forever
  ;;  "sF" #'org-journal-search-future
  ;;  "sw" #'org-journal-search-calendar-week
  ;;  "sm" #'org-journal-search-calendar-month
  ;;  "sy" #'org-journal-search-calendar-year)

  (general-define-key
   :keymaps 'org-journal-search-mode-map
   "n" #'org-journal-search-next
   "p" #'org-journal-search-prev))

(use-package org-modern
  :disabled
  :after org
  :config
  ;; (setq ;; Edit settings
  ;; org-auto-align-tags nil
  ;; org-tags-column 0
  ;; org-catch-invisible-edits 'show-and-error
  ;; org-special-ctrl-a/e t
  ;; org-insert-heading-respect-content t

  ;; ;; Org styling, hide markup etc.
  ;; org-hide-emphasis-markers t
  ;; org-pretty-entities t
  ;; org-ellipsis "…"

  ;; ;; Agenda styling
  ;; org-agenda-tags-column 0
  ;; org-agenda-block-separator ?─
  ;; org-agenda-time-grid
  ;; '((daily today require-timed)
  ;;   (800 1000 1200 1400 1600 1800 2000)
  ;;   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
  ;; org-agenda-current-time-string
  ;; "⭠ now ─────────────────────────────────────────────────")
  (global-org-modern-mode))

(use-package org-contrib
  :after org
  :demand)

(use-package org-super-links
  :ensure ( :host github :repo "toshism/org-super-links" :branch "develop")
  :after org
  :config
  (setq org-super-links-related-into-drawer t)
  (advice-add 'org-capture :before #'org-super-links-store-link))

(use-package org-noter
  :after pdf-tools
  :defer
  :config
  (setq org-noter-notes-window-location 'other-frame
        org-noter-always-create-property-drawer t
        org-noter-insert-note-no-questions t
        org-noter-separate-notes-from-heading t
        org-noter-auto-save-last-location t)
  :general
  (general-define-key
   :states 'normal
   :keymaps 'pdf-view-mode-map
   "i" 'org-noter-insert-note
   "I" 'org-noter-insert-precise-note
   "M-p" 'org-noter-sync-prev-note
   "M-n" 'org-noter-sync-next-note
   "M-." 'org-noter-sync-current-note)

  (general-define-key
   :states 'normal
   :keymaps 'org-noter-notes-mode-map
   "M-p" 'org-noter-sync-prev-note
   "M-n" 'org-noter-sync-next-note
   "M-." 'org-noter-sync-current-note
   "q" 'org-noter-kill-session))

;;; org.el ends here
