;;; org.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

(use-package org
  :defer t
  ;; TODO: latest org does not work with org-habits etc so
  ;; use the internal one for now
  :elpaca nil
  :after general

  ;; LOL @this loading
  :init
  (bergheim/load-file "modules/orgmode/commands.el")
  (bergheim/load-file "modules/orgmode/keybindings.el")
  (bergheim/load-file "modules/orgmode/capture.el")
  (bergheim/load-file "modules/orgmode/agenda.el")
  (bergheim/load-file "modules/orgmode/roam.el")

  :config
  (bergheim/load-file "modules/orgmode/base.el")
  (bergheim/load-file "modules/orgmode/helpers.el")
  (bergheim/load-file "modules/orgmode/attachments.el")
  (bergheim/load-file "modules/orgmode/commands.el")
  (bergheim/load-file "modules/orgmode/style.el")

  :general
  (general-define-key
   :states '(normal insert visual)
   :keymaps 'org-mode-map
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
    "a" 'org-attach
    "e" 'org-edit-special
    "t" 'org-todo
    "l" 'org-store-link
    "b" '(bergheim/org-open-attachments :which-key "open attachments")
    "q" 'org-set-tags-command))

(use-package org-caldav
  :ensure t
  :defer t
  :config
  (setq org-caldav-url bergheim/calendar/nextcloud
        org-caldav-delete-calendar-entries 'ask
        org-caldav-save-directory (concat org-directory "caldav")
        org-caldav-calendar-id "personal"
        org-caldav-inbox (concat org-directory "caldav/personal.org")
        org-caldav-calendars `((:calendar-id "personal"
                                             :inbox ,(concat org-directory "caldav/personal.org"))
                               (:calendar-id "outlookoffice365com"
                                             :inbox ,(concat org-directory "caldav/neptune.org")))))

(use-package org-clock
  :ensure nil
  :elpaca nil
  :after org
  :commands (org-clock-drawer-name))

(use-package org-mru-clock
  :ensure t
  :after org
  :init
  (setq org-mru-clock-files #'org-agenda-files
        org-mru-clock-how-many 100))

(use-package org-ql
  :ensure t
  :after org)

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

;; TODO do I really use this
(use-package org-contacts
  :ensure t
  :after org
  :commands (org-contacts-anniversaries)
  :init
  (setq org-contacts-files '("~/org/contacts.org")))

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
  :ensure t
  :demand t
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


  ;; indent headings
  (org-indent-mode)
  (global-org-modern-mode))

;;; org.el ends here
