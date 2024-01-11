;;; org.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

;;; ~/.config/doom/+org.el -*- lexical-binding: t; -*-

(use-package org
  :defer t
  :after general

  :init
  (bergheim/load-file "modules/orgmode/keybindings.el")

  :config
  (bergheim/load-file "modules/orgmode/base.el")
  (bergheim/load-file "modules/orgmode/commands.el")
  (bergheim/load-file "modules/orgmode/helpers.el")
  (bergheim/load-file "modules/orgmode/attachments.el")
  (bergheim/load-file "modules/orgmode/agenda.el")
  (bergheim/load-file "modules/orgmode/capture.el")
  (bergheim/load-file "modules/orgmode/roam.el")
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
   "C-M-S-l" #'org-shiftmetaright

   ;; FIXME: this is too broad - can't newline anymore
   ;; "RET" #'org-open-at-point
   )

  (general-define-key
   :states '(normal visual)
   :keymaps 'org-mode-map
   "C-h" 'bergheim/org-move-up-header
   "C-l" 'bergheim/org-move-down-header
   "gh" 'bergheim/org-move-up-header
   "gl" 'bergheim/org-move-down-header)

  ;; FIXME: this is straight up ignored
  (bergheim/localleader-keys
    :keymaps 'org-agenda-mode-map
    :states '(emacs normal motion)
    "t" 'org-agenda-todo
    "q" 'org-agenda-set-tags
    "e" 'org-agenda-set-effort)

  (bergheim/localleader-keys
    :keymaps 'org-mode-map
    "t" 'org-todo
    "n" 'org-store-link
    "q" 'org-set-tags-command))

;; ensure we load the latest version of org
(elpaca-wait)

;; (use-package org
;;   :after general
;;   :config
;;   (bergheim/localleader-keys
;;     :states '(normal visual emacs)
;;     :keymaps 'org-mode-map
;;     "t" 'org-todo
;;     "n" 'org-store-link
;;     "q" 'org-set-tags-command))

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
                                             :inbox ,(concat org-directory "caldav/neptune.org"))
                               )))

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
  :after org
  ;; TODO remove this once https://github.com/alphapapa/org-recent-headings/issues/22 is resolved
  :elpaca (:version (lambda (_) "0.1")))

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
  :init (setq org-contacts-files '("~/org/contacts.org")))

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

;;; org.el ends here
