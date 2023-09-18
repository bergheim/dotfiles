;;; org.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

;;; ~/.config/doom/+org.el -*- lexical-binding: t; -*-

(use-package org
  :defer t
  :after general
  ;; :init
  ;; (bergheim/load-file "modules/mu4e/keybindings.el")

  :general
  (bergheim/global-keys
   ;; FIXME: we need to reset `m' on every mode
   "mn" 'org-store-link
   "mq" 'org-set-tags-command)

  :config
  (bergheim/load-file "modules/orgmode/base.el")
  (bergheim/load-file "modules/orgmode/keybindings.el")
  (bergheim/load-file "modules/orgmode/helpers.el")
  (bergheim/load-file "modules/orgmode/attachments.el")
  (bergheim/load-file "modules/orgmode/agenda.el")
  (bergheim/load-file "modules/orgmode/capture.el")
  (bergheim/load-file "modules/orgmode/roam.el")
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

   "RET" #'org-open-at-point))

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
  :defer t
  :init
  (setq org-mru-clock-files #'org-agenda-files
        org-mru-clock-how-many 100))


;; FIXME: remove that pesky line length sorting in vertico
(use-package org-recent-headings
  :ensure t
  :defer t
  :config
  (org-recent-headings-mode))

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


;;; org.el ends here
