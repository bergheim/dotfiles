;;; keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

(defun bergheim//mu4e-headers-setup ()
  (setq-local display-line-numbers nil)
  (general-define-key
   :keymaps 'mu4e-headers-mode-map
   :states 'normal
   "R"     #'bergheim/mu4e-reply-email
   "C"     #'bergheim/mu4e-compose-email
   "C-h"   #'mu4e-search-prev
   "C-l"   #'mu4e-search-next
   "C-S-h" #'helpful-key
   "H"     #'mu4e-search-prev
   "L"     #'mu4e-search-next
   "M"     #'mu4e-headers-mark-for-move
   "m"     #'mu4e-headers-mark-for-something
   "t"     #'mu4e-search-toggle-property
   "T"     #'mu4e-headers-mark-thread))

(defun bergheim//mu4e-view-setup ()
  (setq-local display-line-numbers nil)
  (message-goto-body)
  (general-define-key
   :keymaps 'mu4e-view-mode-map
   :states 'normal
   "A"     #'mu4e-view-mime-part-action
   "R"     #'bergheim/mu4e-reply-email
   "C"     #'bergheim/mu4e-compose-email
   ;; "C-h"   #'mu4e-search-prev
   ;; "C-l"   #'mu4e-search-next
   "C-S-h" #'helpful-key
   "t"     #'mu4e-search-toggle-property
   "T"     #'mu4e-view-mark-thread))

(bergheim/global-menu-keys
  "e" '(:ignore t :which-key "E-mail")
  "e e" '(bergheim/email-today :which-key "Today's unhandled email")
  "e E" '(bergheim/email-week :which-key "This week's email")
  ;; TODO: rename this
  "e d" '(bergheim/mu4e-email-today :which-key "Dashboard")
  "e c" '(mu4e-compose-new :which-key "Compose") ; this is a category for the next set of keybindings

  "e i" '(bergheim/email-important :which-key "Important")
  "e I" '(bergheim/email-inbox :which-key "Inbox")
  "e j" '(bergheim/email-junk :which-key "Junk")
  "e o" '(bergheim/org-subtree-to-mu4e :which-key "Mail org subtree")
  "e p" '(bergheim/email-inbox-personal :which-key "Today's personal email")
  "e S" '(bergheim/email-sent :which-key "Sent")
  "e s" '(mu4e-search :which-key "Search")

  "e u" '(mu4e-update-mail-and-index :which-key "Update index and mail")
  "e U" '(mu4e-update-index :which-key "Update index")
  "e w" '(bergheim/email-week :which-key "This week's email")
  ;; "e w" '((lambda () (interactive)  :which-key "This week's email")
  "e W" '(bergheim/email-inbox-work :which-key "Work email inbox")
  "e t" '(bergheim/email-trash :which-key "Trash")
  "e q" '(mu4e-quit :which-key "Quit"))

;;; keybindings.el ends here
