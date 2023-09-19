;;; ~/.config/neodoom/modules/mu4e/init.el -*- lexical-binding: t; -*-

(use-package mu4e
  :commands mu4e
  :config
  (bergheim/load-file "modules/mu4e/settings.el")
  (bergheim/load-file "modules/mu4e/helpers.el")
  (bergheim/load-file "modules/mu4e/accounts.el")
  (bergheim/load-file "modules/mu4e/view.el")
  (bergheim/load-file "modules/mu4e/actions.el")
  (bergheim/load-file "modules/mu4e/search.el")

  :general
  ;; (bergheim/load-file "modules/mu4e/keybindings.el"))
  (bergheim/global-menu-keys
   "e" '(:ignore t :which-key "E-mail")
   "e e" '(bergheim/email-today :which-key "Today's unhandled email")
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
   "e w" '(bergheim/email-inbox-work :which-key "Work email inbox")
   "e W" '(bergheim/email-week :which-key "This week's email")
   "e t" '(bergheim/email-trash :which-key "Trash")))

(use-package mu4e-org
  :after mu4e)

;; (setq mail-user-agent 'mu4e-user-agent)
(use-package org-msg
  :ensure t
  :after mu4e
  :hook ((mu4e-compose-pre . org-msg-mode))
  :init
  (setq ;; mail-user-agent 'mu4e-user-agent
   org-msg-greeting-fmt "Hello%s,\n\n"
   org-msg-signature bergheim/signature-html
   org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
   org-msg-greeting-name-limit 3
   org-msg-default-alternatives '((new           . (text html))
                                  (reply-to-html . (text html))
                                  (reply-to-text . (text)))
   ;; TODO: what does this do
   ;; org-msg-convert-citation t)
   ))
