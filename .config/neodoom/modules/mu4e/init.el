;;; ~/.config/neodoom/modules/mu4e/init.el -*- lexical-binding: t; -*-

(use-package mu4e
  :commands mu4e
  :init
  (bergheim/load-file "modules/mu4e/keybindings.el")
  :config
  (bergheim/load-file "modules/mu4e/settings.el")
  (bergheim/load-file "modules/mu4e/helpers.el")
  (bergheim/load-file "modules/mu4e/accounts.el")
  (bergheim/load-file "modules/mu4e/view.el")
  (bergheim/load-file "modules/mu4e/actions.el")
  (bergheim/load-file "modules/mu4e/search.el"))

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
        org-msg-default-alternatives '((new		. (text html))
                                       (reply-to-html	. (text html))
                                       (reply-to-text	. (text)))
        ;; TODO: what does this do
        ;; org-msg-convert-citation t)
  ))
