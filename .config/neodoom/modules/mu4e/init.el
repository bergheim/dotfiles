;;; ~/.config/neodoom/modules/mu4e/init.el -*- lexical-binding: t; -*-

(defmacro define-mu4e-search-fn (name docstring bookmark-string)
  "Generate a mu4e search function."
  `(defun ,name ()
     ,docstring
     (interactive)
     (unless (featurep 'mu4e)
       (require 'mu4e))
     (mu4e t)
     (mu4e-search-bookmark ,bookmark-string)))

(use-package mu4e
  :elpaca nil
  :commands (mu4e mu4e-compose-new)
  :init
  (bergheim/load-file "modules/mu4e/keybindings.el")
  :config
  (bergheim/load-file "modules/mu4e/settings.el")
  (bergheim/load-file "modules/mu4e/helpers.el")
  (bergheim/load-file "modules/mu4e/accounts.el")
  (bergheim/load-file "modules/mu4e/view.el")
  (bergheim/load-file "modules/mu4e/actions.el")
  (bergheim/load-file "modules/mu4e/search.el")
  :general
  (general-define-key
   :states 'normal
   :keymaps 'mu4e-headers-mode-map
   "M" #'mu4e-headers-mark-for-move
   "m" #'mu4e-headers-mark-for-something))

(use-package mu4e-org
  :elpaca nil
  :after mu4e)

(use-package org-msg
  :ensure t
  :after (org mu4e)
  :hook ((mu4e-compose-pre . org-msg-mode))
  :init
  (setq mail-user-agent 'mu4e-user-agent
        ;; Disable mu4e's default signature since we rely on org-msg here
        mu4e-compose-signature-auto-include nil
        org-msg-greeting-fmt "Hello%s,\n\n"
        org-msg-signature bergheim/signature-html
        org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
        org-msg-greeting-name-limit 3
        org-msg-default-alternatives '((new           . (text html))
                                       (reply-to-html . (text html))
                                       (reply-to-text . (text)))
        ;; turn > into org quote blocks
        org-msg-convert-citation t))
