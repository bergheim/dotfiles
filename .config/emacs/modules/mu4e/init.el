;;; ~/.config/neodoom/modules/mu4e/init.el -*- lexical-binding: t; -*-

(use-package mu4e
  :ensure `(mu4e :host github
                 :files ("mu4e/*.el"
                         "build/mu4e/mu4e-meta.el"
                         "build/mu4e/mu4e-config.el"
                         "build/mu4e/mu4e.info")
                 :repo "djcb/mu"
                 :main "mu4e/mu4e.el"
                 :pre-build (("./autogen.sh" "-Dtests=disabled")
                             ("ninja" "-C" "build")
                             (make-symbolic-link (expand-file-name "./build/mu/mu")
                                                 (expand-file-name "~/local/bin/mu") 'ok-if-exists))
                 :build (:not elpaca--compile-info)
                 :post-build (("mu" "init" "--quiet" "--maildir" ,(concat (getenv "HOME") "/.mail")
                               "--my-address=" ,bergheim/gmail/email
                               "--my-address=" ,bergheim/ntnu/email
                               "--my-address=" ,bergheim/personal/email
                               "--my-address=" ,bergheim/mailbox/email
                               "--my-address=" ,bergheim/glvortex/email
                               "--my-address=" ,bergheim/glvortex/email-spam
                               "--my-address=" ,bergheim/glvortex/email-me)
                              ("mu" "index" "--quiet")))
  :init
  (bergheim/load-file "modules/mu4e/keybindings.el")
  :config
  (bergheim/load-file "modules/mu4e/settings.el")
  (bergheim/load-file "modules/mu4e/helpers.el")
  (bergheim/load-file "modules/mu4e/accounts.el")
  (bergheim/load-file "modules/mu4e/style.el")
  (bergheim/load-file "modules/mu4e/actions.el")
  (bergheim/load-file "modules/mu4e/search.el")

  (setq mml-secure-openpgp-signers '((password-store-get "email/sign")))
  ;; (setq mm-verify-option 'always)
  ;; (setq mm-decrypt-option 'always)
  ;; (add-hook 'mu4e-compose-mode-hook 'mml-secure-message-sign)
  ;; (add-hook 'mu4e-compose-mode-hook 'mml-secure-message-encrypt)

  ;; `evil-collection` is so aggressive here. I couldn't find a proper way to
  ;; bind them. I give up - just add this to the end
  (add-hook 'mu4e-headers-mode-hook #'bergheim//mu4e-headers-setup)
  (add-hook 'mu4e-view-mode-hook #'bergheim//mu4e-view-setup))

(use-package mu4e-org
  :ensure nil
  :after mu4e)

(use-package org-msg
  ;; TODO temp fix while waiting on https://github.com/jeremy-compostella/org-msg/issues/182 to close
  :ensure (:host github :repo "danielfleischer/org-msg" :branch "master")
  :after (org mu4e)
  :preface
  (defun org-msg-no-temp-buffer (orig-fun &rest args)
    "Advice to set `org-export-show-temporary-export-buffer' to `nil'."
    (let ((org-export-show-temporary-export-buffer nil))
      (apply orig-fun args)))
  :init
  (setq mail-user-agent 'mu4e-user-agent
        ;; Disable mu4e's default signature since we rely on org-msg here
        message-signature nil
        org-msg-greeting-fmt "Hello%s,\n\n"
        org-msg-signature bergheim/signature-html
        org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-greeting-name-limit 3
        org-msg-default-alternatives '((new           . (text html))
                                       (reply-to-html . (text html))
                                       (reply-to-text . (text)))
        ;; turn > into org quote blocks
        org-msg-convert-citation t)
  (org-msg-mode)
  :config
  (advice-add 'org-msg-preview :around #'org-msg-no-temp-buffer)
  (advice-add 'org-msg-ctrl-c-ctrl-c :around #'org-msg-no-temp-buffer)
  (add-hook 'message-sent-hook
            (lambda ()
              (interactive)
              (kill-buffer "*Org ASCII Export*")
              (switch-to-buffer "*mu4e-article*")
              (mu4e-view-quit))))

(use-package gnus-dired
  :ensure nil
  :hook
  (dired-mode . turn-on-gnus-dired-mode))
