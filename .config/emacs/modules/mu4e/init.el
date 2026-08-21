;;; init.el --- mu4e mail config orchestrator -*- lexical-binding: t; -*-

(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :init
  (bergheim/load-file "modules/mu4e/keybindings.el")
  :general
  ;; mu4e's own action list stays the source of truth for per-message
  ;; commands, so wrap it instead of re-listing actions.el here.
  (bergheim/localleader-keys
    :states '(normal visual)
    :keymaps '(mu4e-headers-mode-map mu4e-view-mode-map)
    "f" '(bergheim/mu4e-toggle-fullscreen :which-key "fullscreen")
    "s" '(mu4e-search :which-key "search")
    "b" '(mu4e-search-bookmark :which-key "bookmark")
    "u" '(mu4e-update-mail-and-index :which-key "update mail"))
  (bergheim/localleader-keys
    :states '(normal visual)
    :keymaps 'mu4e-headers-mode-map
    "a" '(mu4e-headers-action :which-key "actions"))
  (bergheim/localleader-keys
    :states '(normal visual)
    :keymaps 'mu4e-view-mode-map
    "a" '(mu4e-view-action :which-key "actions"))
  :config
  (bergheim/load-file "modules/mu4e/settings.el")
  (bergheim/load-file "modules/mu4e/helpers.el")
  (bergheim/load-file "modules/mu4e/accounts.el")
  (bergheim/load-file "modules/mu4e/style.el")
  (bergheim/load-file "modules/mu4e/actions.el")
  (bergheim/load-file "modules/mu4e/search.el")

  (defun bergheim//mu4e-ensure-signer ()
    "Populate `mml-secure-openpgp-signers' lazily on first compose so
package load does not trigger a GPG prompt."
    (unless mml-secure-openpgp-signers
      (setq mml-secure-openpgp-signers (list (password-store-get "email/sign")))))
  (add-hook 'mu4e-compose-mode-hook #'bergheim//mu4e-ensure-signer)
  ;; (setq mm-verify-option 'always)
  ;; (setq mm-decrypt-option 'always)
  ;; (add-hook 'mu4e-compose-mode-hook 'mml-secure-message-sign)
  ;; (add-hook 'mu4e-compose-mode-hook 'mml-secure-message-encrypt)

  ;; `evil-collection` is so aggressive here. I couldn't find a proper way to
  ;; bind them. I give up - just add this to the end
  (add-hook 'mu4e-headers-mode-hook #'bergheim//mu4e-headers-setup)
  (add-hook 'mu4e-view-mode-hook #'bergheim//mu4e-view-setup)
  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'mu4e-complete-contact nil t))
            90)
  )

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
