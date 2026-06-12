;;; init.el --- mu4e mail config orchestrator -*- lexical-binding: t; -*-

(elpaca-defscript mu4e-build-mu (:type system :dir source)
  ("meson" "setup" "build" "-Dtests=disabled")
  ("ninja" "-C" "build")
  ("sh" "-c"
   "mkdir -p \"$HOME/.local/bin\" && ln -sf \"$PWD/build/mu/mu\" \"$HOME/.local/bin/mu\""))

(defun mu4e-init-and-index (e)
  "Ensure mu DB exists and refresh the index. Idempotent on rebuild.
Relies on the symlink dropped by `mu4e-build-mu' putting mu on PATH."
  (let ((maildir (expand-file-name "~/.mail"))
        (xapian  (expand-file-name "mu/xapian"
                                   (or (getenv "XDG_CACHE_HOME")
                                       (expand-file-name "~/.cache"))))
        (addresses (list bergheim/gmail/email
                         bergheim/anthropic/email
                         bergheim/ntnu/email
                         bergheim/personal/email
                         bergheim/mailbox/email
                         bergheim/glvortex/email
                         bergheim/glvortex/email-spam
                         bergheim/glvortex/email-me)))
    (make-directory maildir t)
    (unless (file-directory-p xapian)
      (apply #'call-process "mu" nil "*mu init*" nil
             "init" "--quiet" "--maildir" maildir
             (mapcar (lambda (a) (concat "--my-address=" a)) addresses)))
    (call-process "mu" nil "*mu index*" nil "index" "--quiet"))
  (elpaca--continue-build e))

(use-package mu4e
  :ensure `(mu4e :host github
                 :repo "djcb/mu"
                 :depth nil
                 :files ("mu4e/*.el"
                         "build/mu4e/mu4e-config.el"
                         "build/mu4e/mu4e.info")
                 :main "mu4e/mu4e.el"
                 :build ((:not elpaca-build-docs)
                         (:before elpaca-build-link mu4e-build-mu)
                         (:after  elpaca-activate   mu4e-init-and-index)))
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
