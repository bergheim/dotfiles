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
  :ensure `(mu4e :host github :files ("mu4e/*.el" "build/mu4e/mu4e-meta.el" "build/mu4e/mu4e-config.el" "build/mu4e/mu4e.info") :repo "djcb/mu"
                 :main "mu4e/mu4e.el"
                 :pre-build (("./autogen.sh" "-Dtests=disabled")
                             ("ninja" "-C" "build")
                             (make-symbolic-link (expand-file-name "./build/mu/mu")
                                                 (expand-file-name "~/local/bin/mu") 'ok-if-exists))
                 :build (:not elpaca--compile-info)
                 :post-build (("mu" "init" "--quiet" "--maildir" ,(concat (getenv "HOME") "/.mail")
                               "--my-address=" ,bergheim/gmail/email
                               "--my-address=" ,bergheim/ntnu/email
                               "--my-address=" ,bergheim/glvortex/email
                               "--my-address=" ,bergheim/glvortex/email-spam
                               "--my-address=" ,bergheim/glvortex/email-me)
                              ("mu" "index" "--quiet")))
  :commands (mu4e mu4e-compose-new mu4e-update-index mu4e--jump-to-bookmark mu4e--server-filter)
  :init
  (bergheim/load-file "modules/mu4e/keybindings.el")
  :config
  (bergheim/load-file "modules/mu4e/settings.el")
  (bergheim/load-file "modules/mu4e/helpers.el")
  (bergheim/load-file "modules/mu4e/accounts.el")
  (bergheim/load-file "modules/mu4e/view.el")
  (bergheim/load-file "modules/mu4e/actions.el")
  (bergheim/load-file "modules/mu4e/search.el")

  ;; `evil-collection` is so aggressive here. I couldn't find a proper way to
  ;; bind them. I give up - just add this to the end
  (add-hook 'mu4e-headers-mode-hook #'bergheim//mu4e-headers-setup)
  (add-hook 'mu4e-view-mode-hook #'bergheim//mu4e-view-setup))

(use-package mu4e-org
  :ensure nil
  :after mu4e)

(use-package org-msg
  ;; TODO temp fix while waiting on https://github.com/jeremy-compostella/org-msg/issues/182 to close
  :ensure (:host github :repo "danielfleischer/org-msg" :branch "1.12")
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

(use-package gnus-dired
  :ensure nil
  :hook
  (dired-mode . turn-on-gnus-dired-mode))
