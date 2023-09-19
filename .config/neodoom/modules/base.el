(use-package no-littering
  :ensure t
  :demand t
  :config
  (setq no-littering-var-directory (concat bergheim/cache-dir "/var")))
  ;;       no-littering-etc-directory (concat bergheim/config-dir "/etc")))

(use-package recentf
  :config
  (setq recentf-save-file (concat bergheim/cache-dir "/recentf"))
  (setq recentf-max-menu-items 50)
  (setq recentf-max-saved-items 200)
  (recentf-mode t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3)
  (setq which-key-idle-secondary-delay 0.3))

(use-package general
  :ensure t
  :demand t
  :config
  (general-override-mode))

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode 1))

(use-package default-text-scale
  :ensure t
  :config
  (general-define-key
   :states '(normal visual)
   "C-+" 'default-text-scale-increase
   "C-=" 'default-text-scale-reset
   "C--" 'default-text-scale-decrease))

(use-package git-auto-commit-mode
  :ensure t)

(use-package esup
  :ensure t
  :defer t
  ;; To prevent any graphical interface to pop-up.
  :custom (esup-depth 0))

;; Add extra context to Emacs documentation to help make it easier to
;; search and understand. This configuration uses the keybindings
;; recommended by the package author.
(use-package helpful
  :ensure t
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)
         ("C-c C-d" . #'helpful-at-point)
         ("C-h F" . #'helpful-function)
         ("C-h C" . #'helpful-command))
  :general
  (general-define-key
   :states 'normal
   "K" #'helpful-at-point))

(use-package elfeed
  :ensure t
  :commands elfeed
  :init
  (setq elfeed-db-directory (concat bergheim/cache-dir "elfeed/db/")
        elfeed-enclosure-default-dir (concat bergheim/cache-dir "elfeed/enclosures/"))
  :config
  ;; (setq elfeed-search-filter "@2-week-ago ")
        ;; elfeed-show-entry-switch #'pop-to-buffer
        ;; elfeed-show-entry-delete #'+rss/delete-pane
        ;; shr-max-image-proportion 0.8)
  (make-directory elfeed-db-directory t))

(use-package elfeed-org
  :ensure t
  :after elfeed
  :preface
  (setq rmh-elfeed-org-files (list "elfeed.org"))
  :config
  (elfeed-org))

(use-package elfeed-goodies
  :ensure t
  :after elfeed
  :config
  (elfeed-goodies/setup))

(use-package minibuffer
  :after general
  :config
  (general-define-key
   :keymaps 'minibuffer-local-map
   "C-w" 'backward-kill-word
   "C-u" 'backward-kill-sentence))

(use-package restclient
  :ensure t
  :defer t)

(use-package tramp
  :defer t
  :config
  (setq tramp-persistency-file-name (expand-file-name "tramp" bergheim/cache-dir))
  ;; Use `ssh` by default instead of the default `scp`
  (setq tramp-default-method "ssh")

  (add-to-list 'tramp-methods
               '("yadm"
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 (tramp-login-env (("SHELL") ("/bin/sh")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c")))))

(use-package vundo
  :ensure t
  :after general
  :config
  (setq vundo-dir bergheim/cache-dir)
  (general-define-key
   :states 'normal
   "U" 'vundo))
