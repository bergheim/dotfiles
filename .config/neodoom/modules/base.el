;; -*- lexical-binding: t; -*-
(setq-default indent-tabs-mode nil) ;; I have given up on tabs

;; save minibuffer history
(use-package savehist
  :elpaca nil
  :demand t
  :config
  ;; .. a lot of it
  (dolist (hist '(command-history
                  evil-jumps-history
                  extended-command-history
                  global-mark-ring
                  kill-ring
                  mark-ring
                  regexp-search-ring
                  register-alist
                  search-ring
                  vertico-repeat-history))
    (add-to-list 'savehist-additional-variables hist))
  :init
  (savehist-mode)
  :hook
  (kill-emacs . savehist-save))

;; Reload files that are changed outside of Emacs
(global-auto-revert-mode 1)

;; save on buffer switching, focus loss, everything
(use-package super-save
  :ensure t
  :demand t
  :config
  (super-save-mode +1))

(setq window-resize-pixelwise t
      frame-resize-pixelwise t
      load-prefer-newer t
      backup-by-copying t
      ;; I _think_ this should be something else (ie the cache directory)
      backup-directory-alist `(("." . ,(concat bergheim/cache-dir "backups")))
      ;; TODO I am seeing `#FILE#' in folders - see if this removes them
      auto-save-file-name-transforms `((".*" ,bergheim/cache-dir t))
      custom-file (expand-file-name "custom.el" bergheim/config-dir))

(use-package recentf
  :elpaca nil
  :demand t
  :config
  (setq recentf-max-menu-items 50)
  (setq recentf-max-saved-items 200)
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))
  (recentf-mode t))

(use-package saveplace
  :elpaca nil
  :demand t
  :config
  (setq save-place-forget-unreadable-files t)
  (setq save-place-limit 1000)
  :hook
  ;; FIXME: migrate this to elpaca. see https://github.com/progfolio/elpaca
  (after-init . save-place-mode))

(use-package which-key
  :demand t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3)
  ;; sort A a B b not A B ab
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-idle-secondary-delay 0.3))

(defun bergheim/toggle-big-font-mode (&optional level)
  "Toggle big font mode."
  (interactive)
  (if (or (eq level 0) (eq text-scale-mode-amount 2))
      (progn
        (variable-pitch-mode -1)
        (text-scale-set 0))
    (variable-pitch-mode 1)
    (text-scale-set 2)))

(defun bergheim/present-mode ()
  "Toggle zoom in on the current buffer."
  (interactive)
  (if (function-get 'bergheim/present-mode 'toggled)
      (progn
        (writeroom-mode -1)
        (bergheim/toggle-big-font-mode 0)
        (function-put 'bergheim/present-mode 'toggled nil))
    (writeroom-mode 1)
    (bergheim/toggle-big-font-mode)
    (function-put 'bergheim/present-mode 'toggled t)))

(defun bergheim/write-mode ()
  "Toggle zoom in on the current buffer."
  (interactive)
  (if (function-get 'bergheim/write-mode 'toggled)
      (progn
        (writeroom-mode -1)
        (focus-mode 0)
        (function-put 'bergheim/write-mode 'toggled nil))
    (writeroom-mode 1)
    (focus-mode 1)
    (function-put 'bergheim/write-mode 'toggled t)))

;; emacs startup profiler
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
  (setq elfeed-db-directory (bergheim/get-and-ensure-data-dir "elfeed/db/")
        elfeed-enclosure-default-dir (bergheim/get-and-ensure-data-dir "elfeed/enclosures/"))
  :config
  ;; (setq elfeed-search-filter "@2-week-ago ")
  ;; elfeed-show-entry-switch #'pop-to-buffer
  ;; elfeed-show-entry-delete #'+rss/delete-pane
  ;; shr-max-image-proportion 0.8)
  )

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
  :elpaca nil
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
  :elpaca nil
  :defer t
  :config
  (setq tramp-persistency-file-name (expand-file-name "tramp" bergheim/cache-dir))
  ;; Use `ssh` by default instead of the default `scp`
  (setq tramp-default-method "sshx")

  (add-to-list 'tramp-methods
               '("yadm"
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 (tramp-login-env (("SHELL") ("/bin/sh")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c")))))

;; if undo-tree fails, switch to this
;; (use-package vundo
;;   :ensure t
;;   :after general
;;   :config
;;   (setq vundo-dir bergheim/cache-dir)
;;   (setq vundo-glyph-alist vundo-unicode-symbols)
;;   (general-define-key
;;    :states 'normal
;;    "U" 'vundo))

;; (use-package undo-fu
;;   :ensure t
;;   :after evil)

;; (use-package undo-fu-session
;;   :after undo-fu
;;   :config
;;   (setq undo-fu-session-directory (concat bergheim/cache-dir "/undo-fu-session")
;;         undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
;;   ;; Enable the undo-fu session globally
;;   (global-undo-fu-session-mode)
;;   :general
;;   (general-nmap
;;    "u" 'undo-fu-only-undo
;;    "C-r" 'undo-fu-only-redo))

;; act across files
(use-package wgrep
  :ensure t)

(use-package flymake
  :elpaca nil
  :defer t
  :config
  (setq flymake-temporary-file-directory (bergheim/get-and-ensure-data-dir "flymake")))

(use-package flymake-eslint
  :ensure t
  :after flymake
  :functions flymake-eslint-enable
  :preface
  ;; from https://github.com/angrybacon/dotemacs/blob/master/lisp/use-lsp.el
  (defun me/flymake-eslint-enable-maybe ()
    "Enable `flymake-eslint' based on the project configuration.
Search for the project ESLint configuration to determine whether the buffer
should be checked."
    (when-let* ((root (locate-dominating-file (buffer-file-name) "package.json"))
                (rc (locate-file ".eslintrc" (list root) '(".js" ".json"))))
      (make-local-variable 'exec-path)
      (push (file-name-concat root "node_modules" ".bin") exec-path)
      (flymake-eslint-enable))))

(use-package persistent-scratch
  :ensure t
  :demand t
  :config
  (persistent-scratch-setup-default))


(use-package writeroom-mode
  :ensure t
  :config
  (setq writeroom-width 80)
  (setq writeroom-fullscreen-effect 'maximized)
  (setq writeroom-major-modes '(text-mode markdown-mode org-mode))
  (setq writeroom-global-effects '(writeroom-set-fullscreen))
  (setq writeroom-bottom-divider-width 1))

(use-package pdf-tools)
