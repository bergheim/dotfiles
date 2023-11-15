(setq-default indent-tabs-mode nil) ;; I have given up on tabs
(savehist-mode t) ;; save minibuffer history

;; Reload files that are changed outside of Emacs
(global-auto-revert-mode 1)

(setq window-resize-pixelwise t
      frame-resize-pixelwise t
      load-prefer-newer t
      backup-by-copying t
      ;; I _think_ this should be something else (ie the cache directory)
      backup-directory-alist `(("." . ,(concat bergheim/cache-dir "backups")))
      ;; TODO I am seeing `#FILE#' in folders - see if this removes them
      auto-save-file-name-transforms `((".*" ,bergheim/cache-dir t))
      custom-file (expand-file-name "custom.el" bergheim/config-dir))

(use-package no-littering
  :ensure t
  :demand t
  :init
  (setq no-littering-var-directory (concat bergheim/cache-dir "/var"))
  (setq no-littering-etc-directory (concat bergheim/config-dir "/etc")))

(use-package recentf
  :config
  (setq recentf-max-menu-items 50)
  (setq recentf-max-saved-items 200)
  (recentf-mode t))

(use-package saveplace
  :config
  (setq save-place-forget-unreadable-files t)
  (setq save-place-limit 1000)
  :hook
  (after-init . save-place-mode))

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
  ;; (setq evil-undo-system 'undo-tree)
  (setq evil-undo-system 'undo-redo) ;; for vundo etc
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
  (setq tramp-default-method "sshx")

  (add-to-list 'tramp-methods
               '("yadm"
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 (tramp-login-env (("SHELL") ("/bin/sh")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c")))))

;; as long as this doesn't destroy my data..
;; (use-package undo-tree
;;   :ensure t
;;   :init
;;   (global-undo-tree-mode)
;;   :config
;;   (setq undo-tree-auto-save-history t
;;         undo-tree-history-directory-alist
;;         `(("." . ,(bergheim/get-and-ensure-data-dir "undo"))))
;; 
;;   ;; TODO: For some reason, general.el isn't binding 'U' as expected.
;;   ;; Using define-key as a workaround.
;;   (define-key evil-normal-state-map (kbd "U") 'undo-tree-visualize)
;; 
;;   :general
;;   (general-nmap
;;    "u" 'undo-tree-undo
;;    "U" 'undo-tree-visualize
;;    "C-r" 'undo-tree-redo))

;; if it does, switch to this
(use-package vundo
  :ensure t
  :after general
  :config
  (setq vundo-dir bergheim/cache-dir)
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (general-define-key
   :states 'normal
   "U" 'vundo))

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

(use-package flymake
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
