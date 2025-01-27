;; -*- lexical-binding: t; -*-
;; save minibuffer history
(use-package savehist
  :ensure nil
  :demand t
  :config
  ;; .. a lot of it
  (dolist (hist '(command-history
                  extended-command-history
                  buffer-name-history
                  evil-jumps-history
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

(use-package emacs
  :ensure nil
  :config
  (setq-default indent-tabs-mode nil) ;; I have given up on tabs
  (setq window-resize-pixelwise t
        frame-resize-pixelwise t
        history-length 1000
        load-prefer-newer t
        backup-by-copying t
        ;; I _think_ this should be something else (ie the cache directory)
        backup-directory-alist `(("." . ,(concat bergheim/cache-dir "backups")))
        ;; TODO I am seeing `#FILE#' in folders - see if this removes them
        auto-save-file-name-transforms `((".*" ,(concat bergheim/cache-dir "auto-save-list/") t))
        lock-file-name-transforms `(("\\`/.*/\\([^/]+\\)\\'" ,(concat bergheim/cache-dir "lock/" "\\1") t))
        custom-file (expand-file-name "custom.el" bergheim/config-dir)

        ;; updated things like dired buffers as well (tnx summer)
        global-auto-revert-non-file-buffers t)
  ;; Reload files that are changed outside of Emacs
  (global-auto-revert-mode 1))

;; save on buffer switching, focus loss, everything
(use-package super-save
  :ensure
  :demand
  :config
  (super-save-mode +1))

(use-package recentf
  :ensure nil
  :demand t
  :config
  (setq recentf-max-menu-items 500)
  (setq recentf-max-saved-items 2000)
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))
  (recentf-mode t))

(use-package saveplace
  :ensure nil
  :demand t
  :config
  (setq save-place-forget-unreadable-files t)
  (setq save-place-limit 1000)
  :hook
  ;; FIXME: migrate this to elpaca. see https://github.com/progfolio/elpaca
  (after-init . save-place-mode))

(use-package el-patch)

(use-package which-key
  :demand t
  :custom
  (which-key-idle-delay 0.3)
  (which-key-idle-secondary-delay 0.3)
  ;; sort A a B b not A B a b
  (which-key-sort-order 'which-key-key-order-alpha)
  ;; this fixes which-key window size for me in daemon mode
  (which-key-allow-imprecise-window-fit nil)
  :config
  (which-key-mode))

(use-package exec-path-from-shell
  :config
  (when (or (memq window-system '(mac ns x))
            (daemonp))
    (exec-path-from-shell-initialize)))

(defun bergheim/toggle-big-font-mode (&optional level)
  "Toggle big font mode."
  (interactive)
  (if (or (eq level 0) (eq text-scale-mode-amount 2))
      (progn
        (variable-pitch-mode -1)
        (text-scale-set 0))
    (variable-pitch-mode 1)
    (text-scale-set 2)))

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
  (:states 'normal
   "K" #'helpful-at-point))

(use-package minibuffer
  :ensure nil
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
  :ensure nil
  :init
  (defun my--tramp-send-command--workaround-stty-icanon-bug (conn-vec orig-command &rest args)
    "See: https://github.com/magit/magit/issues/4720"
    (let ((command
           (if (string= "stty -icrnl -icanon min 1 time 0" orig-command)
               "stty -icrnl"
             orig-command)))
      (append (list conn-vec command) args)))
  (defun my--tramp-send-command--workaround-stty-icanon-bug--filter-args (args)
    (apply #'my--tramp-send-command--workaround-stty-icanon-bug args))
  :config
  (advice-add 'tramp-send-command :filter-args
              #'my--tramp-send-command--workaround-stty-icanon-bug--filter-args)
  (setq tramp-pipe-stty-settings "")
  (setq tramp-persistency-file-name (expand-file-name "tramp" bergheim/cache-dir))
  (setq
   ;; `ssh` should be quicker than the default `scp`
   tramp-default-method "ssh"
   ;; Disable version control to avoid delays:
   vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                vc-ignore-dir-regexp tramp-file-name-regexp)
   tramp-copy-size-limit nil
   tramp-completion-reread-directory-timeout t)
  ;; (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='~/.ssh/sockets/tramp-%%r@%%h-%%p' -o ControlPersist=600")

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
  :ensure nil
  :defer t
  :config
  (setq flymake-temporary-file-directory (bergheim/get-and-ensure-data-dir "flymake")))

(use-package persistent-scratch
  :ensure t
  :demand t
  :config
  (persistent-scratch-setup-default))

(use-package pdf-tools)

(use-package systemd
  :mode (("\\.service\\'" . systemd-mode)
         ("\\.socket\\'" . systemd-mode)
         ("\\.timer\\'" . systemd-mode)
         ("\\.target\\'" . systemd-mode)
         ("\\.mount\\'" . systemd-mode)
         ("\\.automount\\'" . systemd-mode)
         ("\\.slice\\'" . systemd-mode)
         ("\\.path\\'" . systemd-mode)))
