;;; bergheim-base.el --- Core Emacs setup, history, sessions -*- lexical-binding: t; -*-

;; for some reason elpaca-diff doesn't use other window, it always comes
;; up at the bottom and we cannot allow that can we
(add-to-list 'display-buffer-alist
             '("\\*elpaca-diff\\*"
               (lambda (buffer _)
                 (when (one-window-p)
                   (split-window-right))
                 (display-buffer-use-some-window buffer nil))
               (inhibit-same-window . t)))

;; save minibuffer history
(use-package savehist
  :ensure nil
  :demand t
  :config
  (setq savehist-save-minibuffer-history t)
  (setq savehist-autosave-interval nil) ;; disable timer so multiple emacs instances don't go fucking nuts
  ;; .. a lot of it
  (dolist (hist '(command-history
                  extended-command-history
                  buffer-name-history
                  file-name-history
                  evil-jumps-history
                  global-mark-ring
                  kill-ring
                  mark-ring
                  regexp-search-ring
                  register-alist
                  search-ring
                  vertico-repeat-history))
    (add-to-list 'savehist-additional-variables hist))
  (savehist-mode 1)
  :hook
  (kill-emacs . savehist-save))

(use-package kkp
  :demand
  :config
  (global-kkp-mode +1)

  ;; hack to make shift-space etc work through tmux on csi-u
  ;; see https://github.com/benotn/kkp/issues/12
  (defun kkp-force-enable-on-tty (&rest _)
    (let ((terminal (kkp--selected-terminal)))
      (when (and (terminal-live-p terminal)
                 (not (display-graphic-p terminal))
                 (not (member terminal kkp--active-terminal-list)))
        (push terminal kkp--active-terminal-list)
        (kkp-setup-function-keys terminal)
        (set-terminal-parameter terminal 'kkp--previous-normal-erase-is-backspace-val
                                (terminal-parameter terminal 'normal-erase-is-backspace))
        (normal-erase-is-backspace-mode 1)
        (dolist (prefix kkp--key-prefixes)
          (define-key input-decode-map (kkp--csi-escape (string prefix))
            (lambda (_prompt) (kkp--process-keys prefix)))))))

  (add-hook 'tty-setup-hook #'kkp-force-enable-on-tty))

(defun bergheim/terminal-clipboard-available-p ()
  "Return non-nil when Emacs should export kills with OSC 52."
  (or (getenv "EMACS_CONTAINER")
      (not (display-graphic-p))))

(defun bergheim/send-osc52-to-terminal (text)
  "Export TEXT to the host clipboard.
On TTY/container frames, write an OSC 52 escape to the
controlling terminal."
  (if (bergheim/terminal-clipboard-available-p)
      (let ((inhibit-message t))
        (send-string-to-terminal
         (format "\e]52;c;%s\a"
                 (base64-encode-string (encode-coding-string text 'utf-8) t))))
    (when (fboundp 'gui-select-text)
      (gui-select-text text))))

(use-package emacs
  :ensure nil
  :config
  (unless (display-graphic-p)
    (xterm-mouse-mode 1))

  ;; optimize for left to right text only
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t)
  ;; ignore syntax highlights etc til done writing
  (setq redisplay-skip-fontification-on-input t)
  ;; copy current clipboard to kill ring before a kill
  (setq save-interprogram-paste-before-kill t)
  (setq kill-do-not-save-duplicates t)
  (setq-default abbrev-mode t
                indent-tabs-mode nil) ;; I have given up on tabs
  ;; (setq confirm-nonexistent-file-or-buffer nil)
  ;; make sure we get as few prompts as possible ("really follow this to source etc")
  (setq confirm-kill-emacs nil)
  (setq shell-kill-buffer-on-exit t)  ; Already in your config
  (setq comint-kill-buffer-on-exit t) ; Add this too
  (setq compilation-always-kill t) ; never ask "a compilation process is running, ...."
  (setq vc-follow-symlinks t
        large-file-warning-threshold nil
        confirm-kill-processes nil)
  (setq use-short-answers t
        ;; don't confirm to kill attached buffer processes
        kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                          kill-buffer-query-functions))
  ;; don't double escape - \< instead of \\< etc
  (setq reb-re-syntax 'string)
  ;; do not ping external files
  ;; (setq ffap-machine-p-known 'reject)
  (setq window-resize-pixelwise t
        frame-resize-pixelwise t
        save-abbrevs 'silently
        history-length 1000
        load-prefer-newer t
        backup-by-copying t
        undo-limit 500000         ; 500KB
        undo-strong-limit 1000000 ; 1MB
        ;; I _think_ this should be something else (ie the cache directory)
        backup-directory-alist `(("." . ,(concat bergheim/cache-dir "backups")))
        ;; TODO I am seeing `#FILE#' in folders - see if this removes them
        auto-save-file-name-transforms `((".*" ,(concat bergheim/cache-dir "auto-save-list/") t))
        lock-file-name-transforms `(("\\`/.*/\\([^/]+\\)\\'" ,(concat bergheim/cache-dir "lock/" "\\1") t))
        ;; updated things like dired buffers as well (tnx summer)
        global-auto-revert-non-file-buffers t)

  (setq interprogram-cut-function #'bergheim/send-osc52-to-terminal)


  ;; Reload files that are changed outside of Emacs
  (global-auto-revert-mode 1)
  ;; auto +x #! files
  (add-hook 'after-save-hook
            #'executable-make-buffer-file-executable-if-script-p))

;; save on buffer switching, focus loss, everything
(use-package super-save
  :ensure
  :demand
  :config
  (setq super-save-auto-save-when-idle t)
  (super-save-mode +1))

(use-package recentf
  :ensure nil
  :demand
  :config
  ;; Track opened directories. Thank you karthink!
  (defun recentf-track-opened-dir ()
    (and default-directory
         (recentf-add-file default-directory)))

  (add-hook 'dired-mode-hook #'recentf-track-opened-dir)

  ;; Track closed directories
  (advice-add 'recentf-track-closed-file :override
              (defun recentf-track-closed-advice ()
                (cond (buffer-file-name (recentf-remove-if-non-kept buffer-file-name))
                      ((equal major-mode 'dired-mode)
                       (recentf-remove-if-non-kept default-directory)))))

  ;; track files in all new buffers for recentf (not just from find-file)
  (add-hook 'buffer-list-update-hook #'recentf-track-opened-file)

  (setq recentf-max-menu-items 500
        recentf-auto-cleanup 'never
        recentf-max-saved-items 2000)
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))
  (recentf-mode 1))

(use-package saveplace
  :ensure nil
  :init
  (save-place-mode 1)
  :config
  (setq save-place-forget-unreadable-files t)
  (setq save-place-limit 1000))

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
  :demand
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

;; TODO replace with verb
(use-package restclient
  :ensure t
  :defer t)

(use-package vundo
  :ensure t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  :general
  (general-define-key
   :states 'normal
   "U" 'vundo))

(use-package undo-fu-session
  :demand
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))

;; act across files
(use-package wgrep
  :ensure t)

(use-package dotenv-mode)

;;; bergheim-base.el ends here
