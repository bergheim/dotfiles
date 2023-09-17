;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

(message "Hello World!")

(setq visible-bell t ;; flash
      inhibit-startup-message t

      bergheim/home-directory (expand-file-name "~/")

      gc-cons-threshold 100000000 ; 100 mb
      read-process-output-max (* 1024 1024) ; 1mb

      initial-major-mode 'emacs-lisp-mode  ; default mode for the *scratch* buffer
      display-time-default-load-average nil ; this information is useless for most

      sentence-end-double-space nil ;; Fix archaic defaults
)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

(require 'uniquify) ;; Make same named buffers unique

(electric-pair-mode t) ;; insert closing parens

(setq-default indent-tabs-mode nil) ;; I have given up on tabs

(save-place-mode t) ;; jump back in old files
(recentf-mode t) ;; recent files
(savehist-mode t) ;; save minibuffer history

;; Reload files that are changed outside of Emacs
(setq auto-revert-interval 1)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

(setq uniquify-buffer-name-style 'forward
      window-resize-pixelwise t
      frame-resize-pixelwise t
      load-prefer-newer t
      backup-by-copying t
      ;; I _think_ this should be something else (ie the cache directory)
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      custom-file (expand-file-name "custom.el" user-emacs-directory))


(defun bergheim/reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (message "Emacs configuration reloaded successfully!"))

;; Show the help buffer after startup
;; (add-hook 'after-init-hook 'help-quick)

;; Bring in package utilities so we can install packages from the web.
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; adds :vc keyword to use-package
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; In your .emacs or init.el or whatever your main configuration file is
(let ((private-file (expand-file-name "private.el" user-emacs-directory)))
  (when (file-exists-p private-file)
    (load private-file)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(let ((module-dir (expand-file-name "modules" user-emacs-directory)))
  (load-file (concat module-dir "/base.el"))
  (load-file (concat module-dir "/style.el"))
  (load-file (concat module-dir "/completion.el"))
  (load-file (concat module-dir "/workspace.el"))
  (load-file (concat module-dir "/formating.el"))
  (load-file (concat module-dir "/nav.el"))
  (load-file (concat module-dir "/orgmode.el"))
  (load-file (concat module-dir "/keybindings.el"))
  ;; (load-file (concat module-dir "/email.el"))
  )

;; or maybe just auto it?
;; (let ((module-dir (expand-file-name "modules" user-emacs-directory)))
;;   (dolist (module (directory-files module-dir t "\\.el$"))
;;     (load-file module)))


;; LSP support
(use-package eglot
  :ensure t
  :bind (("s-<mouse-1>" . eglot-find-implementation)
         ("C-c ." . eglot-code-action-quickfix))
  :hook ((web-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '(web-mode . ("typescript-language-server" "--stdio"))))

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
         ("C-h C" . #'helpful-command)))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org
  :ensure t
  :after org
  :hook
  (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; (use-package evil-surround
;;   :config
;;   (global-evil-surround-mode 1))

;; maybe actually try this
(use-package denote
  :ensure t
  :custom
  (denote-known-keywords '("emacs" "journal"))
  ;; This is the directory where your notes live.
  (denote-directory (expand-file-name "~/denote/"))
  :bind
  (("C-c n n" . denote)
   ("C-c n f" . denote-open-or-create)
   ("C-c n i" . denote-link)))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))

;; foo -> bar -> baz
(use-package breadcrumb
  :vc (:fetcher github :repo joaotavora/breadcrumb)
  :init (breadcrumb-mode))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)))

(use-package elixir-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . flyspell-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package web-mode
  :ensure t
  :mode (("\\.ts\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.mjs\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :custom
  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (web-mode-code-indent-offset 4)
  (web-mode-css-indent-offset 4)
  (web-mode-markup-indent-offset 4)
  (web-mode-enable-auto-quoting nil))