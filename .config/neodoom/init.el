;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

(message "Hello World!")

(setq visible-bell t ;; flash
      inhibit-startup-message t

      gc-cons-threshold 100000000 ; 100 mb
      read-process-output-max (* 1024 1024) ; 1mb

;; Note: height = px * 100
(set-face-attribute 'default nil :font "Ubuntu Mono" :height 120)
(load-theme 'modus-vivendi t)

;; Make same named buffers unique
(require 'uniquify)

;; Automatically insert closing parens
(electric-pair-mode t)

;; Visualize matching parens
(show-paren-mode 1)

;; Prefer spaces to tabs
(setq-default indent-tabs-mode nil)

;; Automatically save your place in files
(save-place-mode t)

;; Save history in minibuffer to keep recent commands easily accessible
(savehist-mode t)

;; Keep track of open files
(recentf-mode t)

;; Reload files that are changed outside of Emacs
(global-auto-revert-mode t)

(setq uniquify-buffer-name-style 'forward
      window-resize-pixelwise t
      frame-resize-pixelwise t
      load-prefer-newer t
      backup-by-copying t
      ;; I _think_ this should be something else (ie the cache directory)
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Bring in package utilities so we can install packages from the web.
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless package-archive-contents
  (package-refresh-contents))

;; Add the :vc keyword to use-package, making it easy to install
;; packages directly from git repositories.
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; (use-package my-package-name
;;   :ensure t    ; Ensure my-package is installed
;;   :after foo   ; Load my-package after foo is loaded (seldom used)
;;   :init        ; Run this code before my-package is loaded
;;   :bind        ; Bind these keys to these functions
;;   :custom      ; Set these variables
;;   :config      ; Run this code after my-package is loaded

(use-package ef-themes
  :ensure t
  :config
  (ef-themes-select 'ef-autumn))

;; Minibuffer completion
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode))

;; Add descriptions to completion
(use-package marginalia
  :after vertico
  :ensure t
  :init
  (marginalia-mode))

;; COmpletion in Region FUnction (code completion)
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (completion-styles '(basic)))

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
  :after evil
  :ensure t
  :config
  (evil-collection-init))

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
