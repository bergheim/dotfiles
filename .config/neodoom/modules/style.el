;;; style.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim
;;
;; Author: Thomas Bergheim
;; Maintainer: Thomas Bergheim
;; Created: September 16, 2023
;; Modified: September 16, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bergheim/dotfiles
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


;; Note: height = px * 100
(cond
 ((>= (display-pixel-height) 2160)  ; 4K screens
  (set-face-attribute 'default nil :font "Ubuntu Mono" :height 150))
 ((>= (display-pixel-height) 1440)  ; 1440p screens
  (set-face-attribute 'default nil :font "Ubuntu Mono" :height 100))
 (t
  (set-face-attribute 'default nil :font "Ubuntu Mono" :height 80)))

(show-paren-mode 1) ;; Visualize matching parens

(use-package ef-themes
  :ensure t
  :config
  (ef-themes-select 'ef-cyprus))
  ;; (ef-themes-select 'ef-elea-dark))

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-one t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; (load-theme 'modus-vivendi t)

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-support-imenu t)
  (doom-modeline-mode 1)
  :config

  ;; Disable icons for a performance boost
  ;; (setq doom-modeline-icon nil)

  ;; Disable major mode icons for additional speedup
  ;; (setq doom-modeline-major-mode-icon nil)
  ;; (setq doom-modeline-major-mode-color-icon nil)

  ;; Use a simpler version checker format
  (setq doom-modeline-checker-simple-format t)

  ;; Disable buffer encoding display
  (setq doom-modeline-buffer-encoding nil)

  ;; If you don't use LSP, or don't want to display its status
  (setq doom-modeline-lsp t)

  ;; Set a fixed width for the modeline for consistency and performance
  (setq doom-modeline-width 40)

  ;; Set the cache directory for doom-modeline
  (setq doom-modeline-cache-directory (expand-file-name "doom-modeline/" bergheim/cache-dir))

  ;; apparently `file-name' is faster than `auto'
  ;; see https://github.com/seagle0128/doom-modeline#customize
  (setq doom-modeline-buffer-file-name-style 'file-name)
  (setq doom-modeline-buffer-encoding nil))


;; foo -> bar -> baz
(use-package breadcrumb
  :vc (:fetcher github :repo joaotavora/breadcrumb)
  :defer t
  :init (breadcrumb-mode))

;; display match info in the modeline
;; has some replace stuff as well, not sure how useful
(use-package evil-anzu
  :ensure t
  :after evil-collection
  :config
  (global-anzu-mode +1))

;; Mode line information
(setq line-number-mode t)                        ; Show current line in modeline
(setq column-number-mode t)                      ; Show column as well

(setq x-underline-at-descent-line nil)           ; Prettier underlines
(setq switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setq-default show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setq-default indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Enable horizontal scrolling
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

;; We won't set these, but they're good to know about
;;
;; (setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-width 3)           ; Set a minimum width

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))


;;;; Tab bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the tab-bar as soon as tab-bar functions are invoked
(setq tab-bar-show 0)

;; Add the time to the tab-bar, if visible
;; (add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
;; (add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
;; (setq display-time-format "%a %F %T")
;; (setq display-time-interval 1)
;; (display-time-mode)

(use-package nerd-icons
  :ensure t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )


(provide 'style)
;;; style.el ends here
