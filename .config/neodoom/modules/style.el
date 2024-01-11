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
(defvar bergheim/font-name "Ubuntu Mono" "Default font for fixed-width.")
(defvar bergheim/variable-font-name "IosevkaTerm Nerd Font Propo" "Default font for variable width.")
(defvar bergheim/fixed-font-name "Ubuntu Mono" "Alternate font for fixed-width.")
(defvar bergheim/font-size-small 100 "Font size for small displays.")
(defvar bergheim/font-size-medium 150 "Font size for medium displays.")
(defvar bergheim/font-size-large 200 "Font size for large displays.")
(defvar bergheim/line-spacing-small 0.2 "Line spacing for small displays.")
(defvar bergheim/line-spacing-medium 0.4 "Line spacing for medium displays.")
(defvar bergheim/line-spacing-large 0.8 "Line spacing for large displays.")
(defvar bergheim/screen-margin 400 "Margin to subtract from screen height.")

(custom-set-faces '(line-number-current-line ((t :weight bold))))

(defun bergheim/set-font-based-on-frame-resolution ()
  "Set font size based on the resolution of the frame's display."
  (let ((height (- (display-pixel-height) bergheim/screen-margin))
        font-size global-line-spacing)
    (cond
     ((< height 1440)
      (setq font-size bergheim/font-size-small
            global-line-spacing bergheim/line-spacing-small))
     ((< height 2160)
      (setq font-size bergheim/font-size-medium
            global-line-spacing bergheim/line-spacing-medium))
     (t
      (setq font-size bergheim/font-size-large
            global-line-spacing bergheim/line-spacing-large)))

    (setq-default line-spacing global-line-spacing)
    (set-face-attribute 'default nil :font bergheim/font-name :height font-size)
    (set-face-attribute 'variable-pitch nil :family bergheim/variable-font-name :height font-size)
    (set-face-attribute 'fixed-pitch nil :font bergheim/fixed-font-name :height font-size)))

;; Adjust font for new frames
(add-hook 'after-make-frame-functions (lambda (frame)
                                        (with-selected-frame frame
                                          (bergheim/set-font-based-on-frame-resolution))))

(bergheim/set-font-based-on-frame-resolution)

(use-package ef-themes
  :demand t
  :elpaca (:host github :repo "protesilaos/ef-themes")
  :config
  (ef-themes-select 'ef-cyprus))
  ;; (ef-themes-select 'ef-elea-dark))
  ;; (ef-themes-select 'ef-maris-dark))

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

;; (use-package mood-line
;;   :ensure t
;;   :init
;;   (mood-line-mode))

;; (with-eval-after-load 'mood-line
;;   (setq mood-line-glyph-alist mood-line-glyphs-fira-code))


(use-package uniquify
  :elpaca nil
  :config
  (setq uniquify-buffer-name-style 'forward))

;; foo -> bar -> baz
;; FIXME: make work for elpaca
;; (use-package breadcrumb
;;   :vc (:fetcher github :repo joaotavora/breadcrumb)
;;   :defer t
;;   :init (breadcrumb-mode))

;; display match info in the modeline
;; has some replace stuff as well, not sure how useful
(use-package evil-anzu
  :ensure t
  :after evil-collection
  :config
  (global-anzu-mode +1))

(defun bergheim/toggle-visual-fluff ()
  "Toggle the menu bar and scroll bar on and off."
  (interactive)
  (if menu-bar-mode
      (progn (menu-bar-mode -1)
             (scroll-bar-mode -1))
    (menu-bar-mode 1)
    (scroll-bar-mode 1)))

(use-package emacs
  :elpaca nil
  :config
  (setq scroll-step 1
        scroll-conservatively 10
        scroll-margin 3)
  ;; Enable smooth pixel scrolling
  (pixel-scroll-precision-mode 1)

  ;; Optional: Adjust pixel scroll settings
  ;; Set the number of pixels to scroll each step
  (setq pixel-scroll-precision-interpolate-page 1
        pixel-scroll-precision-use-momentum t
        pixel-scroll-precision-large-scroll-height 40.0

        mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
        mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
        mouse-wheel-follow-mouse 't)) ;; scroll window under mouse


(defun noct-relative ()
  "Show relative line numbers."
  (setq-local display-line-numbers 'visual))

(defun noct-absolute ()
  "Show absolute line numbers."
  (setq-local display-line-numbers t))

;; TODO: go through this..

(setq-default fill-column 79)
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
;; Uh why won't we set these?
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling
(setq visible-bell nil)                               ; do _not_ flash on esc or anything

(setq-default display-line-numbers 'visual
              display-line-numbers-widen t
              display-line-numbers-width 3
              display-line-numbers-current-absolute t)

(show-paren-mode 1) ;; Visualize matching parens

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))


;;;; Tab bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the tab-bar as soon as tab-bar functions are invoked

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

(use-package rainbow-delimiters
  :defer t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package hl-todo
  :defer t
  :elpaca (:depth nil))

(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(provide 'style)
;;; style.el ends here
