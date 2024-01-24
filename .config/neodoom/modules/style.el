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

(defun bergheim/get-font-size ()
  (interactive)
  (message "Current font size: %s" (face-attribute 'default :height)))

(defvar bergheim/theme-light 'ef-cyprus)
(defvar bergheim/theme-dark 'ef-deuteranopia-dark)

;; so many to choose from.. "Ubuntu" "DejaVu Sans" "Open Sans" "Noto Sans" "IosevkaTerm Nerd Font Propo" "Iosevka Nerd Font"
;; Note: height = px * 100
(defvar bergheim/font-name "Ubuntu Mono" "Default font for fixed-width.")
(defvar bergheim/variable-font-name "IosevkaTerm Nerd Font" "Default font for variable width.")
(defvar bergheim/fixed-font-name "Ubuntu Mono" "Alternate font for fixed-width.")
(defvar bergheim/font-size-small 100 "Font size for small displays.")
(defvar bergheim/font-size-medium 130 "Font size for medium displays.")
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

(bergheim/set-font-based-on-frame-resolution)

(defun bergheim//system-dark-mode-enabled-p ()
  "Check if system dark mode is enabled."
  (string= (string-trim (shell-command-to-string "gsettings get org.gnome.desktop.interface color-scheme")) "'prefer-dark'"))

(defun bergheim/frame-setup (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    ;; Adjust font for new frames
    (bergheim/set-font-based-on-frame-resolution)
    (scroll-bar-mode -1)
    (with-eval-after-load 'ef-themes
      (if (bergheim//system-dark-mode-enabled-p)
          (ef-themes-select bergheim/theme-dark)
        (ef-themes-select bergheim/theme-light)))))

(use-package ef-themes
  :demand t
  :config
  (setq ef-themes-to-toggle '(ef-cyprus ef-deuteranopia-dark)))

(defun bergheim/theme-dark ()
  (interactive)
  (consult-theme bergheim/theme-dark))

(defun bergheim/theme-light ()
  (interactive)
  (consult-theme bergheim/theme-light))

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

(use-package spacious-padding
  :elpaca (:host github :repo "protesilaos/spacious-padding")
  :init (spacious-padding-mode)
  :config
  (setq spacious-padding-widths
      '( :internal-border-width 15
         :header-line-width 4
         :mode-line-width 6
         :tab-width 4
         :right-divider-width 30
         :scroll-bar-width 8)
      spacious-padding-subtle-mode-line nil))

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
  :custom
  (fill-column 79)
  (show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
  (indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin
  (indent-tabs-mode nil)
  (tab-width 4)
  (display-line-numbers 'visual)
  (display-line-numbers-widen t)
  (display-line-numbers-width 3)
  (display-line-numbers-current-absolute t)
  (visible-bell nil)                  ; do _not_ flash on esc or anything
  ;; Enable horizontal scrolling
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-flip-direction t)
  :config
  (setq scroll-step 1
        scroll-conservatively 10
        scroll-margin 3)

  ;; Optional: Adjust pixel scroll settings
  ;; Set the number of pixels to scroll each step
  (setq pixel-scroll-precision-interpolate-page 1
        pixel-scroll-precision-use-momentum t
        pixel-scroll-precision-large-scroll-height 40.0

        mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
        mouse-wheel-progressive-speed nil            ;; don't accelerate scrolling
        mouse-wheel-follow-mouse 't)                 ;; scroll window under mouse

  (setq tab-bar-show t
        tab-bar-auto-width-min '(100 10)
        tab-bar-auto-width-max '(300 30)
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-tab-hints t
        tab-bar-new-tab-choice "*scratch*")

  (setq line-number-mode t                       ; Show current line in modeline
        column-number-mode t                     ; Show column as well

        x-underline-at-descent-line nil          ; Prettier underlines
        switch-to-buffer-obey-display-actions t) ; Make switching buffers more consistent

  (tab-bar-mode 1)
  (tab-bar-history-mode 1)
  (blink-cursor-mode -1)           ; Steady cursor
  (pixel-scroll-precision-mode)    ; Smooth scrolling
  (show-paren-mode 1)              ;; Visualize matching parens
  (pixel-scroll-precision-mode 1)) ;; Enable smooth pixel scrolling

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))


;; TODO install this for smooth image scrolling
;; https://github.com/casouri/iscroll

(defun noct-relative ()
  "Show relative line numbers."
  (setq-local display-line-numbers 'visual))

(defun noct-absolute ()
  "Show absolute line numbers."
  (setq-local display-line-numbers t))


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

(use-package focus)

(use-package writeroom-mode
  :ensure t
  :config
  (setq writeroom-width 80)
  (setq writeroom-fullscreen-effect 'maximized)
  (setq writeroom-major-modes '(text-mode markdown-mode org-mode))
  (setq writeroom-global-effects '(writeroom-set-fullscreen))
  (setq writeroom-bottom-divider-width 1))

(use-package hl-todo
  :defer t
  :elpaca (:depth nil))

(use-package evil-goggles
  :ensure t
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package dashboard
  :after nerd-icons
  :demand t
  :config
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          ;; (agenda . 5)
                          (registers . 5)))
  (setq dashboard-banner-logo-title "NeoDOOOOOM")
  (setq dashboard-startup-banner 'logo)

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)

  ;; (setq dashboard-show-shortcuts nil)
  ;; (setq dashboard-display-icons-p t) ;; display icons on both GUI and terminal
  ;; (setq dashboard-icon-type 'nerd-icons)

  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

  (setq dashboard-set-init-info t)
  (setq dashboard-projects-backend 'project-el)
  (dashboard-setup-startup-hook))

(provide 'style)
;;; style.el ends here
