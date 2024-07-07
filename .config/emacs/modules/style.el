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
(defvar bergheim/theme-dark 'ef-night)
;; (defvar bergheim/theme-dark 'ef-deuteranopia-dark)

;; so many to choose from.. "Ubuntu" "DejaVu Sans" "Open Sans" "Noto Sans" "IosevkaTerm Nerd Font Propo" "Iosevka Nerd Font"
;; Note: height = px * 100
(defvar bergheim/font-name "Ubuntu Mono" "Default font for fixed-width.")
(defvar bergheim/variable-font-name "IosevkaTerm Nerd Font" "Default font for variable width.")
(defvar bergheim/fixed-font-name "Ubuntu Mono" "Alternate font for fixed-width.")
(defvar bergheim/font-size-small 100 "Font size for small displays.")
(defvar bergheim/font-size-medium 130 "Font size for medium displays.")
(defvar bergheim/font-size-large 170 "Font size for large displays.")
(defvar bergheim/line-spacing-small 0.2 "Line spacing for small displays.")
(defvar bergheim/line-spacing-medium 0.4 "Line spacing for medium displays.")
(defvar bergheim/line-spacing-large 0.6 "Line spacing for large displays.")
(defvar bergheim/screen-margin 0 "Margin to subtract from screen height.")

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

  ;; the workspace name is in the tab
  (setq doom-modeline-workspace-name nil)
  ;; this is useless
  (setq doom-modeline-modal nil)

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
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward))

;; foo -> bar -> baz
;; FIXME: make work for elpaca
;; (use-package breadcrumb
;;   :vc (:fetcher github :repo joaotavora/breadcrumb)
;;   :defer t
;;   :init (breadcrumb-mode))

(use-package spacious-padding
  :ensure (:host github :repo "protesilaos/spacious-padding")
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
  :ensure nil
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

  ;; always split vertically
  (split-width-threshold 160)
  (split-height-threshold 80)

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

  (setq xterm-mouse-mode t) ;; allow mouse events in terminal

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

(defun noct-relative ()
  "Show relative line numbers."
  (unless (derived-mode-p 'mu4e-headers-mode 'mu4e-raw-view-mode)
    (setq-local display-line-numbers 'visual)))

(defun noct-absolute ()
  "Show absolute line numbers."
  (unless (derived-mode-p 'mu4e-headers-mode 'mu4e-raw-view-mode)
    (setq-local display-line-numbers t)))


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
  :demand
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package rainbow-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . (lambda () (rainbow-delimiters-mode 1))))

(use-package focus)

(use-package writeroom-mode
  :init
  (defun bergheim/write-mode ()
    "Toggle zoom in on the current buffer."
    (interactive)
    (if (function-get 'bergheim/write-mode 'toggled)
        (progn
          (writeroom-mode -1)
          (focus-mode 0)
          ;; FIXME: is this a bug with activities.el? seems to bug
          ;; (tab-bar-mode 1)
          ;; (toggle-tab-bar-mode-from-frame)
          ;; (toggle-frame-fullscreen)
          (function-put 'bergheim/write-mode 'toggled nil))
      (writeroom-mode 1)
      (focus-mode 1)
      (display-line-numbers-mode -1)
      ;; (tab-bar-mode -1)
      ;; (toggle-frame-fullscreen)
      (function-put 'bergheim/write-mode 'toggled t)))
  :config
  (setq writeroom-width 80)
  (setq writeroom-fullscreen-effect 'maximized)
  (setq writeroom-major-modes '(text-mode markdown-mode org-mode))
  (setq writeroom-global-effects '(writeroom-set-fullscreen))
  (setq writeroom-bottom-divider-width 1))


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

(use-package olivetti)

;; golden ratio mode
(use-package zoom
  :custom
  (zoom-size '(0.618 . 0.618)))

(use-package hl-todo
  :defer t
  :ensure (:depth nil))

(use-package evil-goggles
  :after evil
  :demand t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;; dim the other windows
(use-package dimmer
  :custom
  (dimmer-adjustment-mode :foreground)
  :config
  (dimmer-mode))

(use-package dashboard
  :after nerd-icons
  :demand
  :init
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)))
  (setq dashboard-banner-logo-title "NeoDOOM")
  (setq dashboard-startup-banner 'logo)

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)

  ;; (setq dashboard-show-shortcuts nil)
  (setq dashboard-display-icons-p t) ;; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) 
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

  (setq dashboard-set-init-info t)
  (setq dashboard-projects-backend 'project-el)

  ;; I don't remember writing this
  (defun get-random-file (directory)
    (interactive)
    (let* ((allowed-extensions '(".png" ".svg" ".jpg" ".gif"))
           (filtered-files (directory-files directory t (regexp-opt allowed-extensions))))
      (if filtered-files
          (nth (random (length filtered-files)) filtered-files)
        (progn
          (message "Error: No supported files found in %s" directory)
          nil))))

  ;; Function to set a random picture as the startup banner
  (defun set-random-startup-banner ()
    (setq dashboard-startup-banner (get-random-file dashboard-banner-dir)))

  ;; Set the directory containing the pictures
  (setq dashboard-banner-dir "~/Pictures/emacs-dashboard")

  ;; Set a random picture as the startup banner initially
  (set-random-startup-banner)
  :config
  ;; Advise dashboard-refresh-buffer to set a random picture each time it's called
  (advice-add 'dashboard-refresh-buffer :after 'set-random-startup-banner)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook))

(provide 'style)
;;; style.el ends here
