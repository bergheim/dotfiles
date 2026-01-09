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

(defvar bergheim/theme-light 'modus-operandi)
(defvar bergheim/theme-dark 'modus-vivendi)

(defvar bergheim/screen-margin 0 "Margin to subtract from screen height.")
(defvar bergheim/display 'medium)
(defvar bergheim/font-base 1.0)

(defun bergheim/set-font-size-based-on-frame-resolution ()
  "Set font size based on the resolution of the frame's display."
  (let ((height (- (display-pixel-height) bergheim/screen-margin)))
    (cond
     ((< height 1440)
      (setq bergheim/display 'small
            bergheim/font-base 0.8))
     ((< height 2160)
      (setq bergheim/display 'medium
            bergheim/font-base 1.3))
     (t
      (setq bergheim/display 'big
            bergheim/font-base 1.5)))))

(use-package fontaine
  :if (or (display-graphic-p) (daemonp))
  :demand
  :general
  (bergheim/global-menu-keys
    "hg" '(fontaine-set-preset :which-key "glyphs")
    "hG" '(fontaine-toggle-preset :which-key "toggle glyphs"))
  :hook
  ;; Persist the latest font preset when closing/starting Emacs.
  ((after-init . fontaine-mode)
   (after-init . (lambda ()
                   ;; Set last preset or fall back to desired style from `fontaine-presets'.
                   (fontaine-set-preset (or (fontaine-restore-latest-preset) 'medium)))))
  :config
  ;; TODO this should be part of fontaine
  (defun bergheim/check-available-fonts (presets)
    "Check font availability in PRESETS and warn about missing fonts."
    (let ((font-fields '(:default-family :fixed-pitch-family :variable-pitch-family))
          (fonts '()))

      ;; Collect all font names
      (dolist (preset presets)
        (let ((plist (cdr preset)))
          (dolist (field font-fields)
            (when-let ((font (and (plist-member plist field)
                                  (stringp (plist-get plist field))
                                  (plist-get plist field))))
              (push font fonts)))))

      ;; Remove duplicates and check availability
      (dolist (font (delete-dups fonts))
        (unless (find-font (font-spec :name font))
          ;; (message "Warning: Font '%s' not found on your system" font)))))
          (display-warning 'fonts (format "Font '%s' not found on your system" font))))))

  ;; This is defined in Emacs C code (it belongs to font settings)
  (setq x-underline-at-descent-line nil)

  
  (defun bergheim/generate-fontaine-presets ()
    (setq fontaine-presets
          `((small
             :default-height ,(round (* 100 bergheim/font-base))
             ;; :mode-line-active-height 0.75
             ;; :mode-line-inactive-height 0.75
             :line-spacing 0.2)
            (medium
             :default-height ,(round (* 100 bergheim/font-base))
             ;; :mode-line-active-height 0.75
             ;; :mode-line-active-family nil ; falls back to :default-family
             ;; :mode-line-active-weight nil ; falls back to :default-weight
             ;; :mode-line-inactive-height 0.75
             ;; :mode-line-inactive-family nil ; falls back to :default-family
             ;; :mode-line-inactive-weight nil ; falls back to :default-weight
             :line-spacing 0.4)
            (large
             :default-height ,(round (* 100 bergheim/font-base))
             ;; :mode-line-active-height 0.75
             ;; :mode-line-inactive-height 0.75
             :line-spacing 0.4)

            ;; special use cases
            (condense
             :default-height ,(round (* 100 bergheim/font-base))
             :line-spacing 0)
            (presentation
             :default-height ,(round (* 1.5 (* 100 bergheim/font-base))))
            (jumbo
             :default-height ,(round (* 2.0 (* 100 bergheim/font-base))))

            ;; programming
            (prog/base
             ;; apparently Fontaine does not support multiple generations :p
             ;; :inherit large
             :default-height ,(round (* 100 bergheim/font-base))
             :default-height 200
             :line-spacing 0.2)

            (prog/commit
             :inherit prog/base
             :default-family "Commit Mono Nerd Font"
             :fixed-pitch-family "Commit Mono Nerd Font"
             :variable-pitch-family "Inter")
            (prog/hack
             :inherit prog/base
             :default-family "Hack Nerd Font"
             :fixed-pitch-family "Hack Nerd Font"
             :variable-pitch-family "Hack Nerd Font Propo")
            (prog/ubuntu
             :inherit prog/base
             :default-family "Ubuntu Mono Nerd Font"
             :fixed-pitch-family "Ubuntu Mono Nerd Font"
             :variable-pitch-family "Ubuntu Nerd Font")
            (prog/jetbrains
             :inherit prog/base
             :default-family "JetBrains Mono Nerd Font" ;; NL for non ligatures
             :fixed-pitch-family "JetBrains Mono Nerd Font"
             :variable-pitch-family "JetBrains Mono Nerd Font Propo")
            (prog/noto
             :inherit prog/base
             :default-family "NotoSansM Nerd Font Mono"
             :fixed-pitch-family "NotoSansM Nerd Font Mono"
             :variable-pitch-family "NotoSans Nerd Font Propo")
            (prog/fira-code
             :inherit prog/base
             :default-family "Fira Code Nerd Font"
             :fixed-pitch-family "Fira Code Nerd Font"
             :variable-pitch-family "Fira Sans")
            (prog/caskaydia
             :inherit prog/base
             :default-family "CaskaydiaCove Nerd Font"
             :fixed-pitch-family "CaskaydiaCove Nerd Font"
             :variable-pitch-family "CaskaydiaCove Nerd Font Propo"
             :line-spacing 0.3)
            ;; (prog/cascadia
            ;;  :inherit prog/base
            ;;  :default-family "Cascadia Code"
            ;;  :fixed-pitch-family "Cascadia Code"
            ;;  :variable-pitch-family "Segoe UI Variable")
            (prog/ibm-plex
             :inherit prog/base
             :default-family "IBM Plex Mono"
             :fixed-pitch-family "IBM Plex Mono"
             :variable-pitch-family "IBM Plex Sans"
             :line-spacing 0.25)
            (prog/liberation
             :inherit prog/base
             :default-family "Liberation Mono"
             :fixed-pitch-family "Liberation Mono"
             :variable-pitch-family "Liberation Sans")
            (prog/iosevka
             :inherit prog/base
             ;; :default-height 160
             :default-family "Iosevka Nerd Font"
             :fixed-pitch-family "Iosevka Nerd Font"
             :variable-pitch-family "Iosevka Nerd Font Propo"
             :line-spacing 0.3)
            (prog/iosevka-term
             :inherit prog/base
             ;; :default-height 160
             :default-family "IosevkaTerm Nerd Font"
             :fixed-pitch-family "IosevkaTerm Nerd Font"
             :variable-pitch-family "IosevkaTerm Nerd Font Propo"
             :line-spacing 0.25)
            (prog/iosevka-slab
             :inherit prog/base
             ;; :default-height 160
             :default-family "IosevkaTermSlab Nerd Font"
             :fixed-pitch-family "IosevkaTermSlab Nerd Font"
             :variable-pitch-family "IosevkaTermSlab Nerd Font Propo"
             :line-spacing 0.3)
            (prog/aporetic
             :inherit prog/base
             ;; :default-height 160
             :default-family "Aporetic Sans Mono"
             :fixed-pitch-family "Aporetic Sans Mono"
             :variable-pitch-family "Aporetic Sans"
             :line-spacing 0.3)
            (prog/aporetic-serif
             ;; :inherit prog/aporetic
             :inherit prog/base
             :default-family "Aporetic Sans Mono"
             :fixed-pitch-family "Aporetic Sans Mono"
             :variable-pitch-family "Aporetic Sans"
             :variable-pitch-family "Aporetic Serif")
            (prog/inconsolata
             :inherit prog/base
             :default-family "Inconsolata Nerd Font"
             :fixed-pitch-family "Inconsolata Nerd Font"
             :variable-pitch-family "Inconsolata Nerd Font Propo")
            (prog/intel-one
             :inherit prog/base
             :default-family "IntoneMono Nerd Font" ;; Intel One
             :fixed-pitch-family "IntoneMono Nerd Font"
             :variable-pitch-family "IntoneMono Nerd Font Propo")
            (prog/victor
             :inherit prog/base
             :default-family "VictorMono Nerd Font"
             :fixed-pitch-family "VictorMono Nerd Font"
             :variable-pitch-family "VictorMono Nerd Font Propo")
            (prog/recursive
             :inherit prog/base
             :default-family "RecMonoLinear Nerd Font" ;; Recursive Mono
             :fixed-pitch-family "RecMonoLinear Nerd Font"
             :variable-pitch-family "Inter")
            (prog/monaspace-neon
             :inherit prog/base
             :default-family "MonaspiceNe Nerd Font"
             :fixed-pitch-family "MonaspiceNe Nerd Font"
             ;; :variable-pitch-family "Atkinson Hyperlegible")
             :variable-pitch-family "MonaspiceNe Nerd Font Propo")
            (prog/monaspace-argon
             :inherit prog/base
             :default-family "MonaspiceAr Nerd Font"
             :fixed-pitch-family "MonaspiceAr Nerd Font"
             :variable-pitch-family "MonaspiceAr Nerd Font Propo")
            (prog/monaspace-radon
             :inherit prog/base
             :default-family "MonaspiceRn Nerd Font"
             :fixed-pitch-family "MonaspiceRn Nerd Font"
             :variable-pitch-family "MonaspiceRn Nerd Font Propo")
            (prog/monaspace-xenon
             :inherit prog/base
             :default-family "MonaspiceXe Nerd Font"
             :fixed-pitch-family "MonaspiceXe Nerd Font"
             :variable-pitch-family "MonaspiceXe Nerd Font Propo")
            (prog/monaspace-krypton
             :inherit prog/base
             :default-family "MonaspiceKr Nerd Font"
             :fixed-pitch-family "MonaspiceKr Nerd Font"
             :variable-pitch-family "MonaspiceKr Nerd Font Propo")
            (prog/source
             :inherit prog/base
             :default-family "Source Code Pro"
             :fixed-pitch-family "Source Code Pro"
             :variable-pitch-family "Source Sans Pro")

            ;; org
            (org/base
             ;; :inherit large
             :default-height ,(round (* 100 bergheim/font-base))
             ;; :default-height 150
             :line-spacing 0.4)
            (org/atkinson
             :inherit org/base
             :default-family "MonaspiceNe Nerd Font"
             :fixed-pitch-family "MonaspiceNe Nerd Font"
             :variable-pitch-family "Atkinson Hyperlegible")
            (org/inter
             :inherit org/base
             :default-family "Iosevka Nerd Font"
             :fixed-pitch-family "Iosevka Nerd Font"
             :variable-pitch-family "Inter")
            (org/modern
             :inherit org/base
             :default-family "MonaspiceNe Nerd Font"
             :fixed-pitch-family "MonaspiceNe Nerd Font"
             ;; :variable-pitch-family "Inter"
             :variable-pitch-family "Segoe UI Variable Static Text")

            (t
             :default-family "MonaspiceNe Nerd Font"
             :fixed-pitch-family "MonaspiceNe Nerd Font"
             :variable-pitch-family "Inter"
             :default-height 150
             :line-spacing 0.2
             :tab-bar-height .9))))


  ;; we need a GUI frame to check the fonts - a new daemon will not have one
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook
                (lambda ()
                  (bergheim/check-available-fonts fontaine-presets)

                  (bergheim/set-font-size-based-on-frame-resolution)
                  (bergheim/generate-fontaine-presets)

                  ))
    (bergheim/check-available-fonts fontaine-presets)
    (bergheim/set-font-size-based-on-frame-resolution)
    (bergheim/generate-fontaine-presets)))

(defun bergheim//system-dark-mode-enabled-p ()
  "Check if system dark mode is enabled."
  (string= (string-trim (shell-command-to-string "gsettings get org.gnome.desktop.interface color-scheme")) "'prefer-dark'"))

(defun bergheim/frame-setup (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (scroll-bar-mode -1)
    (if (bergheim//system-dark-mode-enabled-p)
        (load-theme bergheim/theme-dark t)
      (load-theme bergheim/theme-light t))))

(with-eval-after-load 'elpaca
  (add-hook 'elpaca-after-init-hook #'bergheim/frame-setup))

(defun bergheim/zoom-window ()
  "Maximize the window or restore the previous layout."
  (interactive)
  (if (one-window-p)
      (winner-undo)
    (delete-other-windows)))

(use-package ef-themes
  :config
  (setq ef-themes-to-toggle '(ef-cyprus ef-deuteranopia-dark))
  ;; crazy Summer settings
  ;; (setq ef-themes-variable-pitch-ui t
  ;;       ef-themes-mixed-fonts t
  ;;       ef-themes-headings ; read the manual's entry of the doc string
  ;;       '((0 . (variable-pitch Book 1.5))
  ;;         (1 . (variable-pitch Book 1.25))
  ;;         (2 . (variable-pitch Book 1.1))
  ;;         (3 . (variable-pitch Book 1.0))
  ;;         (4 . (variable-pitch Light 1.0))
  ;;         (5 . (variable-pitch Light 1.0)) ; absence of weight means `bold'
  ;;         (6 . (variable-pitch Light 1.0))
  ;;         (7 . (variable-pitch Light 1.0))
  ;;         (agenda-date . (semilight 1.1))
  ;;         (agenda-structure . (variable-pitch Book 1.2))
  ;;         (t . (variable-pitch 1.0))))
  )

(use-package doric-themes)

(defvar bergheim/dark-mode-p nil
  "Is dark mode enabled?")

(defun bergheim/theme-dark ()
  (interactive)
  (consult-theme bergheim/theme-dark)
  (setq bergheim/dark-mode-p t))

(defun bergheim/theme-light ()
  (interactive)
  (consult-theme bergheim/theme-light)
  (setq bergheim/dark-mode-p nil))

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
  (setq doom-modeline-width 20)
  (setq doom-modeline-height 15)

  ;; Set the cache directory for doom-modeline
  (setq doom-modeline-cache-directory (expand-file-name "doom-modeline/" bergheim/cache-dir))

  ;; apparently `file-name' is faster than `auto'
  ;; see https://github.com/seagle0128/doom-modeline#customize
  (setq doom-modeline-buffer-file-name-style 'file-name)
  (setq doom-modeline-buffer-encoding nil)

  (setq doom-modeline-irc nil)
  (setq doom-modeline-mu4e nil)
  (setq doom-modeline-gnus nil)
  (setq doom-modeline-notifications nil))

;; (use-package mood-line
;;   :demand
;;   :config
;;   (mood-line-mode)
;;   :custom
;;   (mood-line-glyph-alist mood-line-glyphs-fira-code))

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
  :ensure
  :config
  (setq spacious-padding-widths
        '(:internal-border-width 15
          :header-line-width 4
          :mode-line-width 6
          :tab-width 4
          :right-divider-width 30
          :scroll-bar-width 8
          :fringe-width 8)
        spacious-padding-subtle-mode-line nil)
  (spacious-padding-mode))

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
  (display-line-numbers nil)
  (display-line-numbers-widen t)
  (display-line-numbers-width 3)
  (display-line-numbers-current-absolute t)
  (visible-bell nil)                  ; do _not_ flash on esc or anything
  ;; Enable horizontal scrolling
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-flip-direction t)

  ;; strongly prefer to split side by side
  ;; these were confusing so:
  (split-width-threshold 120) ;; Split top/bottom when height > X lines
  (split-height-threshold 80) ;; Split side by side when width > X chars

  :config
  ;; show rsync status in the modeline
  (defun bergheim/dired-rsync-modeline ()
    "Show dired-rsync status in modeline."
    (when (and (boundp 'dired-rsync-job-count)
               (> dired-rsync-job-count 0))
      (format " [rsync:%d]" dired-rsync-job-count)))
  (setq-default mode-line-format
                (append mode-line-format
                        '((:eval (bergheim/dired-rsync-modeline)))))

  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t 'emoji
                      (font-spec :family "Noto Color Emoji") nil 'prepend)
    ;; additional unicode ranges (needed for trev)
    (set-fontset-font t '(#x1F000 . #x1FAFF)
                      (font-spec :family "Noto Color Emoji") nil 'prepend))

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

  ;; (tab-bar-mode 1)
  (tab-bar-history-mode 1)
  (blink-cursor-mode -1)           ; Steady cursor
  (pixel-scroll-precision-mode)    ; Smooth scrolling
  (show-paren-mode 1)              ;; Visualize matching parens
  (pixel-scroll-precision-mode 1)) ;; Enable smooth pixel scrolling

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)
;; Indent wrapped lines so they line up
(when (>= emacs-major-version 30)
  (add-hook 'text-mode-hook 'visual-wrap-prefix-mode))

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

(defun bergheim/display-line-numbers? ()
  "Check if the current mode should disable line numbers."
  (derived-mode-p 'prog-mode))

(defun noct-relative ()
  "Show relative line numbers, unless the mode is in the exception list."
  (if (bergheim/display-line-numbers?)
      (setq-local display-line-numbers 'visual)))

(defun noct-absolute ()
  "Show absolute line numbers, unless the mode is in the exception list."
  (if (bergheim/display-line-numbers?)
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

;; make vertico listing perrdy
(use-package nerd-icons-completion
  :after (:all nerd-icons marginalia)
  :demand
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package rainbow-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . (lambda () (rainbow-delimiters-mode 1))))

(use-package focus)

(use-package writeroom-mode
  :commands (bergheim/write-mode)
  :config
  (setq writeroom-width 80)
  (setq writeroom-fullscreen-effect 'maximized)
  (setq writeroom-major-modes '(text-mode markdown-mode org-mode))
  (setq writeroom-global-effects '(writeroom-set-fullscreen))
  (setq writeroom-bottom-divider-width 1)

  (defun bergheim/write-mode (&optional disable)
    "Toggle zoom in on the current buffer."
    (interactive)
    (if (or disable writeroom-mode)
        (progn
          (writeroom-mode -1)
          (focus-mode -1)
          (display-line-numbers-mode 1))
      (writeroom-mode 1)
      (focus-mode 1)
      (display-line-numbers-mode -1))))


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
  :ensure (:depth nil)
  :demand
  :config
  ;; (setq hl-todo-keyword-faces
  ;;       '(("TODO"   . "#FF0000")
  ;;         ("FIXME"  . "#FF0000")
  ;;         ("DEBUG"  . "#A020F0")
  ;;         ("GOTCHA" . "#FF4500")
  ;;         ("STUB"   . "#1E90FF")))
  (global-hl-todo-mode))

(use-package evil-goggles
  :after evil
  :demand t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package dashboard
  :after nerd-icons
  :demand
  :init
  (setq dashboard-items '(
                          ;; this will FUBAR your session if something is TRAMPed
                          ;; see https://github.com/emacs-dashboard/emacs-dashboard/issues/408
                          (recents  . 5)
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
  (when (file-directory-p "~/Pictures/emacs-dashboard")
    (setq dashboard-banner-dir "~/Pictures/emacs-dashboard")
    (set-random-startup-banner))

  ;; Set a random picture as the startup banner initially
  :config
  ;; Advise dashboard-refresh-buffer to set a random picture each time it's called
  (advice-add 'dashboard-refresh-buffer :after 'set-random-startup-banner)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook))


(defun bergheim/dired-set-as-wallpaper (darkmode)
  "Sets FILE to the current wallpaper"
  (interactive "P")

  (if-let* ((file (dired-file-name-at-point))
            (swaysock (car (file-expand-wildcards "/run/user/*/sway-ipc.*.sock")))
            (dest (expand-file-name
                   (if (bergheim//system-dark-mode-enabled-p) "~/Pictures/wallpapers/active/dark/primary.jpg"
                     "~/Pictures/wallpapers/active/light/primary.jpg"))))
      (progn
        (copy-file file dest t)
        (when swaysock
          (let ((process-environment (cons (format "SWAYSOCK=%s" swaysock)
                                           process-environment)))
            (call-process "swaymsg" nil nil nil
                          (format "exec swaybg -m fill -i %s" dest))
            (message "Wallpaper set to %s" dest)
            )))
    (warn "Unable to find a file")))

(provide 'style)
;;; style.el ends here
