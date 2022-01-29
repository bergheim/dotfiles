;;; ~/.config/doom/colors.el -*- lexical-binding: t; -*-

(defun bergheim/set-color-scheme (color)
  "Sets the color scheme based on the color input (light or dark)"
  (setq heaven-and-hell-theme-type color)
  (heaven-and-hell-clean-load-themes (heaven-and-hell-themes-switch-to)))

(after! heaven-and-hell
  ;; Set preferred light and dark themes
  (setq heaven-and-hell-themes
        '((dark . doom-monokai-classic)
          (light . doom-solarized-light)))

  ;; Default is 'dark
  (setq heaven-and-hell-theme-type 'dark
        heaven-and-hell-load-theme-no-confirm t))

(setq doom-theme 'doom-material)

;; Add init-hook so heaven-and-hell can load your theme
(add-hook 'after-init-hook 'heaven-and-hell-init-hook)
