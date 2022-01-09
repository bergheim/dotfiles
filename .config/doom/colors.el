;;; ~/.config/doom/colors.el -*- lexical-binding: t; -*-

(defun bergheim/set-color-scheme (color)
  "Sets the color scheme based on the color input (light or dark)"
  (setq heaven-and-hell-theme-type color)
  (heaven-and-hell-clean-load-themes (heaven-and-hell-themes-switch-to)))

;; Default is 'dark
(setq heaven-and-hell-theme-type 'dark
      heaven-and-hell-load-theme-no-confirm t)

;; Set preferred light and dark themes
(setq heaven-and-hell-themes
      '((dark . doom-material)
        (light . doom-one-light)))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-monokai-spectrum)

;; Add init-hook so heaven-and-hell can load your theme
(add-hook 'after-init-hook 'heaven-and-hell-init-hook)
