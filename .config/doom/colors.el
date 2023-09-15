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

;; make it easy to understand where the current line is
(setq modus-themes-completions 'opinionated
      modus-themes-completions
      '((matches . (extrabold))
        (selection . (semibold italic text-also)))
      modus-themes-intense-hl-line t
      modus-themes-hl-line '(underline accented intense)
      x-underline-at-descent-line t)
