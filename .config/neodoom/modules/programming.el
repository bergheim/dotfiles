;;; programming.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim
;;
;; Author: Thomas Bergheim
;; Maintainer: Thomas Bergheim
;; Created: September 18, 2023
;; Modified: September 18, 2023

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


(defun bergheim/adjust-web-mode-comment-style ()
  "Adjust comment style based on current context in `web-mode`."
  (if (equal web-mode-content-type "jsx")
      ;; For JSX content, use the appropriate comment style
      (progn
        (setq-local comment-start "// ")
        (setq-local comment-end ""))
        ;; (setq-local comment-start "{/* ")
        ;; (setq-local comment-end " */}"))
    ;; Otherwise, for TypeScript, use the regular style
    (setq-local comment-start "// ")
    (setq-local comment-end "")))

(use-package web-mode
  :mode "\\.tsx\\'"
  :config
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  ;; (setq web-mode-code-indent-offset 2)
  (add-hook 'web-mode-hook 'bergheim/adjust-web-mode-comment-style))


;;; programming.el ends here
