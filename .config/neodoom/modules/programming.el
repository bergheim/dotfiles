;;; programming.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim
;;
;; Author: Thomas Bergheim
;; Maintainer: Thomas Bergheim
;; Created: September 18, 2023
;; Modified: September 18, 2023

(use-package ediff
  :ensure nil
  :elpaca nil
  :config
  (setq ediff-show-clashes-only t)
  ;; open diffs horizontally in the current frame
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

(use-package treesit
  :elpaca nil
  :ensure nil
  :config
  (setq treesit-font-lock-level 4))

(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist)
  (global-treesit-auto-mode))

;; (use-package tree-sitter
;;   :ensure t
;;   :config
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs
;;   :ensure t
;;   :after tree-sitter)

;; (electric-pair-mode t) ;; insert closing parens

(use-package emacs
  :elpaca nil
  :after treesit
  :custom-face
  (typescript-ts-jsx-tag-face
   ((t ( :inherit font-lock-type-face))))
  :mode
  ("\\.js$" . js-ts-mode)
  ("\\.ts$" . typescript-ts-mode)
  ("\\.tsx$" . tsx-ts-mode))

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; used by vimish-fold
(use-package hideshow
  :elpaca nil
  :hook (prog-mode . hs-minor-mode))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t))

(use-package elixir-ts-mode
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
  ;; :mode (("\\.html\\'" . web-mode))
  :custom
  (web-mode-enable-autoclosing t)
  (web-mode-code-indent-offset 4)
  (web-mode-css-indent-offset 4)
  (web-mode-markup-indent-offset 4)
  (web-mode-enable-auto-quoting nil))

(use-package typescript-ts-mode
  :elpaca nil
  :ensure nil
  :custom (typescript-ts-mode-indent-offset 4))

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

;;; programming.el ends here
