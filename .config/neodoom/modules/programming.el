;;; programming.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim
;;
;; Author: Thomas Bergheim
;; Maintainer: Thomas Bergheim
;; Created: September 18, 2023
;; Modified: September 18, 2023

(use-package ediff
  :elpaca nil
  :config
  (setq ediff-show-clashes-only t)
  ;; open diffs horizontally in the current frame
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package treesit
  :elpaca nil
  :config
  (setq treesit-font-lock-level 4))

(use-package treesit-auto
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

(use-package emacs-lisp-mode
  :elpaca nil
  :after general
  :general
  (bergheim/localleader-keys
    :states 'normal
    :keymaps 'emacs-lisp-mode-map
    "e" '(:ignore t :which-key "Eval")
    "e d" '(eval-defun :which-key "last defun")
    "e e" '(eval-last-sexp :which-key "last sexp")
    "e b" '(eval-buffer :which-key "buffer")
    "e r" '(elisp-eval-region-or-buffer :which-key "region or buffer")))

(use-package elixir-ts-mode)

(use-package yaml-mode)

(use-package markdown-mode
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
