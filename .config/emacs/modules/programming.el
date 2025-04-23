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
  :config
  (setq ediff-show-clashes-only t)
  ;; open diffs horizontally in the current frame
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

(use-package editorconfig
  :ensure nil ;; part of emacs 30
  :config
  (editorconfig-mode 1))

(use-package treesit
  :ensure nil
  :config
  (setq treesit-font-lock-level 4))

(use-package treesit-auto
  :defer 10
  :config
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist)
  (global-treesit-auto-mode))

;; (use-package tree-sitter
;;   :ensure t
;;   :demand t
;;   :config
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs
;;   :ensure t
;;   :demand t
;;   :after tree-sitter)

(use-package aggressive-indent
  :config
  (aggressive-indent-global-mode t))

(use-package emacs
  :ensure nil
  :after treesit
  :config
  (electric-pair-mode t)
  :custom
  (xref-search-program 'ripgrep)
  (grep-command "rg -nS --no-heading "
                grep-use-null-device nil)
  :custom-face
  (typescript-ts-jsx-tag-face
   ((t ( :inherit font-lock-type-face))))
  :mode
  ("\\.js$" . js-ts-mode)
  ("\\.ts$" . typescript-ts-mode)
  ("\\.tsx$" . tsx-ts-mode))

(use-package dumb-jump
  :ensure t
  :after evil
  :demand t
  :config
  ;; should use `consult-xref`?
  ;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq dumb-jump-prefer-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; used by vimish-fold
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t))

(use-package emacs-lisp-mode
  :ensure nil
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

(use-package elixir-ts-mode
  :config
  (defalias 'elixir-mode 'elixir-ts-mode))

(use-package yaml-mode)

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . flyspell-mode))
  :init
  ;; (setq markdown-command "pandoc -f markdown -t html")
  (setq markdown-command "markdown"))

(use-package web-mode
  ;; :mode (("\\.html\\'" . web-mode))
  :custom
  (web-mode-enable-autoclosing t)
  (web-mode-code-indent-offset 4)
  (web-mode-css-indent-offset 4)
  (web-mode-markup-indent-offset 4)
  (web-mode-enable-auto-quoting nil))

(use-package typescript-ts-mode
  :ensure nil
  :custom (typescript-ts-mode-indent-offset 4))

(use-package python-ts-mode
  :ensure nil
  :hook (python-mode . poetry-tracking-mode))

(use-package lua-mode)

(use-package sxhkdrc-mode)

(use-package ob-typescript
  :after org
  :demand t
  :config
  ;; Assuming ob-typescript expects typescript-mode, we might need to
  ;; temporarily alias typescript-ts-mode to typescript-mode.
  (unless (fboundp 'typescript-mode)
    (defalias 'typescript-mode 'typescript-ts-mode)))

;; see https://github.com/abicky/nodejs-repl.el for options
(use-package nodejs-repl
  :ensure t
  :commands (nodejs-repl)
  :init
  (with-eval-after-load 'repl-toggle
    (cl-pushnew '(js-ts-mode . nodejs-repl) rtog/mode-repl-alist :test #'equal)))

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

(use-package poetry)

(use-package dape)

(when (< emacs-major-version 30)
  ;; needed for dape
  (use-package jsonrpc))

;;; programming.el ends here
