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
;;   :demand t
;;   :config
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs
;;   :ensure t
;;   :demand t
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
  :after (evil)
  :demand t
  :config
  ;; should use `consult-xref`?
  ;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq dumb-jump-prefer-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package evil-textobj-tree-sitter
  :after evil
  :demand t
  :config
  ;; example of custom text object
  ;; (define-key evil-outer-text-objects-map "m" (evil-textobj-tree-sitter-get-textobj "import"
  ;;                                               '((python-mode . [(import_statement) @import])
  ;;                                                 (go-mode . [(import_spec) @import])
  ;;                                                 (rust-mode . [(use_declaration) @import]))))
  (define-key evil-outer-text-objects-map "f" (cons "evil-outer-function" (evil-textobj-tree-sitter-get-textobj "function.outer")))
  (define-key evil-inner-text-objects-map "f" (cons "evil-inner-function" (evil-textobj-tree-sitter-get-textobj "function.inner")))
  (define-key evil-outer-text-objects-map "c" (cons "evil-outer-class" (evil-textobj-tree-sitter-get-textobj "class.outer")))
  (define-key evil-inner-text-objects-map "c" (cons "evil-inner-class" (evil-textobj-tree-sitter-get-textobj "class.inner")))
  (define-key evil-outer-text-objects-map "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key evil-inner-text-objects-map "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key evil-outer-text-objects-map "i" (cons "evil-outer-conditional-loop" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer"))))
  (define-key evil-inner-text-objects-map "i" (cons "evil-inner-conditional-loop" (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner"))))
  (define-key evil-inner-text-objects-map "a" (cons "evil-inner-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.inner")))
  (define-key evil-outer-text-objects-map "a" (cons "evil-outer-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.outer")))

  (define-key evil-normal-state-map (kbd "]a") (cons "goto-parameter-start" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "parameter.inner"))))
  (define-key evil-normal-state-map (kbd "[a") (cons "goto-parameter-start" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "parameter.inner" t))))
  (define-key evil-normal-state-map (kbd "]A") (cons "goto-parameter-end" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "parameter.inner" nil t))))
  (define-key evil-normal-state-map (kbd "[A") (cons "goto-parameter-end" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "parameter.inner" t t))))
  (define-key evil-normal-state-map (kbd "]f") (cons "goto-function-start" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer") (reposition-window)))))
  (define-key evil-normal-state-map (kbd "[f") (cons "goto-function-start" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer" t) (reposition-window)))))
  (define-key evil-normal-state-map (kbd "]F") (cons "goto-function-end" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t) (reposition-window)))))
  (define-key evil-normal-state-map (kbd "[F") (cons "goto-function-end" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer" t t) (reposition-window))))))

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

;;; programming.el ends here
