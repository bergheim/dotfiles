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

(use-package paredit
  :after general
  :hook (emacs-lisp-mode . (lambda ()
                             (setq-local evil-move-beyond-eol t)
                             (paredit-mode))))

;; this is pretty active
(use-package enhanced-evil-paredit
  :after paredit
  :config
  (general-define-key
   :states 'normal
   :keymaps 'paredit-mode-map
   "H" 'paredit-backward
   "J" 'paredit-forward-down
   ;; "M-J" 'paredit-forward-up
   "K" 'paredit-backward-up
   ;; "K" 'paredit-backward-up
   "L" 'paredit-forward
   "C-M-l" 'paredit-forward-slurp-sexp
   "C-M-h" 'paredit-forward-barf-sexp
   "C-M-j" 'paredit-backward-barf-sexp
   "C-M-k" 'paredit-backward-slurp-sexp)
  :hook (paredit-mode . #'enhanced-evil-paredit))

;; emacs lisp debuggers
(use-package emacs
  :ensure nil
  :general
  (:keymaps 'emacs-lisp-mode-map
   "<C-return>" 'eval-defun)

  (bergheim/localleader-keys
    :states 'normal
    :keymaps 'emacs-lisp-mode-map

    "d" '(:ignore t :which-key "debug")
    "de" '(edebug-defun             :which-key "edebug defun")
    "da" '(edebug-all-defs          :which-key "edebug all defs (buffer)")
    "di" '(edebug-on-entry          :which-key "edebug on entry")
    "dI" '(cancel-edebug-on-entry   :which-key "Cancel edebug on entry")

    "dt" '(toggle-debug-on-error    :which-key "toggle debug-on-error")
    "dq" '(toggle-debug-on-quit     :which-key "toggle debug-on-quit")

    "dd" '(debug-on-entry           :which-key "native debug on entry")
    "dD" '(cancel-debug-on-entry    :which-key "Cancel native debug on entry")
    "dw" '(debug-watch              :which-key "native debug watch variable")
    "dW" '(cancel-debug-watch       :which-key "Cancel native debug watch")

    "e" '(:ignore t :which-key "eval")
    "eb" '(eval-buffer              :which-key "buffer")
    "ed" '(eval-defun               :which-key "defun")
    "ee" '(eval-last-sexp           :which-key "last sexp")
    "es" '(eval-last-sexp           :which-key "last sexp")
    "er" '(eval-region              :which-key "region")
    "el" '(eval-print-last-sexp     :which-key "print last sexp")
    "ep" '(pp-eval-last-sexp        :which-key "pprint last sexp")
    "eP" '(pp-eval-defun            :which-key "pprint defun")

    "c" '(check-parens              :which-key "Check parens")
    "p" '(paredit-mode              :which-key "Toggle paredit")

    "m" '(macroexpand-last-sexp     :which-key "Macroexpand last sexp")
    "M" '(macroexpand-all           :which-key "Macroexpand all")))

(use-package edebug
  :ensure nil
  :hook (edebug-mode . bergheim/setup-edebug-keys)
  :config
  (defun bergheim/setup-edebug-keys ()
    "Setup edebug keybindings that work better with evil."
    (general-define-key
     :keymaps 'edebug-mode-map
     ;; TODO make sure these are the same
     ;; "n" 'edebug-next-mode
     ;; "s" 'edebug-step-mode
     ;; "g" 'edebug-go-mode
     ;; "q" 'top-level
     ;; "c" 'edebug-continue-mode
     "v" 'evil-visual-char
     "w" 'edebug-where
     "V" 'edebug-view-outside
     "E" 'edebug-eval-last-sexp)))

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

(use-package go-mode
  :ensure nil
  :mode "\\.go\\'"
  :hook ((go-ts-mode . eglot-ensure)))

(use-package sxhkdrc-mode)

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

(use-package poetry
  :disabled)

;; ;; Enable repeat mode for more ergonomic `dape' use
;; (use-package repeat
;;   :config
;;   (repeat-mode))

(when (< emacs-major-version 30)
  ;; needed for dape
  (use-package jsonrpc))

;;; programming.el ends here
