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

;; emacs lisp debuggers
(use-package emacs
  :ensure nil
  :general
  (:keymaps 'emacs-lisp-mode-map
   "<C-return>" 'eval-defun)

  (bergheim/localleader-keys
    :states 'normal
    :keymaps 'emacs-lisp-mode-map

    "d" '(nil :which-key "debug")
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

    "e" '(nil :which-key "eval")
    "eb" '(eval-buffer              :which-key "buffer")
    "ed" '(eval-defun               :which-key "defun")
    "ee" '(eval-last-sexp           :which-key "defun")
    "es" '(eval-last-sexp           :which-key "last sexp")
    "er" '(eval-region              :which-key "region")
    "el" '(eval-print-last-sexp     :which-key "print last sexp")
    "ep" '(pp-eval-last-sexp        :which-key "pprint last sexp")
    "eP" '(pp-eval-defun            :which-key "pprint defun")

    "c" '(check-parens              :which-key "Check parens")

    "m" '(macroexpand-last-sexp     :which-key "Macroexpand last sexp")
    "M" '(macroexpand-all           :which-key "Macroexpand all")))


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

(use-package poetry
  :disabled)

(use-package dape
  :hook
  (kill-emacs . dape-breakpoint-save)
  (after-init . dape-breakpoint-load)

  :config
  (general-define-key
   :states '(normal motion visual)
   :keymaps 'override
   :predicate 'dape-active-mode
   "gb" '(dape-breakpoint-toggle :wk "Dape Toggle Breakpoint")
   "gB" '(dape-breakpoint-remove-all :wk "Dape Remove Breakpoints")
   "gE" '(dape-breakpoint-expression :wk "Dape Breakpoint Expression")
   "gl" '(dape-breakpoint-log :wk "Dape Breakpoint Log")

    "]s" '(dape-stack-select-down nil :wf "Dape Stack Down")
    "[s" '(dape-stack-select-up nil :wf "Dape Stack Up")

    "<" '(dape-stack-select-down nil :wf "Dape Stack Down")
    ">" '(dape-stack-select-up nil :wf "Dape Stack Up")

   "gc" '(dape-continue :wk "Dape Continue")
   "ge" '(dape-evaluate-expression :wk "Dape Evaluate")
   "gI" '(dape-info :wk "Dape Toggle Info")
   "gi" '(dape-step-in :wk "Dape Step In")
   "gk" '(dape-kill :wk "Dape Kill Session") ;; keeps the buffers open unlike quit
   "gn" '(dape-next :wk "Dape Next")
   "go" '(dape-step-out :wk "Dape Step Out")
   "gq" '(dape-quit :wk "Dape Quit")
   "gR" '(dape-repl :wk "Dape REPL")
   "gr" '(dape-restart :wk "Dape Restart")
   "gs" '(dape-select-stack :wk "Dape Select Stack")
   "gw" '(dape-watch-dwim :wk "Dape Watch DWIM"))

  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)

  ;; Info buffers like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)
  ;; (setq dape-info-hide-mode-line nil)

  ;; Pulse source line (performance hit)
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; Showing inlay hints
  (setq dape-inlay-hints t)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook 'kill-buffer)

  (defhydra bergheim/dape-hydra (:color pink :hint nil)
    "
^Stepping^           ^Breakpoints^               ^Info
^^^^^^^^-----------------------------------------------------------
_n_: Next            _bb_: Toggle (add/remove)   _i_: Info
_s_: Step in         _bd_: Delete                _m_: Memory
_o_: Step out        _bD_: Delete all            _S_: Select Stack
_<_/_>_: Stack up/down _bl_: Set log message       _R_: Repl
_c_: Continue
_r_: Restart
            _d_: Init   _K_: Kill   _q_: Quit
"
    ("n" dape-next nil :exit nil)
    ("s" dape-step-in nil :exit nil)
    ("o" dape-step-out nil :exit nil)
    ("<" dape-stack-select-down nil :exit nil)
    (">" dape-stack-select-up nil :exit nil)
    ("c" dape-continue nil :exit nil)
    ("r" dape-restart nil :exit nil)
    ("ba" dape-breakpoint-toggle nil :exit nil)
    ("bb" dape-breakpoint-toggle nil :exit nil)
    ("be" dape-breakpoint-expression nil :exit nil)
    ("bd" dape-breakpoint-remove-at-point nil :exit nil)
    ("bD" dape-breakpoint-remove-all nil :exit nil)
    ("bl" dape-breakpoint-log nil :exit nil)
    ("i" dape-info nil :exit nil)
    ("m" dape-read-memory nil :exit nil)
    ("S" dape-select-stack nil :exit nil)
    ("R" dape-repl nil :exit nil)
    ("d" dape nil :exit nil)
    ("K" dape-kill :exit t)
    ("q" nil :exit t)

    ;; Exit on any unbound key
    ("C-g" nil :exit t)
    ("[escape]" nil :exit t)))

;; ;; Enable repeat mode for more ergonomic `dape' use
;; (use-package repeat
;;   :config
;;   (repeat-mode))

(when (< emacs-major-version 30)
  ;; needed for dape
  (use-package jsonrpc))

;;; programming.el ends here
