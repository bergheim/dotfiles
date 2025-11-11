;;; bergheim-eglot.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

(defvar me/eglot-inlay-hints-automatic nil
  "Whether `eglot-inlay-hints-mode' should be enabled by default.")

(defun me/eglot-inlay-hints-maybe ()
  "Maybe enable `eglot-inlay-hints-mode'.
See `me/eglot-inlay-hints-automatic'."
  (if me/eglot-inlay-hints-automatic
      (eglot-inlay-hints-mode 1)
    (eglot-inlay-hints-mode -1)))

(defun bergheim/eglot-inlay-hints-toggle ()
  "Toggle `me/eglot-inlay-hints-automatic'.
Also toggle `eglot-inlay-hints-mode' accordingly."
  (interactive)
  (let ((value me/eglot-inlay-hints-automatic))
    (eglot-inlay-hints-mode (if value -1 1))
    (if value
        (message "Inlay hints disabled")
      (message "Inlay hints enabled"))
    (setq-default me/eglot-inlay-hints-automatic (not value))))

(defun bergheim/eglot-capf ()
  ;; just set up the bare minimum when using eglot
  ;; also it seems to highjack everything, so do not put it at the top for now..
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'tempel-expand
                     #'cape-file
                     #'eglot-completion-at-point
                     ))))

(use-package eglot
  ;; FIXME: if we use the one on ELPA we need track-changes
  :ensure nil
  :defer t
  :bind (("M-<mouse-1>" . eglot-find-implementation))
  :config
  ;; disabling event logging
  (setq eglot-events-buffer-size 0)
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode)
                 "typescript-language-server" "--stdio"
                 ;; I totally came up with these myself
                 :initializationOptions
                 (:preferences
                  (:includeInlayEnumMemberValueHints t
                   :includeInlayFunctionLikeReturnTypeHints t
                   :includeInlayFunctionParameterTypeHints t
                   :includeInlayParameterNameHints "all" ; "none" | "literals" | "all"
                   :includeInlayParameterNameHintsWhenArgumentMatchesName t
                   :includeInlayPropertyDeclarationTypeHints t
                   :includeInlayVariableTypeHints t
                   :includeInlayVariableTypeHintsWhenTypeMatchesName t))))

  (add-to-list 'eglot-server-programs
               '((html-mode css-mode web-mode) "tailwindcss-language-server" "--stdio"))

  (add-to-list 'eglot-server-programs
               '((elixir-mode elixir-ts-mode) "/usr/lib/elixir-ls/language_server.sh"))

  (add-to-list 'eglot-server-programs
               '(go-ts-mode . ("gopls")))

  ;; ignore debug logging - should speed up LSP
  (fset #'jsonrpc--log-event #'ignore)
  :custom
  (eglot-autoshutdown t)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  :hook
  (eglot-managed-mode . me/eglot-inlay-hints-maybe)
  (eglot-managed-mode . bergheim/eglot-capf)
  (web-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (elixir-ts-mode . eglot-ensure)
  (typescript-ts-base-mode . eglot-ensure)

  :general
  (bergheim/global-menu-keys
    "c" '(:ignore t :which-key "Code")
    "c a" '(eglot-code-actions :which-key "Format region")
    "c R" '(eglot-rename :which-key "Rename")
    "c f" '(eglot-format-buffer :which-key "Format buffer")
    "c F" '(eglot-format :which-key "Format region")
    "c i" '(eglot-find-implementation :which-key "Find implementation")
    "c q" '(eglot-code-action-quickfix :which-key "Quickfix")
    "c o" '(eglot-code-action-organize-imports :which-key "Organize imports")))

(use-package consult-eglot
  :ensure t
  :after eglot
  :general
  (bergheim/global-menu-keys
    "c s" '(consult-eglot-symbols :which-key "Find symbols")))

(use-package dape
  :demand
  :ensure
  :hook
  (kill-emacs . dape-breakpoint-save)
  (after-init . dape-breakpoint-load)

  :config
  ;; TODO: figure out if these override important things
  ;; (general-define-key
  ;;  :states '(normal motion visual)
  ;;  :keymaps 'override
  ;;  :predicate 'dape-active-mode
  ;;  "gb" '(dape-breakpoint-toggle :wk "Dape Toggle Breakpoint")
  ;;  "gB" '(dape-breakpoint-remove-all :wk "Dape Remove Breakpoints")
  ;;  "gE" '(dape-breakpoint-expression :wk "Dape Breakpoint Expression")
  ;;  "gl" '(dape-breakpoint-log :wk "Dape Breakpoint Log")

  ;;  "]s" '(dape-stack-select-down nil :wf "Dape Stack Down")
  ;;  "[s" '(dape-stack-select-up nil :wf "Dape Stack Up")

  ;;  "<" '(dape-stack-select-down nil :wf "Dape Stack Down")
  ;;  ">" '(dape-stack-select-up nil :wf "Dape Stack Up")

  ;;  "gc" '(dape-continue :wk "Dape Continue")
  ;;  "ge" '(dape-evaluate-expression :wk "Dape Evaluate")
  ;;  "gI" '(dape-info :wk "Dape Toggle Info")
  ;;  "gi" '(dape-step-in :wk "Dape Step In")
  ;;  "gk" '(dape-kill :wk "Dape Kill Session") ;; keeps the buffers open unlike quit
  ;;  "gn" '(dape-next :wk "Dape Next")
  ;;  "go" '(dape-step-out :wk "Dape Step Out")
  ;;  "gq" '(dape-quit :wk "Dape Quit")
  ;;  "gR" '(dape-repl :wk "Dape REPL")
  ;;  "gr" '(dape-restart :wk "Dape Restart")
  ;;  "gs" '(dape-select-stack :wk "Dape Select Stack")
  ;;  "gw" '(dape-watch-dwim :wk "Dape Watch DWIM"))


  (let ((js-debug-base (cdr (assq 'js-debug-node-attach dape-configs))))
    (add-to-list 'dape-configs
                 `(nextjs-debug
                   ;; maybe extend https://github.com/svaante/dape/pull/246/files
                   ,@js-debug-base
                   :skipFiles ["<node_internals>/*"
                               "*/node_modules/*"
                               "*/.next/*"
                               "*/webpack/*"
                               "*/json-schema-to-typescript/*"
                               "*/react-server-dom-webpack/*"
                               "*/.pnpm/*"]
                   )
                 ))
  ;; (setq dape-configs (assq-delete-all 'nextjs-debug dape-configs))

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


;;; bergheim-eglot.el ends here
