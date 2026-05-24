;;; lsp.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Thomas Bergheim

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :general
  (bergheim/global-menu-keys
    "c" '(:ignore t :which-key "code")
    "ca" '(lsp-execute-code-action :which-key "action")
    "cR" '(lsp-rename :which-key "rename")
    "cf" '(lsp-format-buffer :which-key "format buffer")
    "cF" '(lsp-format-region :which-key "format region")
    "ci" '(lsp-find-implementation :which-key "find implementation")
    ;; "ci" '(lsp-goto-implementation :which-key "Find implementation")
    ;; "ci" '(lsp-goto-type-definition :which-key "Find implementation")
    "co" '(lsp-organize-imports :which-key "organize imports"))
  :init
  (setq bergheim/lsp-keymap-prefix "C-c l")
  :hook
  ((js-mode
    js-ts-mode
    typescript-mode
    typescript-ts-mode
    tsx-ts-mode
    html-mode
    css-mode
    go-ts-mode
    web-mode) . lsp-deferred)
  :custom
  (lsp-keymap-prefix bergheim/lsp-keymap-prefix)
  (lsp-enable-folding nil)      ;; tweak to taste
  (lsp-enable-file-watchers nil) ;; maybe speed up on big projects
  )

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :config
  (setq lsp-treemacs-type-hierarchy-expand-depth 5)
  (setq lsp-treemacs-error-list-expand-depth 5)
  :commands lsp-treemacs-errors-list)

;; (use-package dap-mode
;;   :after lsp-mode
;;   :commands dap-debug
;;   :custom
;;   (dap-auto-configure-features '(sessions locals controls tooltip))
;;   :config
;;   ;; install or update the Node debugger if missing
;;   (unless (file-directory-p dap-node-debug-path)
;;     (dap-node-setup))
;;   )

(use-package posframe) ; for dap-ui-controls

(use-package dap-mode

  :bind (("C-c d" . dap-debug-last)
         ("C-c D" . dap-debug))
  :general
  (bergheim/global-menu-keys
    "d" '(:ignore t :which-key "debug")
    "dd" '(dap-debug :which-key "start debug")
    "dl" '(dap-debug-last :which-key "debug last")
    "db" '(dap-breakpoint-toggle :which-key "toggle breakpoint")
    "dB" '(dap-breakpoint-delete-all :which-key "delete all breakpoints")
    "dc" '(dap-continue :which-key "continue")
    "dn" '(dap-next :which-key "next")
    "di" '(dap-step-in :which-key "step in")
    "do" '(dap-step-out :which-key "step out")
    "dr" '(dap-restart-frame :which-key "restart frame")
    "ds" '(dap-switch-session :which-key "switch session")
    "dt" '(dap-terminate :which-key "terminate")
    "du" '(dap-ui-mode :which-key "toggle ui mode")
    "de" '(dap-eval :which-key "eval expression")
    "dh" '(dap-hydra :which-key "debug hydra"))
  :config
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip))

  (require 'dap-node)
  (dap-node-setup)

  ;; Simple Bun execution (most reliable)
  (dap-register-debug-template "Bun::Run"
                               (list :type "node"
                                     :request "launch"
                                     :name "Bun Run"
                                     :program "bun"
                                     :args ["src/index.ts"]
                                     :cwd (expand-file-name (project-root (project-current)))
                                     :sourceMaps t))

  ;; Node.js with compiled JS
  (dap-register-debug-template "Node::Run"
                               (list :type "node"
                                     :request "launch"
                                     :name "Node Compiled"
                                     :program (expand-file-name "dist/index.js" (project-root (project-current)))
                                     :cwd (expand-file-name (project-root (project-current)))
                                     :sourceMaps t))

  ;; Attach to running process
  (dap-register-debug-template "Node::Attach"
                               (list :type "node"
                                     :request "attach"
                                     :name "Node Attach"
                                     :address "localhost"
                                     :port 9229
                                     :program "__ignored"  ; workaround for dap-mode bug
                                     :localRoot (expand-file-name (project-root (project-current)))
                                     :remoteRoot (expand-file-name (project-root (project-current)))))

  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)

  (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra))))

;; (use-package dap-ui
;;   :after dap-mode
;;   :config
;;   (dap-ui-mode 1)
;;   (dap-tooltip-mode 1)
;;   (tooltip-mode 1))
