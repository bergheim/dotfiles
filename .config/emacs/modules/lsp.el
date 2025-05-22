;;; lsp.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Thomas Bergheim

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq bergheim/lsp-keymap-prefix "C-c l")
  :hook
  ((js-mode
    typescript-mode
    tsx-ts-mode
    html-mode
    css-mode
    web-mode) . lsp-deferred)
  :custom
  (lsp-keymap-prefix bergheim/lsp-keymap-prefix)
  (lsp-enable-folding nil)      ;; tweak to taste
  (lsp-enable-file-watchers nil) ;; maybe speed up on big projects
  )

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
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
  :bind ("C-c d" . dap-debug-last)
  :config
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls)) ; https://github.com/emacs-lsp/dap-mode/issues/314

  (require 'dap-js)        ; ships with the latest dap-mode
  (dap-js-setup)

  (dap-register-debug-template "Node::Run"
                               (list :type "pwa-node"
                                     :request "launch"
                                     :name "Node :: Run"
                                     :skipFiles: '("<node_internals>/**")
                                     :cwd (expand-file-name (project-root (project-current)))
                                     :program (expand-file-name
                                               "src/index.js" (project-root (project-current)))))

  (dap-register-debug-template
   "Node::Attach"
   (list :type    "pwa-node"
         :request "attach"
         :name    "JS::Attach"
         :host    "127.0.0.1"
         :port    9229
         :cwd (expand-file-name (project-root (project-current)))))

  ;; pull in support for gdb
  ;; (require 'dap-gdb-lldb)
  (dap-mode 1)
  ;; show fringe indicators for errors and breakpoints and the like
  (dap-ui-mode 1)
  ;; displays floating panel with debug buttons, requires emacs 26+ and posframe package
  (dap-ui-controls-mode 1)

  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  ;; automatically trigger the hydra when the program hits a breakpoint
  (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra)))
  )

;; (use-package dap-ui
;;   :after dap-mode
;;   :config
;;   (dap-ui-mode 1)
;;   (dap-tooltip-mode 1)
;;   (tooltip-mode 1))
