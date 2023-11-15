;;; bergheim-eglot.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

(use-package eglot
  :ensure nil
  :defer t
  :bind (("M-<mouse-1>" . eglot-find-implementation))
  :config
  (add-to-list 'eglot-server-programs
               '(tsx-ts-mode . ("typescript-language-server" "--stdio")))
  ;; ignore debug logging - should speed up LSP
  (fset #'jsonrpc--log-event #'ignore)
  :custom
  (eglot-autoshutdown t)
  :hook ((web-mode . eglot-ensure))
         (typescript-ts-base-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (eglot-managed-mode . me/flymake-eslint-enable-maybe)

  :general
  (bergheim/global-menu-keys
   "c" '(:ignore t :which-key "Code")
   "c a" '(eglot-code-actions :which-key "Format region")
   "c e" '(flymake-show-buffer-diagnostics :which-key "Errors")
   "c r" '(eglot-rename :which-key "Rename")
   "c f" '(eglot-format-buffer :which-key "Format buffer")
   "c F" '(eglot-format :which-key "Format region")
   "c i" '(eglot-find-implementation :which-key "Find implementation")
   "c d" '(eglot-find-typeDefinition :which-key "Find definition")
   "c q" '(eglot-code-action-quickfix :which-key "Quickfix")
   "c o" '(eglot-code-action-organize-imports :which-key "Organize imports")))

;; TODO: maybe put it here and not in `:config' as it can hurt discoverability because of `:defer'
;; (use-package general
;;   :ensure t
;;   :config
;;   (bergheim/global-menu-keys
;;    "c" '(:ignore t :which-key "Code")
;;    "c a" '(eglot-code-actions :which-key "Format region")
;;    "c e" '(flymake-show-buffer-diagnostics :which-key "Errors")
;;    "c r" '(eglot-rename :which-key "Rename")
;;    "c f" '(eglot-format-buffer :which-key "Format buffer")
;;    "c F" '(eglot-format :which-key "Format region")
;;    "c i" '(eglot-find-implementation :which-key "Find implementation")
;;    "c d" '(eglot-find-typeDefinition :which-key "Find definition")
;;    "c q" '(eglot-code-action-quickfix :which-key "Quickfix")
;;    "c o" '(eglot-code-action-organize-imports :which-key "Organize imports")))

(use-package consult-eglot
  :ensure t
  :after eglot
  :general
  (bergheim/global-menu-keys
   "c s" '(consult-eglot-symbols :which-key "Find symbols")))


;;; bergheim-eglot.el ends here
