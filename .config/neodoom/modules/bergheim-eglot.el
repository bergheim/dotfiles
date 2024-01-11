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

(defun berhgeim/eglot-inlay-hints-toggle ()
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
  :elpaca nil
  :ensure nil
  :defer t
  :bind (("M-<mouse-1>" . eglot-find-implementation))
  :config
  ;; disabling event logging
  (setq eglot-events-buffer-size 0)
  (add-to-list
   'eglot-server-programs
   '((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode)
     "typescript-language-server" "--stdio"
     ;; I totally came up with these myself
     :initializationOptions
     (:preferences
      (
       :includeInlayEnumMemberValueHints t
       :includeInlayFunctionLikeReturnTypeHints t
       :includeInlayFunctionParameterTypeHints t
       :includeInlayParameterNameHints "all" ; "none" | "literals" | "all"
       :includeInlayParameterNameHintsWhenArgumentMatchesName t
       :includeInlayPRopertyDeclarationTypeHints t
       :includeInlayVariableTypeHints t
       :includeInlayVariableTypeHintsWhenTypeMatchesName t))))

  ;; ignore debug logging - should speed up LSP
  (fset #'jsonrpc--log-event #'ignore)
  (put 'eglot-error 'flymake-overlay-control nil)
  (put 'eglot-note 'flymake-overlay-control nil)
  (put 'eglot-warning 'flymake-overlay-control nil)
  :custom
  (eglot-autoshutdown t)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  :hook
  (eglot-managed-mode . me/eglot-inlay-hints-maybe)
  (eglot-managed-mode . me/flymake-eslint-enable-maybe)
  (eglot-managed-mode . bergheim/eglot-capf)
  (web-mode . eglot-ensure)
  (typescript-ts-base-mode . eglot-ensure)

  :general
  (bergheim/global-menu-keys
    "c" '(:ignore t :which-key "Code")
    "c a" '(eglot-code-actions :which-key "Format region")
    "c R" '(eglot-rename :which-key "Rename")
    "c f" '(eglot-format-buffer :which-key "Format buffer")
    "c F" '(eglot-format :which-key "Format region")
    "c i" '(eglot-find-implementation :which-key "Find implementation")
    "c d" '(eglot-find-typeDefinition :which-key "Find definition")
    "c q" '(eglot-code-action-quickfix :which-key "Quickfix")
    "c o" '(eglot-code-action-organize-imports :which-key "Organize imports")))

(use-package consult-eglot
  :ensure t
  :after eglot
  :general
  (bergheim/global-menu-keys
    "c s" '(consult-eglot-symbols :which-key "Find symbols")))

;;; bergheim-eglot.el ends here
