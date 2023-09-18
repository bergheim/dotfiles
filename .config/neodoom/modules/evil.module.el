;;; evil.module.el --- Description -*- lexical-binding: t; -*-

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org
  :ensure t
  :after org
  :hook
  (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; much better node matching
(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

;; gl/gL text-object char
(use-package evil-lion
  :ensure t
  ;; :defer t
  :config
  (evil-lion-mode))

;; TODO add this and keybindings
;; (use-package evil-textobj-tree-sitter
;;   :ensure t)

;; gc  evil-commentary
;; gy  evil-commentary-yank
;; s-/ evil-commentary-line
;;     evil-commentary-yank-line
(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package vimish-fold
  :ensure t
  :defer t
  :after evil)

(use-package evil-vimish-fold
  :ensure t
  :defer t
  :after vimish-fold
  :init
  (setq evil-vimish-fold-mode-lighter " ⮒")
  (setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
  :config
  (global-evil-vimish-fold-mode))


;;; evil.module.el ends here
