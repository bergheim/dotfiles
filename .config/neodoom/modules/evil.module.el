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

;;; evil.module.el ends here
