
(use-package eshell
  :bind (("C-r" . consult-history)))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)
