
(use-package eshell
  :elpaca nil
  :bind (("C-r" . consult-history)))

(use-package treemacs
  :defer t
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)
