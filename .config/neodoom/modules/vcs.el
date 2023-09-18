;;; vcs.el --- Description -*- lexical-binding: t; -*-
;;

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))

(use-package browse-at-remote
  :ensure t
  :defer t)
