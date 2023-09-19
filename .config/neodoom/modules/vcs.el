;;; vcs.el --- Description -*- lexical-binding: t; -*-
;;

(use-package magit
  :ensure t
  :hook (with-editor-mode . evil-insert-state))

(use-package browse-at-remote
  :ensure t
  :defer t)
