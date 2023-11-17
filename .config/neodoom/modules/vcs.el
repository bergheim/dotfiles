;;; vcs.el --- Description -*- lexical-binding: t; -*-
;;

(use-package magit
  :ensure t
  :after diff-hl
  :hook
  (with-editor-mode        . evil-insert-state)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh))

(use-package forge
  :ensure t
  :after magit)

;; TODO is this really needed..
(use-package diff-hl
  :ensure t
  :defer t
  :hook ((prog-mode . diff-hl-mode)
         (vc-dir-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode)
  ;; To keep in line with your vim-style keybindings preference:
  (general-define-key
   :states '(normal visual)
   "g p" 'diff-hl-previous-hunk
   "g n" 'diff-hl-next-hunk))

(use-package browse-at-remote
  :ensure t
  :defer t)
