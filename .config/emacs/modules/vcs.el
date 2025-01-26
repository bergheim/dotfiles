;;; vcs.el --- Description -*- lexical-binding: t; -*-
;;

(use-package magit
  :ensure t
  :after diff-hl
  :commands (magit magit-status magit--handle-bookmark)
  :hook
  (with-editor-mode        . evil-insert-state)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)

  :general
  (:states '(normal visual emacs)
   :keymaps 'magit-mode-map
   "Z" 'magit-stash
   "z 1" 'magit-section-show-level-1-all
   "z 2" 'magit-section-show-level-2-all
   "z 3" 'magit-section-show-level-3-all
   "z 4" 'magit-section-show-level-4-all
   "M-RET" 'magit-diff-visit-worktree-file-other-window))

(use-package git-timemachine)

(use-package forge
  :ensure t
  :after magit)

(use-package magit-todos
  :ensure t
  :after magit
  :custom
  (magit-todos-depth 2)
  :config
  (magit-todos-mode 0))

;; git org links
(use-package orgit)

;; shows diffs in the fringe
(use-package diff-hl
  :ensure t
  :defer t
  :hook ((prog-mode . diff-hl-mode)
         (vc-dir-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; (global-diff-hl-mode)
  ;; To keep in line with your vim-style keybindings preference:
  (general-define-key
   :states '(normal visual)
   "g p" 'diff-hl-previous-hunk
   "g n" 'diff-hl-next-hunk))
