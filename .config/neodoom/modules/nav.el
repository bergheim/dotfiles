;;; nav.el --- Description -*- lexical-binding: t; -*-

(use-package dired
  :elpaca nil
  :commands dired-jump
  :after general
  :init
  (setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
        ;; don't prompt to revert, just do it
        ;; dired-auto-revert-buffer #'dired-buffer-stale-p
        ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Ask whether destination dirs should get created when copying/removing files.
        dired-create-destination-dirs 'ask
        ;; Where to store image caches
        image-dired-dir (concat bergheim/cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        ;; Screens are larger nowadays, we can afford slightly larger thumbnails
        image-dired-thumb-size 150)
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "h"   #'dired-up-directory
   "l"   #'dired-find-file))

(use-package dirvish
  :ensure t
  :defer t
  :after general
  :config
  (dirvish-override-dired-mode)
  (setq dirvish-cache-dir (concat bergheim/cache-dir "dirvish/"))
  (setq dirvish-attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size))
  (setq dirvish-subtree-state-style 'nerd)

  (general-define-key
   :states 'normal
   :keymaps 'dirvish-mode-map
   "?"   #'dirvish-dispatch
   "q"   #'dirvish-quit
   "a"   #'dirvish-quick-access
   "f"   #'dirvish-file-info-menu
   "y"   #'dirvish-yank-menu
   "s"   #'dirvish-quicksort
   "TAB" #'dirvish-subtree-toggle
   "M-t" #'dirvish-layout-toggle
   "M-b" #'dirvish-history-go-backward
   "M-f" #'dirvish-history-go-forward
   "M-n" #'dirvish-narrow
   "M-m" #'dirvish-mark-menu
   "M-s" #'dirvish-setup-menu
   "M-e" #'dirvish-emerge-menu)
  )

(defun bergheim/delete-current-file ()
  "Delete the current file and kill its buffer, after asking for confirmation."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (when (and current-file
               (file-exists-p current-file)
               (yes-or-no-p (format "Really delete file %s? " current-file)))
      (delete-file current-file)
      (kill-buffer))))

(use-package affe
  :ensure t)

;;; nav.el ends here
