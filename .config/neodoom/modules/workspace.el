;;; workspace.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim
;;
;; Author: Thomas Bergheim <thomas@glvortex.net>
;; Maintainer: Thomas Bergheim <thomas@glvortex.net>
;; Created: September 16, 2023
;; Modified: September 16, 2023

(use-package popper
  :ensure t
  :after general
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "^\\*Flymake diagnostics.*\\*$"
          flymake-diagnostics-buffer-mode
          ;; help-mode
          compilation-mode
          "\\*eldoc\\*"
          ))

  (setq popper-window-height 16)
  ;; (setq popper-display-control 'user)
  (setq popper-display-function #'popper-display-popup-at-bottom)
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package winner
  :init
  (winner-mode 1))

(defun bergheim/zoom-window ()
  "Maximize the window or restore the previous layout."
  (interactive)
  (if (one-window-p)
      (winner-undo)
    (delete-other-windows)))

(use-package bufler
  :ensure t
  :config
  (bufler-mode)
  (when (bound-and-true-p tab-bar-mode)
    (burly-tabs-mode)))

(use-package burly
  :ensure t
  :config
  (when (bound-and-true-p tab-bar-mode)
    (burly-tabs-mode)))

(use-package project
  :ensure nil  ; built-in package
  :config
  (setq project-use-git t)
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-dir "Find directory")
          (bergheim/open-dirvish-current-project "Browse" "b")
          (consult-ripgrep "Grep" "g")
          (consult-fd "Search" "s")
          (magit-project-status "Magit" "m")
          (project-eshell "Eshell"))))

;; TODO: remove this is `project.el' is enough
;; (use-package projectile
;;   :ensure t
;;   :config
;;   (setq projectile-project-search-path (list (concat bergheim/home-directory "dev")))
;;   (setq projectile-cache-file (concat bergheim/cache-dir "/projectile.cache")
;;         projectile-known-projects-file (concat bergheim/cache-dir "/projectile-bookmarks.eld"))
;;   (projectile-mode +1)
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


;;; workspace.el ends here
