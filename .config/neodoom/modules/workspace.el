;;; workspace.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim
;;
;; Author: Thomas Bergheim <thomas@glvortex.net>
;; Maintainer: Thomas Bergheim <thomas@glvortex.net>
;; Created: September 16, 2023
;; Modified: September 16, 2023


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

(use-package projectile
  :ensure t
  :config
  (setq projectile-project-search-path (list (concat bergheim/home-directory "dev")))
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;;; workspace.el ends here
