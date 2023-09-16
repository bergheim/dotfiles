;;; workspace.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim
;;
;; Author: Thomas Bergheim <thomas@glvortex.net>
;; Maintainer: Thomas Bergheim <thomas@glvortex.net>
;; Created: September 16, 2023
;; Modified: September 16, 2023


(use-package bufler
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

(provide 'workspace)
;;; workspace.el ends here
