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
  :elpaca nil
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
  (bufler-mode))

(use-package burly
  :ensure t
  :config
  (burly-tabs-mode 1))

(use-package project
  :elpaca nil
  :demand t
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

(defun bergheim/open-or-switch-to-project-tab (new-frame)
  "Open a selected project in a new tab, or switch to it if it already exists.
If NEW-FRAME is non-nil, open the project in a new frame."
  (interactive "P")
  (let* ((project-path (project-prompt-project-dir))
         (name (file-name-nondirectory (directory-file-name project-path)))
         (tab-names (mapcar (lambda (tab) (alist-get 'name tab))
                            (funcall tab-bar-tabs-function))))
    (if (member name tab-names)
        (progn
          (tab-bar-switch-to-tab name)
          (if new-frame
              (tab-bar-detach-tab)))
      (tab-new)
      (tab-rename name)
      (cd project-path)
      (project-switch-project project-path)
      (if new-frame
          (tab-bar-detach-tab)))))

;; these are great
(defun siren-tab-bar-switch-to-or-create-tab (name)
  "Switch to or create a tab by NAME."
  (interactive
   (let* ((recent-tabs (mapcar (lambda (tab) (alist-get 'name tab))
                               (tab-bar--tabs-recent))))
     (list (completing-read "Switch to tab by name (default recent): "
                            recent-tabs nil nil nil nil recent-tabs))))
  (let ((tab-names (mapcar (lambda (tab) (alist-get 'name tab))
                           (funcall tab-bar-tabs-function))))
    (if (member name tab-names)
        (tab-bar-switch-to-tab name)
      (siren-tab-bar-new-named-tab name)))
  (tab-bar-select-tab (1+ (or (tab-bar--tab-index-by-name name) 0))))

(defun siren-tab-bar-new-named-tab (name)
  "Create a new tab named NAME."
  (interactive "MName for new tab (leave blank for automatic naming): ")
  (tab-new 99999)
  (if (not (string= name ""))
      (tab-rename name)))

(tab-bar-mode 1)
(tab-bar-history-mode 1)
(setq tab-bar-new-tab-choice "*scratch*")

;;; workspace.el ends here
