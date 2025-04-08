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
          "^\\*helpful .*\\*$"
          flymake-diagnostics-buffer-mode
          help-mode
          compilation-mode
          "\\*eldoc\\*"
          ))

  (setq popper-window-height 16)
  (setq popper-display-control 'user)
  (setq popper-display-function #'popper-display-popup-at-bottom)
  (setq popper-group-function #'popper-group-by-project)
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package winner
  :ensure nil
  :init
  (winner-mode 1))

(defun bergheim/zoom-window ()
  "Maximize the window or restore the previous layout."
  (interactive)
  (if (one-window-p)
      (winner-undo)
    (delete-other-windows)))

(use-package bufler
  :config
  (bufler-mode)
  :general
  (:keymaps 'bufler-list-mode-map
   :states '(normal visual)
   "C-j" (lambda ()
           (interactive)
           (forward-line)
           (bufler-list-buffer-peek))
   "C-k" (lambda ()
           (interactive)
           (forward-line -1)
           (bufler-list-buffer-peek))))

(use-package burly
  :config
  (burly-tabs-mode 1))

(use-package activities
  :demand
  :ensure
  :config
  ;; Automatically save activities' states when Emacs is idle and upon exit.
  (activities-mode)
  (activities-tabs-mode))
(use-package project
  :ensure nil
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

(defun bergheim/project-find-file-other-window (&optional include-all)
  "Run `project-find-file' but open the selected file in another window.
With INCLUDE-ALL prefix arg, include all files, not just tracked files."
  (interactive "P")
  (let ((current-prefix-arg include-all)
        (display-buffer-overriding-action '(display-buffer-use-some-window . ((inhibit-same-window . t)))))
    (call-interactively #'project-find-file)))

(defun bergheim/open-or-switch-to-project-tab (new-frame)
  "Open a selected project in a new tab, or switch to it if it already exists.
If NEW-FRAME is non-nil, open the project in a new frame."
  (interactive "P")
  (let* ((project-path (project-prompt-project-dir))
         (name (file-name-nondirectory (directory-file-name project-path)))
         (tab-names (mapcar (lambda (tab) (alist-get 'name tab))
                            (funcall tab-bar-tabs-function))))
    (if (member name tab-names)
        (tab-bar-switch-to-tab name)
      (tab-new)
      (tab-rename name)
      (cd project-path)
      (project-switch-project project-path))
    (when new-frame
      (tab-bar-detach-tab))))

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
      (siren-tab-bar-new-named-tab name)
      (tab-bar-select-tab (length (funcall tab-bar-tabs-function))))))

(defun siren-tab-bar-new-named-tab (name)
  "Create a new tab named NAME."
  (interactive "MName for new tab (leave blank for automatic naming): ")
  (tab-new 99999)
  (if (not (string= name ""))
      (tab-rename name)))
;;; workspace.el ends here
