
(defun bergheim/get-and-ensure-data-dir (directory &optional filename)
  (unless bergheim/cache-dir
    (error "bergheim/cache-dir is not set."))
  (let* ((dir (or directory ""))
         (temp-dir (expand-file-name dir bergheim/cache-dir)))
    (unless (file-exists-p temp-dir)
      (make-directory temp-dir t))
    (if filename
        (expand-file-name filename temp-dir)
      temp-dir)))

(defun bergheim/get-and-ensure-config-dir (directory &optional filename)
  (unless bergheim/config-dir
    (error "bergheim/config-dir is not set."))
  (let* ((dir (or directory ""))
         (temp-dir (expand-file-name dir bergheim/config-dir)))
    (unless (file-exists-p temp-dir)
      (make-directory temp-dir t))
    (if filename
        (expand-file-name filename temp-dir)
      temp-dir)))

(defun bergheim/load-file (filename)
  "Load a file from the `bergheim/config-dir` directory."
  (let ((full-path (expand-file-name filename bergheim/config-dir)))
    (load-file full-path)))

(defun bergheim/reload-init-file ()
  (interactive)
  (load-file (expand-file-name "init.el" bergheim/config-dir))
  (message "Emacs configuration reloaded successfully!"))

(use-package no-littering
  :ensure t
  :demand t
  :init
  (setq no-littering-var-directory (concat bergheim/cache-dir "var"))
  (setq no-littering-etc-directory (concat bergheim/cache-dir "etc"))
  :config
  (no-littering-theme-backups))

(use-package general
  :ensure t
  :demand t
  :config
  (defvar bergheim/localleader-map (make-sparse-keymap)
    "Keymap for 'SPC m'")

  (general-create-definer bergheim/global-menu-keys
    :states '(normal visual insert motion emacs)
    :prefix "SPC"
    :keymaps 'override
    :non-normal-prefix "M-SPC")

  (general-create-definer bergheim/localleader-keys
    :prefix "SPC m"
    :states '(normal visual emacs)
    :keymaps 'bergheim/localleader-map)

  (general-create-definer bergheim/global-evil-keys
    :states '(normal visual motion operator)
    :keymaps 'override)

  (general-create-definer bergheim/emacs-lisp-keys
    :prefix "SPC m"
    :states '(normal visual emacs)
    :keymaps 'emacs-lisp-mode-map)

  (general-override-mode)
  (general-evil-setup))

;; general modifies use-package so make sure we get it before moving on
(elpaca-wait)
