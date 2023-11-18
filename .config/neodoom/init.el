;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

(defvar bergheim/cache-dir
  (let* ((xdg-cache (or (getenv "XDG_CACHE_HOME")
                        (expand-file-name "~/.cache/")))
         (cache-dir (concat xdg-cache "neodoom/")))
    (unless (file-exists-p cache-dir)
      (make-directory cache-dir))
    cache-dir))

(defvar bergheim/config-dir
  (let ((xdg-config (or (getenv "XDG_CONFIG_HOME")
                        (expand-file-name "~/.config/"))))
    (expand-file-name "neodoom/" xdg-config)))

(defvar bergheim/home-dir
  (let ((xdg-home (or (getenv "HOME")
                      (expand-file-name "~/"))))
    xdg-home))

;; bootstrap helpers
(let ((modules-dir (concat bergheim/config-dir "modules/")))
  (unless (file-exists-p modules-dir)
    (make-directory modules-dir))
  (load-file (expand-file-name "bootstrap.el" modules-dir)))

(setq visible-bell t ;; flash
      inhibit-startup-message t
      evil-want-Y-yank-to-eol t
      package-user-dir (bergheim/get-and-ensure-data-dir "elpa") 

      bergheim/home-directory (expand-file-name "~/")

      gc-cons-threshold 100000000 ; 100 mb
      read-process-output-max (* 1024 1024) ; 1mb
      initial-major-mode 'emacs-lisp-mode  ; default mode for the *scratch* buffer
      display-time-default-load-average nil ; this information is useless for most
      read-answer-short t ;; y means yes
      use-dialog-box nil ;; plz no
      sentence-end-double-space nil) ;; Fix archaic defaults

(package-initialize)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq lock-directory (bergheim/get-and-ensure-data-dir "lock/"))

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Bring in package utilities so we can install packages from the web.
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; adds :vc keyword to use-package
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; In your .emacs or init.el or whatever your main configuration file is
(let ((private-file (expand-file-name "private.el" bergheim/config-dir)))
  (when (file-exists-p private-file)
    (load private-file)))

(setq custom-file (expand-file-name "custom.el" bergheim/config-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(let ((module-dir (expand-file-name "modules/" bergheim/config-dir)))
  (load-file (concat module-dir "base.el"))
  (load-file (concat module-dir "style.el"))
  (load-file (concat module-dir "utils.el"))
  (load-file (concat module-dir "vcs.el"))
  (load-file (concat module-dir "workspace.el"))
  (load-file (concat module-dir "formating.el"))
  (load-file (concat module-dir "nav.el"))
  (load-file (concat module-dir "keybindings.el"))
  (load-file (concat module-dir "orgmode/init.el"))
  (load-file (concat module-dir "bergheim-eglot.el"))
  (load-file (concat module-dir "mu4e/init.el"))
  (load-file (concat module-dir "evil.module.el"))
  (load-file (concat module-dir "programming.el"))
  (load-file (concat module-dir "completion.el"))
  (load-file (concat module-dir "evil.module.el"))
  (load-file (concat module-dir "apps.el"))
  (load-file (concat module-dir "session.el")))

(use-package site-lisp
  :ensure t
  :demand t
  :config
  (setq site-lisp-directory (expand-file-name "autoloads" bergheim/config-dir))
  (site-lisp-initialise))

;; maybe actually try this
(use-package denote
  :ensure t
  :custom
  (denote-known-keywords '("emacs" "journal"))
  ;; This is the directory where your notes live.
  (denote-directory (expand-file-name "~/denote/"))
  :bind
  (("C-c n n" . denote)
   ("C-c n f" . denote-open-or-create)
   ("C-c n i" . denote-link)))

(defun display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook 'display-startup-time)

(defun bergheim/run-all-tests ()
  "Load and run all Neodoom tests."
  (interactive)
  (let ((test-dir (bergheim/get-and-ensure-config-dir "tests/")))
    (dolist (file (directory-files test-dir t "-test\\.el$"))
      (load-file file)))
  (ert t))

(provide 'bergheim-tests)
