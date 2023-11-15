;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; required for the tests
(package-initialize)

(setq visible-bell t ;; flash
      inhibit-startup-message t

      bergheim/home-directory (expand-file-name "~/")

      gc-cons-threshold 100000000 ; 100 mb
      read-process-output-max (* 1024 1024) ; 1mb

      initial-major-mode 'emacs-lisp-mode  ; default mode for the *scratch* buffer
      display-time-default-load-average nil ; this information is useless for most

      read-answer-short t ;; y means yes

      use-dialog-box nil ;; plz no

      sentence-end-double-space nil ;; Fix archaic defaults
      )

(defalias 'yes-or-no-p 'y-or-n-p)

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
  (load-file (concat modules-dir "bergheim-utils.el")))

(setq lock-directory (bergheim/get-and-ensure-data-dir "lock/"))

;; TODO: refactor this. we need the macro before the autoloads
(load (expand-file-name "modules/email.macros.el" bergheim/config-dir))

(loaddefs-generate (concat bergheim/config-dir "modules")
                   (bergheim/get-and-ensure-data-dir nil "neodoom-autoloads.el"))

(let ((autoloads-file (bergheim/get-and-ensure-data-dir nil "neodoom-autoloads.el")))
  (message "Loading neodoom-autoloads.el")
  (when (file-exists-p autoloads-file)
    (load-file autoloads-file)))

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

(require 'uniquify) ;; Make same named buffers unique

(electric-pair-mode t) ;; insert closing parens

(setq-default indent-tabs-mode nil) ;; I have given up on tabs

(save-place-mode t) ;; jump back in old files
(recentf-mode t) ;; recent files
(savehist-mode t) ;; save minibuffer history

;; clean up config dir
(setq auto-save-list-file-prefix (concat bergheim/cache-dir "/auto-save-list/.saves-"))
(setq native-comp-eln-cache-dir (concat bergheim/cache-dir "/eln-cache/"))
(setq recentf-save-file (concat bergheim/cache-dir "/recentf"))
(setq transient-history-file (concat bergheim/cache-dir "/transient/history.el"))
;; TODO: do I want these in config or not
;; (setq bookmark-default-file (concat bergheim/cache-dir "/bookmarks"))
;; (setq package-user-dir (concat bergheim/cache-dir "/elpa"))
;; (setq savehist-file (concat bergheim/cache-dir "/history"))
;; (setq save-place-file (concat bergheim/cache-dir "/places"))

;; Reload files that are changed outside of Emacs
(global-auto-revert-mode 1)

(setq uniquify-buffer-name-style 'forward
      window-resize-pixelwise t
      frame-resize-pixelwise t
      load-prefer-newer t
      backup-by-copying t
      ;; I _think_ this should be something else (ie the cache directory)
      backup-directory-alist `(("." . ,(concat bergheim/cache-dir "backups")))
      ;; TODO I am seeing `#FILE#' in folders - see if this removes them
      auto-save-file-name-transforms `((".*" ,bergheim/cache-dir t))
      custom-file (expand-file-name "custom.el" bergheim/config-dir))


;; Show the help buffer after startup
;; (add-hook 'after-init-hook 'help-quick)

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
  (load-file (concat module-dir "session.el"))
  (load-file (concat module-dir "style.el"))
  (load-file (concat module-dir "vcs.el"))
  (load-file (concat module-dir "completion.el"))
  (load-file (concat module-dir "workspace.el"))
  (load-file (concat module-dir "formating.el"))
  (load-file (concat module-dir "nav.el"))
  (load-file (concat module-dir "keybindings.el"))
  (load-file (concat module-dir "orgmode/init.el"))
  (load-file (concat module-dir "bergheim-eglot.el"))
  (load-file (concat module-dir "mu4e/init.el"))
  (load-file (concat module-dir "evil.module.el"))
  (load-file (concat module-dir "programming.el"))
  (load-file (concat module-dir "evil.module.el")))

;; (use-package evil-surround
;;   :config
;;   (global-evil-surround-mode 1))

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
