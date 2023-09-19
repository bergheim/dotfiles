;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

(message "Hello World!")

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
  (let ((xdg-cache (or (getenv "XDG_CACHE_HOME")
                       (expand-file-name "~/.cache/"))))
    (expand-file-name "neodoom/" xdg-cache)))

(defvar bergheim/config-dir
  (let ((xdg-config (or (getenv "XDG_CONFIG_HOME")
                        (expand-file-name "~/.config/"))))
    (expand-file-name "neodoom/" xdg-config)))

(defvar bergheim/home-dir
  (let ((xdg-home (or (getenv "HOME")
                      (expand-file-name "~/"))))
    xdg-home))

(defun bergheim/load-file (filename)
  "Load a file from the `bergheim/config-dir` directory."
  (let ((full-path (expand-file-name filename bergheim/config-dir)))
    (load-file full-path)))

(unless (file-exists-p bergheim/cache-dir)
  (make-directory bergheim/cache-dir t))


;; TODO: refactor this. we need the macro before the autoloads
(load (expand-file-name "modules/email.macros.el" bergheim/config-dir))


(add-to-list 'load-path (expand-file-name "autoloads" bergheim/config-dir))

;; TODO: remove this once the config settles
(let ((autoloads-dir (expand-file-name "autoloads" bergheim/config-dir)))
  (setq generated-autoload-file (concat autoloads-dir "autoloads.el"))
  (update-directory-autoloads autoloads-dir))

(load (expand-file-name "autoloads/autoloads.el" bergheim/config-dir))

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
(setq auto-revert-interval 1)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

(setq uniquify-buffer-name-style 'forward
      window-resize-pixelwise t
      frame-resize-pixelwise t
      load-prefer-newer t
      backup-by-copying t
      ;; I _think_ this should be something else (ie the cache directory)
      backup-directory-alist `(("." . ,(concat bergheim/cache-dir "backups")))
      custom-file (expand-file-name "custom.el" bergheim/config-dir))

(defun bergheim/reload-init-file ()
  (interactive)
  (load-file (expand-file-name "init.el" bergheim/config-dir))


  (message "Emacs configuration reloaded successfully!"))

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
  (load-file (concat module-dir "bergheim-eglot.el"))
  (load-file (concat module-dir "orgmode/init.el"))
  (load-file (concat module-dir "mu4e/init.el"))
  (load-file (concat module-dir "evil.module.el"))
  (load-file (concat module-dir "programming.el")))

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
