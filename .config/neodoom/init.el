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

(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca" bergheim/cache-dir))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; needed for magit. See https://github.com/progfolio/elpaca/issues/216
(defun +elpaca-unload-seq (e) "Unload seq before continuing the elpaca build, then continue to build the recipe E."
       (and (featurep 'seq) (unload-feature 'seq t))
       (elpaca--continue-build e))
(elpaca `(seq :build ,(append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                                           elpaca--pre-built-steps
                                         elpaca-build-steps))
                              (list '+elpaca-unload-seq 'elpaca--activate-package))))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t)
  (setq elpaca-queue-limit 30))

;; ;; Block until current queue processed.
(elpaca-wait)

;; 'always-defer' means that for a package to load we need a ':hook' or using a ':general' keybinding
;; if there is none, we need to explicitly add ':demand' to load the package
;; can also load with ':defer time'
;; (setq use-package-verbose nil		; don't print anything
;;       use-package-compute-statistics nil ; compute statistics about package initialization
;;       use-package-minimum-reported-time 0.0001
;;       use-package-expand-minimally t	; minimal expanded macro
(setq use-package-always-defer t)	; always defer, don't "require", except when :demand

;; analyze startup time
;; (profiler-start 'cpu+mem)
;; (add-hook 'elpaca-after-init-hook (lambda () (profiler-stop) (profiler-report)))

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

      gc-cons-threshold 100000000           ; 100 mb
      read-process-output-max (* 1024 1024) ; 1mb
      initial-major-mode 'emacs-lisp-mode ; default mode for the *scratch* buffer
      display-time-default-load-average nil ; this information is useless for most
      read-answer-short t                   ;; y means yes
      use-dialog-box nil                    ;; plz no
      sentence-end-double-space nil) ;; Fix archaic defaults

;; (fringe-mode 8)
;; (set-frame-parameter nil 'internal-border-width 10)

(defalias 'yes-or-no-p 'y-or-n-p)
(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))

(setq lock-directory (bergheim/get-and-ensure-data-dir "lock/"))

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Bring in package utilities so we can install packages from the web.
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

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
  (load-file (concat module-dir "bergheim-eglot.el"))
  (load-file (concat module-dir "mu4e/init.el"))
  (load-file (concat module-dir "evil.module.el"))
  (load-file (concat module-dir "orgmode/init.el"))
  (load-file (concat module-dir "programming.el"))
  (load-file (concat module-dir "completion.el"))
  (load-file (concat module-dir "apps.el"))
  (load-file (concat module-dir "session.el"))
  )

(add-hook 'after-make-frame-functions #'bergheim/frame-setup)
(add-hook 'emacs-startup-hook #'bergheim/frame-setup)

(use-package site-lisp
  :demand t
  :config
  (setq site-lisp-directory (expand-file-name "autoloads" bergheim/config-dir))
  (site-lisp-initialise))

(defun display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract elpaca-after-init-time before-init-time)))
           gcs-done))

(add-hook 'elpaca-after-init-hook 'display-startup-time)

(defun bergheim/run-all-tests ()
  "Load and run all Neodoom tests."
  (interactive)
  (let ((test-dir (bergheim/get-and-ensure-config-dir "tests/")))
    (dolist (file (directory-files test-dir t "-test\\.el$"))
      (load-file file)))
  (ert t))

(provide 'bergheim-tests)
