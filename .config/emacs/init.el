;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

(defvar bergheim/cache-dir
  (let* ((xdg-cache (or (getenv "XDG_CACHE_HOME")
                        (expand-file-name "~/.cache")))
         (cache-dir (file-name-as-directory
                     (expand-file-name "emacs" xdg-cache))))
    (unless (file-exists-p cache-dir)
      (make-directory cache-dir t))
    cache-dir))

(defvar bergheim/config-dir
  (let ((xdg-config (or (getenv "XDG_CONFIG_HOME")
                        (expand-file-name "~/.config/"))))
    (expand-file-name "emacs/" xdg-config)))

(defvar bergheim/home-dir
  (let ((xdg-home (or (getenv "HOME")
                      (expand-file-name "~/"))))
    xdg-home))

(defvar bergheim/container-mode-p
  (getenv "EMACS_CONTAINER")
  "Non-nil when running in container/development mode")

(defun bergheim/call-with-universal-arg (fn)
  (lambda ()
    (interactive)
    (let ((current-prefix-arg 4))
      (call-interactively fn))))

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca" bergheim/cache-dir))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-cache-directory (expand-file-name "cache/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

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

(setq custom-file (make-temp-file ""))
(when (file-exists-p custom-file)
  (load custom-file))

(let ((module-dir (expand-file-name "modules/" bergheim/config-dir))
      (modules
       '("evil.module"
         "base"
         "style"
         "vcs"
         "formating"
         "nav"
         "keybindings"
         "programming"
         "completion"
         "lsp"
         ;; "bergheim-eglot"
         ;; I for one come our new AI overlords
         "ai"
         "session"
         ;; TODO split this up
         "utils"
         "workspace"
         )))

  (unless bergheim/container-mode-p
    (setq modules (append modules
                          '("orgmode/init"
                            "karakeep"
                            "bergheim-denote"
                            "mu4e/init"
                            "apps"))))
  (dolist (file modules)
    (load-file (expand-file-name (format "%s.el" file) module-dir))))

(use-package site-lisp
  :demand t
  :config
  ;; (setq site-lisp-directory (expand-file-name "autoloads" bergheim/config-dir))
  (setq site-lisp-directory (expand-file-name "modules" bergheim/config-dir))
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
