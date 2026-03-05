;;; early-init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim
;;
;; Author: Thomas Bergheim
;; Maintainer: Thomas Bergheim
;; Created: September 16, 2023
;; Modified: September 16, 2023
;; Version: 0.0.1

(setq package-enable-at-startup nil)

;; Startup speed, annoyance suppression

;; Startup speed (ignore gc at startup), annoyance suppression
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold (* 1000 1024 1024)  ; 1000MB. a lot but won't trigget any oom busters on new installs
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

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


(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" bergheim/cache-dir))))

(defvar bergheim/treesit-dir
  (expand-file-name "tree-sitter/" bergheim/cache-dir))

(setq treesit-extra-load-path (list bergheim/treesit-dir)
      treesit--install-language-grammar-out-dir-history
      (list bergheim/treesit-dir))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 50 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist last-file-name-handler-alist)))

(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)
;; disable this early to avoid flashing it
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)                      ; All these tools are in the menu-bar anyway
(setq default-frame-alist '((fullscreen . maximized)
                            (ns-transparent-titlebar . t)))


(provide 'early-init)
;;; early-init.el ends here
