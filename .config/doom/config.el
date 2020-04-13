;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; this sets up some stuff like name and emails etc that are not in the dotfiles
(load! "private")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Thomas Bergheim"
      user-mail-address bergheim/email)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 14))

(setq doom-font (font-spec :family "Iosevka" :size 16))
(setq doom-big-font (font-spec :family "Iosevka" :size 20))

(when IS-LINUX
  (font-put doom-font :weight 'semi-light))
(when IS-MAC
  (setq ns-use-thin-smoothing t))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(global-visual-line-mode) ;; Always wrap long lines
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)) ;; Show arrows on wrapped lines

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq confirm-kill-emacs nil)

;; Backups. Make a bunch
(setq make-backup-files t
      version-control t
      kept-new-versions 10
      kept-old-versions 2
      delete-old-versions t
      backup-by-copying t  ;; Copy all files, don't rename them.
      vc-make-backup-files t
      backup-directory-alist '((".*" . "~/.emacs.d/backup")))
;; things I missed from spacemacs
(map! :leader
      "1" 'winum-select-window-1
      "2" 'winum-select-window-2
      "3" 'winum-select-window-3
      "4" 'winum-select-window-4
      ";" 'evilnc-comment-operator ;; was (pp-eval-expression EXPRESSION)
      )

(require 'winum)
(winum-mode)

(defun bergheim-toggle-yadm ()
  "Toggle the GIT_DIR between nil and yadm. Opens magit-status when it is enabled."
  (interactive)
  ;; use a property “state”. Value is t or nil
  (if (get 'tsb-toggle-yadm 'state)
      (progn
        (message "Disabling YADM")
        (setenv "GIT_DIR" nil)
        (put 'tsb-toggle-yadm 'state nil))
    (progn
      (message (concat "Enabling YADM " (getenv "XDG_CONFIG_HOME") "/yadm/repo.git"))
      (setenv "GIT_DIR" (concat (getenv "XDG_CONFIG_HOME") "/yadm/repo.git"))
      (put 'tsb-toggle-yadm 'state t)
      (magit-status))
    ))

(map! :leader
      "agy" 'bergheim-toggle-yadm
      "agd" 'magit-diff-buffer-file
      "agl" 'magit-log-buffer-file

      "ai" (lambda () (interactive) (find-file "~/org/inbox.org"))
      "acg" 'org-clock-goto
      "aci" 'org-clock-in
      "acl" 'org-clock-in-last
      "aco" 'org-clock-out
      "acr" #'org-mru-clock-in
      "acR" #'org-mru-clock-select-recent-task
      "aL" 'org-store-link
      "al" 'org-insert-link
      )
(after! mu4e (load! "modules/mu4e"))
