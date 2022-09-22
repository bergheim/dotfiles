;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(after! doom-modeline
  (setq doom-modeline-display-default-persp-name t
        doom-modeline-persp-name t ;; TODO make it easier to see
        doom-modeline-mu4e t ;; TODO where is this exactly?
        doom-modeline-buffer-encoding nil))

;; default to english.
;; TODO: add ispell-local-dictionary
(setq ispell-dictionary "en")

;; default is 1 second which is a bit slow
(setq which-key-idle-delay 0.3)
;; (which-key-mode 1)

;; helps avoid file sync issues
(global-auto-revert-mode 1)

;; this sets up some stuff like name and emails etc that are not in the dotfiles
(load! "private")

;; this will draw a vertical line to indicate line length
(global-display-fill-column-indicator-mode 0)
(setq-default fill-column 100)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Thomas Bergheim"
      user-mail-address bergheim/email)

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 26)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 32)
      doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font")
      ;; this messes with all-the-icons, leave it
      ;; doom-unicode-font (font-spec :family "Input Mono")
      doom-serif-font (font-spec :family "Noto Serif Myanmar"))

(when IS-MAC
  ;; TODO is this relevant anymore?
  (setq ns-use-thin-smoothing t)
  (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
        doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 22)
        doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font")
        ;; this messes with all-the-icons, leave it
        ;; doom-unicode-font (font-spec :family "Input Mono")
        doom-serif-font (font-spec :family "Noto Serif Myanmar"))
  (add-hook 'window-setup-hook #'toggle-frame-maximized))

(setq doom-theme 'doom-material)

(setq-default line-spacing 0.2)

(use-package! mixed-pitch
  :hook ((org-mode . mixed-pitch-mode)
         (markdown-mode . mixed-pitch-mode)))
  ;; :config
  ;; (setq mixed-pitch-set-heigth t)
  ;; (set-face-attribute 'variable-pitch nil :height 1.3))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; use visible buffer, not just the current line
(setq evil-snipe-scope 'visible)

;; pop up dap-hydra on first stop. i don't like this because it takes over your keys
;; (use-package! dap-mode
;;   ;; :custom (dap-auto-configure-features '(sessions locals tooltip))
;;   :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra))))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
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

(defun bergheim/save-some-buffers ()
  (save-some-buffers t ))

(if (version< emacs-version "27")
    (add-hook! 'focus-out-hook 'bergheim/save-some-buffers)
  (setq after-focus-change-function 'bergheim/save-some-buffers))

;; decrease the timeout before jumping around the buffer
(setq avy-timeout-seconds 0.3)

;; If tooltips turned on, make tips appear promptly
(setq tooltip-delay 0.1)  ; default is 0.7 second)

(after! ivy
  ;; include recent files and bookmarks
  (setq ivy-use-virtual-buffers t
        ;; show index/total in the minibuf prompt
        ivy-count-format "(%d/%d) "
        ;; show the abbreviated path for files, not just project relative
        ivy-rich-path-style 'abbrev))

;; this is a hack that allows us to use lookups to other windows
;; https://github.com/hlissner/doom-emacs/issues/3397
(dolist (fn '(definition references))
  (fset (intern (format "+lookup/%s-other-window" fn))
        (lambda (identifier &optional arg)
          "TODO"
          (interactive (list (doom-thing-at-point-or-region)
                             current-prefix-arg))
          (let ((pt (point)))
            (switch-to-buffer-other-window (current-buffer))
            (goto-char pt)
            (funcall (intern (format "+lookup/%s" fn)) identifier arg)))))

(after! tramp
  (add-to-list 'tramp-methods
               '("yadm"
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 (tramp-login-env (("SHELL") ("/bin/sh")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c")))))

(load! "colors")
(load! "keybindings")


(after! elfeed
  (setq elfeed-search-filter "@2-month-ago +unread"))

(use-package! dired
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (setq dired-dwim-target t ;; guess default target directory
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-kill-when-opening-new-dired-buffer t))

(use-package! mu4e
  :config
  (require 'mu4e-headers)
  (defun bergheim/mail-search (query)
    "Perform a mu4e query"
    (interactive)
    (=mu4e)
    (mu4e-search-bookmark query))

  (defun bergheim/mu4e-email-today(&optional lookback)
    "Opens the inbox with unread and by default shows todays email

If LOOKBACK is specified, use that instead of 1d.
If \\[universal-argument] if called before this, show a week back."
    (interactive)
    (let ((mu4e-headers-include-related t)
          (mu4e-search-threads t)
          (mu4e-headers-sort-field :date)
          (mu4e-headers-sort-direction :ascending))

      (when (not lookback)
        (setq lookback "2d"))
      (if current-prefix-arg
          (setq lookback "1m"))

      (=mu4e)
      (mu4e-search-bookmark (concat "maildir:/Inbox/ AND (date:" lookback "..now)"))))

  (defun bergheim/mu4e-email-sent()
    (interactive)
    (=mu4e)
    (mu4e-search-bookmark "maildir:/Sent/")))

(after! mu4e (load! "+mu4e"))
(after! org (load! "+org"))

(use-package! orgit
  ;; Automatically copy orgit link to last commit after commit
  :hook (git-commit-post-finish . orgit-store-after-commit)
  :config
  (defun orgit-store-after-commit ()
    "Store orgit-link for latest commit after commit message editor is finished."
    (let* ((repo (abbreviate-file-name default-directory))
           (rev (magit-git-string "rev-parse" "HEAD"))
           (link (format "orgit-rev:%s::%s" repo rev))
           (summary (substring-no-properties (magit-format-rev-summary rev)))
           (desc (format "%s (%s)" summary repo)))
      (push (list link desc) org-stored-links))))

(use-package! affe
  :config
    ;; (setq affe-find-command "fd -HI -t f")
    (setq affe-find-command "rg --color=never --hidden --files"))

(defun bergheim/toggle-maximize ()
  (interactive)
  (if (get 'bergheim/toggle-maximize 'enabled)
      (progn
        (put 'bergheim/toggle-maximize 'enabled nil)
        (winner-undo))
      (progn
        (put 'bergheim/toggle-maximize 'enabled t)
        (doom/window-maximize-buffer))))

(use-package! auto-dim-other-buffers
  :hook (after-init . auto-dim-other-buffers-mode))

(defun bergheim/open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:ical-create-source "outlook" bergheim/calendar/neptune/default "Orange")
    (cfw:org-create-file-source "personal" bergheim/calendar/nextcloud/local "DarkGreen"))))
