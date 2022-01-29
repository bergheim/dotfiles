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

;; this sets up some stuff like name and emails etc that are not in the dotfiles
(load! "private")

;; this will draw a vertical line to indicate line length
(global-display-fill-column-indicator-mode 0)
(setq-default fill-column 100)

;; TODO is this relevant anymore?
(when IS-MAC
  (setq ns-use-thin-smoothing t))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Thomas Bergheim"
      user-mail-address bergheim/email)

(setq doom-font (font-spec :family "JetBrainsMono NerdFont" :size 28)
      doom-big-font (font-spec :family "JetBrainsMono NerdFont" :size 36)
      doom-variable-pitch-font (font-spec :family "iMWritingDuoS Nerd Font")
      ;; this messes with all-the-icons, leave it
      ;; doom-unicode-font (font-spec :family "Input Mono")
      doom-serif-font (font-spec :family "Literation Serif NerdFont"))

(setq doom-theme 'doom-material)

(setq-default line-spacing 0.2)


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

;; what the fuck is winum
;; (require 'winum)
;; (winum-mode)

(load! "colors")
(load! "keybindings")


(after! elfeed
  (setq elfeed-search-filter "@2-month-ago +unread"))

(after! mu4e (load! "+mu4e"))
(after! org (load! "+org"))


;; this breaks org-msg at the moment
;; (setq +format-on-save-enabled-modes
;;       '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
;;             sql-mode         ; sqlformat is currently broken
;;             tex-mode         ; latexindent is broken
;;             latex-mode
;;             org-msg-edit-mode  ; this is just broken
;;             org-msg-mode
;;             mu4e-compose-mode
;;             ))
(add-hook 'python-mode-hook #'format-all-mode)
(add-hook 'js-mode-hook #'format-all-mode)
(add-hook 'typescript-mode-hook #'format-all-mode)
;; (add-hook 'org-mode-hook #'format-all-mode)
(add-hook 'markdown-mode-hook #'format-all-mode)
(add-hook 'css-mode-hook #'format-all-mode)
(add-hook 'html-mode-hook #'format-all-mode)
(add-hook 'yaml-mode-hook #'format-all-mode)
(add-hook 'rust-mode-hook #'format-all-mode)

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
