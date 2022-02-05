;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(autoload #'+mu4e-colorize-str
          (doom-module-path :email 'mu4e
                            "autoload/email.el"))

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

(global-auto-revert-mode 1)

;; this sets up some stuff like name and emails etc that are not in the dotfiles
(load! "private")

;; this will draw a vertical line to indicate line length
(global-display-fill-column-indicator-mode 0)
(setq-default fill-column 100)

;; (setq doom-localleader-key "M-SPC")

;; Not sure if I want all the popups to be at the bottom
(set-popup-rule! "^\\*\\([Hh]elp\\|Apropos\\)" :ignore t)

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

;; (load! "colors")
(load! "keybindings")
;; (load! "playground")

;; (setq-default
;;  ;; configure email address and office 365 exchange server adddress for exchange web services
;;  excorporate-configuration (quote ("thomas.bergheim@neptune-software.com" . "https://outlook.office.com/EWS/Exchange.asmx"))
;;  excorporate-calendar-show-day-function 'exco-calfw-show-day
;;  ;; integrate emacs diary entries into org agenda
;;  org-agenda-include-diary t)

;; ;; activate excorporate and request user/password to start connection
;; (excorporate)
;; ;; enable the diary integration (i.e. write exchange calendar to emacs diary file -> ~/.emacs.d/diary must exist)
;; (excorporate-diary-enable)

(defun ab/agenda-update-diary ()
  "call excorporate to update the diary for today"
  (interactive)
  (exco-diary-diary-advice (calendar-current-date) (calendar-current-date) #'message "diary updated"))

;; update the diary every time the org agenda is refreshed
;; (add-hook 'org-agenda-cleanup-fancy-diary-hook 'ab/agenda-update-diary)

(after! calfw
  (defun +calendar/open-calendar ()
    "My calendar definition"
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "Green")  ; orgmode source
      (cfw:cal-create-source "Orange") ; diary source
      (cfw:ical-create-source "gcal" "https://outlook.office365.com/owa/calendar/208b4b6d7a684468b31d3ff871c8bd36@neptune-software.com/9a6efeb08d4147bfaba632c27ad8acae3535930821978552604/calendar.ics" "IndianRed")))))

(after! elfeed
  (setq elfeed-search-filter "@2-month-ago +unread"))


;; (load! "+mu4e")

(use-package! mu4e
  :config
  (require 'mu4e-headers)
  (defun bergheim/mail-search (query)
    "Perform a mu4e query"
    (interactive)
    (mu4e-headers-search-bookmark query))

  (defun bergheim/mu4e-email-today(&optional lookback)
    "Opens the inbox with unread and by default shows todays email

If LOOKBACK is specified, use that instead of 1d.
If \\[universal-argument\] if called before this, show a week back."
    (interactive)
    (let ((mu4e-headers-include-related t)
          (mu4e-headers-show-threads t)
          (mu4e-headers-sort-field :date)
          (mu4e-headers-sort-direction :ascending))

      (when (not lookback)
        (setq lookback "1d"))
      (if current-prefix-arg
        (setq lookback "1w"))

      (mu4e-headers-search-bookmark (concat "maildir:/Inbox/ AND (date:" lookback "..now)"))))

  (defun bergheim/mu4e-email-sent()
    (interactive)
    (mu4e-headers-search-bookmark "maildir:/Sent/")))

;; note: this will kill your drafts and bulk actions!
;; (map! :after mu4e-headers
;;       :map mu4e-headers-mode-map
;;       :n "q" 'mu4e-quit)

(after! mu4e (load! "+mu4e"))
(after! org (load! "+org"))

;; (defun maybe-use-prettier ()
;;   "Enable prettier-js-mode if .prettierrc or .prettierrc.json or .prettierrc.js file is located."
;;   (if (or (locate-dominating-file default-directory ".prettierrc")
;;        (or (locate-dominating-file default-directory ".prettierrc.json"))
;;        (or (locate-dominating-file default-directory ".prettierrc.js")))
;;         (prettier-js-mode +1)))
;;
;; (use-package! prettier-js)
;; (after! prettier-js
;;   :config
;;   (setq! prettier-js-args '(
;;                              "--trailing-comma" "all"
;;                              "--bracket-spacing" "true"
;;                              )))
;; (add-hook! '(
;;               js2-mode-hook
;;               rjsx-mode-hook
;;               web-mode-hook
;;               typescript-mode-hook
;;               typescript-tsx-mode-hook) 'maybe-use-prettier)

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
