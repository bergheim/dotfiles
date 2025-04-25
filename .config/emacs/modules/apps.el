;; -*- lexical-binding: t; -*-

(use-package password-store)

(use-package em-hist
  :ensure nil
  :config
  (setq
   eshell-hist-ignoredups t
   ;; Set the history file.
   ;; eshell-history-file-name "~/.bash_history"
   ;; If nil, use HISTSIZE as the history size.
   eshell-history-size nil))

(use-package eshell
  :ensure nil
  :general
  (:keymaps 'eshell-mode-map
   :states 'insert
   "C-r" #'consult-history
   "C-f" #'consult-dir
   "C-t" #'eshell/find-file-with-consult
   ;; "C-d" . eshell/z
   "C-k" #'eshell-previous-matching-input-from-input
   "C-j" #'eshell-next-matching-input-from-input)

  (bergheim/global-menu-keys
    "as" '(eshell :which-key "eshell"))
  :config
  (setq eshell-destroy-buffer-when-process-dies t)

  (defun bergheim/eshell-git-info ()
    "Return git branch and status."
    (when (eq (call-process "git" nil nil nil "rev-parse" "--is-inside-work-tree") 0)
      (let* ((branch (string-trim
                      (shell-command-to-string
                       "git symbolic-ref --short HEAD 2>/dev/null || echo 'no commits'")))
             (dirty (not (string= "" (string-trim (shell-command-to-string "git status --porcelain")))))
             (dirty-info (if dirty
                             (propertize " ✎" 'face 'error)
                           (propertize " ✔" 'face 'success))))
        (concat (propertize "⎇ " 'face 'success)
                (propertize branch 'face 'warning)
                dirty-info))))

  (defun bergheim/eshell-prompt ()
    "Simple but kewl Eshell prompt with git info."
    (let ((dir (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-ls-directory))
          (git-info (or (bergheim/eshell-git-info) ""))
          (prompt (propertize (if (= (user-uid) 0) "#" "λ") 'face 'warning)))
      (concat dir " " git-info " " prompt " ")))

  (setq eshell-prompt-function 'bergheim/eshell-prompt)

  (defun eshell-get-old-input ()
    "Retrieve the current input from the Eshell prompt in the buffer."
    (buffer-substring-no-properties
     (save-excursion (eshell-bol) (point))
     (point)))

  (defun eshell/vi (filename)
    "Open FILENAME in another buffer within Eshell."
    (find-file-other-window filename))

  (defun eshell/mycat (&rest args)
    "Open files in other buffer"
    (if (null args)
        (user-error "No file specified")
      (dolist (file args)
        (find-file-read-only-other-window file))))

  (defun eshell/gst (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))   ;; The echo command suppresses output

  (defun eshell/find-file-insert-path ()
    "Use `fd` to find files and insert the selected path into the eshell prompt."
    (interactive)
    (let* ((query (read-string "Find file (query): "))
           (results (split-string
                     (shell-command-to-string (format "fd --type f %s" query))
                     "\n" t))
           (selected (completing-read "Select file: " results nil t)))
      (when (and selected (not (string-empty-p selected)))
        (insert selected))))

  (defun eshell/find-file-with-affe ()
    "Search for files using affe based on the current Eshell input and insert the selected file path into Eshell."
    (interactive)
    (let* ((input (eshell-get-old-input))
           ;; Extract the command and arguments from the input
           (args (split-string input "[ \t\n]+" t))
           (command (car args))
           ;; Use the second argument as the directory to search from, default to current
           (raw-dir (or (nth 1 args) "."))
           (base-dir (expand-file-name raw-dir default-directory))
           (valid-dir (if (file-directory-p base-dir) base-dir default-directory))
           ;; Customize affe's action to insert path in Eshell
           (affe-filter-func
            (lambda (path)
              (eshell-bol)
              (kill-line)
              (insert (concat command " " (shell-quote-argument path))))))
      (if (not valid-dir)
          (user-error "Invalid path (%s)" base-dir)
        (affe-find valid-dir))))

  (defun eshell/find-file-with-consult ()
    "Find files from your current dir args"
    (interactive)
    (let* ((input (eshell-get-old-input))
           ;; Extract arguments from input
           (args (split-string input "[ \t\n]+" t))
           (command (or (car args) ""))
           ;; Always expand the filepath no matter what
           (second-arg (or (nth 1 args) "."))
           (base-dir (expand-file-name second-arg default-directory))
           ;; FIXME: . or default-directory?
           (original-dir (or second-arg "."))
           (search-type (if (string-equal command "cd")
                            "d"  ; Search for directories
                          "f")) ; Search for files
           (valid-dir (if (file-directory-p base-dir) base-dir default-directory))
           (selected (consult--read
                      (split-string (shell-command-to-string
                                     (format "fd --type %s --hidden . %s"
                                             search-type
                                             (shell-quote-argument valid-dir)))
                                    "\n" t)
                      :prompt (format "Select %s:"
                                      (if (string-equal command "cd")
                                          "directory"
                                        "file"))
                      :sort nil)))
      (when (and selected (not (string-empty-p selected)))
        (eshell-bol)
        (kill-line)
        (insert (concat command " " (shell-quote-argument selected))))))

  ;; nicked from the consult-dir wiki
  (defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell."
    (interactive)
    (let ((eshell-dirs (delete-dups (mapcar 'abbreviate-file-name
                                            (ring-elements eshell-last-dir-ring)))))
      (cond
       ((and (not regexp) (featurep 'consult-dir))
        (let* ((consult-dir--source-eshell `(:name "Eshell"
                                             :narrow ?e
                                             :category file
                                             :face consult-file
                                             :items ,eshell-dirs))
               (consult-dir-sources (cons consult-dir--source-eshell consult-dir-sources)))
          (eshell/cd (substring-no-properties (consult-dir--pick "Switch directory: ")))))
       (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                       (completing-read "cd: " eshell-dirs)))))))

  (defun bergheim/open-dired-and-insert-file ()
    "Open Dired for the current input directory and insert selected file back into Eshell."
    (interactive)
    (let* ((current-input (eshell-get-old-input))
           (parts (split-string current-input " "))
           (command (car parts))
           (path (mapconcat 'identity (cdr parts) " "))
           (directory (or (file-name-directory (expand-file-name path)) default-directory))
           (filename (progn
                       (dired directory)
                       (let ((selected-file (dired-get-file-for-visit)))
                         (while (not selected-file)
                           (dired-next-line 1)
                           (setq selected-file (dired-get-file-for-visit)))
                         (file-relative-name selected-file directory)))))
      (when filename
        (kill-region (point-at-bol) (point-at-eol))
        (insert (concat command " " directory filename)))))

  (defun bergheim/dired-return-path ()
    "Exit Dired and return the path of the file or directory at point."
    (interactive)
    (let* ((path (dired-get-file-for-visit))
           (relative-path (file-relative-name path bergheim/eshell-complete-from-dir)))
      (dirvish-quit)
      (insert relative-path)))

  (defun bergheim/point-is-directory-p ()
    "Check if the word at point is a directory path, or default-directory if not."
    (let ((word (thing-at-point 'filename t)))
      (if (or (not word) (string-empty-p word))
          (file-directory-p default-directory)
        (file-directory-p word))))

  (defvar bergheim/last-completion-point nil
    "Stores the last point of completion.")

  (defvar bergheim/eshell-complete-from-dir nil
    "Stores the directory where we started the completion.")

  (defun bergheim/extract-path ()
    "Extract the path or return nil if not found."
    (interactive) ;; TODO remove this
    (let* ((current-input (eshell-get-old-input))
           (parts (split-string current-input " "))
           (command (car parts))
           (path (mapconcat 'identity (cdr parts) " "))
           (directory (or (expand-file-name path) default-directory)))
      (when (bergheim/point-is-directory-p)
        (setq bergheim/eshell-complete-from-dir directory)
        directory)))

  (defun bergheim/completion-at-point-or-dired ()
    "Trigger `completion-at-point` or `dired` if called twice without moving point.
Open `dired` in the resolved directory of the current command."
    (interactive)
    (if (and (eq major-mode 'eshell-mode)
             (eq last-command this-command)
             (eq (point) bergheim/last-completion-point))
        (let ((path (bergheim/extract-path)))
          (when (and path (file-directory-p path))
            (setq bergheim/last-completion-point nil)
            (dirvish (or path default-directory))))
      (setq bergheim/last-completion-point (point))
      (completion-at-point)))

  (defun bergheim/exit-eshell-from-insert-mode ()
    "Exit Eshell if in `evil-insert' state."
    (interactive)
    (when (eq evil-state 'insert)
      (eshell-life-is-too-much)))

  (defun bergheim/eshell-tramp-cd-advice (orig-func &rest args)
    "Make `cd` with no args go to remote home when in a TRAMP connection."
    (if (and (file-remote-p default-directory)
             (or (null args) (= (length args) 0)))
        ;; If we're remote and no args, go to remote home
        (let ((remote-prefix (file-remote-p default-directory)))
          (funcall orig-func (concat remote-prefix "~")))
      ;; Otherwise use normal behavior
      (apply orig-func args)))

  (advice-add 'eshell/cd :around #'bergheim/eshell-tramp-cd-advice)

  (add-hook 'eshell-first-time-mode-hook
            (lambda ()
              (evil-define-key 'insert eshell-mode-map (kbd "TAB") 'bergheim/completion-at-point-or-dired)
              (evil-define-key 'insert eshell-mode-map (kbd "C-d") 'bergheim/exit-eshell-from-insert-mode)

              (eshell/alias "cat" "eshell/cat $*")

              (evil-define-key 'normal eshell-mode-map
                ;; this binding is pretty non-standard, but who uses A in the shell..
                (kbd "A")
                (lambda ()
                  (interactive)
                  (end-of-buffer)
                  (evil-append-line 1)))))


  (add-hook 'eshell-mode-hook (lambda ()
                                (eshell/alias "ll" "ls -lh $*")
                                (eshell/alias "l" "ll")
                                (eshell/alias "gs" "magit-status")
                                (eshell/alias "gd" "magit-diff-unstaged")
                                (eshell/alias "gds" "magit-diff-staged")


                                (eshell/alias "cat" "eshell/mycat $1")

                                (define-key eshell-mode-map (kbd "C-c f") 'eshell/find-file-with-consult)
                                (define-key eshell-mode-map (kbd "C-c t") 'eshell/find-file-with-consult)
                                (define-key eshell-mode-map (kbd "C-c d") 'eshell/affe-find))))

(use-package eat
  :commands eat
  :custom
  (eat-kill-buffer-on-exit t)
  :general
  (bergheim/global-menu-keys
    "aS" '(eat :which-key "Eat"))
  :hook
  (eshell-first-time-mode . eat-eshell-mode))

(use-package vterm
  :ensure t
  :commands vterm
  :general
  (bergheim/global-menu-keys
    "av" '(vterm :which-key "vterm"))
  :config
  (setq vterm-max-scrollback 10000)
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local evil-insert-state-cursor 'box)
              (evil-insert-state))))

(use-package shr
  :ensure nil
  :custom
  ;; (toggle-truncate-lines 1)
  (shr-max-width 120))

(use-package treemacs
  :defer t
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package proced
  :ensure nil
  :commands proced
  :general
  (bergheim/global-menu-keys
    "ap" '(proced :which-key "Proced"))
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 1)
  (proced-goal-attribute nil) ;; don't move cursor to args when navigating
  (proced-show-remote-processes t) ;; enable TRAMP support
  (proced-enable-color-flag t)
  (proced-format 'custom)
  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid ppid sess tree pcpu pmem rss start time state (args comm))))

(use-package denote
  :ensure t
  :custom
  (denote-known-keywords '("emacs" "journal"))
  (denote-directory (expand-file-name "denote" org-directory))
  (denote-date-prompt-use-org-read-date t)
  (denote-backlinks-show-context t)
  :config
  (defun my-denote-tmr ()
    (tmr "5" "Write focused now.."))
  (add-hook 'denote-journal-hook 'my-denote-tmr)
  ;; (add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe)
  (denote-rename-buffer-mode 1)

  (setq denote-templates
        `((default . "")
          (person . ,(concat "* Contact Info\n"
                             "- Name: \n"
                             "- Website: \n"
                             "- Github: \n"
                             "- Email: \n"
                             "\n"
                             "* About\n"
                             "* Notes and Quotes\n"))))

  (defun bergheim/denote-new-journal-entry ()
    "Create a new journal entry and enter writer mode"
    (interactive)
    (siren-tab-bar-switch-to-or-create-tab "journal")
    (let ((entry-today (denote-journal--entry-today)))
      (if entry-today
          (denote-open-or-create (car entry-today))
        (denote-journal-new-entry)))
    (bergheim/write-mode t)
    (goto-char (point-max))
    (delete-trailing-whitespace)
    (insert "\n* [" (format-time-string "%H:%M") "] ")
    (evil-insert 0))

  (defun bergheim/denote-last-journal-entry ()
    "Open the newest entry"
    (interactive)
    (let* ((all-entries (directory-files denote-journal-directory t "^[^.]"))
           (files (seq-filter #'file-regular-p all-entries)))
      (when files
        (siren-tab-bar-switch-to-or-create-tab "journal")
        (find-file (expand-file-name
                    (car (last (sort files 'string<))) denote-journal-directory))
        (bergheim/write-mode t))))

  :general
  (bergheim/global-menu-keys
    "n" '(:ignore t :which-key "Denote")
    "na" '(denote-add-links :which-key "Add all inks")
    "nb" '(denote-find-backlink :which-key "Show backlinks")
    "nB" '(denote-backlinks :which-key "Show backlinks")
    "nd" '(denote :which-key "New note")
    "nf" '(denote-open-or-create :which-key "Find")
    ;;"ndj" '(denote-journal-extras-new-or-existing-entry :which-key "Journal")
    "ne" '(denote-org-extract-org-subtree :which-key "Extract from node")
    "nh" '(denote-org-link-to-heading :which-key "Link to heading")
    "ni" '(:ignore t :which-key "Insert")
    "nib" '(denote-org-dblock-insert-backlinks :which-key "backlinks")
    "nif" '(denote-org-dblock-insert-files :which-key "files")
    "nil" '(denote-org-dblock-insert-links :which-key "links")
    "nj" '(:ignore t :which-key "Journal")
    "njj" '(bergheim/denote-new-journal-entry :which-key "New journal")
    "njl" '(bergheim/denote-last-journal-entry :which-key "Last journal entry")
    "njb" '((lambda () (interactive) (find-file denote-journal-directory)) :which-key "Browse journals")
    "nL" '(denote-find-link :which-key "Show links")
    "nl" '(denote-link-or-create :which-key "Link")
    "nn" '(denote-open-or-create :which-key "Open/create")
    "nr" '(denote-rename-file-using-front-matter :which-key "Rename")
    "nr" '(denote-rename-file :which-key "Rename")
    "nR" '(denote-rename-file-signature :which-key "Rename signature")
    "ns" '(consult-notes-search-in-all-notes :which-key "Search")
    "nS" '(denote-grep :which-key "Grep")))

(use-package denote-journal
  :after denote
  :demand)

(use-package denote-org
  :after denote
  :demand)

(use-package denote-journal-capture
  :after denote-journal
  :demand)

(use-package denote-menu :after denote)

(use-package consult-denote
  :after denote)

;; TODO: see https://lucidmanager.org/productivity/denote-explore/
;; (use-package denote-explore
;;   :after denote)

;; pastebin stuff
(use-package 0x0
  :after general

  :general
  (bergheim/global-menu-keys
    "ys" '(:ignore t :which-key "Share")
    "yss" '(0x0-dwim :which-key "Dwim")
    "ysp" '(0x0-popup :which-key "Text")
    "ysf" '(0x0-upload-file :which-key "File")))

(use-package elfeed
  :after general
  :commands elfeed
  :init
  (setq elfeed-db-directory (bergheim/get-and-ensure-data-dir "elfeed/db/")
        elfeed-enclosure-default-dir (bergheim/get-and-ensure-data-dir "elfeed/enclosures/"))

  :hook
  (elfeed-search . (lambda () (setq-local display-line-numbers nil)))
  (elfeed-show . (lambda () (setq-local display-line-numbers nil)))

  :general
  (:keymaps 'elfeed-search-mode-map
   :states 'normal
   "d" #'bergheim/elfeed-by-domain
   "C" #'bergheim/elfeed-by-domain)

  :config
  (defun bergheim/elfeed-by-domain ()
    "Filter Elfeed search results to show only entries from the domain of the currently selected feed."
    (interactive)
    (let* ((entry (or (elfeed-search-selected :single)
                      (user-error "No entry selected")))
           (feed (elfeed-entry-feed entry))
           (feed-url (elfeed-feed-url feed))
           (url-host (when feed-url
                       (url-host (url-generic-parse-url feed-url)))))
      (unless url-host
        (user-error "Unable to determine feed's domain"))
      (elfeed-search-set-filter (format "=%s" url-host))))

  (defhydra bergheim/hydra-elfeed (:foreign-keys run)
    "filter"
    ("a" (elfeed-search-set-filter "@6-months-ago")            "All")
    ("d" (elfeed-search-set-filter "@6-months-ago +dev")       "Development")
    ("e" (elfeed-search-set-filter "@6-months-ago +emacs")     "Emacs")
    ("*" (elfeed-search-set-filter "@6-months-ago +star")      "Starred")
    ("r" (elfeed-search-set-filter "@6-months-ago -unread")      "Read")
    ("u" (elfeed-search-set-filter "@6-months-ago +unread")      "Unread")
    ;; ("m" (bergheim/elfeed-toggle-starred)                                    "Star")
    ("m" (lambda () (interactive) (elfeed-search-toggle-all 'star))                                    "Star")
    ("t" (elfeed-search-set-filter "@1-day-ago")               "Today")
    ("q" nil                                                   "quit" :color blue))

  ;; (transient-define-prefix bergheim/elfeed-transient ()
  ;;   "Elfeed Transient"
  ;;   ["Elfeed Filters"
  ;;    ("e" "emacs"       (lambda () (interactive) (elfeed-search-set-filter "@6-months-ago +emacs")) :transient t)
  ;;    ("d" "dev"   (lambda () (interactive) (elfeed-search-set-filter "@6-months-ago +dev")))
  ;;    ("*" "Starred"     (lambda () (interactive) (elfeed-search-set-filter "@6-months-ago +star")))
  ;;    ;; ("M" "Mark"        elfeed-toggle-star)
  ;;    ("a" "All"         (lambda () (interactive) (elfeed-search-set-filter "@6-months-ago")))
  ;;    ("t" "Today"       (lambda () (interactive) (elfeed-search-set-filter "@1-day-ago")))
  ;;    ["General"
  ;;     ;; ("Q" "Quit Elfeed" bjm/elfeed-save-db-and-bury)
  ;;     ;; ("q" "quit" nil)
  ;;     ]]
  ;;   )
  )

;; from https://github.com/skeeto/elfeed/issues/466#issuecomment-1275327427
(define-advice elfeed-search--header (:around (oldfun &rest args))
  (if elfeed-db
      (apply oldfun args)
    "No database loaded yet"))

(use-package elfeed-org
  :after elfeed
  :demand
  :init
  (setq rmh-elfeed-org-files (list (expand-file-name "elfeed/elfeed.org" org-directory)))
  :config
  (elfeed-org))

(use-package elfeed-protocol
  :after elfeed
  :demand
  :general
  (:keymaps 'elfeed-search-mode-map
   :states 'normal
   "gr" #'bergheim/elfeed-refresh)
  :init
  (defun bergheim/elfeed-refresh ()
    (interactive)
    (mark-whole-buffer)
    (cl-loop for entry in (elfeed-search-selected)
             do (elfeed-untag-1 entry 'unread))
    (elfeed-search-update--force)
    (elfeed-protocol-fever-reinit "https://tsb@thomasbergheim.com/rss"))
  :config
  (setq elfeed-use-curl t)
  ;; nextcloud
  ;; (setq elfeed-protocol-feeds '(("owncloud+https://tsb@cloud.thomasbergheim.com"
  ;;                                :password (password-store-get "websites/cloud.thomasbergheim.com/tsb"))))
  ;; (setq elfeed-protocol-enabled-protocols '(owncloud))
  ;; (setq elfeed-protocol-owncloud-star-tag 'star)

  ;; miniflux / fever
  (setq elfeed-protocol-fever-update-unread-only nil)
  (setq elfeed-protocol-fever-fetch-category-as-tag nil)
  (setq elfeed-protocol-feeds '(("fever+https://tsb@thomasbergheim.com/rss"
                                 :api-url "https://thomasbergheim.com/rss/fever/"
                                 :password (password-store-get "mycloud/miniflux/fever"))))
  (setq elfeed-protocol-enabled-protocols '(fever))

  ;; (defvar elfeed-protocol-orig-feeds nil
  ;;   "Store original content of `elfeed-feeds'.")
  ;; (defadvice elfeed (after configure-elfeed-feeds activate)
  ;;   "Make elfeed-org autotags rules works with elfeed-protocol."
  ;;   (setq
  ;;    elfeed-protocol-orig-feeds elfeed-protocol-feeds
  ;;    elfeed-protocol-feeds (list
  ;;                           (list "fever+https://tsb@thomasbergheim.com/rss"
  ;;                                 :api-url "https://thomasbergheim.com/rss/fever/"
  ;;                                 :password (password-store-get "mycloud/miniflux/fever")
  ;;                                 :autotags  elfeed-protocol-orig-feeds)))
  ;;   (elfeed-update))


  ;; enable elfeed-protocol
  (elfeed-protocol-enable))

;; (use-package elfeed-goodies
;;   :after elfeed
;;   :demand
;;   :config
;;   (elfeed-goodies/setup))

(use-package smudge
  :custom
  (smudge-oauth2-client-secret bergheim/spotify/client-secret)
  (smudge-oauth2-client-id bergheim/spotify/client-id)
  (smudge-player-use-transient-map t)
  (smudge-transport 'connect)
  (smudge-player-status-refresh-interval 0)
  (smudge-api-locale "nb_NO")
  (smudge-api-country "NO")
  (smudge-status-location nil)
  :config
  ;; A hydra for controlling spotify.
  (defhydra hydra-spotify (:hint nil)
    "
^Search^                  ^Control^               ^Manage^
^^^^^^^^-----------------------------------------------------------------
_t_: Track               _SPC_: Play/Pause        _+_: Volume up
_m_: My Playlists        _n_  : Next Track        _-_: Volume down
_f_: Featured Playlists  _p_  : Previous Track    _x_: Mute
_u_: User Playlists      _r_  : Repeat            _d_: Device
^^                       _s_  : Shuffle           _q_: Quit
"
    ("t" smudge-track-search :exit t)
    ("m" smudge-my-playlists :exit t)
    ("f" smudge-featured-playlists :exit t)
    ("u" smudge-user-playlists :exit t)
    ("SPC" smudge-controller-toggle-play :exit nil)
    ("n" smudge-controller-next-track :exit nil)
    ("p" smudge-controller-previous-track :exit nil)
    ("r" smudge-controller-toggle-repeat :exit nil)
    ("s" smudge-controller-toggle-shuffle :exit nil)
    ("+" smudge-controller-volume-up :exit nil)
    ("-" smudge-controller-volume-down :exit nil)
    ("x" smudge-controller-volume-mute-unmute :exit nil)
    ("d" smudge-select-device :exit nil)
    ("q" quit-window "quit" :color blue)))

;; nicked from https://codeberg.org/alternateved/dotfiles/src/branch/main/emacs/.config/emacs/init.el
(use-package erc
  :ensure nil
  :after consult
  :autoload erc-buffer-list
  :init
  (setq erc-hide-list
        '("JOIN" "PART" "QUIT" "NICK" "MODE"  ; standard events
          "324"                               ; channel mode
          "329"                               ; channel creation time
          "332" "333"                         ; topic and topic setter
          "353" "366"                         ; names list and end of names
          "477"                               ; channel not available
          "305" "306"                         ; away status
          "328"                               ; channel URL
          "250" "251" "252" "253" "254" "255" ; server stats
          "265" "266"                         ; local/global users
          "401" "404" "405" "406"             ; various errors
          "471" "473" "474" "475"             ; channel errors
          "476")                              ; bad channel mask
        erc-track-exclude-types erc-hide-list)
  :hook
  ;; (erc-mode . erc-spelling-mode)
  (erc-mode . erc-notifications-mode)
  (erc-mode . (lambda ()
                (setq-local orderless-matching-styles '(orderless-literal-prefix)
                            confirm-kill-processes nil
                            corfu-auto-prefix 3)
                (if (featurep 'jinx)
                    (jinx-mode 1))
                (erc-fill-wrap-mode 1)
                (setq-local completion-at-point-functions
                            '(cape-emoji
                              erc-complete-word-at-point))
                (display-line-numbers-mode 0)))
  (erc-status-sidebar-mode . (lambda () (display-line-numbers-mode 0)))
  (speedbar-mode . (lambda () (display-line-numbers-mode 0)))
  :custom
  (erc-autojoin-channels-alist bergheim/irc-channels)

  (erc-autojoin-timing 'ident)
  (erc-autojoin-delay 5)
  (erc-fill-static-center 12)
  (erc-fool-highlight-type 'all)

  ;;;; Logging
  (erc-log-channels-directory (expand-file-name "logs/erc/channels/" bergheim/home-dir))
  (erc-log-write-after-insert t)
  (erc-log-write-after-send t)
  (erc-save-buffer-on-part t)
  (erc-save-queries-on-quit t)

  (erc-fools bergheim/irc-fools)
  (erc-header-line-format nil)
  (erc-log-insert-log-on-open t)
  (erc-insert-timestamp-function 'erc-insert-timestamp-left)
  (erc-timestamp-only-if-changed-flag nil)
  (erc-interpret-mirc-color t)
  (erc-join-buffer 'buffer)
  ;; (erc-nicks-contrast-range '(1 . 100))
  (erc-nick "tsb")
  (erc-prompt (format ">"))
  (erc-prompt-for-password nil)
  (erc-prompt "⟩")

  :general
  (bergheim/global-menu-keys
    "ai" '(:ignore t :which-key "irc (erc)")
    "aii" '(bergheim/erc-connect :which-key "init")
    "aiq" '(erc-quit-server :which-key "quit")
    "aib" '(bergheim/consult-erc-buffer :which-key "buffers"))
  (bergheim/localleader-keys
    :states '(normal visual)
    :keymaps 'erc-mode-map
    ;; TODO erc-imenu-mode
    "b" '(bergheim/consult-erc-buffer :which-key "channels")
    "c" 'erc-bufbar-mode
    "s" '(bergheim/erc-swoop-nick :which-key "swoop")
    "d" '(bergheim/erc-open-or-capture-user-note-denote :which-key "denote")
    "n" 'erc-nickbar-mode)
  (:states 'normal
   :keymaps 'erc-mode-map
   "A" (lambda ()
         (interactive)
         (goto-char (point-max))
         (evil-append 0)))
  (:states 'insert
   :keymaps 'erc-mode-map
   "M-h" (lambda ()
           (interactive)
           (evil-normal-state)
           (evil-window-left 1))
   "M-l" (lambda ()
           (interactive)
           (evil-normal-state)
           (evil-window-right 1))
   "C-k" 'erc-previous-command
   "C-j" 'erc-next-command
   "M-h" 'evil-window-left
   "M-l" 'evil-window-right
   "C-u" #'evil-change-whole-line)
  :config
  (erc-log-mode 1)
  (if (< emacs-major-version 30)
      (use-package erc-hl-nicks)
    (add-to-list 'erc-modules 'nicks))

  (defun bergheim/erc-buffer-connected-p (buffer)
    "Check if ERC BUFFER is connected."
    (with-current-buffer buffer
      (and (derived-mode-p 'erc-mode)
           (erc-server-process-alive)
           erc-server-connected)))

  (defun bergheim/erc-connected-p ()
    "Check if any ERC buffer is connected."
    (seq-some #'bergheim/erc-buffer-connected-p (erc-buffer-list)))

  (defun bergheim/erc-connect ()
    "Open ERC in a dedicated frame and show specified channels."
    (interactive)
    (unless (bergheim/erc-connected-p)
      (erc-tls :server bergheim/irc-server :port 7667 :user bergheim/irc-username))
    ;; create or switch to erc frame
    (let* ((frame-name "erc")
           (target-frame
            (or (car (seq-filter
                      (lambda (frame)
                        (and (frame-live-p frame)
                             (string= frame-name (frame-parameter frame 'name))))
                      (frame-list)))
                (make-frame `((name . ,frame-name))))))
      (select-frame-set-input-focus target-frame)
      (delete-other-windows) ;; Ensure any existing splits are removed
      (split-window-right)
      ;; LOL
      (run-with-timer
       3 nil
       (lambda ()
           (when (get-buffer bergheim/irc-channel-a)
             (switch-to-buffer bergheim/irc-channel-a))
           (other-window 1)
           (when (get-buffer bergheim/irc-channel-b)
             (switch-to-buffer bergheim/irc-channel-b))))))


  (defun bergheim/erc-setup-completions ()
    "Set up completions for ERC"
    (setq-local completion-at-point-functions (list #'cape-emoji))
    (corfu-mode 1))

  (autoload 'erc-buffer-list "erc")
  (defvar erc-buffer-source
    `(:name     "ERC"
      :hidden   t
      :narrow   ?e
      :category buffer
      :state    ,#'consult--buffer-state
      :items    ,(lambda () (mapcar #'buffer-name (erc-buffer-list)))))

  (add-to-list 'consult-buffer-sources 'erc-buffer-source 'append)

  (defun bergheim/consult-erc-buffer ()
    "Consult ERC buffers directly."
    (interactive)
    (let ((erc-buffer-source-visible (copy-sequence erc-buffer-source)))
      ;; Temporarily set :hidden to nil for this buffer source
      (setf (plist-get erc-buffer-source-visible :hidden) nil)
      (let ((consult-buffer-sources (list erc-buffer-source-visible)))
        (consult-buffer))))

  (defun bergheim/erc-swoop-nick ()
    "Search for nick in the current ERC buffer, prepopulated with nicks."
    (interactive)
    (let* ((nicks (bergheim/erc-collect-nicks))
           (nick (completing-read "Choose nick: " nicks nil t)))
      (consult-line (format "<%s>" (regexp-quote nick)))))

  (defun bergheim/erc-open-or-capture-user-note-roam-simple ()
    "Search for nick in the current ERC buffer, prepopulated with nicks."
    (interactive)
    (let* ((nicks (bergheim/erc-collect-nicks))
           (nick (completing-read "Choose nick: " nicks nil t)))
      (org-roam-node-find nil (concat nick "-" (symbol-name (erc-network))))))

  (defun bergheim/erc-open-or-capture-user-note-denote ()
    "Open or capture information in a denote note for an IRC user in the current network."
    (interactive)
    (require 'denote)
    (let* ((nicks (bergheim/erc-collect-nicks))
           (nick (completing-read "Choose nick: " nicks nil t))
           (network (symbol-name (erc-network)))
           (slugified-title (denote-sluggify 'title nick))
           (keywords `("irc" ,network))
           (selected-text (when (use-region-p)
                            (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))))
           (choice (completing-read "Action: " '("Open note" "Capture info") nil t))
           (file-regex (format ".*%s.*%s"
                               (regexp-quote slugified-title)
                               (string-join (mapcar (lambda (kw)
                                                      (denote-sluggify 'keyword kw))
                                                    keywords) ".*")))
           (existing-file (car (denote-directory-files file-regex))))

      (if existing-file
          (find-file-other-window existing-file)
        (if (one-window-p)
            (split-window-right))
        (other-window 1)
        (denote slugified-title keywords nil nil nil 'person))
      (when (string-equal choice "Capture info")
        (goto-char (point-max))
        (insert (format "\n** %s\n" (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (when selected-text
          (insert (format "#+begin_quote\n%s\n#+end_quote\n" selected-text)))
        (save-buffer)
        (when (featurep 'evil)
          (evil-insert-state)))))

  ;; ;; bergheim/erc-collect-nicks seems more useful since that is only active users
  ;; (defun bergheim/erc-nicks-from-channel ()
  ;;   "Get a list of nicks from the current ERC channel."
  ;;   (let (nicks)
  ;;     (when (bound-and-true-p erc-channel-users)
  ;;       (maphash (lambda (nick _)
  ;;                  (push nick nicks))
  ;;                erc-channel-users))
  ;;     nicks))

  (defun bergheim/erc-collect-nicks ()
    "Collect active/visible nicks from the current ERC buffer."
    (let (nicks)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                "<\\(@?[A-Za-z0-9_]+\\)> "
                nil t)
          (push (match-string 1) nicks)))
      nicks)))
