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
  :bind (("C-r" . consult-history)
         ("C-f" . consult-dir)
         ("C-t" . eshell/find-file-with-consult)
         ;; ("C-d" . eshell/z)
         )
  :general
  (bergheim/global-menu-keys
    "as" '(eshell :which-key "eshell"))
  :config
  (defun eshell-get-old-input ()
    "Retrieve the current input from the Eshell prompt in the buffer."
    (buffer-substring-no-properties
     (save-excursion (eshell-bol) (point))
     (point)))

  (defun eshell/vi (filename)
    "Open FILENAME in another buffer within Eshell."
    (if (file-exists-p filename)
        (find-file-other-window filename)
      (message "File does not exist: %s" filename)))

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
                                (eshell/alias "ll" "ls -AFGhl --color=always $1")
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

(use-package shr
  :ensure nil
  :custom
  ;; (toggle-truncate-lines 1)
  (shr-max-width 120))

(use-package treemacs
  :ensure t
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
  (denote-directory (expand-file-name "~/denote/"))
  (denote-date-prompt-use-org-read-date t)
  (denote-backlinks-show-context t)
  :config
  (defun my-denote-tmr ()
    (tmr "5" "Write focused now.."))
  (add-hook 'denote-journal-extras-hook 'my-denote-tmr)
  ;; (add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe)
  (denote-rename-buffer-mode 1)

  ;; (setq denote-templates nil)
  ;; `((report . "* Some heading\n\n* Another heading")
  ;;   (memo . ,(concat "* Some heading"
  ;;                    "\n\n"
  ;;                    (shell-command-to-string "fortune -s")
  ;;                    "* Another heading"
  ;;                    "\n\n")))

  (defun bergheim/denote-new-journal-entry ()
    "Create a new journal entry and enter writer mode"
    (interactive)
    (unless (featurep 'denote-journal-extras)
        (require 'denote-journal-extras))
    (siren-tab-bar-switch-to-or-create-tab "journal")
    (let ((entry-today (denote-journal-extras--entry-today)))
      (if entry-today
          (denote-open-or-create (car entry-today))
        (denote-journal-extras-new-entry)))
    (bergheim/write-mode t)
    (goto-char (point-max))
    (delete-trailing-whitespace)
    (insert "\n* " (format-time-string "%H:%M") " ")
    (evil-insert 0))

  (defun bergheim/denote-last-journal-entry ()
    "Open the newest entry"
    (interactive)
    (let ((files (directory-files denote-journal-extras-directory nil "^[^.]")))
      (when files
        (siren-tab-bar-switch-to-or-create-tab "journal")
        (find-file (expand-file-name
                    (car (last (sort files 'string<))) denote-journal-extras-directory))
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
    "ne" '(denote-org-extras-extract-org-subtree :which-key "Extract from node")
    "nh" '(denote-org-extras-link-to-heading :which-key "Link to heading")
    "ni" '(:ignore t :which-key "Insert")
    "nib" '(denote-org-extras-dblock-insert-backlinks :which-key "backlinks")
    "nif" '(denote-org-extras-dblock-insert-files :which-key "files")
    "nil" '(denote-org-extras-dblock-insert-links :which-key "links")
    "nj" '(:ignore t :which-key "Journal")
    "njj" '(bergheim/denote-new-journal-entry :which-key "New journal")
    "njl" '(bergheim/denote-last-journal-entry :which-key "Last journal entry")
    "njb" '((lambda () (interactive) (find-file denote-journal-extras-directory)) :which-key "Browse journals")
    "nL" '(denote-find-link :which-key "Show links")
    "nl" '(denote-link-or-create :which-key "Link")
    "nn" '(denote-open-or-create :which-key "Open/create")
    "nr" '(denote-rename-file-using-front-matter :which-key "Rename")
    "nr" '(denote-rename-file :which-key "Rename")
    "nR" '(denote-rename-file-signature :which-key "Rename signature")
    "ns" '(consult-notes-search-in-all-notes :which-key "Search")))

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
