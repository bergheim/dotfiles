;; -*- lexical-binding: t; -*-

(use-package eshell
  :ensure nil
  :bind (("C-r" . consult-history)))

(use-package eat
  :commands eat
  :general
  (bergheim/global-menu-keys
    "aa" '(eat :which-key "Eat"))
  :config
  (add-hook 'eshell-first-time-mode-hook #'eat-eshell-mode))

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
    (siren-tab-bar-switch-to-or-create-tab "journal")
    (denote-journal-extras-new-entry)
    (bergheim/write-mode t)
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
