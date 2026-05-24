;; -*- lexical-binding: t; -*-

(use-package password-store
  :general
  (bergheim/global-menu-keys
    "yp" 'password-store-copy
    "ip" 'password-store-generate
    "iP" 'password-store-generate-no-symbols))

(use-package pass
  :unless bergheim/container-mode-p)

(use-package shr
  :ensure nil
  :custom
  ;; (toggle-truncate-lines 1)
  (shr-max-width 120))

(use-package proced
  :ensure nil
  :commands proced
  :general
  (bergheim/global-menu-keys
    "ap" '(proced :which-key "Proced"))
  :hook
  (proced-post-display . hl-line-mode)
  :custom
  (proced-auto-update-flag 'visible)
  (proced-auto-update-interval 2)
  (proced-goal-attribute nil) ;; don't move cursor to args when navigating
  (proced-show-remote-processes t) ;; enable TRAMP support
  (proced-enable-color-flag t)
  (proced-format 'custom)
  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid tree pcpu rss start state (args comm))))

;; pastebin stuff
(use-package 0x0
  :unless bergheim/container-mode-p
  :after general

  :general
  (bergheim/global-menu-keys
    "ys" '(:ignore t :which-key "Share")
    "yss" '(0x0-dwim :which-key "Dwim")
    "ysp" '(0x0-popup :which-key "Text")
    "ysf" '(0x0-upload-file :which-key "File")))

(use-package elfeed
  :unless bergheim/container-mode-p
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
    (elfeed-protocol-fever-reinit bergheim/elfeed-fever-url))
  :config
  (setq elfeed-use-curl t)
  ;; miniflux / fever
  (setq elfeed-protocol-fever-update-unread-only nil)
  (setq elfeed-protocol-fever-fetch-category-as-tag nil)
  (setq elfeed-protocol-feeds
        `((,(concat "fever+" bergheim/elfeed-fever-url)
           :api-url ,bergheim/elfeed-fever-api-url
           :password ,(password-store-get bergheim/elfeed-fever-password-store-key))))
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
  :unless bergheim/container-mode-p
  :init
  (setq smudge-api-oauth2-token-directory
        (file-name-as-directory (bergheim/get-and-ensure-data-dir "smudge"))
        smudge-api-oauth2-token-file
        (expand-file-name "token" smudge-api-oauth2-token-directory))
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
;; see http://blog.binchen.org/posts/how-to-be-extremely-efficient-in-emacs/
;; (use-package keyfreq
;;   :config
;;   (keyfreq-mode 1)
;;   (keyfreq-autosave-mode 1)
;;   )

(use-package erc
  :ensure t
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
  (erc-fill-static-center 3)
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
  (erc-insert-timestamp-function 'erc-insert-timestamp-right)
  (erc-timestamp-only-if-changed-flag t)
  (erc-interpret-mirc-color t)
  (erc-join-buffer 'buffer)
  ;; (erc-nicks-contrast-range '(1 . 100))
  (erc-nick "tsb")
  (erc-prompt-for-password nil)
  (erc-prompt "⟩")
  (erc-prompt (lambda () (concat
                          (propertize
                           (format "%c"
                                   (char-from-name
                                    "TOP LEFT HALF BRACKET"))
                           ;;"⸢"
                           'face '(:family "Symbola")
                           )
                          (buffer-name)
                          (propertize
                           (format "%c ⟩ "
                                   (char-from-name
                                    "BOTTOM RIGHT HALF BRACKET"))
                           ;;"⸥"
                           'face '(:family "Symbola"))
                          )))


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

  (add-to-list 'erc-modules 'scrolltobottom)

  (defun erc-cmd-LATEST (&rest args)
    "Get latest messages using IRCv3 CHATHISTORY.
Usage: /LATEST [count] (defaults to 100)"
    (let ((count (if args
                     (string-to-number (car args))
                   100)))
      (erc-server-send (format "CHATHISTORY LATEST %s * %d" 
                               (erc-default-target) count))))
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
      (erc :server bergheim/irc-server
           :port 6667
           :nick bergheim/irc-nick
           :user bergheim/irc-username
           :password (password-store-get "apps/soju")))
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
      nicks))

;;;; (core) erc erc-match

  ;; Hijack ERC-match module in order to create a "unified inbox" type
  ;; of buffer which contains all incoming messages from all channel
  ;; buffers.

  (define-advice erc-log-match-make-buffer (:filter-return (buffer))
    "Enable `visual-line-mode'."
    (with-current-buffer buffer (visual-line-mode))
    buffer)

  (setopt erc-keyword-highlight-type
          nil)
  (setopt erc-keywords
          (cons "$" bergheim/irc-keywords))
  (setopt erc-log-matches-flag
          t)
  (setopt erc-log-matches-types-alist
          '((keyword . "erc-unified")))
  (setopt erc-match-exclude-server-buffer
          t)

  ;; (setopt erc-log-match-format
  ;;         (concat
  ;;          (propertize "%t" 'face (list :foreground faded))
  ;;          " "
  ;;          (propertize "%c" 'face (list :foreground bright-blue))
  ;;          " "
  ;;          (propertize "%n" 'face (list :foreground bright-yellow))
  ;;          ": "
  ;;          (propertize "%m"
  ;;                      'wrap-prefix (list 'space :width 4)
  ;;                      'line-prefix (list 'space :width 4))))



  (defun bergheim/set-erc-log-match-format ()
    "Set `erc-log-match-format` colors based on the current theme."
    (let* ((is-dark (bergheim//system-dark-mode-enabled-p))
           (faded (if is-dark "#5b5b5b" "#888888"))
           (bright-blue (if is-dark "#61afef" "#007acc"))
           (bright-yellow (if is-dark "#e4c44c" "#b5a11e")))
      (setopt erc-log-match-format
              (concat
               (propertize "%t" 'face (list :foreground faded))
               " "
               (propertize "%c" 'face (list :foreground bright-blue))
               " "
               (propertize "%n" 'face (list :foreground bright-yellow))
               ": "
               (propertize "%m"
                           'wrap-prefix (list 'space :width 4)
                           'line-prefix (list 'space :width 4))))))


  ;; Call the function to set the format initially
  (bergheim/set-erc-log-match-format)

  ;; Update the ERC log format when the theme changes.
  (add-hook 'after-load-theme-hook #'bergheim/set-erc-log-match-format)

  (defun bergheim/erc-custom-prompt ()
    (setq-local erc-prompt
                (lambda ()
                  (concat
                   (propertize
                    (format "%c"
                            (char-from-name
                             "TOP LEFT HALF BRACKET"))
                    ;;"⸢"
                    'face '(:family "Symbola")
                    )
                   (buffer-name)
                   (propertize
                    (format "%c ⟩"
                            (char-from-name
                             "BOTTOM RIGHT HALF BRACKET"))
                    ;;"⸥"
                    'face '(:family "Symbola"))))))

  (add-hook 'erc-join-hook #'bergheim/erc-custom-prompt)

  (define-derived-mode bergheim/erc-unified-mode fundamental-mode "ERC-Unified"
    "Major mode for ERC unified log buffer."

    ;; (setq-local erc-input-line-position 0)
    ;; (setq-local erc-scrolltobottom-all 'relaxed)
    ;; (setq-local erc-scrolltobottom-all nil)
    (setq-local scroll-conservatively most-positive-fixnum)
    (setq-local auto-window-vscroll nil)
    ;; Disable ERC scrolling behaviors
    (remove-hook 'post-command-hook 'erc-scrolltobottom-all t)
    (remove-hook 'window-scroll-functions 'erc-scrolltobottom-all t)
    (setq-local buffer-read-only t))

  (evil-define-key 'normal bergheim/erc-unified-mode-map
    (kbd "RET") #'bergheim/erc-unified-visit)

  (defun bergheim/erc-unified-visit ()
    "In an `erc-unified' buffer, jump to the same message in its channel buffer.
Searches from the bottom of the channel buffer backward for the exact text."
    (interactive)
    (let* ((line (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position)))
           ;; match “[time] #chan nick:
           message…”
           (re   "^\\[.*?\\] \\([^ ]+\\) [^:]+: \\(.*\\)$")
           (chan (and (string-match re line) (match-string 1 line)))
           (msg  (and (string-match re line) (match-string 2 line)))
           (buf  (and chan (get-buffer chan))))
      (unless (and buf msg)
        (user-error "Cannot parse channel or message"))
      ;; switch to channel buffer in another window (splits if needed)
      (switch-to-buffer-other-window buf)
      (evil-normal-state)
      ;; go to bottom and search backward for the exact message text
      (goto-char (point-max))
      (if (re-search-backward (regexp-quote msg) nil t)
          (goto-char (match-beginning 0))
        (message "ERC[%s]: \"%s\" not found, at bottom" chan msg)
        (goto-char (point-max))))))

(use-package erc-image
  :after erc
  :demand t
  :hook (erc-mode-hook . erc-image-mode)
  :config
  (setopt erc-image-inline-rescale 'window)
  (erc-update-modules))

;; FIXME: fucks up mu4e view
(use-package image-slicing
  :demand
  :ensure (:host github :repo "ginqi7/image-slicing")
  :general
  (bergheim/global-menu-keys
    "tu" #'bergheim/toggle-line-spacing-for-images)
  :config
  (defvar bergheim/original-line-spacing nil
    "Store the original line-spacing value before setting to 0.")

  (defun bergheim/toggle-line-spacing-for-images ()
    "Toggle line-spacing between 0 and whatever it was before."
    (interactive)
    (if (and (numberp line-spacing) (= line-spacing 0))
        (setq line-spacing bergheim/original-line-spacing)
      (setq bergheim/original-line-spacing line-spacing)
      (setq line-spacing 0))
    (redisplay))
  ;; (setopt line-spacing 0) ;; without this we get gaps

  ;; Only enable in eww-mode, not globally
  (defun bergheim/enable-image-slicing ()
    "Enable image-slicing for this buffer."
    (setq-local shr-external-rendering-functions
                '((img . image-slicing-tag-img)))
    (image-slicing-mode 1))

  (add-hook 'eww-mode-hook #'bergheim/enable-image-slicing))


(use-package keymap-popup
  :ensure (keymap-popup :host github :repo "emacs-straight/keymap-popup"))

(elpaca-defscript jabber-build-omemo (:type system :dir source)
  ("make" "module"))

(use-package jabber
  :ensure (jabber
           :host codeberg
           :repo "emacs-jabber/emacs-jabber"
           :files ("lisp/*.el" "lisp/*.so")
           :build (:before elpaca-build-link jabber-build-omemo))

  :commands (jabber-connect-all jabber-display-roster jabber-roster-popup
                                jabber-muc-leave jabber-muc-names
                                jabber-activity-switch-to jabber-chat-with-jid-at-point
                                jabber-muc-set-topic jabber-vcard-get
                                jabber-send-presence)
  :general
  (bergheim/global-menu-keys
    "aj"  '(:ignore t :which-key "Jabber")
    "ajc" '(jabber-connect-all :which-key "Connect")
    "ajr" '(jabber-roster-popup :which-key "Roster")
    "ajq" '(jabber-disconnect :which-key "Disconnect")
    "ajx" '(jabber-muc-leave :which-key "Leave room")
    "ajn" '(jabber-muc-names :which-key "Participants")
    "aja" '(jabber-activity-switch-to :which-key "Next unread")
    "ajp" '(jabber-chat-with-jid-at-point :which-key "Chat at point")
    "ajt" '(jabber-muc-set-topic :which-key "Set room topic")
    "ajv" '(jabber-vcard-get :which-key "View vCard")
    "aju" '(jabber-send-presence :which-key "Update presence"))
  :custom
  (jabber-chat-default-encryption 'plaintext)
  ;; Password is NOT here — jabber.el pulls it from auth-source.
  ;; You already use password-store; either add an ~/.authinfo(.gpg)
  ;; line  `machine xmpp.glvortex.net login tsb password …`  or enable
  ;; auth-source-pass so it reads your pass store.
  (jabber-account-list
   `(("tsb@xmpp.glvortex.net"
      (:password . ,(password-store-get "apps/ejabberd/tsb"))
      (:network-server . ,bergheim/irc-server)
      (:port . 5222)
      (:connection-type . starttls))))
  (jabber-history-enabled t)
  (jabber-use-global-history nil)
  (jabber-db-path (bergheim/get-and-ensure-data-dir "jabber" "jabber.db"))
  (jabber-history-dir (expand-file-name "jabber-history" bergheim/cache-dir))
  (jabber-avatar-cache-directory (bergheim/get-and-ensure-data-dir "jabber/avatars"))
  (jabber-auto-reconnect t)
  (jabber-show-resources nil)
  (jabber-roster-show-title nil)
  (jabber-vcard-avatars-retrieve nil)
  (jabber-alert-presence-hooks nil)
  (jabber-alert-message-hooks '(jabber-message-echo jabber-message-scroll)))

(use-package jabber-extra
  :ensure nil
  :load-path (lambda () (expand-file-name "modules" bergheim/config-dir))
  :commands (bergheim/jabber-switch
             bergheim/jabber-chat-with
             bergheim/jabber-join
             bergheim/jabber-discover-conferences)
  :general
  (bergheim/global-menu-keys
    "ajj" '(bergheim/jabber-switch :which-key "Switch (all)")
    "ajc" '(bergheim/jabber-chat-with :which-key "Chat with...")
    "ajg" '(bergheim/jabber-join :which-key "Join room")
    "ajd" '(bergheim/jabber-discover-conferences :which-key "Discover conferences")))
