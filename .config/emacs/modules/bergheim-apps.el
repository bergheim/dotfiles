;;; bergheim-apps.el --- Misc applications (password-store, proced, share, smudge) -*- lexical-binding: t; -*-

(use-package password-store
  :general
  (bergheim/global-menu-keys
    "yp" 'password-store-copy
    "ip" 'password-store-generate
    "iP" 'password-store-generate-no-symbols))

(use-package pass
  :unless bergheim/container-mode-p)

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

(use-package keymap-popup
  :ensure (keymap-popup :host github :repo "emacs-straight/keymap-popup"))

;;; bergheim-apps.el ends here
