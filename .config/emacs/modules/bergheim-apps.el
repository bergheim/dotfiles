;;; bergheim-apps.el --- Misc applications (password-store, proced, share, smudge) -*- lexical-binding: t; -*-

(defvar-local bergheim/pass--entry nil)

(defvar bergheim/pass-skeleton "user: \nurl: \n"
  "Fields prefilled for a new entry, below the generated password.")

(defun bergheim/pass-edit (entry)
  "Edit ENTRY in a buffer: password on line 1, `key: value' below.
A new entry starts with a generated password and `bergheim/pass-skeleton'.
C-c C-c writes it through pass, C-c C-k throws it away."
  (interactive (progn (require 'password-store)
                      (list (completing-read "Entry: " (password-store-list)))))
  (let ((new (not (member entry (password-store-list)))))
    (pop-to-buffer (get-buffer-create (format "*pass: %s*" entry)))
    (erase-buffer)
    (insert (if new
                (concat (string-trim
                         (shell-command-to-string "gpg --gen-random --armor 1 18"))
                        "\n" bergheim/pass-skeleton)
              (password-store--run-show entry)))
    (setq bergheim/pass--entry entry)
    (keymap-local-set "C-c C-c" #'bergheim/pass-save)
    (keymap-local-set "C-c C-k" #'kill-current-buffer)
    (goto-char (point-min))
    (when new (forward-line 1) (end-of-line))
    (message "C-c C-c to save, C-c C-k to abort")))

(defun bergheim/pass-save ()
  "Write the current pass edit buffer back to the store."
  (interactive)
  (unless bergheim/pass--entry (user-error "Not a pass buffer"))
  (password-store-insert bergheim/pass--entry
                         (string-trim-right (buffer-string) "\n+"))
  (kill-buffer))

(use-package password-store
  :init
  (auth-source-pass-enable)
  (setq auth-sources '(password-store))
  :general
  (bergheim/global-menu-keys
    "yp" 'password-store-copy
    "yf" '(password-store-copy-field :which-key "Copy pass field")
    "yu" '(password-store-url :which-key "Open pass url")
    "ip" 'password-store-generate
    "iP" 'password-store-generate-no-symbols
    "in" '(bergheim/pass-edit :which-key "New/edit entry")
    "io" '(password-store-otp-append :which-key "Add OTP uri")))

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
(use-package bergheim-microbin
  :ensure nil
  :custom
  (bergheim/microbin-password-function
   (lambda () (password-store-get "homelab/microbin/uploader")))
  :general
  (bergheim/global-menu-keys
    "ys"  '(:ignore t :which-key "Share")
    "yss" '(bergheim/microbin-upload-dwim          :which-key "Dwim (raw)")
    "ysS" '(bergheim/microbin-upload-dwim-pretty   :which-key "Dwim (pretty)")
    "ysb" '(bergheim/microbin-upload-buffer        :which-key "Buffer (raw)")
    "ysB" '(bergheim/microbin-upload-buffer-pretty :which-key "Buffer (pretty)")
    "ysr" '(bergheim/microbin-upload-region        :which-key "Region (raw)")
    "ysR" '(bergheim/microbin-upload-region-pretty :which-key "Region (pretty)")
    "ysf" '(bergheim/microbin-upload-file          :which-key "File")
    "ysh" '(bergheim/microbin-upload-html          :which-key "HTML page")))

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
  :ensure (keymap-popup :host github :repo "emacs-straight/keymap-popup")
  :config
  ;; keymap-popup 0.4.1 nils out popup metadata when the last entry is
  ;; removed; jabber-keymap.el's bootstrap (define C-g, remove it, then
  ;; add entries) relies on it surviving.  Re-seed empty metadata.
  (define-advice keymap-popup-add-entry (:before (keymap &rest _) bergheim/reseed)
    (unless (keymap-popup--meta keymap 'descriptions)
      (setf (keymap-popup--meta keymap 'descriptions)
            (list (list (keymap-popup--group nil nil)))))))


;;; bergheim-apps.el ends here
