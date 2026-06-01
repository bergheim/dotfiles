;;; bergheim-chat.el --- IRC (erc) and XMPP (jabber) -*- lexical-binding: t; -*-

(use-package erc
  :disabled
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
                                jabber-send-presence
                                bergheim/jabber-unified-show
                                bergheim/jabber-launch)
  :general
  (bergheim/global-menu-keys
    "aj"  '(:ignore t :which-key "Jabber")
    "ajc" '(bergheim/jabber-launch :which-key "Launch (connect + layout)")
    "ajr" '(jabber-roster-popup :which-key "Roster")
    "ajq" '(jabber-disconnect :which-key "Disconnect")
    "ajx" '(jabber-muc-leave :which-key "Leave room")
    "ajn" '(jabber-muc-names :which-key "Participants")
    "aja" '(jabber-activity-switch-to :which-key "Next unread")
    "ajp" '(jabber-chat-with-jid-at-point :which-key "Chat at point")
    "ajt" '(jabber-muc-set-topic :which-key "Set room topic")
    "ajv" '(jabber-vcard-get :which-key "View vCard")
    "aju" '(jabber-send-presence :which-key "Update presence")
    "aji" '(bergheim/jabber-unified-show :which-key "Unified inbox"))
  (bergheim/localleader-keys
    :states '(normal visual)
    :keymaps '(jabber-chat-mode-map bergheim/jabber-unified-mode-map)
    "b" '(bergheim/jabber-switch :which-key "switch chat"))
  (bergheim/localleader-keys
    :states '(normal visual)
    :keymaps 'jabber-chat-mode-map
    "s" '(bergheim/jabber-swoop-nick :which-key "swoop")
    "d" '(bergheim/jabber-open-or-capture-user-note-denote :which-key "denote")
    "f" '(jabber-chat-attach-file :which-key "attach file/image")
    ;; jabber.el's own popup menus (also on C-c C-o / C-c C-m / C-c C-e)
    "o" '(jabber-chat-operations-menu :which-key "chat operations")
    "m" '(jabber-muc-menu :which-key "MUC menu")
    "e" '(jabber-chat-encryption-menu :which-key "encryption")
    ;; quick standalone actions (all live in the operations menu too)
    "r" '(jabber-chat-reply :which-key "reply to message")
    "k" '(jabber-chat-cancel-reply :which-key "cancel reply")
    "E" '(jabber-correct-last-message :which-key "edit last message")
    "i" '(jabber-chat-get-info :which-key "peer info")
    "R" '(jabber-chat-buffer-refresh :which-key "refresh")
    "S" '(jabber-mam-sync-buffer :which-key "sync (MAM)"))
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
  ;; Drop the chat/room header line; the only unique info it carries
  ;; (encryption + MAM-sync indicators) is surfaced in the modeline
  ;; instead via `bergheim/jabber--chat-modeline-encryption' below.
  (jabber-chat-header-line-format nil)
  (jabber-muc-header-line-format nil)
  (jabber-muc-private-header-line-format nil)

  :config
  (setopt jabber-groupchat-buffer-format "*%b*")
;;;; Unified jabber buffer
  ;; Funnel all incoming PMs and MUC messages into a single buffer,
  ;; mirroring the ERC unified-inbox setup above. Jabber's alert hooks
  ;; hand us the source BUFFER, so RET jumps back via a stored marker
  ;; rather than regex-parsing the line.

  (defvar bergheim/jabber-unified-buffer-name "*jabber-unified*")

  (defcustom bergheim/jabber-unified-lines-to-keep 5000
    "Maximum number of lines to retain in the unified jabber buffer.
After each append, lines beyond this from the top are deleted.
Set to nil to disable trimming.  Mirrors jabber's own
`jabber-log-lines-to-keep' but defaults higher since this buffer
aggregates traffic from every room and DM."
    :type '(choice (integer :tag "Lines to keep")
                   (const :tag "Unlimited" nil))
    :group 'bergheim)

  (defvar-local bergheim/jabber-unified--last-date nil
    "Date string of the most recently inserted line, for day separators.")

  (define-derived-mode bergheim/jabber-unified-mode fundamental-mode "Jabber-Unified"
    "Major mode for the jabber unified log buffer."
    (visual-line-mode 1)
    (goto-address-mode 1)
    (setq-local scroll-conservatively most-positive-fixnum)
    (setq-local auto-window-vscroll nil)
    (setq-local buffer-read-only t))

  (with-eval-after-load 'evil
    (evil-define-key 'normal bergheim/jabber-unified-mode-map
      (kbd "RET") #'bergheim/jabber-unified-visit))

  (defun bergheim/jabber-unified--buffer ()
    "Return the unified jabber buffer, creating it if needed."
    (or (get-buffer bergheim/jabber-unified-buffer-name)
        (with-current-buffer (get-buffer-create bergheim/jabber-unified-buffer-name)
          (bergheim/jabber-unified-mode)
          (current-buffer))))

  (defun bergheim/jabber-unified--source-marker (source-buffer)
    "Return a marker pointing into SOURCE-BUFFER for RET-jump.
Prefers the start of the last ewoc node so we land on the right
message; falls back to `jabber-point-insert' or `point-max' when
the ewoc lookup is unavailable (e.g. MUC paths that fire the alert
hook without an ewoc-enter).  Returns nil only if SOURCE-BUFFER is
not live."
    (when (buffer-live-p source-buffer)
      (with-current-buffer source-buffer
        (let ((ewoc (bound-and-true-p jabber-chat-ewoc)))
          (copy-marker
           (or (and ewoc
                    (when-let ((node (ewoc-nth ewoc -1)))
                      (ewoc-location node)))
               (and (markerp (bound-and-true-p jabber-point-insert))
                    (marker-position jabber-point-insert))
               (point-max)))))))

  (defun bergheim/jabber-unified--append (prefix nick text source-buffer type jid &optional mention)
    "Render a unified-buffer line and stash source info for RET-jump.
PREFIX is the displayed source context (\"#room\" or sender JID).
NICK is the MUC sender's nick, or nil for 1:1 messages.
TEXT is the message body.
SOURCE-BUFFER is the chat buffer at capture time (may be nil for
MUC messages received for rooms with no open buffer).
TYPE is `:pm' or `:muc' and JID is the bare JID of the source,
both used by `bergheim/jabber-unified-visit' to re-resolve the
chat buffer when the marker is missing or detached.  TEXT is also
stashed so visit can locate the exact line via text search when
no marker is available (e.g. after auto-creating the buffer)."
    (let* ((time (format-time-string "%H:%M"))
           (date (format-time-string "%Y-%m-%d %A"))
           (marker (bergheim/jabber-unified--source-marker source-buffer))
           ;; Property is (MARKER-OR-NIL . PLIST).  PLIST always has
           ;; :type, :jid, :text and :time so visit can recover even
           ;; when MARKER is nil or its buffer has been killed.  TIME
           ;; matches jabber's `jabber-chat-time-format' (\"%H:%M\")
           ;; so the line search can anchor on it.
           (prop (cons marker
                       (list :type type :jid jid :text text :time time))))
      (with-current-buffer (bergheim/jabber-unified--buffer)
        (let* ((inhibit-read-only t)
               (start (point-max))
               ;; Tail-follow: windows whose point sits at the (old)
               ;; end-of-buffer get advanced to the new end after we
               ;; insert.  Windows scrolled back to read older messages
               ;; have point < start and are left alone.
               (tailing (seq-filter
                         (lambda (w) (= (window-point w) start))
                         (get-buffer-window-list (current-buffer) nil t))))
          (goto-char start)
          ;; Day separator when the date rolls over.  The separator line
          ;; gets no `bergheim/jabber-source' property so RET on it
          ;; harmlessly says "No source recorded for this line".
          (unless (equal date bergheim/jabber-unified--last-date)
            (insert (propertize (format "── %s ──\n" date)
                                'face 'shadow))
            (setq bergheim/jabber-unified--last-date date)
            (setq start (point)))
          (insert (propertize (format "[%s] " time) 'face 'shadow))
          (insert (propertize prefix 'face 'font-lock-keyword-face))
          (when nick
            (insert " " (propertize nick 'face 'font-lock-function-name-face)))
          (insert ": " (or text "") "\n")
          (put-text-property start (point)
                             'bergheim/jabber-source prop)
          ;; Mention/PM: overlay `bold' on top of the existing per-segment
          ;; faces so timestamp/prefix/nick colours stay intact.
          (when mention
            (add-face-text-property start (point) 'bold))
          (bergheim/jabber-unified--trim)
          (dolist (w tailing)
            (set-window-point w (point-max)))))))

  (defun bergheim/jabber-unified--trim ()
    "Trim top of current buffer to `bergheim/jabber-unified-lines-to-keep'."
    (when (and (integerp bergheim/jabber-unified-lines-to-keep)
               (> bergheim/jabber-unified-lines-to-keep 0))
      (let ((excess (- (count-lines (point-min) (point-max))
                       bergheim/jabber-unified-lines-to-keep)))
        (when (> excess 0)
          (save-excursion
            (goto-char (point-min))
            (forward-line excess)
            (delete-region (point-min) (point)))))))

  (defun bergheim/jabber-unified--own-jid-p (jid)
    "Non-nil if JID matches any of our connected bare JIDs.
Catches carbon-copied messages we sent from another device."
    (when-let* ((bare (jabber-jid-user jid))
                (mine (mapcar #'jabber-connection-bare-jid
                              (bound-and-true-p jabber-connections))))
      (member bare mine)))

  (defun bergheim/jabber-unified-capture-pm (from buffer text _title)
    "Capture an incoming 1:1 message into the unified buffer.
Skips messages we sent ourselves (e.g. carbons from another device).
All other 1:1 messages count as a ping (bolded)."
    (let ((jid (jabber-jid-user from)))
      (unless (bergheim/jabber-unified--own-jid-p jid)
        (bergheim/jabber-unified--append jid nil text buffer :pm jid t))))

  (defun bergheim/jabber-unified--room-label (group)
    "Return a short channel label for the MUC GROUP JID.
Prefers a friendly name from XEP-0048 bookmarks or the roster.
Falls back to stripping the biboumi-style `%network' suffix from the
node and prepending `#' for rooms with no bookmark/roster entry."
    (let* ((bare (jabber-jid-user group))
           (friendly (jabber-jid-bookmarkname group)))
      (if (and friendly (not (equal friendly bare)))
          friendly
        (let* ((node (jabber-jid-username group))
               (label (replace-regexp-in-string "%.*\\'" "" node)))
          (if (string-prefix-p "#" label)
              label
            (concat "#" label))))))

  (defun bergheim/jabber-unified-preview-room-labels ()
    "Show how `bergheim/jabber-unified--room-label' resolves every bookmarked room.
Useful for inspecting whether bridge-supplied friendly names are
arriving via XEP-0048 bookmarks the way Dino sees them."
    (interactive)
    (require 'jabber-bookmarks)
    (let (rows)
      (maphash (lambda (_account bookmarks)
                 (dolist (bm bookmarks)
                   (let ((jid (plist-get bm :jid)))
                     (push (format "%-50s → %s"
                                   jid
                                   (bergheim/jabber-unified--room-label jid))
                           rows))))
               jabber-bookmarks)
      (with-current-buffer (get-buffer-create "*jabber-unified-room-labels*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (string-join (nreverse rows) "\n"))
          (insert "\n"))
        (special-mode)
        (pop-to-buffer (current-buffer)))))

  (defun bergheim/jabber-unified--mentioned-p (group text)
    "Non-nil if TEXT word-mentions our nick in GROUP."
    (when-let* ((mynick (jabber-muc-nickname group))
                ((stringp text)))
      (let ((case-fold-search t))
        (string-match-p (concat "\\b" (regexp-quote mynick) "\\b") text))))

  (defun bergheim/jabber-unified-capture-muc (nick group buffer text _title)
    "Capture an incoming MUC message into the unified buffer.
Skips messages we sent ourselves (echoed back by the room).  Bolds
the line when our nick is mentioned in TEXT (case-insensitive,
word-boundary match)."
    (unless (jabber-muc-our-nick-p group nick)
      (bergheim/jabber-unified--append
       (bergheim/jabber-unified--room-label group) nick text buffer :muc group
       (bergheim/jabber-unified--mentioned-p group text))))

  (defun bergheim/jabber-unified--resolve-buffer (source)
    "Look up a live chat buffer from SOURCE plist, or nil."
    (when source
      (pcase (plist-get source :type)
        (:muc (jabber-muc-find-buffer (plist-get source :jid)))
        (:pm  (jabber-chat-find-buffer (plist-get source :jid))))))

  (defun bergheim/jabber-unified--ensure-buffer (source)
    "Return the chat buffer for SOURCE, creating it if needed.
Uses the first live jabber connection so the buffer can be
populated from `jabber-db-backlog' on creation."
    (or (bergheim/jabber-unified--resolve-buffer source)
        (when-let* ((jc (car (bound-and-true-p jabber-connections)))
                    (jid (plist-get source :jid)))
          (pcase (plist-get source :type)
            (:muc (jabber-muc-create-buffer jc jid))
            (:pm  (jabber-chat-create-buffer jc jid)))
          (bergheim/jabber-unified--resolve-buffer source))))

  (defun bergheim/jabber-unified--locate-line (buffer text time)
    "Return BOL of the most recent line in BUFFER matching TEXT and TIME.
TIME is jabber's `%H:%M' timestamp captured when the message
arrived; the search prefers a line containing both TIME and TEXT
and falls back to the latest TEXT match if no time-anchored line
is found."
    (when (and (buffer-live-p buffer)
               (stringp text) (not (string-empty-p text)))
      (with-current-buffer buffer
        (save-excursion
          (or (and (stringp time)
                   (catch 'found
                     (goto-char (point-max))
                     (while (search-backward text nil t)
                       (let ((bol (line-beginning-position))
                             (eol (line-end-position)))
                         (save-excursion
                           (goto-char bol)
                           (when (search-forward time eol t)
                             (throw 'found bol)))))
                     nil))
              (progn
                (goto-char (point-max))
                (when (search-backward text nil t)
                  (line-beginning-position))))))))

  (defun bergheim/jabber-unified-visit ()
    "Jump from the unified buffer to the original message location.
Auto-creates the chat buffer if it isn't open, then jumps to the
marker if it's still valid, otherwise searches the buffer for the
captured message text.  When point is on the empty trailing line
(typical right after the buffer received new messages), resolves
the previous line instead."
    (interactive)
    (let* ((line-bol (save-excursion
                       (when (and (eobp) (bolp))
                         (forward-line -1))
                       (line-beginning-position)))
           (prop (get-text-property line-bol 'bergheim/jabber-source))
           (marker (and (consp prop) (car prop)))
           (source (and (consp prop) (cdr prop)))
           (live-marker (and (markerp marker) (marker-buffer marker) marker))
           (buffer (or (and live-marker (marker-buffer live-marker))
                       (bergheim/jabber-unified--ensure-buffer source))))
      (cond
       ((null prop)
        (user-error "No source recorded for this line"))
       ((null buffer)
        (user-error "Could not open source buffer: %s"
                    (or (plist-get source :jid) "unknown")))
       (t
        (switch-to-buffer-other-window buffer)
        (when (featurep 'evil) (evil-normal-state))
        (let ((pos (or (and live-marker (eq (marker-buffer live-marker) buffer)
                            (marker-position live-marker))
                       (bergheim/jabber-unified--locate-line
                        buffer
                        (plist-get source :text)
                        (plist-get source :time)))))
          (if pos
              (progn (goto-char pos) (beginning-of-line))
            (goto-char (point-max))))))))

  (defun bergheim/jabber-unified-show ()
    "Open the jabber unified buffer and snap to the latest line."
    (interactive)
    (let ((buf (bergheim/jabber-unified--buffer)))
      (pop-to-buffer buf)
      (with-current-buffer buf
        (goto-char (point-max)))))

;;;; Launcher: connect + lay out (unified inbox + predefined room)

  (defcustom bergheim/jabber-startup-room nil
    "MUC JID to display on the right when `bergheim/jabber-launch' runs.
The buffer is created via `jabber-muc-create-buffer' (idempotent;
populates from `jabber-db-backlog'), so the room doesn't need to
be in autojoin first.  When nil, the launcher just opens the
unified inbox."
    :type '(choice (const :tag "None" nil)
                   (string :tag "MUC JID"))
    :group 'bergheim)

  (defun bergheim/jabber--launch-layout ()
    "Arrange the launch layout in the current frame.
Unified inbox in the main window; `bergheim/jabber-startup-room'
in a plain right-split window (~40% width).  Regular windows
\(not side windows) so they can be moved, rotated and deleted."
    (delete-other-windows)
    (switch-to-buffer (bergheim/jabber-unified--buffer))
    (goto-char (point-max))
    (when bergheim/jabber-startup-room
      (when-let ((jc (car (bound-and-true-p jabber-connections))))
        (jabber-muc-create-buffer jc bergheim/jabber-startup-room))
      (when-let ((chan (jabber-muc-find-buffer bergheim/jabber-startup-room)))
        (let ((win (split-window-right
                    (- (round (* 0.4 (frame-width)))))))
          (set-window-buffer win chan)))))

  (defun bergheim/jabber--launch-layout-once (&rest _)
    "One-shot wrapper for `jabber-post-connect-hooks'."
    (remove-hook 'jabber-post-connect-hooks
                 #'bergheim/jabber--launch-layout-once)
    (bergheim/jabber--launch-layout))

  (defun bergheim/jabber-launch ()
    "Connect (if needed), open the unified inbox, dock the predefined room.
If already connected, just re-applies the layout.  The post-connect
layout work runs from `jabber-post-connect-hooks' so it doesn't
race against the handshake (no `run-with-timer' hack)."
    (interactive)
    (if (bound-and-true-p jabber-connections)
        (bergheim/jabber--launch-layout)
      (add-hook 'jabber-post-connect-hooks
                #'bergheim/jabber--launch-layout-once)
      (jabber-connect-all)))

  (defun bergheim/jabber--swoop-nicks ()
    "Return candidate nicks for the current jabber chat buffer.
For MUC buffers, this is the active participant list.  For 1:1
chats, it's the single contact (so prompts can skip selection)."
    (cond
     ((bound-and-true-p jabber-group)
      (cdr (assoc jabber-group jabber-muc-participants)))
     ((bound-and-true-p jabber-chatting-with)
      (list (jabber-jid-displayname jabber-chatting-with)))))

  (defun bergheim/jabber--pick-nick ()
    "Prompt for a nick, auto-selecting when only one candidate exists."
    (let ((nicks (or (bergheim/jabber--swoop-nicks)
                     (user-error "No nicks available in this buffer"))))
      (if (length= nicks 1)
          (car nicks)
        (completing-read "Choose nick: " nicks nil t))))

  (defun bergheim/jabber-swoop-nick ()
    "Search the current jabber chat buffer for messages from a chosen nick.
Mirrors `bergheim/erc-swoop-nick': prompts with active nicks and
calls `consult-line' scoped to the `<nick>` prefix jabber prints."
    (interactive)
    (let ((nick (bergheim/jabber--pick-nick)))
      (consult-line (format "<%s>" (regexp-quote nick)))))

  (defun bergheim/jabber--note-keywords (jid)
    "Return denote keywords for JID, matching the ERC contact-note format.

For biboumi-style bridged JIDs (e.g. `nick%irc.libera.chat@biboumi…'),
returns the friendly transport + the network behind the `%' suffix
\(stripping a leading `irc.') so a libera-bridged contact ends up
under `(\"irc\" \"liberachat\")' — the same keywords used by
`bergheim/erc-open-or-capture-user-note-denote'.

For native XMPP JIDs, returns `(\"jabber\" SERVER)' with a leading
`xmpp.' stripped so `xmpp.glvortex.net' tags as `glvortexnet'
after `denote-sluggify' rather than the uglier `xmppglvortexnet'."
    (let* ((node (jabber-jid-username jid))
           (bridged (and node
                         (string-match "%\\([^@]+\\)\\'" node)
                         (match-string 1 node))))
      (if bridged
          (list (downcase (bergheim/jabber--transport jid))
                (replace-regexp-in-string "^irc\\." "" bridged))
        (list "jabber"
              (replace-regexp-in-string "^xmpp\\." ""
                                        (jabber-jid-server jid))))))

  (defun bergheim/jabber-open-or-capture-user-note-denote ()
    "Open or capture a denote note for a jabber contact / MUC participant.
Mirrors `bergheim/erc-open-or-capture-user-note-denote': looks up
an existing note by slug + transport-aware keywords (see
`bergheim/jabber--note-keywords'), otherwise creates one.  If the
region is active when capturing, the selected text is inserted as
an org quote block under a timestamped heading."
    (interactive)
    (require 'denote)
    (require 'bergheim-jabber-extra)
    (let* ((nick (bergheim/jabber--pick-nick))
           (conv-jid (or (bound-and-true-p jabber-group)
                         (bound-and-true-p jabber-chatting-with)))
           (slugified-title (denote-sluggify 'title nick))
           (keywords (bergheim/jabber--note-keywords conv-jid))
           (selected-text (when (use-region-p)
                            (string-trim
                             (buffer-substring-no-properties
                              (region-beginning) (region-end)))))
           (choice (completing-read "Action: " '("Open note" "Capture info") nil t))
           (file-regex (format ".*%s.*%s"
                               (regexp-quote slugified-title)
                               (string-join
                                (mapcar (lambda (kw)
                                          (denote-sluggify 'keyword kw))
                                        keywords)
                                ".*")))
           (existing-file (car (denote-directory-files file-regex))))
      (if existing-file
          (find-file-other-window existing-file)
        (when (one-window-p) (split-window-right))
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

  (defun bergheim/jabber--chat-modeline-encryption ()
    "Splice a 🔒 / 🔓 indicator into `mode-line-misc-info'.
Derived from `jabber-chat-encryption' directly so the modeline
shows a single glyph instead of jabber's `[plaintext]'/`[OMEMO]'
text.  doom-modeline renders `mode-line-misc-info' automatically,
but only on the active window."
    (setq-local mode-line-misc-info
                (append mode-line-misc-info
                        '((:eval
                           (pcase (bound-and-true-p jabber-chat-encryption)
                             ((or 'omemo 'openpgp 'openpgp-legacy) " 🔒")
                             (_ " 🔓")))))))

  (add-hook 'jabber-chat-mode-hook #'bergheim/jabber--chat-modeline-encryption)

  (add-hook 'jabber-alert-message-hooks #'bergheim/jabber-unified-capture-pm)
  (add-hook 'jabber-alert-muc-hooks #'bergheim/jabber-unified-capture-muc))

;; goto-addr only binds mouse-2 / C-c RET; without plain RET on the overlay,
;; evil-ret falls through to push-button which crashes on Emacs 31.
(with-eval-after-load 'goto-addr
  (define-key goto-address-highlight-keymap (kbd "RET") #'goto-address-at-point))

(use-package bergheim-jabber-extra
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

;;; bergheim-chat.el ends here
