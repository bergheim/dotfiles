;;; bergheim-chat.el --- IRC (erc) and XMPP (jabber) -*- lexical-binding: t; -*-

(use-package erc
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
                                bergheim/jabber-unified-show)
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
    "aju" '(jabber-send-presence :which-key "Update presence")
    "aji" '(bergheim/jabber-unified-show :which-key "Unified inbox"))
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
  (jabber-alert-message-hooks '(jabber-message-echo jabber-message-scroll))

  :config
;;;; Unified jabber buffer
  ;; Funnel all incoming PMs and MUC messages into a single buffer,
  ;; mirroring the ERC unified-inbox setup above. Jabber's alert hooks
  ;; hand us the source BUFFER, so RET jumps back via a stored marker
  ;; rather than regex-parsing the line.

  (defvar bergheim/jabber-unified-buffer-name "*jabber-unified*")

  (define-derived-mode bergheim/jabber-unified-mode fundamental-mode "Jabber-Unified"
    "Major mode for the jabber unified log buffer."
    (visual-line-mode 1)
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

  (defun bergheim/jabber-unified--append (prefix nick text source-buffer type jid)
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
           (marker (bergheim/jabber-unified--source-marker source-buffer))
           ;; Property is (MARKER-OR-NIL . PLIST).  PLIST always has
           ;; :type, :jid, :text and :time so visit can recover even
           ;; when MARKER is nil or its buffer has been killed.  TIME
           ;; matches jabber's `jabber-chat-time-format' (\"%H:%M\")
           ;; so the line search can anchor on it.
           (prop (cons marker
                       (list :type type :jid jid :text text :time time))))
      (with-current-buffer (bergheim/jabber-unified--buffer)
        (let ((inhibit-read-only t)
              (start (point-max)))
          (goto-char start)
          (insert (propertize (format "[%s] " time) 'face 'shadow))
          (insert (propertize prefix 'face 'font-lock-keyword-face))
          (when nick
            (insert " " (propertize nick 'face 'font-lock-function-name-face)))
          (insert ": " (or text "") "\n")
          (put-text-property start (point)
                             'bergheim/jabber-source prop)))))

  (defun bergheim/jabber-unified-capture-pm (from buffer text _title)
    "Capture an incoming 1:1 message into the unified buffer."
    (let ((jid (jabber-jid-user from)))
      (bergheim/jabber-unified--append jid nil text buffer :pm jid)))

  (defun bergheim/jabber-unified--room-label (group)
    "Return a short channel label for the MUC GROUP JID.
Strips the biboumi-style `%network' suffix used by IRC bridge
JIDs (e.g. `#emacs%irc.libera.chat' → `#emacs') and prepends `#'
only if the result doesn't already start with one."
    (let* ((node (jabber-jid-username group))
           (label (replace-regexp-in-string "%.*\\'" "" node)))
      (if (string-prefix-p "#" label)
          label
        (concat "#" label))))

  (defun bergheim/jabber-unified-capture-muc (nick group buffer text _title)
    "Capture an incoming MUC message into the unified buffer."
    (bergheim/jabber-unified--append
     (bergheim/jabber-unified--room-label group) nick text buffer :muc group))

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
    "Open the jabber unified buffer."
    (interactive)
    (pop-to-buffer (bergheim/jabber-unified--buffer)))

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
