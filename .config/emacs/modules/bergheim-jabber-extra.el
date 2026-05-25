;;; bergheim-jabber-extra.el --- vertico/marginalia-friendly jabber helpers -*- lexical-binding: t; -*-

;; `jabber-chat-with' calls `jabber-read-jid-completing', which does its own
;; first-keystroke filtering inside the completion table and stops vertico /
;; orderless from doing real substring matching.  This module ships a thin
;; wrapper that hands flat candidate strings to `completing-read', plus an
;; `affixation-function' that draws a presence dot + groups + status text.

;; TODO: invoking `bergheim/jabber-switch' (or its inner completing-read) via
;; M-x prints "ad-Advice-delete-backward-char: Text is read-only" twice and
;; needs 2-3 keypresses to actually fire.  Cause is some pre-existing advice
;; on `delete-backward-char' (evil minibuffer keymap or electric-pair) hitting
;; the read-only consult/vertico prompt prefix.  Diagnose with
;; `toggle-debug-on-error' and inspect the backtrace; meanwhile, binding the
;; command to a key (e.g. `SPC a j SPC') avoids the M-x path entirely.
;;
;; Live channel-name autocomplete in `bergheim/jabber-join' is implemented
;; below via `jabber-disco-get-items'.  Reviewed by codex/claude/gemini —
;; race conditions in dedupe + watchdog fixed; minibuffer refresh pinned to
;; the originating session.  See the stash design note for details.

(require 'cl-lib)

(declare-function jabber-concat-rosters    "jabber-util")
(declare-function jabber-jid-displayname   "jabber-util")
(declare-function jabber-jid-symbol        "jabber-util")
(declare-function jabber-jid-user          "jabber-util")
(declare-function jabber-jid-server        "jabber-util")
(declare-function jabber-chat-with         "jabber-chat")
(declare-function jabber-muc-active-rooms  "jabber-muc")
(declare-function jabber-muc-switch-to     "jabber-muc")
(declare-function jabber-muc-join          "jabber-muc")
(declare-function jabber-muc-joined-p      "jabber-muc")
(declare-function jabber-muc-find-buffer   "jabber-muc")
(declare-function jabber-chat-find-buffer  "jabber-chat")
(declare-function jabber-disco-get-items   "jabber-disco")
(declare-function vertico--exhibit         "vertico")
(declare-function jabber-muc-nickname      "jabber-muc")
(declare-function jabber-muc-read-my-nickname "jabber-muc")
(declare-function jabber-connection-bare-jid "jabber-util")
(declare-function jabber-activity-find-buffer-name "jabber-activity")
(declare-function consult--multi           "consult")
(defvar jabber-buffer-connection)
(defvar jabber-connections)
(defvar jabber-presence-faces)
(defvar jabber-activity-jids)
(defvar jabber-muc-participants)
(defvar jabber-bookmarks)

(defcustom bergheim/jabber-transport-labels
    '(("\\btelegram\\b"   . "Telegram")
         ("\\bsignal\\b"     . "Signal")
         ("\\birc\\b"        . "IRC")
         ("\\bbiboumi\\b"    . "IRC")
         ("\\bslidge\\b"     . "Slidge")
         ("\\bmattermost\\b" . "Mattermost")
         ("\\bdiscord\\b"    . "Discord")
         ("\\bslack\\b"      . "Slack")
         ("\\bwhatsapp\\b"   . "WhatsApp")
         ("\\bmessenger\\b"  . "Messenger")
         ("\\bmatrix\\b"     . "Matrix"))
    "Alist of (REGEXP . LABEL) mapping JID server hostnames to transport names.
First matching entry wins; unmatched servers fall back to the raw hostname."
    :type '(alist :key-type regexp :value-type string)
    :group 'jabber)

(defun bergheim/jabber--transport (jid)
    "Return a friendly transport label for JID's server, or the raw host."
    (let ((host (jabber-jid-server jid)))
        (or (cl-some (lambda (cell)
                         (and (string-match-p (car cell) host) (cdr cell)))
                bergheim/jabber-transport-labels)
            host)))

(defun bergheim/jabber--transport-tag (jid)
    (propertize (format "  %s" (bergheim/jabber--transport jid))
        'face 'shadow))

;; Ride entirely on jabber's activity tracker: bold when JID is in
;; `jabber-activity-jids', no number (jabber doesn't track per-JID
;; counts). `jabber-activity--init' wires the internal hooks and
;; `jabber-activity-clean' on `window-configuration-change-hook'
;; handles automatic clearing when a chat buffer becomes visible.
(declare-function jabber-activity--init "jabber-activity")
(defvar jabber-activity-jids)

(with-eval-after-load 'jabber
    (require 'jabber-activity)
    (jabber-activity--init))

(declare-function jabber-activity-clean "jabber-activity")

(defun bergheim/jabber--candidate-unread-p (cand)
    "Whether candidate string CAND points at a JID with unread activity."
    (let ((jid (get-text-property 0 'bergheim/jabber-jid cand)))
        (and jid (bergheim/jabber--unread-p jid))))

(defun bergheim/jabber--unread-first (cands)
    "Stable-sort CANDS so unread items come first.  Preserves original order
within each unread/read bucket."
    (sort (copy-sequence cands)
        (lambda (a b)
            (and (bergheim/jabber--candidate-unread-p a)
                (not (bergheim/jabber--candidate-unread-p b))))))

(defun bergheim/jabber--unread-p (jid)
    "Non-nil when `jabber-activity-jids' has an entry whose bare form matches JID.
Trusts jabber's activity tracker — `jabber-activity-clean' has already
been invoked from `bergheim/jabber-switch' before candidates are built."
    (let ((bare (jabber-jid-user jid)))
        (and (boundp 'jabber-activity-jids)
            (cl-some (lambda (a) (string= bare (jabber-jid-user a)))
                jabber-activity-jids))))

(defun bergheim/jabber--with-transport (text jid)
    "Append a `— Transport' suffix to TEXT for JID; prepend the unread
marker and bold the LHS when JID has unread activity."
    (let* ((unread (bergheim/jabber--unread-p jid))
              (lhs (if unread
                       (propertize (concat bergheim/jabber-unread-marker " " text)
                           'face 'bold)
                       (concat (make-string (1+ (length bergheim/jabber-unread-marker)) ?\s)
                           text))))
        (concat lhs
            (propertize (format " — %s" (bergheim/jabber--transport jid))
                'face 'shadow))))

(defconst bergheim/jabber--show-rank
    '(("chat"  . 0)
         (""      . 1)
         ("away"  . 2)
         ("xa"    . 3)
         ("dnd"   . 4)
         ("error" . 5))
    "Sort rank by `show' value; nil (offline) is treated as 6.")

(defun bergheim/jabber--rank (sym)
    (or (alist-get (get sym 'show) bergheim/jabber--show-rank nil nil #'equal)
        6))

(defun bergheim/jabber--glyph (show)
    (pcase show
        ("chat"  "●")
        (""      "●")
        ("away"  "◐")
        ("xa"    "◌")
        ("dnd"   "■")
        ("error" "✕")
        (_       "○")))

(defun bergheim/jabber--candidate (sym)
    "Return a completion candidate string for roster JID symbol SYM.
The bare JID is stored as the text property `bergheim/jabber-jid'.
A baked-in transport suffix keeps the gateway visible even when the
annotation column is suppressed (eg. by marginalia not handling
`multi-category')."
    (let* ((jid  (symbol-name sym))
              (name (let ((n (get sym 'name))) (and n (> (length n) 0) n)))
              (text (bergheim/jabber--with-transport (or name jid) jid)))
        (propertize text 'bergheim/jabber-jid jid)))

(defun bergheim/jabber--affix (cands)
    "Affixation function for jabber JID candidates."
    (mapcar
        (lambda (cand)
            (let* ((jid    (or (get-text-property 0 'bergheim/jabber-jid cand) cand))
                      (sym    (jabber-jid-symbol jid))
                      (show   (get sym 'show))
                      (face   (or (cdr (assoc show jabber-presence-faces))
                                  'jabber-roster-user-offline))
                      (groups (get sym 'groups))
                      (status (let ((s (get sym 'status))) (and s (> (length s) 0) s)))
                      (glyph  (propertize (bergheim/jabber--glyph show) 'face face))
                      (suffix
                          (concat
                              (when groups
                                  (propertize (format "  [%s]" (mapconcat #'identity groups ", "))
                                      'face 'completions-annotations))
                              (when status
                                  (propertize (format "  — %s" status)
                                      'face 'completions-annotations))
                              (unless (string= cand jid)
                                  (propertize (format "  %s" jid)
                                      'face 'completions-annotations)))))
                (list (propertize cand 'face face)
                    (concat glyph " ")
                    suffix)))
        cands))

(defun bergheim/jabber--read-jid (prompt)
    "Read a roster JID using a vertico/orderless-friendly completion table.
Returns the bare JID string."
    (unless (bound-and-true-p jabber-connections)
        (user-error "No jabber connection; M-x jabber-connect-all first"))
    (let* ((syms (jabber-concat-rosters)))
        (unless syms (user-error "Roster is empty"))
        (let* ((sorted (sort (copy-sequence syms)
                           (lambda (a b)
                               (let ((ra (bergheim/jabber--rank a))
                                        (rb (bergheim/jabber--rank b)))
                                   (if (= ra rb)
                                       (string-lessp (jabber-jid-displayname a)
                                           (jabber-jid-displayname b))
                                       (< ra rb))))))
                  (cands (mapcar #'bergheim/jabber--candidate sorted))
                  (table
                      (lambda (str pred action)
                          (if (eq action 'metadata)
                              '(metadata
                                   (category            . jabber-jid)
                                   (affixation-function . bergheim/jabber--affix))
                              (complete-with-action action cands str pred))))
                  (chosen (completing-read prompt table nil t)))
            (or (get-text-property 0 'bergheim/jabber-jid chosen) chosen))))

;;;###autoload
(defun bergheim/jabber-chat-with (&optional other-window)
    "Open a chat buffer with a roster contact.
Vertico/orderless-friendly replacement for `jabber-chat-with'.
With prefix arg OTHER-WINDOW, open the buffer in another window."
    (interactive "P")
    (unless (bound-and-true-p jabber-connections)
        (user-error "No jabber connection; M-x jabber-connect-all first"))
    (let ((jid (bergheim/jabber--read-jid "Chat with: "))
             (jc  (or jabber-buffer-connection (car jabber-connections))))
        (jabber-chat-with jc jid other-window)))


;;;; Unified switcher -----------------------------------------------------
;;
;; `bergheim/jabber-switch' uses `consult--multi' so the standard
;; `consult-narrow-key' workflow applies (< a / < c / < r / < b).

(defun bergheim/jabber--connection ()
    (or jabber-buffer-connection (car jabber-connections)))

(defun bergheim/jabber--annot (str)
    "Annotation suffix used for both roster and activity items pointing at JIDs."
    (let* ((jid    (or (get-text-property 0 'bergheim/jabber-jid str) str))
              (sym    (jabber-jid-symbol jid))
              (show   (get sym 'show))
              (face   (or (cdr (assoc show jabber-presence-faces))
                          'jabber-roster-user-offline))
              (glyph  (propertize (bergheim/jabber--glyph show) 'face face))
              (groups (get sym 'groups))
              (status (let ((s (get sym 'status))) (and s (> (length s) 0) s))))
        (concat
            " " glyph
            (when groups
                (propertize (format "  [%s]" (mapconcat #'identity groups ", "))
                    'face 'completions-annotations))
            (when status
                (propertize (format "  — %s" status)
                    'face 'completions-annotations))
            (propertize (format "  %s" jid)
                'face 'completions-annotations))))

(defun bergheim/jabber--roster-items ()
    (let ((syms (sort (copy-sequence (jabber-concat-rosters))
                    (lambda (a b)
                        (let ((ra (bergheim/jabber--rank a))
                                 (rb (bergheim/jabber--rank b)))
                            (if (= ra rb)
                                (string-lessp (jabber-jid-displayname a)
                                    (jabber-jid-displayname b))
                                (< ra rb)))))))
        (bergheim/jabber--unread-first
            (mapcar #'bergheim/jabber--candidate syms))))

(defun bergheim/jabber--channel-kind (jid)
    "Classify MUC JID as one of: channel, group, bot, irc, room.
Telegram (via Slidge) uses local-part prefixes; Biboumi-style IRC
JIDs start with `#' or contain `%irc.'."
    (let ((local (or (jabber-jid-user jid) "")))
        (cond
            ((string-prefix-p "channel-" local) 'channel)
            ((string-prefix-p "group-"   local) 'group)
            ((string-prefix-p "bot-"     local) 'bot)
            ((string-prefix-p "#"        local) 'irc)
            ((string-match-p  "%irc\\."  local) 'irc)
            (t 'room))))

(defcustom bergheim/jabber-unread-marker "●"
    "String prepended to a candidate row when it has unread activity.
Some good alternatives: \"U\", \"●\", \"▎\", \"◆\", \"🔴\", \"❗\"."
    :type 'string
    :group 'jabber)

(defcustom bergheim/jabber-kind-glyphs
    '((channel . "►")
         (group   . "▤")
         (bot     . "⚙")
         (irc     . "#")
         (room    . "◇"))
    "Glyph string per channel kind in the unified switcher.
BMP monochrome characters; replace per environment if any tofu."
    :type '(alist :key-type symbol :value-type string)
    :group 'jabber)

(defun bergheim/jabber--kind-glyph (kind)
    "Glyph + trailing space for KIND, looked up in `bergheim/jabber-kind-glyphs'."
    (concat (or (alist-get kind bergheim/jabber-kind-glyphs)
                (alist-get 'room bergheim/jabber-kind-glyphs)
                "?")
        " "))

(defun bergheim/jabber--bookmark-name (jid)
    "Return the `:name' from any bookmark matching JID, or nil."
    (catch 'found
        (when (hash-table-p jabber-bookmarks)
            (maphash
                (lambda (_account bms)
                    (when (listp bms)
                        (dolist (bm bms)
                            (when (equal (plist-get bm :jid) jid)
                                (let ((n (plist-get bm :name)))
                                    (when (and n (> (length n) 0))
                                        (throw 'found n)))))))
                jabber-bookmarks))
        nil))

(defun bergheim/jabber--room-name (jid)
    "Best-effort human label for room JID.
Preference: bookmark name → stripped Biboumi-style local-part → raw local-part."
    (or (bergheim/jabber--bookmark-name jid)
        (let* ((local (or (jabber-jid-user jid) jid))
                  ;; Biboumi-style IRC: '#channel%irc.libera.chat' → '#channel'
                  (head  (car (split-string local "%" t))))
            (or head local))))

(defun bergheim/jabber--channel-items ()
    (bergheim/jabber--unread-first
        (mapcar (lambda (jid)
                    (let* ((glyph (bergheim/jabber--kind-glyph
                                      (bergheim/jabber--channel-kind jid)))
                              (text  (bergheim/jabber--with-transport
                                         (concat glyph (bergheim/jabber--room-name jid))
                                         jid)))
                        (propertize text 'bergheim/jabber-jid jid)))
            (jabber-muc-active-rooms))))

(defun bergheim/jabber--channel-annot (str)
    (let* ((jid (or (get-text-property 0 'bergheim/jabber-jid str) str))
              (n   (length (cdr (assoc jid jabber-muc-participants)))))
        (concat
            (when (> n 0)
                (propertize (format "  %d participant%s" n (if (= n 1) "" "s"))
                    'face 'completions-annotations))
            (propertize (format "  %s" jid)
                'face 'completions-annotations))))

(defun bergheim/jabber--activity-render (jid)
    "Build an activity candidate for JID, reusing channel or roster format."
    (if (jabber-muc-joined-p jid)
        (let* ((glyph (bergheim/jabber--kind-glyph
                          (bergheim/jabber--channel-kind jid)))
                  (text  (bergheim/jabber--with-transport
                             (concat glyph (bergheim/jabber--room-name jid))
                             jid)))
            (propertize text 'bergheim/jabber-jid jid))
        (bergheim/jabber--candidate (jabber-jid-symbol jid))))

(defun bergheim/jabber--activity-items ()
    "All `jabber-activity-jids' entries, bare-deduplicated and formatted."
    (let ((seen (make-hash-table :test 'equal))
             out)
        (dolist (raw jabber-activity-jids)
            (let ((jid (jabber-jid-user raw)))
                (unless (gethash jid seen)
                    (puthash jid t seen)
                    (push (bergheim/jabber--activity-render jid) out))))
        (nreverse out)))

(defun bergheim/jabber--bookmarked-rooms ()
    "Return list of (cons jid plist) for bookmarks across all accounts.
Already-joined rooms are excluded so they only appear under Channels."
    (let (out)
        (maphash
            (lambda (_account bms)
                (when (listp bms)
                    (dolist (bm bms)
                        (let ((jid (plist-get bm :jid)))
                            (when (and jid (not (jabber-muc-joined-p jid)))
                                (push (cons jid bm) out))))))
            jabber-bookmarks)
        (sort out (lambda (a b) (string-lessp (car a) (car b))))))

(defun bergheim/jabber--bookmark-items ()
    (bergheim/jabber--unread-first
        (mapcar (lambda (cell)
                    (let* ((jid   (car cell))
                              (bm    (cdr cell))
                              (name  (plist-get bm :name))
                              (base  (if (and name (> (length name) 0))
                                         name
                                         (bergheim/jabber--room-name jid)))
                              (glyph (bergheim/jabber--kind-glyph
                                         (bergheim/jabber--channel-kind jid)))
                              (text  (bergheim/jabber--with-transport
                                         (concat glyph base) jid)))
                        (propertize text
                            'bergheim/jabber-jid jid
                            'bergheim/jabber-bookmark bm)))
            (bergheim/jabber--bookmarked-rooms))))

(defun bergheim/jabber--bookmark-annot (str)
    (let* ((jid (or (get-text-property 0 'bergheim/jabber-jid str) str)))
        (propertize (format "  %s" jid)
            'face 'completions-annotations)))

(defun bergheim/jabber--act-chat (str)
    (let ((jid (or (get-text-property 0 'bergheim/jabber-jid str) str)))
        (jabber-chat-with (bergheim/jabber--connection) jid nil)))

(defun bergheim/jabber--act-channel (str)
    (let ((jid (or (get-text-property 0 'bergheim/jabber-jid str) str)))
        (jabber-muc-switch-to jid)))

(defun bergheim/jabber--act-bookmark (str)
    (let* ((jid (or (get-text-property 0 'bergheim/jabber-jid str) str))
              (bm  (get-text-property 0 'bergheim/jabber-bookmark str))
              (jc  (bergheim/jabber--connection))
              (nick (or (and bm (plist-get bm :nick))
                        (jabber-muc-nickname jid jc)
                        (jabber-muc-read-my-nickname jc jid))))
        (jabber-muc-join jc jid nick t)))

(defun bergheim/jabber--act-activity (str)
    "Switch to the chat or room buffer for an activity entry."
    (let* ((jid (or (get-text-property 0 'bergheim/jabber-jid str) str))
              (buf (jabber-activity-find-buffer-name jid)))
        (cond
            ((buffer-live-p buf) (switch-to-buffer buf))
            ((jabber-muc-joined-p jid) (jabber-muc-switch-to jid))
            (t (jabber-chat-with (bergheim/jabber--connection) jid nil)))))

(defvar bergheim/jabber--source-unread
    `(:name     "Unread"
         :narrow   ?u
         :category jabber-jid
         :face     warning
         :items    ,#'bergheim/jabber--activity-items
         :annotate ,#'bergheim/jabber--annot
         :action   ,#'bergheim/jabber--act-activity))

(defvar bergheim/jabber--source-channels
    `(:name     "Channels"
         :narrow   ?c
         :category jabber-channel
         :items    ,#'bergheim/jabber--channel-items
         :annotate ,#'bergheim/jabber--channel-annot
         :action   ,#'bergheim/jabber--act-channel))

(defvar bergheim/jabber--source-roster
    `(:name     "Roster"
         :narrow   ?r
         :category jabber-jid
         :items    ,#'bergheim/jabber--roster-items
         :annotate ,#'bergheim/jabber--annot
         :action   ,#'bergheim/jabber--act-chat))

(defvar bergheim/jabber--source-bookmarks
    `(:name     "Bookmarks"
         :narrow   ?b
         :category jabber-channel
         :items    ,#'bergheim/jabber--bookmark-items
         :annotate ,#'bergheim/jabber--bookmark-annot
         :action   ,#'bergheim/jabber--act-bookmark))

;;;###autoload
(defun bergheim/jabber-switch ()
    "Unified switcher for jabber: activity, channels, roster, bookmarks.
Use `consult-narrow-key' (e.g. `< c') to filter to one source."
    (interactive)
    (unless (bound-and-true-p jabber-connections)
        (user-error "No jabber connection; M-x jabber-connect-all first"))
    (jabber-activity-clean)
    (consult--multi
        (list bergheim/jabber--source-unread
            bergheim/jabber--source-channels
            bergheim/jabber--source-roster
            bergheim/jabber--source-bookmarks)
        :prompt "Jabber: "
        :require-match t
        :sort nil
        :history 'bergheim/jabber-switch-history))

(defvar bergheim/jabber-switch-history nil)


;;;; Unified join ---------------------------------------------------------

(defun bergheim/jabber--known-networks ()
    "Return alist of (LABEL . PLIST) for networks/services we already use.
PLIST keys: :type ('bridged or 'xmpp), :gateway, :network (bridged only).
Bridges using the `name%network@gateway' JID shape (biboumi for IRC,
Matrix bridges, etc.) are labeled by transport detected via
`bergheim/jabber-transport-labels'."
    (let (out)
        (dolist (jid (append (jabber-muc-active-rooms)
                         (mapcar #'car (bergheim/jabber--bookmarked-rooms))))
            (cond
                ;; bridged: name%network@gateway (IRC, Matrix, etc.)
                ((string-match "^[^%@]+%\\([^@]+\\)@\\(.+\\)$" jid)
                    (let* ((net (match-string 1 jid))
                              (gw  (match-string 2 jid))
                              (tr  (bergheim/jabber--transport jid))
                              (label (format "%s  (%s)" net tr)))
                        (cl-pushnew (cons label (list :type 'bridged :gateway gw :network net))
                            out :test #'equal)))
                ;; native: name@host
                ((string-match "^[^%@]+@\\(.+\\)$" jid)
                    (let* ((host (match-string 1 jid))
                              (label (format "%s  (XMPP)" host)))
                        (cl-pushnew (cons label (list :type 'xmpp :gateway host))
                            out :test #'equal)))))
        (sort out (lambda (a b) (string-lessp (car a) (car b))))))

(defun bergheim/jabber--rooms-on-network (info)
    "Local-part list of rooms already known on the network described by INFO."
    (let ((type (plist-get info :type))
             (gw   (plist-get info :gateway))
             (net  (plist-get info :network))
             rooms)
        (dolist (jid (append (jabber-muc-active-rooms)
                         (mapcar #'car (bergheim/jabber--bookmarked-rooms))))
            (cond
                ((and (eq type 'bridged)
                     (string-match (format "^\\([^%%]+\\)%%%s@%s$"
                                       (regexp-quote net) (regexp-quote gw))
                         jid))
                    (push (match-string 1 jid) rooms))
                ((and (eq type 'xmpp)
                     (string-match (format "^\\([^@]+\\)@%s$" (regexp-quote gw)) jid))
                    (push (match-string 1 jid) rooms))))
        (sort (delete-dups rooms) #'string-lessp)))

;;;; Live channel discovery (disco#items) -------------------------------

(defcustom bergheim/jabber-disco-cache-ttl 600
    "Cache TTL in seconds for our disco#items channel lookups.
Positive value: entries expire after this many seconds.  Zero or
negative: our TTL layer is disabled and every completion re-fires
the query (jabber's own permanent disco cache still serves the
result instantly, so the cost is just one extra dispatch)."
    :type 'integer
    :group 'jabber)

(defvar bergheim/jabber--disco-cache (make-hash-table :test 'equal)
    "Hash: disco-target JID -> (ROOMS . TIMESTAMP).
Layered on top of jabber's own `jabber-disco-items-cache' so we can
expire entries on our own TTL.")

(defvar bergheim/jabber--disco-in-flight (make-hash-table :test 'equal)
    "Hash: disco target -> list of pending ON-RESULT callbacks.
A target with an entry has an outstanding query; concurrent fetches
append their callbacks instead of re-firing.  All queued callbacks
fire (in arbitrary order) when the result arrives.")

(defun bergheim/jabber--disco-target (info)
    "Construct the disco#items target JID for a network INFO plist.

For bridged services (biboumi IRC, Matrix bridges) the target is
NETWORK@GATEWAY (e.g. `irc.libera.chat@irc.xmpp.glvortex.net').
For native XMPP MUC conferences the target is the conference host
itself."
    (pcase (plist-get info :type)
        ('bridged (format "%s@%s"
                      (plist-get info :network)
                      (plist-get info :gateway)))
        ('xmpp    (plist-get info :gateway))))

(defun bergheim/jabber--disco-cached (target)
    "Return cached disco results for TARGET, or nil if missing/expired.
A non-positive `bergheim/jabber-disco-cache-ttl' always returns nil
so callers re-fire (jabber's own cache layer absorbs the cost)."
    (let ((entry (gethash target bergheim/jabber--disco-cache)))
        (when (and entry
                  (> bergheim/jabber-disco-cache-ttl 0)
                  (< (- (float-time) (cdr entry))
                      bergheim/jabber-disco-cache-ttl))
            (car entry))))

(defcustom bergheim/jabber-disco-fetch-timeout 30
    "Seconds after which a stuck in-flight disco query is cleared.
Guards against jabber.el dropping a callback (connection torn down
mid-flight, internal error) which would otherwise leave the target
permanently pinned and block future fetches."
    :type 'integer
    :group 'jabber)

(defun bergheim/jabber--disco-fetch (target on-result)
    "Fire a disco#items query at TARGET and call ON-RESULT with the items.
Returns immediately.  Concurrent calls for the same TARGET queue
their callbacks; all fire once the response arrives.  Errors are
swallowed and produce no callback.  A per-fetch watchdog clears the
in-flight flag after `bergheim/jabber-disco-fetch-timeout' seconds,
and a SETTLED flag guards against the race where a late response
would clobber a subsequent fetch for the same target."
    (let ((queued (gethash target bergheim/jabber--disco-in-flight)))
        (cond
            (queued
                (when on-result
                    (puthash target (cons on-result queued)
                        bergheim/jabber--disco-in-flight)))
            (t
                (puthash target (if on-result (list on-result) '())
                    bergheim/jabber--disco-in-flight)
                (let ((settled nil))
                    (run-at-time
                        bergheim/jabber-disco-fetch-timeout nil
                        (lambda ()
                            (unless settled
                                (setq settled t)
                                (remhash target bergheim/jabber--disco-in-flight))))
                    (jabber-disco-get-items
                        (bergheim/jabber--connection)
                        target nil
                        (lambda (_jc _data result)
                            (unless settled
                                (setq settled t)
                                (let ((callbacks (gethash target bergheim/jabber--disco-in-flight)))
                                    (remhash target bergheim/jabber--disco-in-flight)
                                    (when (and result
                                              (listp result)
                                              (not (eq (car-safe result) 'error)))
                                        (puthash target (cons result (float-time))
                                            bergheim/jabber--disco-cache)
                                        (dolist (cb callbacks)
                                            (when cb (funcall cb result)))))))
                        nil))))))

(defun bergheim/jabber--disco-item-channel (item type)
    "Pull a channel name out of disco ITEM, a [name jid node] vector.
TYPE is `bridged' or `xmpp' and controls how the local-part is split.
Returns nil for malformed items, non-string JIDs, or JIDs lacking
a local part (bare-domain JIDs like `muc.example.com')."
    (when (and (vectorp item) (>= (length item) 2))
        (let ((jid (aref item 1)))
            (when (and jid (stringp jid))
                (let ((local (jabber-jid-user jid)))
                    (when (and local (stringp local) (not (string-empty-p local)))
                        (pcase type
                            ('bridged (car (split-string local "%" t)))
                            (_        local))))))))

(defun bergheim/jabber--channel-candidates (info known)
    "Return sorted, deduplicated channel candidates for network INFO.
Always includes KNOWN local-part strings, plus any cached disco
results.  Fires a background fetch and schedules a vertico refresh
when results arrive."
    (let* ((target (bergheim/jabber--disco-target info))
              (type   (plist-get info :type))
              (cached (bergheim/jabber--disco-cached target))
              (disco  (delq nil
                          (mapcar (lambda (it)
                                      (bergheim/jabber--disco-item-channel it type))
                              cached))))
        (unless cached
            (let ((mb (and (active-minibuffer-window)
                          (window-buffer (active-minibuffer-window)))))
                (bergheim/jabber--disco-fetch
                    target
                    (lambda (_)
                        (when (and mb (buffer-live-p mb) (fboundp 'vertico--exhibit))
                            (run-at-time
                                0 nil
                                (lambda ()
                                    ;; Refresh only if the originating minibuffer is still the
                                    ;; live, selected one (user hasn't dismissed/opened another).
                                    (when (and (buffer-live-p mb)
                                              (active-minibuffer-window)
                                              (eq mb (window-buffer (active-minibuffer-window))))
                                        (with-current-buffer mb
                                            (ignore-errors (vertico--exhibit))))))))))
            )
        (sort (delete-dups (append known disco)) #'string-lessp)))

(defun bergheim/jabber--channel-table (info known)
    "Build a dynamic completion table for channel names on network INFO."
    (lambda (string pred action)
        (cond
            ((eq action 'metadata)
                '(metadata (category . jabber-channel-name)))
            (t
                (complete-with-action
                    action (bergheim/jabber--channel-candidates info known) string pred)))))

;;;###autoload
(defun bergheim/jabber-join ()
    "Join a MUC room on a known network/service.
Pick a network (detected from already-joined rooms + bookmarks),
then type or pick a channel name.  The gateway-encoded JID is
built automatically so you never have to type
`#emacs%irc.libera.chat@irc.example.org' by hand."
    (interactive)
    (unless (bound-and-true-p jabber-connections)
        (user-error "No jabber connection; M-x jabber-connect-all first"))
    (let ((networks (bergheim/jabber--known-networks)))
        (unless networks
            (user-error "No known networks yet; join one channel manually first"))
        (let* ((label  (completing-read "Network: " networks nil t))
                  (info   (cdr (assoc label networks)))
                  (known  (bergheim/jabber--rooms-on-network info))
                  (prefix (if (eq (plist-get info :type) 'bridged) "#" ""))
                  (chan   (completing-read
                              "Channel: "
                              (bergheim/jabber--channel-table info known)
                              nil nil prefix))
                  (_      (when (string-empty-p chan) (user-error "No channel given")))
                  (jid    (pcase (plist-get info :type)
                              ('bridged (format "%s%%%s@%s" chan
                                            (plist-get info :network)
                                            (plist-get info :gateway)))
                              ('xmpp    (format "%s@%s" chan (plist-get info :gateway)))))
                  (jc     (bergheim/jabber--connection))
                  (nick   (or (jabber-muc-nickname jid jc)
                              (jabber-muc-read-my-nickname jc jid))))
            (jabber-muc-join jc jid nick t))))

(provide 'bergheim-jabber-extra)
;;; bergheim-jabber-extra.el ends here
