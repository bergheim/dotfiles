;;; settings.el --- Description -*- lexical-binding: t; -*-

;; open mu4e in the current window instead of current frame
(add-to-list 'display-buffer-alist
             `(,(regexp-quote mu4e-main-buffer-name)
               display-buffer-same-window))

(setq user-mail-address bergheim/email
      user-full-name  bergheim/name
      mu4e-compose-signature bergheim/signature

      mu4e-get-mail-command "sync-mail"
      mu4e-attachment-dir "~/Downloads/email"
      mu4e-org-contacts-file (expand-file-name "contacts.org" org-directory)
      mu4e-confirm-quit nil

      ;; this fixes some sync issues with mbsync and office365
      mu4e-change-filenames-when-moving t
      ;; the servers handle this
      mu4e-sent-messages-behavior 'delete
      ;; the servers handle this
      mu4e-compose-dont-reply-to-self t
      ;; display is nicer with these. in theory. in practice, alignme
      ;; nt is ;; messed up
      mu4e-use-fancy-chars t

      mail-user-agent 'mu4e-user-agent
      ;; don't keep message buffers around
      message-kill-buffer-on-exit t

      ;; do not fetch mail in the background (handled by the system now)
      mu4e-update-interval nil

      ;; this is insanely annoying. it kills whatever is in the minibuffer
      mu4e-hide-index-messages t

      mu4e-headers-include-related nil

      ;; include threads in search results. we might not get the "correct"
      ;; email from the thread anyway, as quotes and replies messes the matches
      ;; up a bit and we only get one email from the thread if so
      mu4e-search-threads t
      ;; mu4e-headers-sort-direction 'ascending

      ;; set up a more concise timestamp
      mu4e-headers-date-format "%d/%m/%y %H:%M"
      mu4e-headers-time-format "%H:%M"

      ;; and make room for the subject
      mu4e-headers-fields '((:account      .  8)
                            (:human-date   . 14)
                            (:flags        .  6)
                            (:shortened-maildir     .  8)
                            ;; TODO: find a way to toggle this - it is sometimes useful
                            ;; (:mailing-list . 10)
                            (:from         . 20)
                            (:subject))

      ;; show overview to left, email to the right
      mu4e-split-view 'horizontal
      mu4e-headers-visible-columns 110
      mu4e-headers-visible-lines 8

      ;; SMTP stuff
      send-mail-function 'sendmail-send-it
      message-send-mail-function 'message-send-mail-with-sendmail
      ;; substitute sendmail with msmtp
      sendmail-program "msmtp"
      ;; allow setting account through email header
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-sendmail-f-is-evil t

      ;; figure out the account to reply from based on addresses
      mu4e-context-policy 'pick-first
      mu4e-compose-context-policy 'ask

      ;; this makes html emails easier to read. in theory.
      shr-color-visible-luminance-min 80

      ;; well that was naive. kill colors!
      shr-use-colors nil

      ;; notification settings
      ;; TODO: move this to the sync script?
      ;; mu4e-alert-set-window-urgency nil
      ;; mu4e-alert-interesting-mail-query (concat "flag:unread"
      ;;                                           " AND NOT flag:trashed"
      ;;                                           " AND NOT maildir:"
      ;;                                           "\"/[Gmail].All Mail\"")
      )

(setq mu4e-bookmarks
      `((:name  "Unread messages"
         :query "flag:unread AND NOT (flag:trashed OR maildir:/Trash/)"
         :key ?u)

        (:name  "Inbox work"
         :query "maildir:/neptune/Inbox"
         :key ?n)

        (:name  "Inbox work unread"
         :query "flag:unread AND maildir:/neptune/Inbox"
         :key ?N)

        (:name  "Inbox glvortex"
         :query "maildir:/glvortex/Inbox"
         :key ?g)

        (:name  "Inbox glvortex unread"
         :query "flag:unread AND maildir:/glvortex/Inbox"
         :key ?G)

        (:name  "Inbox gmail"
         :query "maildir:/gmail/Inbox"
         :key ?q)

        (:name  "Inbox gmail unread"
         :query "flag:unread AND maildir:/gmail/Inbox"
         :key ?Q)

        (:name  "Support"
         :query ,(concat "to:" bergheim/neptune/email " AND from:" bergheim/neptune/support " AND maildir:/neptune/Inbox")
         :key ?s)

        ;; TODO does not work
        (:name "Today's messages"
         :query "maildir:/Inbox/ AND (date:1d..now)"
         :key ?t)

        (:name "Today's unhandled messages"
         :query "maildir:/Inbox/ AND (flag:unread OR date:1d..now)"
         :key ?T)

        (:name "Last week"
         :query "maildir:/Inbox/ AND date:1w..now"
         :key ?W)

        (:name "Recent personal messages"
         :query "(maildir:/gmail/Inbox/ OR maildir:/glvortex/Inbox) AND (flag:unread OR date:1w..now)"
         :key ?T)

        (:name "Messages with images"
         ;; everybody has some huge image in their sig. sigh..
         :query "mime:image/* AND size:100K..100M"
         :key ?i)

        (:name "Messages with attachments"
         ;; everybody has some huge image in their sig. sigh..
         :query "flag:attach AND NOT mime:application/ics AND NOT ((mime:image/jpg OR mime:image/jpeg OR mime:image/png) AND size:0..2m)"
         :key ?a)

        (:name "Trash"
         :query "maildir:/Trash/ OR flag:trashed"
         :key ?x)

        (:name "Economy"
         :query ,(concat "maildir:/Inbox/ AND contact:/.*" bergheim/mu4e/economy "$/")
         :key ?e)

        (:name "Focus inbox"
         :query "flag:flagged OR (maildir:/Inbox/ AND flag:unread AND date:1w..now)"
         :key ?I)

        (:name "Sent items"
         :query "maildir:/Sent/"
         :key ?S)))

;;; settings.el ends here
