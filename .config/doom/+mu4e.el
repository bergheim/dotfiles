;;; ~/.config/doom/+mu4e.el -*- lexical-binding: t; -*-

(setq user-mail-address bergheim/email
      user-full-name  bergheim/name
      mu4e-compose-signature bergheim/signature
      mu4e-maildir "~/.mail"
      mu4e-trash-folder "/Deleted Items"
      mu4e-refile-folder "/Archive"
      mu4e-sent-folder "/Sent Items"
      mu4e-drafts-folder "/Drafts"
      mu4e-get-mail-command "mbsync -a"
      mu4e-attachment-dir "~/Downloads/email"
      mu4e-confirm-quit nil
      mu4e-view-show-images t
      mu4e-view-show-addresses t
      ;; this fixes some sync issues with mbsync and office365
      mu4e-change-filenames-when-moving t
      ;; the servers handle this
      mu4e-sent-messages-behavior 'delete
      ;; outlook handles this
      ;; FIXME I am put on CC - look into contexts
      mu4e-compose-dont-reply-to-self t
      ;; display is nicer with these
      mu4e-use-fancy-chars t
      mail-user-agent 'mu4e-user-agent
      ;; don't keep message buffers around
      message-kill-buffer-on-exit t
      mu4e-update-interval 60

      ;; this is insanely annoying. it kills whatever is in the minibuffer
      mu4e-hide-index-messages t

      ;; set up a more ISO timestamp
      ;; mu4e-headers-date-format "%Y-%m-%d %H:%M"

      ;; show overview to left, email to the right
      mu4e-split-view 'vertical
      mu4e-headers-visible-columns 110

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
      mu4e-user-mail-address-list (append `(,bergheim/neptune/email ,bergheim/glvortex/email ,bergheim/gmail/email) bergheim/glvortex/aliases)

      ;; notification settings
      alert-fade-time 20
      ;; mu4e-alert-set-window-urgency nil
      ;; mu4e-alert-interesting-mail-query (concat "flag:unread"
      ;;                                           " AND NOT flag:trashed"
      ;;                                           " AND NOT maildir:"
      ;;                                           "\"/[Gmail].All Mail\"")
      )

(setq mu4e-bookmarks
      '(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
      ("date:today..now" "Today's messages" ?t)
      ("date:7d..now" "Last 7 days" ?w)
      ("mime:image/*" "Messages with images" ?p)
      ("flag:attach" "Messages with attachments" ?a)
      ("maildir:/Inbox" "Inbox" ?i)
      ("maildir:/Sent Items" "Sent" ?s)
      ))

(add-to-list 'mu4e-view-actions
  '("Open in Browser" . mu4e-action-view-in-browser) t)

(setq mu4e-contexts
      (list
       (make-mu4e-context
        :name "work"
        ;; :match-func (lambda (msg)
        ;;               (when msg
        ;;                 (mu4e-message-maildir-matches msg "^/neptune/")))
        :match-func (lambda (msg)
                      (when msg
                        (string-match-p "^/neptune" (mu4e-message-field msg :maildir))))
        :vars `(
                (user-full-name     . ,bergheim/neptune/name)
                (user-mail-address  . ,bergheim/neptune/email )
                (mu4e-compose-signature . ,bergheim/neptune/signature)

                (mu4e-compose-format-flowed . t)

                (mu4e-sent-folder   . "/neptune/Sent Items")
                (mu4e-drafts-folder . "/neptune/Drafts")
                (mu4e-trash-folder  . "/neptune/Deleted Items")
                (mu4e-refile-folder . "/neptune/Archive")

                (mu4e-maildir-shortcuts . ( ("/neptune/Inbox"         . ?i)
                                            ("/neptune/Sent Items"    . ?s)
                                            ("/neptune/Deleted Items" . ?t)
                                            ("/neptune/Drafts"        . ?d)
                                            ("/neptune/Archive"       . ?a)
                                            ))))
       (make-mu4e-context
        :name "public"
        :match-func (lambda (msg)
                      (when msg
                        (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
        :vars `(
                (user-full-name      . ,bergheim/gmail/name)
                (user-mail-address   . ,bergheim/gmail/email)
                (mu4e-compose-signature . ,bergheim/gmail/signature)

                (mu4e-compose-format-flowed . t)

                (mu4e-sent-folder   . "/gmail/[Gmail].Sent Mail")
                (mu4e-trash-folder  . "/gmail/[Gmail].Trash")
                (mu4e-drafts-folder . "/gmail/[Gmail].Drafts")
                (mu4e-refile-folder . "/gmail/[Gmail].Archive")

                (mu4e-maildir-shortcuts . ( ("/gmail/Inbox"            . ?i)
                                            ("/gmail/[Gmail].Sent Mail" . ?s)
                                            ("/gmail/[Gmail].Trash"       . ?t)
                                            ("/gmail/[Gmail].Drafts"    . ?d)
                                            ))))
       (make-mu4e-context
        :name "glvortex"
        :enter-func (lambda () (mu4e-message "Switch to Personal Context"))
        :match-func (lambda (msg)
                      (when msg
                        (string-match-p "^/glvortex" (mu4e-message-field msg :maildir))))
        :vars `(
                (user-full-name     . ,bergheim/glvortex/name)
                (user-mail-address  . ,bergheim/glvortex/email)
                (mu4e-compose-signature . ,bergheim/glvortex/signature)

                (mu4e-compose-format-flowed . t)

                (mu4e-sent-folder   . "/glvortex.net/Sent")
                (mu4e-trash-folder  . "/glvortex.net/Trash")
                (mu4e-drafts-folder . "/glvortex.net/Drafts")
                (mu4e-refile-folder . "/glvortex.net/Archive")

                (mu4e-maildir-shortcuts . ( ("/glvortex.net/Inbox" . ?i)
                                            ("/glvortex.net/Sent"     . ?s)
                                            ("/glvortex.net/Trash"    . ?t)
                                            ("/glvortex.net/Drafts"   . ?d)
                                            ("/glvortex.net/Archive"  . ?a)
                                            ))))
       ))


;; (with-eval-after-load 'mu4e-alert
;;   ;; Enable Desktop notifications
;;   (mu4e-alert-set-default-style 'libnotify))

;; TODO: verify that this works. If so, remove the stuff above
(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

;; this seems nice - had I only had xwidgets support
;; (defun mu4e-action-view-in-browser-webkit (msg)
;;   (let ((url (concat "file://" (mu4e~write-body-to-html msg))))
;;     (xwidget-webkit-browse-url url)))

;; (after! mu4e
;;   :config
;;   (add-to-list 'mu4e-view-actions
;;                '("web browser" . mu4e-action-view-in-browser-webkit)))

  (map!
   (:map (mu4e-headers-mode-map mu4e-view-mode-map)
     ;; I prefer getting asked about what to do with the thread
     :n "T" #'mu4e-headers-mark-thread))
