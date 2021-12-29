;;; ~/.config/doom/+mu4e.el -*- lexical-binding: t; -*-

(defun bergheim-mu4e-narrow-this-sender (_)
  "Quickly narrow view to emails sent from the selected email"

  ;; TODO: get this to work. msg contains :from! No need to use message field at point..
  ;; (print msg)
  ;; (mu4e-headers-search-narrow (concat "from:" (cdr (first (msg :from))))))
  (mu4e-headers-search-narrow (concat "from:" (cdr (car (mu4e-message-field-at-point :from))))))

(defun bergheim-mu4e-relate-this-message (msg)
  "Quickly find all mails sent to or from this address"

  (let (email)
    (setq email (cdr (car (mu4e-message-field-at-point :from))))
    (if (equal current-prefix-arg nil) ; no C-u
        (setq query-string "NOT maildir:/Trash/ AND (from:%s or to:%s)")
        (setq query-string "(from:%s or to:%s)"))

    (let ((msgid (mu4e-message-field msg :message-id)))
      (when msgid
        (mu4e-headers-search
         (format query-string email email)
         nil nil nil
         msgid (and (eq major-mode 'mu4e-view-mode)
                    (not (eq mu4e-split-view 'single-window))))))))

(defun bergheim-mu4e-read-later (msg)
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "el"))

(defun bergheim-mu4e-follow-up (msg)
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "ef"))

(defun bergheim-mu4e-store-link-to-query ()
  (interactive)
  (let ((org-mu4e-link-query-in-headers-mode t))
    (call-interactively 'org-store-link)))

;; This makes mu4e buffers more prominent. Read doom-real-buffer-p
;; (add-hook 'mu4e-headers-mode-hook #'doom-mark-buffer-as-real-h)
;; (add-hook 'mu4e-view-mode-hook #'doom-mark-buffer-as-real-h)

(setq user-mail-address bergheim/email
      user-full-name  bergheim/name
      mu4e-compose-signature bergheim/signature
      mu4e-get-mail-command "mbsync inbox"
      mu4e-attachment-dir "~/Downloads/email"
      mu4e-confirm-quit nil

      ;; TODO confirm that these are only local images (trackers etc)
      mu4e-view-show-images nil
      mu4e-view-show-addresses t
      ;; this fixes some sync issues with mbsync and office365
      mu4e-change-filenames-when-moving t
      ;; the servers handle this
      mu4e-sent-messages-behavior 'delete
      ;; outlook handles this
      ;; FIXME I am put on CC - look into contexts
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
      mu4e-headers-show-threads nil

      ;; set up a more concise timestamp
      mu4e-headers-date-format "%d/%m/%y %H:%M"
      mu4e-headers-time-format "%H:%M"

      ;; and make room for the subject
      mu4e-headers-fields '((:account      .  8)
                            (:human-date   . 14)
                            (:flags        .  4)
                            (:maildir      .  26)
                            ;; (:mailing-list . 10)
                            (:from         . 25)
                            (:subject      ))

      ;; show overview to left, email to the right
      ;; mu4e-split-view 'single-window
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

      ;; notification settings
      ;; mu4e-alert-set-window-urgency nil
      ;; mu4e-alert-interesting-mail-query (concat "flag:unread"
      ;;                                           " AND NOT flag:trashed"
      ;;                                           " AND NOT maildir:"
      ;;                                           "\"/[Gmail].All Mail\"")
      )

(setq mu4e-bookmarks
      '((:name  "Unread messages"
                :query "flag:unread AND NOT flag:trashed"
                :key ?u)

        (:name  "Inbox work"
                :query "maildir:/neptune/Inbox"
                :key ?n)

        (:name  "Inbox work unread"
                :query "maildir:/neptune/Inbox AND flag:unread"
                :key ?N)

        (:name  "Inbox glvortex"
                :query "maildir:/glvortex/Inbox"
                :key ?g)

        (:name  "Inbox glvortex unread"
                :query "maildir:/glvortex/Inbox AND flag:unread"
                :key ?G)

        (:name  "Inbox gmail"
                :query "maildir:/gmail/Inbox"
                :key ?q)

        (:name  "Inbox gmail unread"
                :query "maildir:/gmail/Inbox AND flag:unread"
                :key ?Q)

        (:name  "Support"
                :query "to:thomas.bergheim@neptune-software.com AND from:no-reply@neptune-software.com AND maildir:/neptune/Inbox"
                :key ?s)

        (:name "Today's messages"
               :query "date:1d..now"
               :key ?t)

        (:name "Today's unhandled messages"
               :query "date:1d..now AND maildir:/Inbox/ AND flag:unread"
               :key ?i)

        (:name "Last 7 days"
               :query "date:7d..now AND maildir:/Inbox/"
               :key ?w)

        (:name "Messages with images"
               ;; everybody has some huge image in their sig. sigh..
               :query "mime:image/* AND size:50K..100M"
               :key ?p)

        (:name "Messages with attachments"
               ;; everybody has some huge image in their sig. sigh..
               :query "flag:attach AND size:50K..1000M"
               :key ?a)

        (:name "All sent items"
               :query "maildir:/Sent/"
               :key ?o)))

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

                (mu4e-sent-folder   . "/neptune/Sent")
                (mu4e-trash-folder  . "/neptune/Trash")
                (mu4e-drafts-folder . "/neptune/Drafts")
                (mu4e-refile-folder . "/neptune/Archive")

                (mu4e-maildir-shortcuts . ( ("/neptune/Inbox"         . ?i)
                                            ("/neptune/Sent"    . ?s)
                                            ("/neptune/Trash" . ?t)
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

                (mu4e-sent-folder   . "/gmail/Sent")
                (mu4e-trash-folder  . "/gmail/Trash")
                (mu4e-drafts-folder . "/gmail/Drafts")
                (mu4e-refile-folder . "/gmail/Archives")

                (mu4e-maildir-shortcuts . ( ("/gmail/Inbox"            . ?i)
                                            ("/gmail/Sent" . ?s)
                                            ("/gmail/Trash"       . ?t)
                                            ("/gmail/Drafts"    . ?d)
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

                (mu4e-sent-folder   . "/glvortex/Sent")
                (mu4e-trash-folder  . "/glvortex/Trash")
                (mu4e-drafts-folder . "/glvortex/Drafts")
                (mu4e-refile-folder . "/glvortex/Archive")

                (mu4e-maildir-shortcuts . ( ("/glvortex/Inbox" . ?i)
                                            ("/glvortex/Sent"     . ?s)
                                            ("/glvortex/Trash"    . ?t)
                                            ("/glvortex/Drafts"   . ?d)
                                            ("/glvortex/Archive"  . ?a)
                                            ))))
       ))

;; this seems nice - requires xwidgets support
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

(add-to-list 'mu4e-headers-actions
             '("find all from" . bergheim-mu4e-relate-this-message) t)

(add-to-list 'mu4e-headers-actions
             '("narrow to sender" . bergheim-mu4e-narrow-this-sender) t)

(add-to-list 'mu4e-headers-actions
             '("Follow up" . bergheim-mu4e-follow-up) t)

(add-to-list 'mu4e-headers-actions
             '("read later" . bergheim-mu4e-read-later) t)

;; (add-to-list 'mu4e-view-actions '("Eww view" . jcs-view-in-eww) t)

;; based on https://github.com/djcb/mu/issues/1136#issuecomment-486177435
(setf (alist-get 'trash mu4e-marks)
      (list :char '("d" . "▼")
            :prompt "dtrash"
            :dyn-target (lambda (target msg)
                          (mu4e-get-trash-folder msg))
            :action (lambda (docid msg target)
                      ;; Here's the main difference to the regular trash mark,
                      ;; no +T before -N so the message is not marked as
                      ;; IMAP-deleted:
                      (mu4e~proc-move docid (mu4e~mark-check-target target) "+S-u-N"))))
