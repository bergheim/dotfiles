;;; ~/.config/doom/+mu4e.el -*- lexical-binding: t; -*-

(defun bergheim/mu4e-narrow-to-sender (_)
  "Quickly narrow view to emails sent from the selected email"

  (mu4e-headers-search-narrow (concat "from:" (plist-get (car (mu4e-message-field-at-point :from)) :email))))

(defun bergheim/utils--get-domain (email)
  "Get the main domain of an email address"
  (let* ((domain (cadr (split-string email "@")))
        (parts (reverse (split-string domain "\\."))))

    (unless current-prefix-arg
        domain
      (format "%s.%s" (nth 1 parts) (nth 0 parts)))))

(defun bergheim/mu4e-search-from-domain (msg &optional inbox-only)
  "Quickly find all mails sent to or from this domain"

  (let* ((email (plist-get (car (mu4e-message-field-at-point :from)) :email))
        (msgid (mu4e-message-field msg :message-id))
        (domain (bergheim/utils--get-domain email))
        (query-string "(from:/.*%s$/ or to:/.*%s$/)")
        (maildir-filter))

    (if inbox-only
        (setq maildir-filter "maildir:/Inbox/")
      (setq maildir-filter "NOT maildir:/Trash/"))

    (setq query-string (concat maildir-filter " AND " query-string))

    (mu4e-headers-search
     (format query-string domain domain)
     nil nil nil
     msgid (and (eq major-mode 'mu4e-view-mode)
                (not (eq mu4e-split-view 'single-window))))))

(defun bergheim/mu4e-search-inbox-from-domain (msg)
  "Quickly find all INBOX mails sent to or from this domain"
  (bergheim/mu4e-search-from-domain msg t))

(defun bergheim/mu4e-search-from-address (msg)
  "Quickly find all mails sent to or from this address"

  (let ((email (plist-get (car (mu4e-message-field-at-point :from)) :email))
        (msgid (mu4e-message-field msg :message-id))
        (query-string "(from:%s or to:%s)"))

    (unless current-prefix-arg
        (setq query-string (concat "NOT maildir:/Trash/ AND " query-string)))

    (mu4e-headers-search
     (format query-string email email)
     nil nil nil
     msgid (and (eq major-mode 'mu4e-view-mode)
                (not (eq mu4e-split-view 'single-window))))))

(defun bergheim/mu4e-search-this-subject (msg)
  "Quickly find all mails sent to or from this address

Strips away subject action prefixes and special characters to capture more emails.

If \\[universal-argument\] is called before this, include the trash."

  (let ((subject (mu4e-message-field msg :subject))
        (msgid (mu4e-message-field msg :message-id))
        (query-string "%s"))

    (unless current-prefix-arg
        (setq query-string (concat "NOT maildir:/Trash/ AND " query-string)))

    ;; remove any "Re: ", "Fwd: " etc
    (setq subject (replace-regexp-in-string "^\\(\\ca\\{2,3\\}: ?\\)+" "" subject))

    ;; remove characters that make mu unhappy
    (setq subject (replace-regexp-in-string "\\W" " " subject))

    (setq subject (s-trim subject))

    (mu4e-headers-search
     (format query-string subject)
     nil nil nil
     msgid (and (eq major-mode 'mu4e-view-mode)
                (not (eq mu4e-split-view 'single-window))))))


(defun bergheim/mu4e-search-to-me (msg)
  "Quickly find all mails sent to me from this address.

Includes BCC emails, but does not include CC, because that point just use from:address"

  ;; TODO: consider using `mu4e-message-contact-field-matches-me'
  (let* ((from (plist-get (car (mu4e-message-field-at-point :from)) :email))
         (maildir (mu4e-message-field msg :maildir))
         (msgid (mu4e-message-field msg :message-id))
         (my-email (bergheim/mu4e--get-account-email maildir))
         (query-string (format "(from:%s AND (to:%s OR NOT to:*)" from my-email)))

    (unless current-prefix-arg
        (setq query-string (concat "NOT maildir:/Trash/ AND " query-string)))

    (mu4e-headers-search
     query-string
     nil nil nil
     msgid (and (eq major-mode 'mu4e-view-mode)
                (not (eq mu4e-split-view 'single-window))))))

(defun bergheim/mu4e--get-account (maildir)
  (when (string-match "^/\\(\\w+\\)/" maildir)
    (match-string 1 maildir)))

(defun bergheim/mu4e--get-account-email (maildir)
  (let ((account (bergheim/mu4e--get-account maildir)))
    (pcase account
        ("neptune" bergheim/neptune/email)
        ("gmail" bergheim/gmail/email)
        ("glvortex" bergheim/glvortex/email))))

(defun bergheim/mu4e-open-message-in-webclient (msg)
  (let* ((maildir (mu4e-message-field msg :maildir))
         (msgid (mu4e-message-field msg :message-id))
         (subject (mu4e-message-field msg :subject))
         (to (cdar (mu4e-message-field msg :to)))
         (from (cdar (mu4e-message-field msg :from)))
         (account (bergheim/mu4e--get-account maildir)))

    (pcase account
      ("neptune" (let ((query (format "SUBJECT:(%s) %s %s" subject to from)))
                   ;; ms could not care less about standards, as always. you
                   ;; can't even produce a link to a search, let alone to an
                   ;; email without crawling through their enterprise API
                   ;; "offerings". I am amazed this even works over IMAP
                   ;; so uh, paste this into the searchbox in outlook I guess
                   (kill-new query)
                   (browse-url "https://outlook.office.com/mail/")))
      ("gmail"
       (let ((url (concat "https://mail.google.com/mail/u/0/#search/rfc822msgid%3A"
                          (url-encode-url msgid))))
         (start-process "" nil "chromium" url)))
      ("glvortex"
       ;; I am not seing any Message-Id or anything else in the headers on
       ;; ProtonMail - I assume they have filtered it
       (browse-url (format "https://mail.proton.me/u/0/all-mail#keyword=%s&from=%s&to=%s"
                ;; protonmail does not allow searches for [ ] etc so strip them
                (url-encode-url (replace-regexp-in-string "\\W" " " subject))
                (url-encode-url from)
                (url-encode-url to))))
      (_ (display-warning :warning (format "Account \"%s\" not found!" account))))))

(defun bergheim/mu4e-read-later (msg)
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "el")
  (if (eq major-mode 'mu4e-headers-mode)
      (mu4e-headers-mark-for-refile)
    (mu4e-view-mark-for-refile)))

(defun bergheim/mu4e-follow-up (msg)
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "ef")
  (if (eq major-mode 'mu4e-headers-mode)
      (mu4e-headers-mark-for-refile)
    (mu4e-view-mark-for-refile)))

(defun bergheim/mu4e-store-link-to-query ()
  (interactive)
  (let ((mu4e-org-link-query-in-headers-mode t))
    (call-interactively 'org-store-link)))

;; Don't bury mu4e buffers
(add-hook 'mu4e-headers-mode-hook #'doom-mark-buffer-as-real-h)
(add-hook 'mu4e-view-mode-hook #'doom-mark-buffer-as-real-h)

(use-package! org-msg
  :init
  (setq org-msg-greeting-fmt "Hello%s,\n\n"
        org-msg-signature bergheim/signature-html
        org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil tex:dvipng \\n:t"))

(setq user-mail-address bergheim/email
      user-full-name  bergheim/name
      mu4e-compose-signature bergheim/signature
      mu4e-get-mail-command "sync-mail"
      mu4e-attachment-dir "~/Downloads/email"
      mu4e-confirm-quit nil

      mu4e-view-show-images t
      mu4e-view-show-addresses t
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
      mu4e-headers-show-threads nil

      ;; set up a more concise timestamp
      mu4e-headers-date-format "%d/%m/%y %H:%M"
      mu4e-headers-time-format "%H:%M"

      ;; and make room for the subject
      mu4e-headers-fields '((:account      .  8)
                            (:human-date   . 14)
                            (:flags        .  4)
                            (:maildir      .  17)
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
      '((:name  "Unread messages"
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
                :query (concat "to:" bergheim/neptune/email " AND from:" bergheim/neptune/support " AND maildir:/neptune/Inbox")
                :key ?s)

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
               :query "mime:image/* AND size:50K..100M"
               :key ?i)

        (:name "Messages with attachments"
               ;; everybody has some huge image in their sig. sigh..
               :query "flag:attach AND size:50K..1000M"
               :key ?a)

        (:name "Trash"
               :query "maildir:/Trash/ OR flag:trashed"
               :key ?x)

        (:name "Focus inbox"
               :query "flag:flagged OR (maildir:/Inbox/ AND flag:unread AND date:1w..now)"
               :key ?I)

        (:name "Sent items"
               :query "maildir:/Sent/"
               :key ?S)))

(setq mu4e-contexts
      (list
       (make-mu4e-context
        :name "work"
        :match-func (lambda (msg)
                      (when msg
                        (string-match-p "^/neptune" (mu4e-message-field msg :maildir))))
        :vars `(
                (user-full-name     . ,bergheim/neptune/name)
                (user-mail-address  . ,bergheim/neptune/email )
                (mu4e-compose-signature . ,bergheim/neptune/signature)
                (org-msg-signature . ,bergheim/neptune/signature-html)

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
        :name "gmail"
        :match-func (lambda (msg)
                      (when msg
                        (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
        :vars `(
                (user-full-name      . ,bergheim/gmail/name)
                (user-mail-address   . ,bergheim/gmail/email)
                (mu4e-compose-signature . ,bergheim/gmail/signature)
                (org-msg-signature . ,bergheim/gmail/signature-html)

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
        :name "private"
        :enter-func (lambda () (mu4e-message "Switch to Personal Context"))
        :match-func (lambda (msg)
                      (when msg
                        (string-match-p "^/glvortex" (mu4e-message-field msg :maildir))))
        :vars `(
                (user-full-name     . ,bergheim/glvortex/name)
                (user-mail-address  . ,bergheim/glvortex/email)
                (mu4e-compose-signature . ,bergheim/glvortex/signature)
                (org-msg-signature . ,bergheim/glvortex/signature-html)

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

(setq mu4e-headers-actions (delete '("show this thread" . mu4e-action-show-thread) mu4e-headers-actions))
(add-to-list 'mu4e-headers-actions '("narrow to sender" . bergheim/mu4e-narrow-to-sender) t)
(add-to-list 'mu4e-headers-actions '("follow up" . bergheim/mu4e-follow-up) t)
(add-to-list 'mu4e-headers-actions '("later" . bergheim/mu4e-read-later) t)
(add-to-list 'mu4e-headers-actions '("browser" . bergheim/mu4e-open-message-in-webclient) t)
(add-to-list 'mu4e-headers-actions '("email" . bergheim/mu4e-search-from-address) t)
(add-to-list 'mu4e-headers-actions '("Domain" . bergheim/mu4e-search-from-domain) t)
(add-to-list 'mu4e-headers-actions '("domain inbox" . bergheim/mu4e-search-inbox-from-domain) t)
(add-to-list 'mu4e-headers-actions '("me" . bergheim/mu4e-search-to-me) t)
(add-to-list 'mu4e-headers-actions '("subject" . bergheim/mu4e-search-this-subject) t)
(add-to-list 'mu4e-headers-actions '("thread" . mu4e-action-show-thread) t)

(setq mu4e-view-actions (delete '("View in browser" . mu4e-action-view-in-browser) mu4e-view-actions))
(setq mu4e-view-actions (delete '("show this thread" . mu4e-action-show-thread) mu4e-view-actions))
(add-to-list 'mu4e-view-actions '("follow up" . bergheim/mu4e-follow-up) t)
(add-to-list 'mu4e-view-actions '("later" . bergheim/mu4e-read-later) t)
(add-to-list 'mu4e-view-actions '("browser" . bergheim/mu4e-open-message-in-webclient) t)
(add-to-list 'mu4e-view-actions '("email" . bergheim/mu4e-search-from-address) t)
(add-to-list 'mu4e-view-actions '("Domain" . bergheim/mu4e-search-from-domain) t)
(add-to-list 'mu4e-view-actions '("domain inbox" . bergheim/mu4e-search-inbox-from-domain) t)
(add-to-list 'mu4e-view-actions '("me" . bergheim/mu4e-search-to-me) t)
(add-to-list 'mu4e-view-actions '("subject" . bergheim/mu4e-search-this-subject) t)
(add-to-list 'mu4e-view-actions '("thread" . mu4e-action-show-thread) t)

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
                      (mu4e--server-move docid (mu4e--mark-check-target target) "+S-u-N"))))

(defun bergheim/org-subtree-to-mu4e ()
  "Send the current subtree to mu4e and promote it to level 1"
  (interactive)
  (org-copy-subtree)
  (+mu4e/compose)
  (unless (derived-mode-p 'org-msg-edit-mode)
    (org-msg-edit-mode))
  (org-msg-goto-body)
  (save-excursion
    (yank)
    (insert ?\n))

  ;; don't keep the subtree in the kill-ring
  (when kill-ring
    (setq kill-ring (cdr kill-ring)))

  ;; not really elegant but it gets the job done..
  (let* ((heading-level (- (car (org-heading-components)) 1)))
    (cl-loop repeat heading-level do (org-promote-subtree))))
