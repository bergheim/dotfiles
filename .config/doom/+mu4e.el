;;; ~/.config/doom/+mu4e.el -*- lexical-binding: t; -*-

(defun bergheim/mu4e-narrow-to-sender (_)
  "Quickly narrow view to emails sent from the selected email"

  (mu4e-search-narrow (concat "from:" (plist-get (car (mu4e-message-field-at-point :from)) :email))))

(cl-defun bergheim/utils--get-domain (email &optional full-domain)
  "Get the main domain of an email address"
  (let* ((domain (cadr (split-string email "@")))
         (parts (reverse (split-string domain "\\."))))

    ;; FIXME: do not always use full domain
    (if (or current-prefix-arg full-domain)
        domain
      (format "%s.%s" (nth 1 parts) (nth 0 parts)))))

(defun bergheim/mu4e-search-from-domain (msg &optional everything custom-domain)
  "Quickly find all mails sent to or from this domain"

  ;; TODO: this can be cleaner
  (let* ((email (plist-get (car (mu4e-message-field-at-point :from)) :email))
         (msgid (mu4e-message-field msg :message-id))
         (domain (bergheim/utils--get-domain email))
         (query-string "(from:/.*%s$/ or to:/.*%s$/)")
         (maildir-filter)
         ;; always sort descending as there might be thousands of emails
         (mu4e-headers-sort-field :date)
         (mu4e-headers-sort-direction 'descending))

    (when custom-domain
      (setq domain custom-domain))

    (if (or everything
            current-prefix-arg)
        (setq maildir-filter "NOT maildir:/Trash/")
      (setq maildir-filter "maildir:/Inbox/"))

    (setq query-string (concat maildir-filter " AND " query-string))

    (mu4e-search
     (format query-string domain domain)
     nil nil nil
     msgid (and (eq major-mode 'mu4e-view-mode)
                (not (eq mu4e-split-view 'single-window))))))

(defun bergheim/mu4e-search-from-mail-list (msg)
  "Open the whole email list, if any"

  (let ((msgid (mu4e-message-field msg :message-id))
        (email-list (mu4e-message-field-at-point :list)))

    (if email-list
        (mu4e-search
         (format "maildir:/Inbox/ AND list:%s" email-list)
         nil nil nil
         msgid (and (eq major-mode 'mu4e-view-mode)
                    (not (eq mu4e-split-view 'single-window)))))
    (message "Not part of a list")))

(defun bergheim/mu4e-search-from-domain-all (msg)
  "Quickly find all INBOX mails sent to or from this domain"
  (bergheim/mu4e-search-from-domain msg t))

(defun bergheim/mu4e-search-from-address (msg &optional from-address)
  "Quickly find all mails sent from the current address

With \\[universal-argument], include emails to this address as well"

  (let ((email (plist-get (car (mu4e-message-field-at-point :from)) :email))
        (msgid (mu4e-message-field msg :message-id))
        (query-string "(from:%s"))

    (when from-address
      (setq email from-address))

    (if current-prefix-arg
        (setq query-string (concat query-string " OR to:%s"))
      (setq query-string (concat "maildir:/Inbox/ AND " query-string " OR to:%s")))

    (setq query-string (concat query-string ")"))

    (mu4e-search
     (format query-string email email)
     nil nil nil
     msgid (and (eq major-mode 'mu4e-view-mode)
                (not (eq mu4e-split-view 'single-window))))))

(defun bergheim/mu4e-search-from-name (msg)
  "Quickly find all mails sent from the current name.

This might have more matches than `bergheim/mu4e-search-from-address'
as people can have multiple email addresses.

With \\[universal-argument], include emails to this name as well"

  (let ((name (plist-get (car (mu4e-message-field-at-point :from)) :name))
        (msgid (mu4e-message-field msg :message-id))
        (query-string "NOT maildir:/Trash/ AND (from:%s"))

    (when current-prefix-arg
      (setq query-string (concat query-string " OR to:%s")))

    (setq query-string (concat query-string ")"))

    (mu4e-search
     (format query-string name name)
     nil nil nil
     msgid (and (eq major-mode 'mu4e-view-mode)
                (not (eq mu4e-split-view 'single-window))))))

(defun bergheim/mu4e--clean-subject-for-mu (subject)
  "Clean up the subject before sending it to mu4e.

This is because xapian has a lot of characters that will break the search. I could not find docs on
it, and this mess has just evolved over time.."

  (let* (;; remove any "Re: ", "Fwd: " etc
         (subject (replace-regexp-in-string "^\\(\\ca\\{2,3\\}: ?\\)+" "" subject))
         ;; remove characters that make mu unhappy and breaks xapian keywords
         (subject (replace-regexp-in-string "’" "'" subject))
         (subject (replace-regexp-in-string "[^[:alnum:]_'\.]" " " subject))
         ;; (subject (replace-regexp-in-string "[[:nonascii:]]" " " subject))
         ;; (subject (replace-regexp-in-string "[[:punct:]]" " " subject))
         ;; (subject (replace-regexp-in-string "[[:graph:]]" " " subject))
         ;; remove more breaking xapian keywords
         (subject (replace-regexp-in-string "[-:]" " " subject))
         ;; apparently surrounding 's is a nono
         (subject (replace-regexp-in-string "\\('\\)[[:blank:]]" "" subject nil nil 1))
         (subject (replace-regexp-in-string "[[:blank:]]\\('\\)" "" subject nil nil 1))
         ;; xapian matches 10.10 but not foo.bar
         (subject (replace-regexp-in-string "[^[:digit:]]\\(\\\.+\\)" " " subject nil nil 1))
         (subject (replace-regexp-in-string "[[:digit:]]\\(\\\.+\\)" "\\\\." subject nil nil 1)))

    (s-trim subject)))

(defun bergheim/mu4e--pattern-match-subject (subject &optional only-match)
  "Match the SUBJECT for anything interesting, and return that.
If ONLY-MATCH is non-nil, only return if anything actually matched

Used to quickly match similar messages"

  (let ((match
         (or (seq-some (lambda (r) (and (string-match-p r subject)
                                        (concat "subject:/" (s-trim (string-replace " " "\\ " (string-replace "\\" "\\\\" r))) "/")))
                       bergheim/email-dwim-subjects)
             (unless only-match
               subject))))
    match))

(defun bergheim/mu4e-search-dwim (msg)
  (let* ((email (plist-get (car (mu4e-message-field-at-point :from)) :email))
         (email-list (mu4e-message-field-at-point :list))
         (email-references (car (mu4e-message-field-at-point :references)))
         (msgid (mu4e-message-field msg :message-id))
         (subject (mu4e-message-field msg :subject))
         (domain (bergheim/utils--get-domain email t)))

    (cond
     ;; if we are on a thread, show that.. maybe?
     (email-references
      (mu4e-action-show-thread msg))

     ;; if we match on a specific subject pattern, assume that is the most important
     ((bergheim/mu4e--pattern-match-subject subject t)
      (bergheim/mu4e-search-this-subject msg t))

     ;; if it is a mailing list, just show everything from that
     ;; (unforunately, many provide a "List-Unsubscribe", but not the List-Id itself)
     (email-list
      (mu4e-search
       (format "maildir:/Inbox/ AND list:%s" email-list)
       nil nil nil
       msgid (and (eq major-mode 'mu4e-view-mode)
                  (not (eq mu4e-split-view 'single-window)))))

     ;; filter on the other "email lists"
     ((seq-contains-p bergheim/email-dwim-lists email)
      (bergheim/mu4e-search-from-address msg))

     ;; filter on domains if they all send the same kind of emails
     ((when (cl-some (lambda (d) (string-match-p d domain))
                     bergheim/email-dwim-domains)
        (bergheim/mu4e-search-from-domain msg)))

     ;; fallback to search this subject
     (t (bergheim/mu4e-search-this-subject msg t)))))

(defun bergheim/mu4e-search-this-subject (msg &optional match-partial-subject)
  "Quickly find all mails containing words from this subject

Strips away subject action prefixes and special characters to capture more emails.

If \\[universal-argument] is called before this, include the trash."

  (let* ((subject (mu4e-message-field msg :subject))
         (msgid (mu4e-message-field msg :message-id))
         ;; TODO serch for domain if it is an interesting one?
         (email (plist-get (car (mu4e-message-field-at-point :from)) :email))
         (query-string "%s")
         (num 0))

    ;; (unless current-prefix-arg
    ;;     (setq query-string (concat "maildir:/Inbox/ AND " query-string)))

    ;; (if current-prefix-arg
    ;;   (setq subject (replace-regexp-in-string "[^[:alpha:]_']" " " subject)))

    (setq subject (bergheim/mu4e--clean-subject-for-mu subject))

    (when (and match-partial-subject (not (string-empty-p subject)))
      (setq subject (bergheim/mu4e--pattern-match-subject subject)))

    ;; numbers usually make a pretty good differentiator
    (when (string-match "\\(\\b[0-9][0-9]+\\b\\)" subject)
      ;; often if a subject has something like "#34234" in it, that is a good identifier as it
      ;; allows tracking the ticket issue across subjects
      (setq num (string-to-number (match-string 1 subject))))

    (when (> num 2050)
      ;; larger numbers are more likely to be unique, so full text search them. chose a number
      ;; larger than our year because they are more common
      (setq subject (number-to-string num)))

    (mu4e-search
     ;; (format query-string (concat "subject:" subject))
     (format query-string subject)
     nil nil nil
     msgid (and (eq major-mode 'mu4e-view-mode)
                (not (eq mu4e-split-view 'single-window))))))

(defun bergheim/mu4e-search-around-message (msg)
  "Show messages around the time of the selected MSG in the given context

With \\[universal-argument], include all contexts"

  (let* ((days-around 10)
         (msgid (mu4e-message-field msg :message-id))
         (email-date (mu4e-message-field msg :date))
         (account (bergheim/mu4e--get-account (mu4e-message-field msg :maildir)))
         (date-range-from (decode-time (time-subtract email-date (days-to-time days-around))))
         (date-range-to (decode-time (time-add email-date (days-to-time days-around))))
         (query-string (format "NOT maildir:/Trash/ AND date:%s..%s"
                               (format "%d%02d%02d" (nth 5 date-range-from) (nth 4 date-range-from) (nth 3 date-range-from))
                               (format "%d%02d%02d" (nth 5 date-range-to) (nth 4 date-range-to) (nth 3 date-range-to)))))

    (when current-prefix-arg
      (setq query-string (concat (format "maildir:/%s/ AND " account) query-string)))

    (mu4e-search query-string
                 nil nil nil
                 msgid (and (eq major-mode 'mu4e-view-mode)
                            (not (eq mu4e-split-view 'single-window))))))

(add-to-list 'mu4e-marks
             '(spam
               :char       "X"
               :prompt     "spam"
               :dyn-target (lambda (target msg)
                             (with-mu4e-context-vars (mu4e-context-determine msg nil)
                                 mu4e-spam-folder))
               :action      (lambda (docid msg target)
                              (mu4e--server-move docid (mu4e--mark-check-target target) "+S-u-N"))))

(mu4e~headers-defun-mark-for spam)
(mu4e--view-defun-mark-for spam)

(defun bergheim/mu4e-refile-as-spam (msg)
  (if (eq major-mode 'mu4e-headers-mode)
      (mu4e-headers-mark-for-spam)
    (mu4e-view-mark-for-spam)))

(defun bergheim/mu4e--addressed-to-me ()
  "Search all mails sent to me specifically.

With \\[universal-argument], search all emails where I am a recipient"
  (let ((keyword "to:%s"))
    (when current-prefix-arg
      (setq keyword "recip:%s"))
    (mapconcat
     (lambda (address) (format keyword address))
     (mu4e-personal-addresses) " OR ")))

(defun bergheim/mu4e-search-to-me (msg)
  "Quickly find all mails sent to me from this address.

Includes BCC emails, but does not include CC, because that point just use from:address"

  ;; TODO: consider using `mu4e-message-contact-field-matches-me'
  (let* ((from (plist-get (car (mu4e-message-field-at-point :from)) :email))
         (maildir (mu4e-message-field msg :maildir))
         (msgid (mu4e-message-field msg :message-id))
         ;; show me emails sent to any of my adresses
         (my-email (bergheim/mu4e--addressed-to-me))
         (query-string (format "(from:%s AND (%s)" from my-email)))

    (unless current-prefix-arg
      (setq query-string (concat "NOT maildir:/Trash/ AND " query-string)))

    (mu4e-search
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
         (to (plist-get (car (mu4e-message-field-at-point :to)) :email))
         (from (plist-get (car (mu4e-message-field-at-point :from)) :email))
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

;; TODO: see if this works when sending works again
;; message-id is apparently not generated on the server..? which sounds strange
;; (add-hook 'message-send-mail-hook (lambda () (message "id %s") (org-store-link nil)))

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
  ;; TODO: No hooks for `org-capture' I think? I want to capture, then refile on completion
  (let ((mu4e-headers-advance-after-mark nil))
    (if (eq major-mode 'mu4e-headers-mode)
        (mu4e-headers-mark-for-refile)
      (mu4e-view-mark-for-refile)))
  (org-capture nil "ef"))

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
        org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil tex:dvipng \\n:t"
        org-msg-default-alternatives '((new             . (text html))
                                       (reply-to-html   . (text html))
        ;; replies are currently broken with mu 1.8. this hack prioritizes work mail. From https://github.com/jeremy-compostella/org-msg/issues/157#issuecomment-1233791513
                                       (reply-to-text   . (text html)))))

(setq user-mail-address bergheim/email
      user-full-name  bergheim/name
      mu4e-compose-signature bergheim/signature
      mu4e-get-mail-command "sync-mail"
      mu4e-attachment-dir "~/Downloads/email"
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
      mu4e-search-threads nil
      ;; mu4e-headers-sort-direction 'ascending

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
                (mu4e-spam-folder   . "/neptune/Spam")

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
                (mu4e-spam-folder   . "/gmail/Spam")

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
                (mu4e-spam-folder   . "/glvortex/Spam")

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

;; TODO in general, lower-case should match /Inbox/, upper-case should mean everything but /Trash/
(setq mu4e-headers-actions (delete '("show this thread" . mu4e-action-show-thread) mu4e-headers-actions))
(add-to-list 'mu4e-headers-actions '("Narrow to sender" . bergheim/mu4e-narrow-to-sender) t)
(add-to-list 'mu4e-headers-actions '("follow up" . bergheim/mu4e-follow-up) t)
(add-to-list 'mu4e-headers-actions '("later" . bergheim/mu4e-read-later) t)
(add-to-list 'mu4e-headers-actions '("browser" . bergheim/mu4e-open-message-in-webclient) t)
(add-to-list 'mu4e-headers-actions '("email" . bergheim/mu4e-search-from-address) t)
(add-to-list 'mu4e-headers-actions '("name" . bergheim/mu4e-search-from-name) t)
(add-to-list 'mu4e-headers-actions '("domain" . bergheim/mu4e-search-from-domain-all) t)
(add-to-list 'mu4e-headers-actions '("Domain inbox" . bergheim/mu4e-search-from-domain) t)
(add-to-list 'mu4e-headers-actions '("mail list" . bergheim/mu4e-search-from-mail-list) t)
(add-to-list 'mu4e-headers-actions '("Me" . bergheim/mu4e-search-to-me) t)

;; TODO: make a general universal arg wrapper
(add-to-list 'mu4e-headers-actions '("Me" . (lambda (msg)
                                              (interactive)
                                              (let ((current-prefix-arg '(4))) ; C-u
                                                (bergheim/mu4e-search-to-me msg)))) t)


(add-to-list 'mu4e-headers-actions '("subject" . bergheim/mu4e-search-this-subject) t)
(add-to-list 'mu4e-headers-actions '("thread" . mu4e-action-show-thread) t)
(add-to-list 'mu4e-headers-actions '("junk" . bergheim/mu4e-refile-as-spam) t)
(add-to-list 'mu4e-headers-actions '("Around" . bergheim/mu4e-search-around-message) t)
(add-to-list 'mu4e-headers-actions '("adwim" . bergheim/mu4e-search-dwim) t)

(setq mu4e-view-actions (delete '("View in browser" . mu4e-action-view-in-browser) mu4e-view-actions))
(setq mu4e-view-actions (delete '("show this thread" . mu4e-action-show-thread) mu4e-view-actions))
(add-to-list 'mu4e-view-actions '("follow up" . bergheim/mu4e-follow-up) t)
(add-to-list 'mu4e-view-actions '("later" . bergheim/mu4e-read-later) t)
(add-to-list 'mu4e-view-actions '("List" . bergheim/mu4e-search-from-list) t)
(add-to-list 'mu4e-view-actions '("browser" . bergheim/mu4e-open-message-in-webclient) t)
(add-to-list 'mu4e-view-actions '("email" . bergheim/mu4e-search-from-address) t)
(add-to-list 'mu4e-view-actions '("name" . bergheim/mu4e-search-from-name) t)
(add-to-list 'mu4e-view-actions '("domain" . bergheim/mu4e-search-from-domain-all) t)
(add-to-list 'mu4e-view-actions '("Domain inbox" . bergheim/mu4e-search-from-domain) t)
(add-to-list 'mu4e-view-actions '("mail list" . bergheim/mu4e-search-from-mail-list) t)

(add-to-list 'mu4e-view-actions '("Me" . bergheim/mu4e-search-to-me) t)
(add-to-list 'mu4e-view-actions '("subject" . bergheim/mu4e-search-this-subject) t)
(add-to-list 'mu4e-view-actions '("thread" . mu4e-action-show-thread) t)
(add-to-list 'mu4e-view-actions '("junk" . bergheim/mu4e-refile-as-spam) t)
(add-to-list 'mu4e-view-actions '("Around" . bergheim/mu4e-search-around-message) t)
(add-to-list 'mu4e-view-actions '("adwim" . bergheim/mu4e-search-dwim) t)

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

(defun bergheim/mu4e--headers-goto-bottom ()
  "Move point to the last email in the mu4e-headers buffer.

Since the headers display is async, and we only want this for some functions,
remove the hook after invocation"
  (with-current-buffer (get-buffer "*mu4e-headers*")
    (goto-char (point-max))
    (remove-hook 'mu4e-headers-found-hook #'bergheim/mu4e--headers-goto-bottom)
    (mu4e-headers-prev)))

;; this is a bit nuts, but it works! opens a link on a predefined spot from the entire list
;; its a lot of async stuff going on, and I couldn't get the hooks to work..
(defun bergheim/mu4e--wait-for-email-body (callback)
  "Wait until the email body is rendered, then execute CALLBACK."
  (let ((max-attempts 10))
    (unless (eq major-mode 'mu4e-view-mode)
      (error "Not in mu4e-view-mode"))
    (run-with-timer
     0.1 nil
     (lambda ()
       (unless (or (zerop max-attempts)
                   (bergheim/mu4e--email-body-rendered-p))
         (setq max-attempts (1- max-attempts))
         (bergheim/mu4e--wait-for-email-body callback))
       (when (bergheim/mu4e--email-body-rendered-p)
         (funcall callback))))))

(defun bergheim/mu4e--email-body-rendered-p ()
  "Check if the email body is rendered in mu4e."
  (or (text-property-any (point-min) (point-max) 'shr-url nil)
      (text-property-any (point-min) (point-max) 'mu4e-url nil)))

(defvar bergheim/current-navigate-fn nil
  "Current function used to navigate links in mu4e message view.")

(defun bergheim/mu4e--process-single-message ()
  "Process the current mu4e message based on bergheim/current-navigate-fn."
  (bergheim/mu4e--wait-for-email-body
   (lambda ()
     (funcall bergheim/current-navigate-fn)
     (shr-browse-url)
     (remove-hook 'mu4e-view-mode-hook 'bergheim/mu4e--process-single-message)
     (if (mu4e-view-headers-next)
         (progn
           (setq bergheim/current-navigate-fn bergheim/current-navigate-fn)
           (add-hook 'mu4e-view-mode-hook 'bergheim/mu4e--process-single-message))
       (mu4e-view-quit)))))

(defun bergheim/mu4e--open-links (&optional navigate-function)
  "Visit each mu4e message, open the first link, and move to the next message."
  (interactive)
  (setq bergheim/current-navigate-fn (or navigate-function 'bergheim/mu4e-navigate-first-link))
  (when (eq major-mode 'mu4e-headers-mode)
    (add-hook 'mu4e-view-mode-hook 'bergheim/mu4e--process-single-message)
    (mu4e-headers-view-message)))

(defun bergheim/mu4e--navigate-first-link ()
  "Navigate to the first link in mu4e message view and open it."
  (shr-next-link))

(defun bergheim/mu4e--navigate-second-to-last-links ()
  "Navigate to the second-to-last link in mu4e message view and open it."
  (goto-char (point-max))
  (shr-previous-link))

(defun bergheim/mu4e-open-first-links (&optional navigate-function)
  "Visit each mu4e message, open the first link, and move to the next message."
  (interactive)
  (bergheim/mu4e--open-links 'bergheim/mu4e--navigate-first-link))

(defun bergheim/mu4e-open-second-last-links ()
  "Visit each mu4e message, open the second to last link, and move to the next message."
  (interactive)
  (bergheim/mu4e--open-links 'bergheim/mu4e--navigate-second-to-last-links))
