;;; search.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

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
  (bergheim/mu4e-search-from-domain msg nil))

(defun bergheim/mu4e-search-from-address (msg &optional to-and-from-p)
  "Quickly find all mails sent from the current address; include both to and
from if TO-AND-FROM-P is non-nil. With \\[universal-argument], exclude mails in Trash."

  (let* ((from (plist-get (car (mu4e-message-field-at-point :from)) :email))
         (msgid (mu4e-message-field msg :message-id))
         (query (format "%s:%s AND %s"
                        (if to-and-from-p "contact" "from")
                        from
                        (if current-prefix-arg "NOT maildir:/Trash/" "maildir:/Inbox/"))))
    (mu4e-search query nil nil nil msgid (and (eq major-mode 'mu4e-view-mode)
                                              (not (eq mu4e-split-view 'single-window))))))

(defun bergheim/mu4e-search-from-address-all (msg)
  "Find all emails sent to or from this address"
  (bergheim/mu4e-search-from-address msg t))

(defun bergheim/mu4e-search-from-name (msg)
  "Quickly find all mails sent from the current name.

This might have more matches than `bergheim/mu4e-search-from-address'
as people can have multiple email addresses.

With \\[universal-argument], include emails to this name as well"

  (let ((name (plist-get (car (mu4e-message-field-at-point :from)) :name))
        (msgid (mu4e-message-field msg :message-id))
        (query-string "from:%s AND maildir:/Inbox/"))

    (when current-prefix-arg
      (setq query-string "from:%s AND NOT maildir:/Trash/"))

    (setq name (bergheim/mu4e--clean-string-for-mu name))

    (mu4e-search
     (format query-string name name)
     nil nil nil
     msgid (and (eq major-mode 'mu4e-view-mode)
                (not (eq mu4e-split-view 'single-window))))))

(defun bergheim/mu4e-search-from-domain (msg &optional everything custom-domain)
  "Quickly find all mails sent to or from this domain"

  ;; TODO: this can be cleaner
  (let* ((email (plist-get (car (mu4e-message-field-at-point :from)) :email))
         (msgid (mu4e-message-field msg :message-id))
         (domain (bergheim/utils--get-domain email))
         (query-string "contact:/.*%s$/")
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

    (setq query-string (concat query-string " AND " maildir-filter))

    (mu4e-search
     (format query-string domain)
     nil nil nil
     msgid (and (eq major-mode 'mu4e-view-mode)
                (not (eq mu4e-split-view 'single-window))))))

(defun bergheim/mu4e-search-dwim (msg)
  (let* ((email (plist-get (car (mu4e-message-field-at-point :from)) :email))
         (email-list (mu4e-message-field-at-point :list))
         (email-list (if (stringp email-list)
                         (s-trim email-list)
                       ""))
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

     ;; filter on the other "email lists"
     ((seq-contains-p bergheim/email-dwim-lists email)
      (bergheim/mu4e-search-from-address msg))

     ;; if it is a mailing list, just show everything from that
     ;; (unfortunately, many provide a "List-Unsubscribe", but not the List-Id itself)
     ((> (length email-list) 0)
      (mu4e-search
       (format "list:%s AND maildir:/Inbox/" email-list)
       nil nil nil
       msgid (and (eq major-mode 'mu4e-view-mode)
                  (not (eq mu4e-split-view 'single-window)))))

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

    (setq subject (bergheim/mu4e--clean-string-for-mu subject))

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

    (if current-prefix-arg
        (setq query-string (concat query-string " AND NOT maildir:/Trash/"))
      (setq query-string (concat query-string " AND maildir:/Inbox/")))

    (mu4e-search
     query-string
     nil nil nil
     msgid (and (eq major-mode 'mu4e-view-mode)
                (not (eq mu4e-split-view 'single-window))))))

;; TODO: merge this and search-to-me
(defun bergheim/mu4e-search-to-address (msg)
  "Quickly find all mails sent to the current address

With \\[universal-argument], include emails from this address as well"

  (let* ((email (plist-get (car (mu4e-message-field-at-point :to)) :email))
         (msgid (mu4e-message-field msg :message-id))
         (query-string (format "to:%s" email)))

    (if current-prefix-arg
        (setq query-string (concat query-string " AND NOT maildir:/Trash/"))
      (setq query-string (concat query-string " AND maildir:/Inbox/")))

    (mu4e-search
     query-string
     nil nil nil
     msgid (and (eq major-mode 'mu4e-view-mode)
                (not (eq mu4e-split-view 'single-window))))))

;; TODO: add -search?
(defun bergheim/mu4e-narrow-to-sender (_)
  "Quickly narrow view to emails sent from the selected email"

  (mu4e-search-narrow (concat "from:" (plist-get (car (mu4e-message-field-at-point :from)) :email))))

(defun bergheim/mu4e-email-today(&optional lookback)
  "Opens the inbox with unread and by default shows todays email

If LOOKBACK is specified, use that instead of 1d.
If \\[universal-argument] if called before this, show a week back."
  (interactive)
  (require 'mu4e)
  (let ((mu4e-search-include-related t)
        (mu4e-search-threads t)
        (mu4e-search-sort-field :date)
        (mu4e-search-sort-direction :ascending))

    ;; ask if you want to apply any changes made before leaving
    ;; (mu4e-mark-handle-when-leaving)

    (unless lookback
      (setq lookback "2m"))
    (if current-prefix-arg
        (setq lookback "1y"))

    (mu4e t)
    ;; Add the hook temporarily
    ;; this is just awful :p
    ;; (unless has-marks
    ;;   (setq bergheim/mu4e--headers-goto-bottom-counter 1))
    ;; (add-hook 'mu4e-headers-found-hook #'bergheim/mu4e--headers-goto-bottom)
    (mu4e-search (concat "maildir:/Inbox/ AND date:" lookback "..now"))))

;;; search.el ends here
