;;; autoloads.el --- Description -*- lexical-binding: t; -*-

;;;###autoload
(define-mu4e-search-fn bergheim/email-today
                       "Opens the inbox with unread and today's email."
                       "(date:1d..now) AND maildir:/Inbox/")

;;;###autoload
(define-mu4e-search-fn bergheim/email-inbox
                       "Opens the entire inbox"
                       "maildir:/Inbox/")

;;;###autoload
(define-mu4e-search-fn bergheim/email-important
                       "Important email"
                       "(maildir:/Inbox/ AND date:1w..now AND flag:unread) OR flag:flagged")

;;;###autoload
(define-mu4e-search-fn bergheim/email-inbox-work
                       "Work email"
                       "maildir:/neptune/Inbox/")

;;;###autoload
(define-mu4e-search-fn bergheim/email-inbox-personal
                       "Personal email"
                       "(maildir:/glvortex/Inbox/ OR maildir:/gmail/Inbox) AND (date:1w..now OR flag:unread)")

;;;###autoload
(define-mu4e-search-fn bergheim/email-today
                       "Opens the inbox with unread and todays email"
                       "(date:1d..now) AND maildir:/Inbox/")

;;;###autoload
(define-mu4e-search-fn bergheim/email-today-or-unread
                       "Opens the inbox with unread and todays email"
                       "maildir:/Inbox/ AND (date:1d..now OR flag:unread)")

;;;###autoload
(define-mu4e-search-fn bergheim/email-week
                       "Opens the inbox with unread and this weeks email"
                       "(date:1w..now) AND maildir:/Inbox/")

;;;###autoload
(define-mu4e-search-fn bergheim/email-sent
                       "Sent email"
                       "maildir:/Sent/")

;;;###autoload
(define-mu4e-search-fn bergheim/email-trash
                       "Trash"
                       "maildir:/Trash/ OR flag:trashed")

;;;###autoload
(define-mu4e-search-fn bergheim/email-junk
                       "Show the junk mail from all accounts"
                       "maildir:/Spam/")

;;;###autoload
(defun bergheim/lookup-anything ()
  "Look up a word in some way"
  (interactive)
  (require 'powerthesaurus nil t)
  (powerthesaurus-lookup-dwim))
