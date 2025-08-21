;;; autoloads.el --- Description -*- lexical-binding: t; -*-

;;;###autoload
(defmacro define-mu4e-search-fn (name docstring bookmark-string)
  "Generate a mu4e search function."
  `(defun ,name ()
     ,docstring
     (interactive)
     (unless (featurep 'mu4e)
       (require 'mu4e))
     (mu4e-search-bookmark ,bookmark-string)))

;;;###autoload
(define-mu4e-search-fn bergheim/email-today
  "Opens the inbox with unread and today's email."
  "maildir:/Inbox/ AND date:1y..now")

;;;###autoload
(define-mu4e-search-fn bergheim/email-inbox
  "Opens the entire inbox"
  "maildir:/Inbox/")

;;;###autoload
(define-mu4e-search-fn bergheim/email-important
  "Important email"
  "flag:flagged OR (maildir:/Inbox/ AND flag:unread AND date:1w..now)")

;;;###autoload
(define-mu4e-search-fn bergheim/email-inbox-work
  "Work email"
  "maildir:/neptune/Inbox/")

;;;###autoload
(define-mu4e-search-fn bergheim/email-inbox-personal
  "Personal email"
  "(maildir:/glvortex/Inbox/ OR maildir:/gmail/Inbox) AND (flag:unread OR date:1w..now)")

;;;###autoload
(define-mu4e-search-fn bergheim/email-today-or-unread
  "Opens the inbox with unread and today's email"
  "maildir:/Inbox/ AND (flag:unread OR date:2d..now)")

;;;###autoload
(define-mu4e-search-fn bergheim/email-week
  "Opens the inbox with unread and this weeks email"
  "maildir:/Inbox/ AND date:1w..now")

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
