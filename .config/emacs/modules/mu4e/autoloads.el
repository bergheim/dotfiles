;;; autoloads.el --- Mail-search shortcuts available without loading mu4e -*- lexical-binding: t; -*-
;;
;; ;;;###autoload must sit on real defuns. site-lisp copies the next form
;; verbatim; a (define-mu4e-search-fn ...) call blows up when mu4e/init
;; never ran (tty/container) because the macro is not defined.

;;;###autoload
(defun bergheim/email-today ()
  "Opens the inbox with unread and today's email."
  (interactive)
  (unless (featurep 'mu4e)
    (require 'mu4e))
  (mu4e-search-bookmark "maildir:/Inbox/ AND date:1y..now"))

;;;###autoload
(defun bergheim/email-inbox ()
  "Opens the entire inbox."
  (interactive)
  (unless (featurep 'mu4e)
    (require 'mu4e))
  (mu4e-search-bookmark "maildir:/Inbox/"))

;;;###autoload
(defun bergheim/email-important ()
  "Important email."
  (interactive)
  (unless (featurep 'mu4e)
    (require 'mu4e))
  (mu4e-search-bookmark "flag:flagged OR (maildir:/Inbox/ AND flag:unread AND date:1w..now)"))

;;;###autoload
(defun bergheim/email-inbox-work ()
  "Work email."
  (interactive)
  (unless (featurep 'mu4e)
    (require 'mu4e))
  (mu4e-search-bookmark "maildir:/neptune/Inbox/"))

;;;###autoload
(defun bergheim/email-inbox-personal ()
  "Personal email."
  (interactive)
  (unless (featurep 'mu4e)
    (require 'mu4e))
  (mu4e-search-bookmark "(maildir:/glvortex/Inbox/ OR maildir:/gmail/Inbox) AND (flag:unread OR date:1w..now)"))

;;;###autoload
(defun bergheim/email-today-or-unread ()
  "Opens the inbox with unread and today's email."
  (interactive)
  (unless (featurep 'mu4e)
    (require 'mu4e))
  (mu4e-search-bookmark "maildir:/Inbox/ AND (flag:unread OR date:2d..now)"))

;;;###autoload
(defun bergheim/email-week ()
  "Opens the inbox with unread and this weeks email."
  (interactive)
  (unless (featurep 'mu4e)
    (require 'mu4e))
  (mu4e-search-bookmark "maildir:/Inbox/ AND date:1w..now"))

;;;###autoload
(defun bergheim/email-sent ()
  "Sent email."
  (interactive)
  (unless (featurep 'mu4e)
    (require 'mu4e))
  (mu4e-search-bookmark "maildir:/Sent/"))

;;;###autoload
(defun bergheim/email-trash ()
  "Trash."
  (interactive)
  (unless (featurep 'mu4e)
    (require 'mu4e))
  (mu4e-search-bookmark "maildir:/Trash/ OR flag:trashed"))

;;;###autoload
(defun bergheim/email-junk ()
  "Show the junk mail from all accounts."
  (interactive)
  (unless (featurep 'mu4e)
    (require 'mu4e))
  (mu4e-search-bookmark "maildir:/Spam/"))
