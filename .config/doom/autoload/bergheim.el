;;; ~/.config/doom/autoload/bergheim.el -*- lexical-binding: t; -*-

;;;###autoload
(defun find-in-dotfiles ()
  "Open a file somewhere in ~/.config via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/.config/")))

;;;###autoload
(defun browse-dotfiles ()
  "Browse the files in ~/.config."
  (interactive)
  (doom-project-browse (expand-file-name "~/.config/")))

;;;###autoload
(defun bergheim-email-inbox()
  (interactive)
  (mu4e-headers-search-bookmark "maildir:/Inbox/"))

;;;###autoload
(defun bergheim-email-sent()
  (interactive)
  (mu4e-headers-search-bookmark "maildir:/Sent/"))

;;;###autoload
(defun bergheim-email-trash()
  (interactive)
  (mu4e-headers-search-bookmark "maildir:/Trash/ OR flag:trashed"))

;;;###autoload
(defun bergheim-email-important()
  (interactive)
  (mu4e-headers-search-bookmark "(maildir:/Inbox/ AND date:1w..now AND flag:unread) OR flag:flagged"))

;;;###autoload
(defun bergheim-email-work-inbox()
  (interactive)
  (mu4e-headers-search-bookmark "maildir:/neptune/Inbox/"))

;;;###autoload
(defun bergheim-email-personal-inbox()
  (interactive)
  (mu4e-headers-search-bookmark "(maildir:/glvortex/Inbox/ OR maildir:/gmail/Inbox) AND (date:1w..now OR flag:unread)"))

;;;###autoload
(defun bergheim-email-today()
  "Opens the inbox with unread and todays email"
  (interactive)
  (mu4e-headers-search-bookmark "(date:1d..now) AND maildir:/Inbox/"))

;;;###autoload
(defun bergheim-email-week()
  "Opens the inbox with unread and this weeks email"
  (interactive)
  (mu4e-headers-search-bookmark "(date:1w..now) AND maildir:/Inbox/"))

;;;###autoload
(defun bergheim-email-today-or-unread()
  "Opens the inbox with unread and todays email"
  (interactive)
  (mu4e-headers-search-bookmark "maildir:/Inbox/ AND (date:1d..now OR flag:unread)"))
