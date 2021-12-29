;;; ~/.config/doom/autoload/bergheim.el -*- lexical-binding: t; -*-

;;;###autoload
(defun find-in-dotfiles ()
  "Open a file somewhere in ~/.config via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/.config")))

;;;###autoload
(defun browse-dotfiles ()
  "Browse the files in ~/.config."
  (interactive)
  (doom-project-browse (expand-file-name "~/.config")))


;;;###autoload
(defun bergheim-toggle-yadm ()
  "Toggle the GIT_DIR between nil and yadm. Opens magit-status when it is enabled."
  (interactive)
  ;; use a property “state”. Value is t or nil
  (if (get 'tsb-toggle-yadm 'state)
      (progn
        (message "Disabling YADM")
        (setenv "GIT_DIR" nil)
        (put 'tsb-toggle-yadm 'state nil))
    (progn
      ;; TODO enable the XDG env for this
      (message (concat "Enabling YADM " (getenv "HOME") "/.local/share/yadm/repo.git"))
      (setenv "GIT_DIR" (concat (getenv "HOME") "/.local/share/yadm/repo.git"))
      (put 'tsb-toggle-yadm 'state t)
      (magit-status))
    ))

;;;###autoload
(defun bergheim-email-inbox()
  (interactive)
  (mu4e-headers-search-bookmark "maildir:/Inbox/"))

;;;###autoload
(defun bergheim-email-trash()
  (interactive)
  (mu4e-headers-search-bookmark "maildir:/glvortex/Trash OR maildir:/gmail/[Gmail].Trash OR maildir:\"/neptune/Deleted Items\""))

;;;###autoload
(defun bergheim-email-work-inbox()
  (interactive)
  (mu4e-headers-search-bookmark "maildir:/neptune/Inbox/"))

;;;###autoload
(defun bergheim-email-personal-inbox()
  (interactive)
  (mu4e-headers-search-bookmark "maildir:/glvortex/Inbox/ OR maildir:/gmail/Inbox"))

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
  (mu4e-headers-search-bookmark "(date:1d..now OR flag:unread) AND maildir:/Inbox/"))
