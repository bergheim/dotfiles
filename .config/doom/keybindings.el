;;; ~/.config/doom/keybindings.el -*- lexical-binding: t; -*-

(map!
 (:leader
   "1" 'winum-select-window-1
   "2" 'winum-select-window-2
   "3" 'winum-select-window-3
   "4" 'winum-select-window-4

   (:prefix ("a" . "custom bindings")

     (:prefix ("g" . "git")
       "y" 'bergheim-toggle-yadm
       "d" 'magit-diff-buffer-file
       "l" 'magit-log-buffer-file)

     :desc "org inbox" "i" (lambda () (interactive) (find-file "~/org/inbox.org"))

     (:prefix ("o" . "org")
       "a" 'org-agenda
       "g" 'org-clock-goto
       "i" 'org-clock-in
       "n" 'org-add-note
       "l" 'org-clock-in-last
       "o" 'org-clock-out
       "c" 'org-capture
       "r" #'org-mru-clock-in
       "R" #'org-mru-clock-goto
       "s" 'org-store-link
       "l" 'org-insert-link)


     ;; shortcuts for org since I use these so much
     (:prefix ("c" . "org-clock")
       "g" 'org-clock-goto
       "i" 'org-clock-in
       "l" 'org-clock-in-last
       "o" 'org-clock-out
       "c" 'org-capture
       "r" #'org-mru-clock-in
       "R" #'org-mru-clock-goto)

     (:prefix ("d" . "dotfiles")
       "d" 'find-in-dotfiles
       "f" 'browse-dotfiles)

     (:prefix ("m" . "E-mail")
      "m" '=mu4e
      ;; TODO figure out how to load mu4e if this is called
      (:desc "Compose" "c" #'+mu4e/compose)
      (:desc "Search" "s" #'mu4e-headers-search)
      (:desc "Sent" "S" 'bergheim-email-sent)
      (:desc "Today's email" "t" 'bergheim-email-today)
      (:desc "Today's unhandled email" "T" 'bergheim-email-today-or-unread)
      (:desc "Today's work email" "w" 'bergheim-email-work-inbox)
      (:desc "Today's personal email" "p" 'bergheim-email-personal-inbox)
      (:desc "This weeks email" "W" 'bergheim-email-week)
      (:desc "Update index" "u" 'mu4e-update-index)
      (:desc "Update index and mail" "U" 'mu4e-update-mail-and-index)
      (:desc "Trash" "x" 'bergheim-email-trash)
      (:desc "Important" "I" 'bergheim-email-important)
      (:desc "Inbox" "i" 'bergheim-email-inbox))

     "e" 'elfeed

     "r" 'rainbow-mode

     "t" 'heaven-and-hell-toggle-theme
     "T" 'heaven-and-hell-load-default-theme)))
