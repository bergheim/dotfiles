;;; ~/.config/doom/keybindings.el -*- lexical-binding: t; -*-

(map!
 (:leader

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
       "l" 'org-clock-in-last
       "o" 'org-clock-out
       "c" 'org-capture
       "r" #'org-mru-clock-in
       "R" #'org-mru-clock-select-recent-task
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
       "R" #'org-mru-clock-select-recent-task)

     (:prefix ("d" . "dotfiles")
       "d" 'find-in-dotfiles
       "f" 'browse-dotfiles)

     "m" 'mu4e
     "e" 'elfeed

     "r" 'rainbow-mode

     "t" 'heaven-and-hell-toggle-theme
     "T" 'heaven-and-hell-load-default-theme)))
