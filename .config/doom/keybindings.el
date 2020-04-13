;;; ~/.config/doom/keybindings.el -*- lexical-binding: t; -*-


(map! :leader
      ;; things I missed from spacemacs
      "1" 'winum-select-window-1
      "2" 'winum-select-window-2
      "3" 'winum-select-window-3
      "4" 'winum-select-window-4
      ";" 'evilnc-comment-operator ;; was (pp-eval-expression EXPRESSION)

      "agy" 'bergheim-toggle-yadm
      "agd" 'magit-diff-buffer-file
      "agl" 'magit-log-buffer-file

      "ai" (lambda () (interactive) (find-file "~/org/inbox.org"))
      "acg" 'org-clock-goto
      "aci" 'org-clock-in
      "acl" 'org-clock-in-last
      "aco" 'org-clock-out
      "acr" #'org-mru-clock-in
      "acR" #'org-mru-clock-select-recent-task
      "aL" 'org-store-link
      "al" 'org-insert-link

      "am" 'mu4e

      "at" 'heaven-and-hell-toggle-theme
      "aT" 'heaven-and-hell-load-default-theme)
