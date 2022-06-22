;;; ~/.config/doom/keybindings.el -*- lexical-binding: t; -*-

(map!
 (:leader
   (:desc "Open journal"
    "n j o" #'org-journal-open-current-journal-file)

   (:prefix ("d" . "custom bindings")

    (:desc "Pop up dired" "b" 'bergheim/dired--other-window)

    (:desc "Pop up dired" "b" 'deer-jump-other-window)
    ;; norton commander style
    (:desc "Pop up dired" "B" 'deer-dual-pane)

    ;; TODO: shouldn't this just be an embark command?
    (:desc "Find a file in HOME" "f"
     (λ! (affe-find "~/")))

    ;; TODO: shouldn't this just be an embark command?
    (:desc "Find a dir in HOME" "F"
     (lambda (&optional arg)
       (interactive)
       (let ((affe-find-command "fd --color=never -t directory"))
        (affe-find "~/"))))

    (:desc "Grep in dev" "G"
     (λ! (affe-grep "~/dev")))

    (:prefix ("j" . "journal")
      (:desc "Todays entry" "t" 'org-roam-dailies-goto-today)
      (:desc "New entry" "j" 'org-roam-dailies-capture-today))

     (:prefix ("g" . "git")
       "y" 'bergheim-toggle-yadm
       "d" 'magit-diff-buffer-file
       "l" 'magit-log-buffer-file)

     (:prefix ("o" . "org")
       "a" 'org-agenda
       "b" 'bergheim/org--open-attachments
       "g" 'org-clock-goto
       "i" 'org-clock-in
       "n" 'org-add-note
       "m" 'bergheim/org-subtree-to-mu4e
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
       "r" 'bergheim/org-mru-clock-in
       "R" 'bergheim/org-mru-goto)

     (:prefix ("d" . "dotfiles")
      (:desc "Magit" "d" (λ! (magit-status "/yadm::")))
      (:desc "Find a file" "f" 'bergheim/find-in-dotfiles)
      (:desc "Browse" "b" 'bergheim/browse-dotfiles))

     (:prefix ("m" . "E-mail")
      "m" '=mu4e
      ;; TODO figure out how to load mu4e if this is called
      (:desc "Compose" "c" #'+mu4e/compose)
      (:desc "Search" "s" #'mu4e-headers-search)
      (:desc "Sent" "S" 'bergheim/mu4e-email-sent)
      (:desc "Today's email" "t" 'bergheim/mu4e-email-today)
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

     (:desc "Orgmode Dashboard" "a" (lambda (&optional arg) (interactive) (org-agenda arg "d")))
     (:desc "Email Dashboard today" "s" 'bergheim/mu4e-email-today)
     (:desc "Email Dashboard week" "S" (λ! (bergheim/mu4e-email-today "1w")))
     (:desc "Orgmode Work" "w" 'bergheim/org-agenda-work-items)

     (:prefix ("r" . "Recent")
      (:desc "Last week by date" "r"
       (λ! (bergheim/org-agenda-recent-changes)))
      (:desc "Last week by date work" "w"
       (λ! (bergheim/org-agenda-recent-changes '("@work" "work" "neptune"))))
      (:desc "Last week by date personal" "p"
       (λ! (bergheim/org-agenda-recent-changes '("@life" "life"))))
      (:desc "Last month by category" "c"
       (λ! (org-ql-view-recent-items :num-days 31 :type 'clocked)))
      (:desc "Recent tasks in buffer" "b"
       (λ! (bergheim/org-recent-tasks-in-buffer))))

     "t" 'heaven-and-hell-toggle-theme
     "T" 'heaven-and-hell-load-default-theme)))

(map!
 ;; overrides (mark-paragraph)
 :ni "M-h" #'evil-window-left
 ;; overrides default-indent-new-line
 :ni "M-j" #'evil-window-down
 ;; overrides kill-sentence
 :ni "M-k" #'evil-window-up
 ;; overrides downcase-word
 :ni "M-l" #'evil-window-right

 ;; hold shift to drag the window with you
 :ni "M-H" #'+evil/window-move-left
 :ni "M-J" #'+evil/window-move-down
 :ni "M-K" #'+evil/window-move-up
 :ni "M-L" #'+evil/window-move-right

 ;; easy splits
 :ni "M-\\" #'evil-window-vsplit
 :ni "M-]" #'evil-window-split
 :ni "M-DEL" #'+workspace/close-window-or-workspace

 "M-o" #'evil-window-next
 "M-f" #'doom/window-maximize-buffer
 "M-F" #'winner-undo)

(map! :after evil-org
      :map evil-org-mode-map
      :niv "M-h" #'evil-window-left
      :niv "M-j" #'evil-window-down
      :niv "M-k" #'evil-window-up
      :niv "M-l" #'evil-window-right

      :niv "M-H" #'+evil/window-move-left
      :niv "M-J" #'+evil/window-move-down
      :niv "M-K" #'+evil/window-move-up
      :niv "M-L" #'+evil/window-move-right

      ;; these override things I never use
      :niv "C-M-h" #'org-metaleft
      :niv "C-M-j" #'org-metadown
      :niv "C-M-k" #'org-metaup
      :niv "C-M-l" #'org-metaright

      ;; add shift to org-mode tree maniplations shortcuts
      :niv "C-M-S-h" #'org-shiftmetaleft
      :niv "C-M-S-j" #'org-shiftmetadown
      :niv "C-M-S-k" #'org-shiftmetaup
      :niv "C-M-S-l" #'org-shiftmetaright

      :niv "M-\\" #'evil-window-vsplit
      :niv "M-]" #'evil-window-split
      :niv "M-DEL" #'+workspace/close-window-or-workspace)

(map! :after evil-org-agenda
      :map evil-org-agenda-mode-map
      ;; I never use the defaults
      :m "M-h" #'evil-window-left
      :m "M-j" #'evil-window-down
      :m "M-k" #'evil-window-up
      :m "M-l" #'evil-window-right

      :m "M-H" #'+evil/window-move-left
      :m "M-J" #'+evil/window-move-down
      :m "M-K" #'+evil/window-move-up
      :m "M-L" #'+evil/window-move-right

      :m "M-\\" #'evil-window-vsplit
      :m "M-RET" #'evil-window-split
      :m "M-DEL" #'+workspace/close-window-or-workspace

      :m "W" 'bergheim/org-agenda-toggle-work
      :m "T" 'bergheim/org-agenda-mark-done-and-add-followup)

(map! :after treemacs
      :map treemacs-mode-map

      ;; this overrides expand, but tab does that as well
      "M-h" #'evil-window-left
      "M-j" #'evil-window-down
      "M-k" #'evil-window-up
      "M-l" #'evil-window-right)

(map! :after dired

      :map ranger-mode-map
      "M-h" #'evil-window-left
      "M-j" #'evil-window-down
      "M-k" #'evil-window-up
      "M-l" #'evil-window-right

      "M-\\" #'evil-window-vsplit
      "M-]" #'evil-window-split
      "M-DEL" #'+workspace/close-window-or-workspace

      "M-1" #'+workspace/switch-to-0
      "M-2" #'+workspace/switch-to-1
      "M-3" #'+workspace/switch-to-2

      :map (dired-mode-map ranger-mode-map)
      (:localleader
       (:desc "Browse externally" "b" (λ! (browse-url-xdg-open dired-directory)))
       (:desc "Create empty file" "c" 'dired-create-empty-file)
       (:desc "Attach to org node" "o" 'org-attach-dired-to-subtree)
       (:desc "MOVE to org node" "O" 'bergheim/org--move-attach-dired-to-subtree)
       (:desc "Attach to email" "m" 'gnus-dired-attach)
       (:desc "Ranger to dired" "d" 'ranger-to-dired)
       (:desc "Dired to ranger" "r" 'deer-from-dired)))
