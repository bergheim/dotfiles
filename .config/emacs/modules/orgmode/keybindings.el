;;; keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

(bergheim/global-menu-keys
  "C" '(org-capture :which-key "capture")
  "k" '(org-capture :which-key "capture")
  "x" '(org-capture :which-key "capture")

  "o" '(:ignore t :which-key "Org Mode")

  "od" '((lambda (&optional arg) (interactive) (bergheim/org-super-agenda arg "d")) :which-key "Orgmode Dashboard")
  "oo" '((lambda (&optional arg) (interactive) (bergheim/org-super-agenda arg "d")) :which-key "Orgmode Dashboard")
  "oa" '(org-agenda :which-key "org-agenda")
  "ob" '(bergheim/org-open-attachments :which-key "open attachments")

  ;; "og" '(org-clock-goto :which-key "clock goto")
  ;; "oi" '(org-clock-in :which-key "clock in")
  ;; "ol" '(org-clock-in-last :which-key "clock in last")
  ;; "oo" '(org-clock-out :which-key "clock out")

  "oC" '(org-capture :which-key "capture")

  "or" '(:ignore t :which-key "Recent changes")
  "orr" '(bergheim/org-agenda-recent-changes :which-key "this week")
  "orw" '((lambda () (interactive) (bergheim/org-agenda-recent-changes '("@work" "work" "neptune"))) :which-key "work")
  "orp" '((lambda () (interactive) (bergheim/org-agenda-recent-changes '("@life" "life"))) :which-key "private")
  "orm" '((lambda () (interactive) (org-ql-view-recent-items :num-days 31 :type 'clocked)) :which-key "Last month")

  "on" '(bergheim/org-add-note-to-clocked-task :which-key "add note for...")
  "oN" '(org-add-note :which-key "add note")
  "om" '(bergheim/org-subtree-to-mu4e :which-key "subtree to mu4e")

  "os" '(:ignore t :which-key "Search")
  "osa" '(consult-org-agenda :which-key "agenda")
  "osi" '(consult-org-heading :which-key "headings")
  "osn" '((lambda () (interactive) (consult-find org-directory)) :which-key "filenames")
  "oss" '(bergheim/org-ql-find-in-org-directory-recursively :which-key "headings")
  "osS" '(org-ql-find-in-agenda :which-key "org-agenda files")
  "osT" `(,(bergheim/call-with-universal-arg #'bergheim/org-ql-search-for-tag)  :which-key "ALL tags")
  "ost" '(bergheim/org-ql-search-for-tag  :which-key "tags")

  "ou" '(bergheim/org-copy-url-only :which-key "copy URL only")

  "ol" '(:ignore t :which-key "Links")
  "old" '(org-super-links-quick-insert-drawer-link :which-key "drawer search")
  "oli" '(org-insert-link :which-key "insert manually")
  "oll" '(org-super-links-quick-insert-inline-link :which-key "inline search")
  "olP" '((lambda () (interactive) (let ((org-super-links-related-into-drawer nil)) (org-super-links-insert-link))) :which-key "paste here")
  "olp" '(org-super-links-insert-link :which-key "paste to drawer")
  "ols" '(org-super-links-quick-insert-inline-link :which-key "inline search")
  "oly" '(org-super-links-store-link :which-key "copy")

  "oj" '(:ignore t :which-key "journal")
  "ojj" '(org-journal-new-entry :which-key "New entry")
  "ojJ" '(org-journal-new-scheduled-entry :which-key "New scheduled entry")
  "ojo" '(org-journal-open-current-journal-file :which-key "Open journal")
  "ojs" '(org-journal-search-forever :which-key "Search journal")

  "oc" '(:ignore t :which-key "org-clock")
  "ocC" '(org-clock-cancel :which-key "cancel")
  "ocg" '(org-clock-goto :which-key "clock goto")
  "ocG" '(bergheim/org-mru-goto :which-key "mru goto")
  "oci" '(org-clock-in :which-key "clock in")
  "ocl" '(org-clock-in-last :which-key "clock in last")
  "oco" '(org-clock-out :which-key "clock out")
  "ocr" '(bergheim/org-mru-clock-in :which-key "mru clock in")
  "ocR" '(org-resolve-clocks :which-key "resolve")

  "oq" '(:ignore t :which-key "org-ql")
  "oqv" #'org-ql-view
  "oqr" #'org-ql-view-recent-items
  "oqs" #'org-ql-search
  "oqt" #'org-ql-sparse-tree
  "oqq" #'org-ql-view
  "oqb" #'org-ql-view-sidebar)

(provide 'keybindings)
;;; keybindings.el ends here
