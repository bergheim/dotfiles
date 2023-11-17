;;; keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

(bergheim/global-menu-keys
  "C" '(org-capture :which-key "capture")

  "o" '(:ignore t :which-key "Org Mode")

  "od" '((lambda (&optional arg) (interactive) (org-agenda arg "d")) :which-key "Orgmode Dashboard")
  "oa" '(org-agenda :which-key "org-agenda")
  "ob" '(bergheim/org--open-attachments :which-key "open attachments")
  "og" '(org-clock-goto :which-key "clock goto")
  "oi" '(org-clock-in :which-key "clock in")
  "ol" '(org-clock-in-last :which-key "clock in last")
  "oo" '(org-clock-out :which-key "clock out")
  "oC" '(org-capture :which-key "capture")

  "or" '(:ignore t :which-key "Recent changes")
  "orr" '(bergheim/org-agenda-recent-changes :which-key "this week")

  "orw" '((lambda () (interactive) (bergheim/org-agenda-recent-changes '("@work" "work" "neptune"))) :which-key "work")
  "orp" '((lambda () (interactive) (bergheim/org-agenda-recent-changes '("@life" "life"))) :which-key "work")

  "orm" '((lambda () (interactive) (org-ql-view-recent-items :num-days 31 :type 'clocked)) :which-key "Last month")

  "on" '(org-add-note :which-key "add note")
  "om" '(bergheim/org-subtree-to-mu4e :which-key "subtree to mu4e")
  "os" '(org-store-link :which-key "store link")
  "ou" '(bergheim/org-copy-url-only :which-key "copy url only")
  "ol" '(org-insert-link :which-key "insert link")

  "oj" '(:ignore t :which-key "journal")
  "ojj" '(org-journal-new-entry :which-key "New entry")
  "ojJ" '(org-journal-new-scheduled-entry :which-key "New scheduled entry")
  "ojo" '(org-journal-open-current-journal-file :which-key "Open journal")
  "ojs" '(org-journal-search-forever :which-key "Search journal")

  "oc" '(:ignore t :which-key "org-clock")
  "ocg" '(org-clock-goto :which-key "clock goto")
  "oci" '(org-clock-in :which-key "clock in")
  "ocl" '(org-clock-in-last :which-key "clock in last")
  "oco" '(org-clock-out :which-key "clock out")
  "ocr" '(bergheim/org-mru-clock-in :which-key "mru clock in")
  "ocR" '(bergheim/org-mru-goto :which-key "mru goto"))

(provide 'keybindings)
;;; keybindings.el ends here
