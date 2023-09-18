;;; roam.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim


(use-package org-roam
  :ensure t
  :defer t
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("m" "meeting" plain "%?" :target
           (file+head "work/meetings.org" "#+title: ${title}\n")
           :headline "Meetings"
           :jump-to-captured t
           :unnarrowed t)))

  (defun bergheim/org-roam-create-node (start end)
    "Insert a note based on the word point is on"
    (interactive "r")
    (save-excursion
      (let ((pt)
            (word)
            (boundry "-_A-Za-z0-9"))

        (if (use-region-p)
            (org-roam-node-insert)
          (let ((regionp (buffer-substring start end)))
            (skip-chars-backward boundry)
            (setq pt (point))
            (skip-chars-forward boundry)
            (set-mark pt)
            ;; TODO: downcase this
            ;; (setq word (buffer-substring pt (point)))
            ;; (downcase-region pt (point))
            (org-roam-node-insert)))))))


;;; roam.el ends here
