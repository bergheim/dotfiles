;;; roam.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim


(use-package org-roam
  :ensure nil
  :disabled
  :defer t
  :after org
  :init
  (setq org-roam-directory (expand-file-name "~/org/roam"))
  :general
  (bergheim/global-menu-keys
    "nN" '(org-roam-node-find :which-key "Open/capture org-roam"))
  :config
  (setq org-roam-completion-everywhere t)
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
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
