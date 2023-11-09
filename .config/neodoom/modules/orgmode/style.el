;;; style.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

(defun org-capture-select-template-prettier (&optional keys)
  "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
  (let ((org-capture-templates
         (or (org-contextualize-keys
              (org-capture-upgrade-templates org-capture-templates)
              org-capture-templates-contexts)
             '(("t" "Task" entry (file+headline "" "Tasks")
                "* TODO %?\n  %u\n  %a")))))
    (if keys
        (or (assoc keys org-capture-templates)
            (error "No capture template referred to by \"%s\" keys" keys))
      (org-mks org-capture-templates
               "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
               "Template key: "
               `(("q" ,(concat (nerd-icons-octicon "nf-oct-stop" :face `nerd-icons-red :v-adjust 0.01) "\tAbort")))))))

(advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

;; TODO: still needed?
;; indent items that span lines in the agenda. it's not perfect but it's better (divinedominion)
(defun ct/org-agenda-set-wrap-prefix ()
  (setq-local wrap-prefix "                          "))
(add-hook 'org-agenda-mode-hook #'ct/org-agenda-set-wrap-prefix)

;; indent by default
(add-hook 'org-mode-hook 'org-indent-mode)

;; TODO: read the man here
(use-package org-modern
  :ensure t
  :after org
  :config
  ;; (setq ;; Edit settings
   ;; org-auto-align-tags nil
   ;; org-tags-column 0
   ;; org-catch-invisible-edits 'show-and-error
   ;; org-special-ctrl-a/e t
   ;; org-insert-heading-respect-content t

   ;; ;; Org styling, hide markup etc.
   ;; org-hide-emphasis-markers t
   ;; org-pretty-entities t
   ;; org-ellipsis "…"

   ;; ;; Agenda styling
   ;; org-agenda-tags-column 0
   ;; org-agenda-block-separator ?─
   ;; org-agenda-time-grid
   ;; '((daily today require-timed)
   ;;   (800 1000 1200 1400 1600 1800 2000)
   ;;   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   ;; org-agenda-current-time-string
   ;; "⭠ now ─────────────────────────────────────────────────")


  ;; indent headings
  (org-indent-mode)
  (global-org-modern-mode))

;; (setq org-agenda-prefix-format
;;       (quote
;;        ((agenda . "%-12c%?-18t% s")
;;         (timeline . "% s")
;;         (todo . "%-20c")
;;         (tags . "%-22c")
;;         (search . "%-12c"))))


;;; style.el ends here
