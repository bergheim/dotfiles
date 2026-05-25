;;; hugo.el --- Org-mode publishing to Hugo -*- lexical-binding: t; -*-

(use-package ox-hugo
  :after ox
  :config
  (defun my/org-change-draft-when-blog-state-changes ()
    (interactive)
    (pcase (org-get-todo-state)
      ("PUBLISH" (org-set-property "EXPORT_HUGO_DRAFT" "false")
       (org-hugo-export-wim-to-md))
      ("DRAFT" (org-set-property "EXPORT_HUGO_DRAFT" "true"))
      ("POST" (org-set-property "EXPORT_HUGO_DRAFT" "true")
       (org-hugo-export-wim-to-md))
      (_ ())))

  (add-hook 'org-after-todo-state-change-hook
            'my/org-change-draft-when-blog-state-changes))

;;; hugo.el ends here
