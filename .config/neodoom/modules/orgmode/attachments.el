;;; attachments.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

(setq org-id-link-to-org-use-id t)
(defun bergheim/org-id-advice (&rest args)
  "Add unique and clear IDs to everything, except modes where it does not make sense"

  ;; FIXME: maybe skip update-id if some mode?
  (message (format "%s" major-mode))
  ;; (unless (string-match "^\\(magit\\|mu4e\\)-.*" (format "%s" major-mode))
  ;; (message "Current ID %s" (org-entry-get (point) "ID" t))

  (if (string-prefix-p "org-" (format "%s" major-mode))
      (bergheim/~id-get-or-generate)
    ;; this will keep things more up to date but will make capturing a lot slower
    ;; I've not noticed any downsides, though
    ;; (org-id-update-id-locations)
    )
  args)

(advice-add 'org-store-link :before #'bergheim/org-id-advice)
;; (advice-remove 'org-store-link #'bergheim/org-id-advice)
;; FIXME: if we find an ID in parent, use that
(advice-add 'org-attach-attach :before #'bergheim/org-id-advice)



;;; attachments.el ends here
