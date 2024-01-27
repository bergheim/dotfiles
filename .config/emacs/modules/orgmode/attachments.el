;;; attachments.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

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

(defun bergheim/org-attach-id-uuid-folder-format (id)
  "Puts everything in the same path, in the folder ID.

Assumes the ID will be unique across all items."
  (format "%s" id))

;; TODO: clean this up
(autoload 'org-attach-attach "org-attach" nil t)

(setq org-id-link-to-org-use-id t)
(advice-add 'org-store-link :before #'bergheim/org-id-advice)
;; FIXME: if we find an ID in parent, use that
(advice-add 'org-attach-attach :before #'bergheim/org-id-advice)

(with-eval-after-load 'org-attach
  (add-to-list 'org-attach-id-to-path-function-list 'bergheim/org-attach-id-uuid-folder-format))

;;; attachments.el ends here
