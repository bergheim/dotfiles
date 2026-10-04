;;; attachments.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

(defvar bergheim/org-id-inhibit nil
  "Non-nil while Org stores links internally: no new IDs, no kill-ring.")

(defun bergheim/org-id-inhibit-around (fn &rest args)
  (let ((bergheim/org-id-inhibit t)
        (org-id-link-to-org-use-id nil))
    (apply fn args)))

;; ob-tangle calls org-store-link once per block for its link comments.
;; Adding an ID there modifies the buffer, and org-babel-tangle-file then
;; prompts on kill-buffer -- blocking the daemon for every emacsclient.
(advice-add 'org-babel-tangle :around #'bergheim/org-id-inhibit-around)

(defun bergheim/org-id-advice (&rest args)
  "Add unique and clear IDs to everything, except modes where it does not make sense"

  ;; FIXME: maybe skip update-id if some mode?
  ;; (unless (string-match "^\\(magit\\|mu4e\\)-.*" (format "%s" major-mode))
  ;; (message "Current ID %s" (org-entry-get (point) "ID" t))

  (if (and (not bergheim/org-id-inhibit)
           (string-prefix-p "org-" (format "%s" major-mode)))
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

(defun bergheim/org-attach-save-file-list-to-property (dir)
  "Save list of attachments to ORG_ATTACH_FILES property."
  (when-let* ((files (org-attach-file-list dir)))
    (org-set-property "ORG_ATTACH_FILES" (mapconcat #'identity files ", "))))
(add-hook 'org-attach-after-change-hook #'bergheim/org-attach-save-file-list-to-property)

;;; attachments.el ends here
