;;; bergheim-utils.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

;; TODO: rename this to bootstrap and move the others here out
(defun bergheim/toggle-scratch-buffer ()
  "Toggle the *scratch* buffer: show, hide, or switch to it based on its current state."
  (interactive)
  (let ((scratch-buffer (get-buffer-create "*scratch*")))
    (cond
     ;; hide if active
     ((eq (current-buffer) scratch-buffer)
      (bury-buffer))
     ;; switch to it if visible
     ((get-buffer-window scratch-buffer t)
      (pop-to-buffer scratch-buffer))
     ;; else open in current
     (t
      (switch-to-buffer scratch-buffer)))))

(defun bergheim/open-dirvish-current-project ()
  "Open dirvish in the root directory of the current project."
  (interactive)
  (let ((project-root (project-root (project-current t))))
    (if project-root
        (dirvish project-root)
      (message "No project found!"))))

(defun bergheim/copy-current-buffer-file ()
  "Copy the current buffer's file to a specified location."
  (interactive)
  (if buffer-file-name
      (let ((destination (read-file-name "Copy to: ")))
        (copy-file buffer-file-name destination t)
        (find-file destination)
        (message "File copied to: %s" destination))
    (message "No file is associated with this buffer.")))


(provide 'bergheim-utils)
;;; bergheim-utils.el ends here
