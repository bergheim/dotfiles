;;; bergheim-utils.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

(defun bergheim/get-and-ensure-data-dir (directory &optional filename)
  (unless bergheim/cache-dir
    (error "bergheim/cache-dir is not set."))
  (let* ((dir (or directory ""))
         (temp-dir (expand-file-name dir bergheim/cache-dir)))
    (unless (file-exists-p temp-dir)
      (make-directory temp-dir t))
    (if filename
        (expand-file-name filename temp-dir)
      temp-dir)))

(defun bergheim/get-and-ensure-config-dir (directory &optional filename)
  (unless bergheim/config-dir
    (error "bergheim/config-dir is not set."))
  (let* ((dir (or directory ""))
         (temp-dir (expand-file-name dir bergheim/config-dir)))
    (unless (file-exists-p temp-dir)
      (make-directory temp-dir t))
    (if filename
        (expand-file-name filename temp-dir)
      temp-dir)))


(defun bergheim/load-file (filename)
  "Load a file from the `bergheim/config-dir` directory."
  (let ((full-path (expand-file-name filename bergheim/config-dir)))
    (load-file full-path)))

(defun bergheim/reload-init-file ()
  (interactive)
  (load-file (expand-file-name "init.el" bergheim/config-dir))


  (message "Emacs configuration reloaded successfully!"))

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

(provide 'bergheim-utils)
;;; bergheim-utils.el ends here
