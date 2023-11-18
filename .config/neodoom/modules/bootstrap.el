
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
