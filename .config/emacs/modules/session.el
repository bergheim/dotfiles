;;; session.el --- Description -*- lexical-binding: t; -*-

;; this causes warnings when we restart emacs with eglot-buffers open
;; and anyway, this is no legacy config!
(with-eval-after-load 'flymake
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(use-package desktop
  :ensure nil
  :demand
  ;; :hook
  ;; (desktop-after-read . (desktop-read (expand-file-name "desktops/default" bergheim/cache-dir)))
  ;; (elpaca-after-init . desktop-read)
  ;; Auto-save and load without prompting.
  ;; (desktop-after-read  . (lambda () (desktop-save-mode 1)))

  :init
  (defconst bergheim/desktops-dir
    (expand-file-name "desktops" bergheim/cache-dir)
    "Directory of saved desktops.")

  (defun bergheim/load-desktop ()
    "Load a desktop from '`user-emacs-directory'/desktops/"
    (interactive)
    (let* ((desktops-dir bergheim/desktops-dir)
           (desktops (directory-files desktops-dir nil "^[^.].*" t))
           (name (completing-read "Load Desktop: " desktops nil t)))
      (unless (string= name "")
        (let ((desktop-dirname (expand-file-name name desktops-dir)))
          (desktop-read desktop-dirname)
          (set-frame-parameter nil 'desktop-dir desktop-dirname)))))

  (defun bergheim/save-desktop (&optional name)
    "Save current desktop with NAME at '`user-emacs-directory'/desktops/'"
    (interactive)
    (let* ((desktops-dir bergheim/desktops-dir)
           (desktops (directory-files desktops-dir nil "^[^.].*" t))
           (name (or name (completing-read "Save Desktop As: " desktops nil nil))))
      (unless (string= name "")
        (if (member name desktops)
            (when (yes-or-no-p (format "Desktop `%s' already exists. Do you want to overwrite it?" name))
              (let ((desktop-dirname (expand-file-name name desktops-dir)))
                (make-directory desktop-dirname t)
                (desktop-save desktop-dirname)))
          (let ((desktop-dirname (expand-file-name name desktops-dir)))
            (make-directory desktop-dirname t)
            (desktop-save desktop-dirname))))))

  (defun bergheim/delete-desktop ()
    "Delete a desktop from `bergheim/desktops-dir'."
    (interactive)
    (let* ((desktops (directory-files bergheim/desktops-dir nil "^[^.].*" t))
           (name (completing-read "Delete Desktop: " desktops nil t)))
      (unless (string= name "")
        (delete-directory (expand-file-name name bergheim/desktops-dir) t))))

  :config
  (setq desktop-path (list bergheim/desktops-dir)
        desktop-auto-save-timeout 3600
        desktop-save 'ask-if-new
        desktop-load-locked-desktop t
        desktop-save-mode nil))

;; desktop-mode hacks. pretty sure https://github.com/jamescherti/easysession.el should replace this..

(defun bergheim/desktop-delete-lock-file (&optional frame)
  "Delete the desktop lock file for the given frame.
If FRAME is nil or not provided, use the selected frame."
  (let* ((frame (or frame (selected-frame)))
         (desktop-dir (frame-parameter frame 'desktop-dir))
         (lock-file (when desktop-dir
                      (expand-file-name ".emacs.desktop.lock" desktop-dir))))
    (when (and lock-file (file-exists-p lock-file))
      (delete-file lock-file))))

(add-to-list 'delete-frame-functions #'bergheim/desktop-delete-lock-file)

(use-package restart-emacs
  :after desktop
  :commands 'restart-emacs
  :init
  (defun bergheim/restart-emacs ()
    "Save desktop and then restart Emacs with custom init directory."
    (interactive)
    ;; (desktop-release-lock)
    (bergheim/desktop-delete-lock-file)
    ;; (desktop-save (bergheim/get-and-ensure-data-dir "desktops/restart"))
    (restart-emacs)))

(use-package beframe
  :general
  (bergheim/global-menu-keys
    "wn" 'make-frame-command
    "wd" 'delete-frame
    ;; "wo" 'other-frame
    )
  :config
  (beframed-mode 1))

;; WIP. lol
(defun bergheim/load ()
  (interactive)
  (tab-bar-mode -1)
  (activities-tabs-mode -1)
  (let ((frame (make-frame `((name . "email")))))
    (select-frame-set-input-focus frame)
    (bergheim/email-today))

  (let ((frame (make-frame `((name . "org")))))
    (select-frame-set-input-focus frame)
    (activities-resume (activities-named "org"))
    )
  )

;;; session.el ends here
