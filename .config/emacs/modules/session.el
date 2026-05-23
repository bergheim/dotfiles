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
    "Prompt for a desktop subdirectory under `bergheim/desktops-dir' and load it."
    (interactive)
    (let* ((names (directory-files bergheim/desktops-dir nil "^[^.].*"))
           (name  (completing-read "Load Desktop: " names nil t)))
      (unless (string-empty-p name)
        (let ((dir (expand-file-name name bergheim/desktops-dir)))
          (unless (file-directory-p dir)
            (user-error "No such desktop: %s" name))
          ;; Force‐reload:
          (desktop-change-dir dir)
          (message "Loaded desktop: %s" name)))))

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
  (beframe-mode 1))

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

;; Route mu4e file opens and shr links through ssherpa, so attachments
;; and web links pop on the laptop when SSH'd in from the road.
(use-package ssherpa
  :ensure nil
  :commands (ssherpa-connect ssherpa-disconnect ssherpa-open)
  :init
  (defun bergheim/mu4e-open-file-via-ssherpa (_orig-fun &rest args)
    "Route `mu4e--view-open-file' through `ssherpa-open'."
    (ssherpa-open (car args)))

  (defun bergheim/shr-browse-url-via-ssherpa (orig-fun &optional external mouse-event new-window)
    "Around advice for `shr-browse-url': route URL at point through `ssherpa-open'."
    (let ((url (get-text-property (point) 'shr-url)))
      (if url
          (ssherpa-open url)
        (funcall orig-fun external mouse-event new-window))))

  (advice-add 'mu4e--view-open-file :around #'bergheim/mu4e-open-file-via-ssherpa)
  (advice-add 'shr-browse-url :around #'bergheim/shr-browse-url-via-ssherpa)
  (setq browse-url-browser-function #'ssherpa-open))

;;; session.el ends here
