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

(defun browse-url-host (url &optional _new-window)
  "Open URL on the host machine via an SSH tunnel."
  (interactive (browse-url-interactive-arg "URL: "))

  (if (not (bergheim/ssh-status))
      (browse-url-default-browser url)
    (when-let* ((ssh-client (or (getenv "SSH_CLIENT") (bergheim/ssh-status)))
                (remote-user "tsb")
                (remote-host "localhost")
                (remote-port "25509")
                (remote-path (concat "/tmp/" (file-name-nondirectory url)))
                (ssh-cmd ""))
      ;; if it is an actual file, transfer it before calling open
      (if (string-prefix-p "/" url)
          (progn
            (setq remote-path (format "~'%s'" remote-path))
            (call-process-shell-command (format "scp -P %s \"%s\" %s@%s:%s"
                                                remote-port url remote-user remote-host remote-path))
            (setq url remote-path))
        (setq url (format "'%s'" url)))

      (setq ssh-cmd (format "ssh %s@%s -p %s \"systemd-run --user --quiet --pipe xdg-open %s\""
                            remote-user remote-host remote-port url))
      (call-process shell-file-name nil 0 nil "-c" ssh-cmd))))

;; (advice-add 'shr-browse-url :around
;;             (lambda (orig-func url &optional new-window)
;;               "Use `browse-url-host` to handle URLs when browsing via `shr-browse-url`."
;;               (browse-url-host url new-window)))

;; (advice-add 'shr-browse-url :around
;;             (lambda (orig-func url &optional new-window)
;;               (interactive (browse-url-interactive-arg "URL: "))
;;               ;; Directly call `browse-url-host` with the correct parameters.
;;               (browse-url-host url new-window)))






(defun my-mu4e-view-open-file-advice (orig-fun &rest args)
  "Advice function to override `mu4e~view-open-file` behavior with custom function."
  ;; Extract the file path from args. The original function signature is
  ;; (mu4e~view-open-file path), where `path` is the first element in args.
  (let ((file-path (car args)))
    (message "Path %s" file-path)
    (browse-url-host file-path)))

(advice-add 'mu4e--view-open-file :around #'my-mu4e-view-open-file-advice)

;; (advice-remove 'mu4e--view-open-file #'my-mu4e-view-open-file-advice)








;; FIXME: update for linux?
(defun bergheim/shr-browse-url-advice (original-func url &optional new-window)
  (when-let* ((ssh-client (or (getenv "SSH_CLIENT") (bergheim/ssh-status)))
              (ssh-cmd (format "ssh tsb@localhost -p 25509 \"open '%s'\"" url)))
    (message "%s" ssh-cmd)
    (call-process shell-file-name nil 0 nil "-c" ssh-cmd)
    t))  ; Return t to indicate the URL was handled


(defun bergheim/shr-browse-url-advice (orig-fun url &optional new-window)
  "Advice to make `shr-browse-url' use `browse-url-host' instead of its default behavior."
  ;; Ensure URL is not nil and is passed correctly
  (if url
      (browse-url-host url new-window)
    (message "No URL providddded.")))


;; (advice-add 'shr-browse-url :around #'bergheim/shr-browse-url-advice)

;; (defun shr-browse-url (&optional external mouse-event new-window)
;;   "Redefine `shr-browse-url` to open URLs on the host machine via SSH tunnel."
;;   (interactive (list current-prefix-arg last-nonmenu-event))
;;   (let ((url (shr-get-url-at-point)))
;;     (if url
;;         (browse-url-host url)
;;       (message "No URL found at point!"))))


(defun bergheim/shr-browse-url-advice (original-func &optional external mouse-event new-window)
  "Around advice for `shr-browse-url'. Uses `browse-url-host' for URLs at point."
  (let ((url (get-text-property (point) 'shr-url)))
    (if url
        (browse-url-host url)
      (funcall original-func external mouse-event new-window))))

(advice-add 'shr-browse-url :around #'bergheim/shr-browse-url-advice)




;; (advice-add 'shr-browse-url :around
;;             (lambda (orig-fun url &rest args)
;;               (browse-url-host url)))





;; (when (getenv "SSH_CLIENT")
;;   (setq browse-url-browser-function 'browse-url-host))

(setq browse-url-browser-function 'browse-url-host)

;; Ensure my-custom-ssh-flag is defined globally
;; (defvar bergheim/ssh-connection-active nil "Flag to indicate an SSH connection for emacsclient sessions.")

(defun bergheim/ssh-connect ()
  "Mark the current frame as being connected over SSH"
  (interactive)
  ;; (setq bergheim/ssh-connection-active t)
  ;; Use a frame parameter to mark this frame as connected via SSH
  (set-frame-parameter nil 'ssh-connected t))

(defun bergheim/ssh-status ()
  "Return T if we are using SSH"
  (interactive)
  (frame-parameter nil 'ssh-connected))

(defun bergheim/ssh-disconnect (&optional frame)
  "Function to execute actions when an emacsclient frame is deleted."
  (interactive)
  (set-frame-parameter frame 'ssh-connected nil)
  (message "Emacsclient frame closed."))

;; Add the reset function to server-done-hook
;; (add-hook 'server-done-hook 'bergheim/ssh-disconnect)
;; (add-hook 'server-visit-hook 'bergheim/ssh-disconnect)

;; (add-hook 'delete-frame-functions #'bergheim/ssh-disconnect)

;;; session.el ends here
