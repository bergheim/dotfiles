;;; session.el --- Description -*- lexical-binding: t; -*-
;;
(use-package desktop
  :init
  ;; Store desktop files in `bergheim/cache-dir`.
  (setq desktop-dirname (expand-file-name "desktop/" bergheim/cache-dir)
        desktop-base-file-name "emacs.desktop"
        desktop-base-lock-name "lock"
        desktop-path (list desktop-dirname)
        desktop-save t
        desktop-load-locked-desktop nil) ; Don't load desktops that are locked.

  ;; Create the directory if it doesn't exist.
  (unless (file-exists-p desktop-dirname)
    (make-directory desktop-dirname t))  ; The `t` argument creates parent directories as needed.

  ;; Auto-save and load the desktop.
  :config
  (desktop-save-mode 1)

  ;; Ensure the desktop is saved before quitting.
  (add-hook 'kill-emacs-hook (lambda ()
                               (desktop-save-in-desktop-dir))))

(use-package restart-emacs
  :ensure t
  :after desktop
  :config
  ;; Ensure desktop saving occurs before `restart-emacs` is invoked.
  (advice-add 'restart-emacs :before 'desktop-save-in-desktop-dir)

  ;; TODO: remove this once I move off the init stuff
  (defun bergheim/restart-emacs--translate-current-executable-advice (orig-fun &rest args)
    (let ((result (apply orig-fun args)))
      (list result "--init-directory" "~/.config/neodoom" "--debug-init")))

  (advice-add 'restart-emacs--translate-current-executable
              :around 'bergheim/restart-emacs--translate-current-executable-advice))

;;; session.el ends here
