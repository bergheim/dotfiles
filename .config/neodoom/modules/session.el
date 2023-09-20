;;; session.el --- Description -*- lexical-binding: t; -*-

;; this causes warnings when we restart emacs with eglot-buffers open
;; and anyway, this is no legacy config!
(with-eval-after-load 'flymake
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(use-package desktop
  :init
  (add-to-list 'desktop-minor-mode-handlers
               '(eglot--managed-mode . ignore))
  ;; Define the path for desktop files.
  (setq desktop-path (list bergheim/cache-dir)
        desktop-auto-save-timeout 30
        desktop-save 'if-exists
        desktop-load-locked-desktop t) ; Load even if there's a lock.

  ;; Auto-save and load without prompting.
  (setq desktop-save-mode 1)
  (add-hook 'desktop-after-read-hook (lambda () (desktop-save-mode 1)))
  (desktop-read bergheim/cache-dir))

(use-package restart-emacs
  :ensure t
  :after desktop
  :config
  (defun bergheim/restart-emacs ()
    "Save desktop and then restart Emacs with custom init directory."
    (interactive)
    (desktop-save bergheim/cache-dir)
    (let ((restart-args `("--init-directory" ,(expand-file-name "~/.config/neodoom") "--debug-init")))
      (restart-emacs restart-args))))

;;; session.el ends here
