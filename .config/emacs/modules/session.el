;;; session.el --- Description -*- lexical-binding: t; -*-

;; this causes warnings when we restart emacs with eglot-buffers open
;; and anyway, this is no legacy config!
(with-eval-after-load 'flymake
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(use-package desktop
  :hook
  (elpaca-after-init . desktop-read)
  :ensure nil
  ;; Auto-save and load without prompting.
  (desktop-after-read  . (lambda () (desktop-save-mode 1)))

  ;; :init
  ;; (add-to-list 'desktop-minor-mode-handlers
  ;;              '(eglot--managed-mode . ignore))
  :config
  (setq desktop-path (list (bergheim/get-and-ensure-data-dir "desktop"))
        desktop-auto-save-timeout 3600
        desktop-save 'ask-if-new
        desktop-load-locked-desktop t
        desktop-save-mode 1))

(use-package restart-emacs
  :after desktop
  :commands 'restart-emacs
  :init
  (defun bergheim/restart-emacs ()
    "Save desktop and then restart Emacs with custom init directory."
    (interactive)
    (desktop-save (expand-file-name "desktop" bergheim/cache-dir))
    (let ((restart-args `("--init-directory" ,(expand-file-name "~/.config/neodoom") "--debug-init")))
      (restart-emacs restart-args))))

;;; session.el ends here
