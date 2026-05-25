;;; bergheim-tramp.el --- Tramp configuration -*- lexical-binding: t; -*-

(use-package tramp
  :ensure nil
  :init
  (defun bergheim/tramp-abort ()
    (interactive)
    (tramp-cleanup-all-buffers)
    (tramp-cleanup-all-connections)
    (recentf-cleanup))
  :config
  (setq tramp-persistency-file-name (expand-file-name "tramp" bergheim/cache-dir)
        remote-file-name-access-timeout 5 ;; give up quickly instead of locking all of emacs
        remote-file-name-inhibit-locks t ;; do not create remote locks - should speed things up a bit
        ;; this causes Remote file error: Forbidden reentrant call of Tramp calls
        auto-revert-remote-files nil
        ;; optional performance improvements
        remote-file-name-inhibit-auto-save t
        remote-file-name-inhibit-auto-save-visited t
        vc-handled-backends '(Git)
        tramp-connection-timeout 3
        ;; `ssh` should be quicker than the default `scp`
        tramp-default-method "ssh"
        tramp-copy-size-limit nil
        ;; just use the SSH settings
        tramp-use-connection-share nil
        ;; copy directly between hosts with no progress bar
        tramp-use-scp-direct-remote-copying nil
        ;; show warnings and connection status
        tramp-verbose 3)

  (add-to-list 'tramp-remote-path "/home/tsb/local/bin")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path 'tramp-default-remote-path)

  (add-to-list 'tramp-methods
               '("yadm"
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 (tramp-login-env (("SHELL") ("/bin/sh")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c")))))

;;; bergheim-tramp.el ends here
