;;; bergheim-session.el --- Description -*- lexical-binding: t; -*-

;; this causes warnings when we restart emacs with eglot-buffers open
;; and anyway, this is no legacy config!
(with-eval-after-load 'flymake
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))

(use-package beframe
  :demand t
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

;;; bergheim-session.el ends here
