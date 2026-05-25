;;; bergheim-compile.el --- Compilation buffer and project compile commands -*- lexical-binding: t; -*-

(use-package compile
  :ensure nil
  :hook
  (compilation-filter . ansi-osc-compilation-filter)
  (compilation-filter . ansi-color-compilation-filter)
  (typescript-ts-mode . +typescript-compiler-h)
  :general
  (bergheim/global-menu-keys
    "pc" '(bergheim/project-compile-dwim :which-key "compile")
    "pC" '(bergheim/open-project-compilation-buffer
           :which-key "open compile buffer"))
  :custom
  (compilation-max-output-line-length nil)
  (compilation-ask-about-save nil)
  (compilation-scroll-output t)
  (compilation-auto-jump-to-first-error t)

  :config
  (defun bergheim/trust-dev-dir-locals (orig-fun var val)
    "Auto-approve all dir-locals under ~/dev/."
    (if (and buffer-file-name
             (string-prefix-p (expand-file-name "~/dev/")
                              (file-name-directory buffer-file-name)))
        t
      (funcall orig-fun var val)))
  (advice-add 'safe-local-variable-p :around #'bergheim/trust-dev-dir-locals)

  ;; Add a generic file:line:col matcher without wiping the built-ins
  ;; (gcc, python, ruby, etc. ship with sensible regexps already).
  (add-to-list 'compilation-error-regexp-alist-alist
               '(file-line-col
                 "\\([^[:space:]:\n]+\\.[a-zA-Z0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)"
                 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'file-line-col)

  (defun +typescript-compiler-h ()
    (setq-local compile-command "npm run dev")
    ;; Register the TS stack-trace matcher globally (idempotent) and
    ;; prefer it for this buffer's compilations — don't clobber others.
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(typescript-stack
                   "(\\([^):\n]+\\):\\([0-9]+\\):\\([0-9]+\\))"
                   1 2 3))
    (setq-local compilation-error-regexp-alist
                '(typescript-stack file-line-col)))

  (setq project-compilation-buffer-name-function
        (lambda (dir)
          (format "*compilation-%s*" (project-name (project-current)))))

  (defun bergheim/project-compile-dwim (arg)
    "Smart project compilation. Recompile if the buffer exists and do not change window focus."
    (interactive "P")
    (let ((curwin (selected-window)))
      (save-buffer)
      (if (or arg (not (get-buffer (funcall project-compilation-buffer-name-function default-directory))))
          ;; New compilation with comint mode
          (let ((current-prefix-arg '(4)))
            (call-interactively #'project-compile))
        (let ((buffer-name (funcall project-compilation-buffer-name-function default-directory)))
          (pop-to-buffer buffer-name)
          (project-recompile)))
      (select-window curwin)))

  (defun bergheim/open-project-compilation-buffer ()
    "Open the project compilation buffer if it exists."
    (interactive)
    (if-let ((buffer-name (funcall project-compilation-buffer-name-function default-directory))
             (buffer (get-buffer buffer-name)))
        (pop-to-buffer buffer)
      (message "No compilation buffer exists for this project."))))

;; (use-package fancy-compilation
;;   :commands (fancy-compilation-mode)
;;   :config
;;   (setopt fancy-compilation-override-colors nil)

;;   (with-eval-after-load 'compile
;; 						(fancy-compilation-mode)))

(defun bergheim/sanitize-compile-command (command)
  "Sanitize COMMAND for use in buffer names.
Replace non-alphanumeric characters with dashes, collapse multiple dashes."
  (let ((sanitized (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" command)))
    (replace-regexp-in-string "^-+\\|-+$" "" sanitized)))

(defun bergheim/project-compilation-buffer-name (&optional command)
  "Get compilation buffer name for current project.
If COMMAND is provided, append sanitized version to buffer name."
  (let* ((project (project-current))
         (project-name (if project
                           (file-name-nondirectory
                            (directory-file-name (project-root project)))
                         "default")))
    (if command
        (format "*compilation-%s-%s*" project-name
                (bergheim/sanitize-compile-command command))
      (format "*compilation-%s*" project-name))))

(defun bergheim/compile (command &optional comint)
  "Run COMMAND in a project-specific compilation buffer.
With COMINT non-nil, use `comint-mode'."
  (interactive
   (list
    (let ((command (eval compile-command t)))
      (if (or compilation-read-command current-prefix-arg)
          (compilation-read-command command)
        command))
    (consp current-prefix-arg)))

  (let* ((default-directory (or (when (project-current)
                                  (project-root (project-current)))
                                default-directory))
         (compilation-buffer-name-function
          (lambda (_mode) (bergheim/project-compilation-buffer-name command)))
         (existing-buffer (get-buffer (bergheim/project-compilation-buffer-name command))))

    (if (and existing-buffer
             (with-current-buffer existing-buffer
               (and (derived-mode-p 'compilation-mode 'comint-mode)
                    (get-buffer-process existing-buffer))))
        (with-current-buffer existing-buffer
          (if (derived-mode-p 'comint-mode)
              (progn
                (pop-to-buffer existing-buffer)
                (message "Already running in comint mode. Switch to buffer."))
            (recompile)))
      (compile command comint))))

(defun bergheim/project-compile (type default-command &optional command)
  "Generic project compilation function."
  (if command
      (bergheim/compile command)
    (let ((cmd (read-string (format "%s command: " (capitalize type)) default-command)))
      (bergheim/compile cmd))))

(defun bergheim/project-test (&optional command)
  (interactive)
  (bergheim/project-compile "test" "npm test" command))

(defun bergheim/project-lint (&optional command)
  (interactive)
  (bergheim/project-compile "lint" "npm run lint" command))

(defun bergheim/project-run (&optional command)
  (interactive)
  (bergheim/project-compile "run" "npm run dev" command))

;;; bergheim-compile.el ends here
