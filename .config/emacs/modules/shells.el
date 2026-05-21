;;; shells.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Thomas Bergheim
;;
;; Author: Thomas Bergheim
;; Maintainer: Thomas Bergheim

(use-package em-hist
  :ensure nil
  :config
  (setq
   eshell-hist-ignoredups t
   ;; Set the history file.
   ;; eshell-history-file-name "~/.bash_history"
   ;; If nil, use HISTSIZE as the history size.
   eshell-history-size nil))

(use-package multishell
  :unless bergheim/container-mode-p
  :ensure t
  :general
  (bergheim/global-menu-keys
    ;; "att" '((lambda () (interactive) (multishell-pop-to-shell nil (expand-file-name default-directory))) :which-key "shell")
    "att" '(shell :which-key "shell")
    "atT" '((lambda () (interactive) (multishell-pop-to-shell '(4))) :which-key "new shell"))
  :config
  ;; don't ask for history confirmation on quit
  (remove-hook 'kill-buffer-query-functions #'multishell-kill-buffer-query-function))

(defun bergheim/is-remote-shell-p ()
  "Check if shell is remote by looking for @ in recent prompt."
  (save-excursion
    (goto-char (point-max))
    (re-search-backward "@.*λ" (max 1 (- (point-max) 200)) t)))

(use-package shell
  :ensure nil
  :general
  (:states '(normal insert)
   :keymaps 'shell-mode-map
   "C-b" (lambda ()
           (interactive)
           (comint-send-string (current-buffer) "\C-r"))
   "M-p" (lambda ()
           (interactive)
           (goto-char (point-max))
           (call-interactively #'comint-previous-input))
   "M-k" #'bergheim/woman-shell-command-other-window
   "M-n" (lambda ()
           (interactive)
           (goto-char (point-max))
           (call-interactively #'comint-next-input)))
  (:states 'normal
   :keymaps 'shell-mode-map
   "C-d" (lambda ()
           (interactive)
           (if (comint-after-pmark-p)
               (comint-send-eof)
             (evil-scroll-down nil)))
   "RET" (lambda ()
           (interactive)
           (when (comint-after-pmark-p)
             (comint-send-input)))
   ;; "C-r" (lambda ()
   ;;         (interactive)
   ;;         (if (bergheim/is-remote-shell-p)
   ;;             ;; Remote: let shell handle it (fzf via your zsh config)
   ;;             (comint-send-string (current-buffer) "\C-r")
   ;;           ;; Local: use consult-history
   ;;           (goto-char (point-max))
   ;;           (comint-kill-input)
   ;;           (consult-history)))
   "C-r" (lambda ()
           (interactive)
           (goto-char (point-max))
           (comint-kill-input)
           (consult-history))
   "RET" (lambda ()
           (interactive)
           (if (comint-after-pmark-p)
               (comint-send-input)
             (evil-ret))))
  (:states 'insert
   :keymaps 'shell-mode-map
   "C-r" (lambda ()
           (interactive)
           (let ((input (comint-get-old-input-default)))
             (comint-kill-input)
             (insert (completing-read "History: " (ring-elements comint-input-ring) 
                                      nil nil input))))
   "C-d" 'comint-send-eof
   "C-a" #'comint-bol
   "C-e" #'end-of-line
   "M-h" (lambda ()
           (interactive)
           (evil-normal-state)
           (evil-window-left 1))
   "M-l" (lambda ()
           (interactive)
           (evil-normal-state)
           (evil-window-right 1)))
  (:keymaps 'shell-mode-map
   "C-M-j" #'compilation-next-error
   "C-M-k" #'compilation-previous-error)
  (bergheim/global-menu-keys
    "atx" 'bergheim/tmux-shell-attach-flat
    "atX" 'bergheim/tmux-shell-attach
    "bs" '(bergheim/switch-to-shell :which-key "shells")
    "ps" '((lambda ()
             (interactive)
             (other-window-prefix)
             (project-shell)) :which-key "shell")
    "p!" '(project-shell-command :which-key "shell command"))
  (bergheim/localleader-keys
    :states '(normal visual)
    :keymaps 'shell-mode-map
    "t" 'bergheim/tmux-shell-attach-flat
    "T" 'bergheim/tmux-shell-attach)
  :hook
  (shell-mode . bergheim/setup-shell)
  ;; this should improve how current directories are tracked
  (comint-output-filter-functions . comint-osc-process-output)
  :config
  (setq comint-check-proc nil)
  (setq confirm-kill-processes nil)
  (advice-add 'shell-mode :after
              (lambda ()
                (remove-hook 'kill-buffer-query-functions
                             'comint-kill-buffer-query-function t)))
  (setq shell-file-name "/bin/zsh"
        explicit-shell-file-name "/bin/zsh"
        explicit-zsh-args '("-i")
        shell-completion-execonly nil)

  (defun bergheim/setup-shell ()
    "Custom configurations for shell mode."
    (setq comint-input-ring-file-name "~/.histfile")
    (comint-read-input-ring 'silent)

    ;; stop duplicate input from appearing
    (setq-local comint-process-echoes t)
    (compilation-shell-minor-mode 1)
    (completion-preview-mode 1)

    ;; match the prompt so history works
    (setq-local comint-prompt-regexp "^[^λ]+λ ")

    ;; Don't add space after file completions (helps with directory traversal)
    (setq comint-completion-addsuffix nil)

    ;; Better file completion settings
    (setq comint-completion-autolist t)
    (setq comint-completion-fignore nil)

    ;; Improve history handling
    (setq comint-input-autoexpand t)
    (setq comint-completion-recexact nil)

    ;; Ensure we can complete from history
    (setq-local completion-at-point-functions
                (list #'comint-completion-at-point
                      #'comint-filename-completion
                      ;; #'cape-file
                      ;; #'cape-history
                      #'cape-dabbrev)))

  (cl-pushnew 'file-uri compilation-error-regexp-alist)
  (cl-pushnew '(file-uri "^file://\\([^:]+\\):\\([0-9]+\\)" 1 2)
              compilation-error-regexp-alist-alist :test #'equal)

  ;; this matches things like ./foo/bar/src.c and /foo/bar/src.c
  (cl-pushnew
   '(bare-file-col "^\\(\\(?:\\.\\.?/\\|/\\)[^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)
   compilation-error-regexp-alist-alist :test #'equal)
  (cl-pushnew 'bare-file-col compilation-error-regexp-alist)

  ;; this matches things like ╭─[/home/tsb/dev/nextjs-payload-optimized/src/collections/Media.ts:8:1]
  (cl-pushnew '(bracket-loc "\\[\\([^]\n]+\\):\\([0-9]+\\):\\([0-9]+\\)\\]" 1 2 3)
              compilation-error-regexp-alist-alist :test #'equal)
  (cl-pushnew 'bracket-loc compilation-error-regexp-alist)
  (setq comint-prompt-read-only t
        comint-scroll-to-bottom-on-input 'this
        ;; keep lots of history
        comint-input-ring-size 50000
        comint-input-ignoredups t
        shell-command-prompt-show-cwd t
        comint-completion-addsuffix '("/" . "")
        shell-kill-buffer-on-exit t)

  ;; this switches between only active shells, unlike multishell
  (defun bergheim/switch-to-shell ()
    "Switch to an active shell buffer using completion with directory info."
    (interactive)
    (if-let ((shell-buffers (seq-filter (lambda (buf)
                                          (with-current-buffer buf
                                            (derived-mode-p 'shell-mode 'eshell-mode 'term-mode 'vterm-mode)))
                                        (buffer-list))))
        (let* ((candidates (mapcar (lambda (buf)
                                     (cons (format "%s (%s)"
                                                   (buffer-name buf)
                                                   (with-current-buffer buf
                                                     (abbreviate-file-name default-directory)))
                                           buf))
                                   shell-buffers))
               (choice (completing-read "Switch to shell: " candidates nil t)))
          (switch-to-buffer (alist-get choice candidates nil nil #'string=)))
      (message "No active shell buffers")))

  (defun bergheim/tmux-shell-attach ()
    "Choose tmux session → window → pane, then attach via shell."
    (interactive)
    ;; get all sessions
    (let* ((sessions-output (shell-command-to-string "tmux list-sessions -F '#{session_name}'"))
           (sessions (split-string sessions-output "\n" t)))

      (if (null sessions)
          (message "No tmux sessions found")

        ;; choose session and skip of only one
        (let* ((session (if (= (length sessions) 1)
                            (car sessions)
                          (completing-read "Choose session: " sessions)))

               ;; get the windows
               (windows-output (shell-command-to-string
                                (format "tmux list-windows -t %s -F '#{window_index} #{window_name} [#{window_panes} panes]'" 
                                        session)))
               (windows (split-string windows-output "\n" t)))

          (if (null windows)
              (message "No windows found in session %s" session)

            ;; choose one and skip if only one
            (let* ((window-choice (if (= (length windows) 1)
                                      (car windows)
                                    (completing-read (format "Choose window from %s: " session) windows)))

                   (window-index (car (split-string window-choice " ")))

                   ;; get the panes
                   (panes-output (shell-command-to-string
                                  (format "tmux list-panes -t %s:%s -F '#{pane_index} [#{pane_current_command}] #{pane_current_path}'" 
                                          session window-index)))
                   (panes (split-string panes-output "\n" t)))

              (if (null panes)
                  (message "No panes found in window %s:%s" session window-index)

                ;; choose a pane and skip if only one
                (let* ((pane-choice (if (= (length panes) 1)
                                        (car panes)
                                      (completing-read (format "Choose pane from %s:%s: " session window-index) panes)))

                       (pane-index (car (split-string pane-choice " ")))
                       (full-target (format "%s:%s.%s" session window-index pane-index))
                       (buffer-name (format "*tmux-%s*" full-target)))

                  ;; create a shell, name it like tmux
                  (shell buffer-name)
                  (with-current-buffer buffer-name
                    (goto-char (point-max))
                    ;; attach!
                    (insert (format "tmux attach-session -t %s" full-target))
                    (comint-send-input))
                  (switch-to-buffer buffer-name)
                  (message "Attaching to tmux pane %s" full-target)))))))))

  (defun bergheim/woman-shell-command-other-window ()
    "Open the WoMan manpage for the current shell command in another window."
    (interactive)
    (let* ((input
            (save-excursion
              (comint-bol)
              (when (looking-at (format "[^%s]*" comint-prompt-regexp))
                (goto-char (match-end 0)))
              (buffer-substring-no-properties (point) (line-end-position))))
           (cmd (car (split-string input))))
      (if (and cmd (not (string= cmd "")))
          (let ((buf (save-window-excursion
                       (woman cmd)
                       (current-buffer))))
            (pop-to-buffer buf))
        (message "No command found on this line."))))

  (defun bergheim/tmux-shell-attach-flat ()
    "Choose any pane from any session/window and attach."
    (interactive)
    (let* ((panes-output (shell-command-to-string
                          "tmux list-panes -a -F '#{session_name}:#{window_index}.#{pane_index} #{window_name} [#{pane_current_command}] #{pane_current_path}'"))
           (panes (split-string panes-output "\n" t)))

      (if (null panes)
          (message "No tmux panes found")

        (let* ((choice (completing-read "Choose pane: " panes))
               (pane-target (car (split-string choice " ")))
               (buffer-name (format "*tmux-%s*" pane-target)))

          (shell buffer-name)
          (with-current-buffer buffer-name
            (goto-char (point-max))
            (insert (format "tmux attach-session -t %s" pane-target))
            (comint-send-input))
          (switch-to-buffer buffer-name)
          (message "Attaching to tmux pane %s" pane-target)
          ;; (run-with-timer 2 nil #'bergheim/tmux-emacs-mode-on)
          (bergheim/tmux-emacs-mode-on)
          )))))

(add-hook 'kill-buffer-hook
          (lambda ()
            (when (and (derived-mode-p 'comint-mode)
                       (string-match-p "tmux" (buffer-name))
                       (get-buffer-process (current-buffer)))
              (bergheim/tmux-emacs-mode-off))))

(use-package compile
  :ensure nil
  :hook
  (compilation-filter . ansi-osc-compilation-filter)
  (compilation-filter . ansi-color-compilation-filter)
  :general
  (bergheim/global-menu-keys
    "pc" '(bergheim/project-compile-dwim :which-key "compile")
    "pC" '(bergheim/open-project-compilation-buffer
           :which-key "open compile buffer"))
  :custom
  (compilation-max-output-line-length nil)
  (compilation-ask-about-save nil)
  (compilation-scroll-output t)

  :config
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


(defun bergheim/tmux-emacs-mode-on ()
  "Configure current tmux session for Emacs-friendly behavior."
  (interactive)
  (when (and (derived-mode-p 'comint-mode)
             (string-match-p "tmux" (buffer-name)))
    (let ((commands '("tmux set-option -p status off"
                      "_zsh_autosuggest_disable"
                      "unsetopt AUTO_MENU"
                      "bindkey -M viins '^R' history-incremental-search-backward")))
      ;; kind of dangerous to just straight up send these
      ;; (comint-send-string (current-buffer) "\C-c")
      ;; (sit-for 0.1)
      ;; (comint-send-string (current-buffer) "\C-u")
      ;; (sit-for 0.1)
      (dolist (cmd commands)
        (comint-send-string (current-buffer) (concat cmd "\n")))
      (message "Tmux session configured for Emacs"))))

(defun bergheim/tmux-emacs-mode-off ()
  "Restore original tmux session behavior."
  (interactive)
  (when (and (derived-mode-p 'comint-mode)
             (string-match-p "tmux" (buffer-name)))
    (let ((commands '("tmux set-option -p status on"
                      "_zsh_autosuggest_enable"
                      "setopt AUTO_MENU"
                      "bindkey -M viins '^R' fzf-history-widget"))) ; Restore fzf
      (dolist (cmd commands)
        (comint-send-string (current-buffer) (concat cmd "\n")))
      (message "Tmux session restored to original behavior"))))

(use-package coterm
  :after shell
  :init
  (coterm-mode 1))

(use-package eshell
  :ensure nil
  :general
  (:keymaps 'eshell-mode-map
   :states 'insert
   "C-r" #'consult-history
   "C-f" #'consult-dir
   "C-t" #'eshell/find-file-with-consult
   ;; "C-d" . eshell/z
   "C-k" #'eshell-previous-matching-input-from-input
   "C-j" #'eshell-next-matching-input-from-input)

  (bergheim/global-menu-keys
    "ate" '(eshell :which-key "eshell"))
  :config
  (setq eshell-destroy-buffer-when-process-dies t)

  (defun bergheim/eshell-git-info ()
    "Return git branch and status."
    (when (eq (call-process "git" nil nil nil "rev-parse" "--is-inside-work-tree") 0)
      (let* ((branch (string-trim
                      (shell-command-to-string
                       "git symbolic-ref --short HEAD 2>/dev/null || echo 'no commits'")))
             (dirty (not (string= "" (string-trim (shell-command-to-string "git status --porcelain")))))
             (dirty-info (if dirty
                             (propertize " ✎" 'face 'error)
                           (propertize " ✔" 'face 'success))))
        (concat (propertize "⎇ " 'face 'success)
                (propertize branch 'face 'warning)
                dirty-info))))

  (defun bergheim/eshell-prompt ()
    "Simple but kewl Eshell prompt with git info."
    (let ((dir (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-ls-directory))
          (git-info (or (bergheim/eshell-git-info) ""))
          (prompt (propertize (if (= (user-uid) 0) "#" "λ") 'face 'warning)))
      (concat dir " " git-info " " prompt " ")))

  (setq eshell-prompt-function 'bergheim/eshell-prompt)

  (defun eshell-get-old-input ()
    "Retrieve the current input from the Eshell prompt in the buffer."
    (buffer-substring-no-properties
     (save-excursion (eshell-bol) (point))
     (point)))

  (defun eshell/vi (filename)
    "Open FILENAME in another buffer within Eshell."
    (find-file-other-window filename))

  (defun eshell/mycat (&rest args)
    "Open files in other buffer"
    (if (null args)
        (user-error "No file specified")
      (dolist (file args)
        (find-file-read-only-other-window file))))

  (defun eshell/gst (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))   ;; The echo command suppresses output

  (defun eshell/find-file-insert-path ()
    "Use `fd` to find files and insert the selected path into the eshell prompt."
    (interactive)
    (let* ((query (read-string "Find file (query): "))
           (results (split-string
                     (shell-command-to-string (format "fd --type f %s" query))
                     "\n" t))
           (selected (completing-read "Select file: " results nil t)))
      (when (and selected (not (string-empty-p selected)))
        (insert selected))))

  (defun eshell/find-file-with-affe ()
    "Search for files using affe based on the current Eshell input and insert the selected file path into Eshell."
    (interactive)
    (let* ((input (eshell-get-old-input))
           ;; Extract the command and arguments from the input
           (args (split-string input "[ \t\n]+" t))
           (command (car args))
           ;; Use the second argument as the directory to search from, default to current
           (raw-dir (or (nth 1 args) "."))
           (base-dir (expand-file-name raw-dir default-directory))
           (valid-dir (if (file-directory-p base-dir) base-dir default-directory))
           ;; Customize affe's action to insert path in Eshell
           (affe-filter-func
            (lambda (path)
              (eshell-bol)
              (kill-line)
              (insert (concat command " " (shell-quote-argument path))))))
      (if (not valid-dir)
          (user-error "Invalid path (%s)" base-dir)
        (affe-find valid-dir))))

  (defun eshell/find-file-with-consult ()
    "Find files from your current dir args"
    (interactive)
    (let* ((input (eshell-get-old-input))
           ;; Extract arguments from input
           (args (split-string input "[ \t\n]+" t))
           (command (or (car args) ""))
           ;; Always expand the filepath no matter what
           (second-arg (or (nth 1 args) "."))
           (base-dir (expand-file-name second-arg default-directory))
           ;; FIXME: . or default-directory?
           (original-dir (or second-arg "."))
           (search-type (if (string-equal command "cd")
                            "d"  ; Search for directories
                          "f")) ; Search for files
           (valid-dir (if (file-directory-p base-dir) base-dir default-directory))
           (selected (consult--read
                      (split-string (shell-command-to-string
                                     (format "fd --type %s --hidden . %s"
                                             search-type
                                             (shell-quote-argument valid-dir)))
                                    "\n" t)
                      :prompt (format "Select %s:"
                                      (if (string-equal command "cd")
                                          "directory"
                                        "file"))
                      :sort nil)))
      (when (and selected (not (string-empty-p selected)))
        (eshell-bol)
        (kill-line)
        (insert (concat command " " (shell-quote-argument selected))))))

  ;; nicked from the consult-dir wiki
  (defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell."
    (interactive)
    (let ((eshell-dirs (delete-dups (mapcar 'abbreviate-file-name
                                            (ring-elements eshell-last-dir-ring)))))
      (cond
       ((and (not regexp) (featurep 'consult-dir))
        (let* ((consult-dir--source-eshell `(:name "Eshell"
                                             :narrow ?e
                                             :category file
                                             :face consult-file
                                             :items ,eshell-dirs))
               (consult-dir-sources (cons consult-dir--source-eshell consult-dir-sources)))
          (eshell/cd (substring-no-properties (consult-dir--pick "Switch directory: ")))))
       (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                       (completing-read "cd: " eshell-dirs)))))))

  (defun bergheim/open-dired-and-insert-file ()
    "Open Dired for the current input directory and insert selected file back into Eshell."
    (interactive)
    (let* ((current-input (eshell-get-old-input))
           (parts (split-string current-input " "))
           (command (car parts))
           (path (mapconcat 'identity (cdr parts) " "))
           (directory (or (file-name-directory (expand-file-name path)) default-directory))
           (filename (progn
                       (dired directory)
                       (let ((selected-file (dired-get-file-for-visit)))
                         (while (not selected-file)
                           (dired-next-line 1)
                           (setq selected-file (dired-get-file-for-visit)))
                         (file-relative-name selected-file directory)))))
      (when filename
        (kill-region (point-at-bol) (point-at-eol))
        (insert (concat command " " directory filename)))))

  (defun bergheim/point-is-directory-p ()
    "Check if the word at point is a directory path, or default-directory if not."
    (let ((word (thing-at-point 'filename t)))
      (if (or (not word) (string-empty-p word))
          (file-directory-p default-directory)
        (file-directory-p word))))

  (defvar bergheim/last-completion-point nil
    "Stores the last point of completion.")

  (defvar bergheim/eshell-complete-from-dir nil
    "Stores the directory where we started the completion.")

  (defun bergheim/extract-path ()
    "Extract the path or return nil if not found."
    (interactive) ;; TODO remove this
    (let* ((current-input (eshell-get-old-input))
           (parts (split-string current-input " "))
           (command (car parts))
           (path (mapconcat 'identity (cdr parts) " "))
           (directory (or (expand-file-name path) default-directory)))
      (when (bergheim/point-is-directory-p)
        (setq bergheim/eshell-complete-from-dir directory)
        directory)))

  (defun bergheim/completion-at-point-or-dired ()
    "Trigger `completion-at-point` or `dired` if called twice without moving point.
Open `dired` in the resolved directory of the current command."
    (interactive)
    (if (and (eq major-mode 'eshell-mode)
             (eq last-command this-command)
             (eq (point) bergheim/last-completion-point))
        (let ((path (bergheim/extract-path)))
          (when (and path (file-directory-p path))
            (setq bergheim/last-completion-point nil)
            (dirvish (or path default-directory))))
      (setq bergheim/last-completion-point (point))
      (completion-at-point)))

  (defun bergheim/exit-eshell-from-insert-mode ()
    "Exit Eshell if in `evil-insert' state."
    (interactive)
    (when (eq evil-state 'insert)
      (eshell-life-is-too-much)))

  (defun bergheim/eshell-tramp-cd-advice (orig-func &rest args)
    "Make `cd` with no args go to remote home when in a TRAMP connection."
    (if (and (file-remote-p default-directory)
             (or (null args) (= (length args) 0)))
        ;; If we're remote and no args, go to remote home
        (let ((remote-prefix (file-remote-p default-directory)))
          (funcall orig-func (concat remote-prefix "~")))
      ;; Otherwise use normal behavior
      (apply orig-func args)))

  (advice-add 'eshell/cd :around #'bergheim/eshell-tramp-cd-advice)

  (add-hook 'eshell-first-time-mode-hook
            (lambda ()
              (evil-define-key 'insert eshell-mode-map (kbd "TAB") 'bergheim/completion-at-point-or-dired)
              (evil-define-key 'insert eshell-mode-map (kbd "C-d") 'bergheim/exit-eshell-from-insert-mode)

              (eshell/alias "cat" "eshell/cat $*")

              (evil-define-key 'normal eshell-mode-map
                ;; this binding is pretty non-standard, but who uses A in the shell..
                (kbd "A")
                (lambda ()
                  (interactive)
                  (end-of-buffer)
                  (evil-append-line 1)))))


  (add-hook 'eshell-mode-hook (lambda ()
                                (eshell/alias "ll" "ls -lh $*")
                                (eshell/alias "l" "ll")
                                (eshell/alias "gs" "magit-status")
                                (eshell/alias "gd" "magit-diff-unstaged")
                                (eshell/alias "gds" "magit-diff-staged")


                                (eshell/alias "cat" "eshell/mycat $1")

                                (define-key eshell-mode-map (kbd "C-c f") 'eshell/find-file-with-consult)
                                (define-key eshell-mode-map (kbd "C-c t") 'eshell/find-file-with-consult)
                                (define-key eshell-mode-map (kbd "C-c d") 'eshell/affe-find))))

(use-package eat
  :commands eat
  :config
  (defun bergheim/eat-ctrl-r ()
    "Enhanced C-r for eat mode that handles fzf properly."
    (interactive)
    (if (derived-mode-p 'eat-mode)
        (progn
          (eat-char-mode)
          (eat-self-input 1 ?\C-r)
          ;; Monitor for return to prompt
          (add-hook 'eat-exec-hook #'bergheim/maybe-restore-line-mode nil t))
      (call-interactively 'isearch-backward)))

  (defun bergheim/maybe-restore-line-mode ()
    "Restore line mode if we're at a shell prompt."
    (when (and (derived-mode-p 'eat-mode)
               eat-enable-auto-line-mode)
      (eat-line-mode)
      (remove-hook 'eat-exec-hook #'bergheim/maybe-restore-line-mode t)))

  ;; Bind in eat-mode
  (add-hook 'eat-mode-hook
            (lambda ()
              (evil-define-key 'insert eat-mode-map (kbd "C-r") 'bergheim/eat-ctrl-r)))
  :custom
  (eat-shell "/bin/zsh")
  ;; works pretty well with vi mode.. except for fzf
  (eat-enable-auto-line-mode t)
  (eat-kill-buffer-on-exit t)
  :general
  (:states 'insert
   :keymaps 'eat-mode-map
   "C-r" #'consult-history)  ; Same as shell-mode
  (bergheim/global-menu-keys
    "atE" '(eat :which-key "Eat"))
  :config
  (add-hook 'eat-mode-hook
            (lambda ()
              (setq-local comint-input-ring-file-name "~/.histfile")
              (setq-local comint-input-ring (make-ring 1000))
              ;; Set up the filter to strip zsh extended history format
              (setq-local comint-input-ring-separator "\n")
              (setq-local comint-input-ring-file-prefix ": [0-9]+:[0-9]+;")
              (comint-read-input-ring 'silent)))

  ;; Add eat-mode to consult's mode histories
  (with-eval-after-load 'consult
    (add-to-list 'consult-mode-histories
                 '(eat-mode comint-input-ring comint-input-ring-index comint-bol)))
  :hook
  (eshell-first-time-mode . eat-eshell-mode)
  (eshell-first-time-mode . eat-eshell-visual-command-mode))

(use-package vterm
  :commands vterm
  :general
  (bergheim/global-menu-keys
    "atv" '(vterm :which-key "vterm"))
  :config
  (setq vterm-shell "/usr/bin/zsh")
  (setq vterm-max-scrollback 10000)
  (setq vterm-set-bold-hightbright t)
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local evil-insert-state-cursor 'box)
              (evil-insert-state))))
