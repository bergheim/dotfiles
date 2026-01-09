;; -*- lexical-binding: t; -*-

(use-package password-store
  :general
  (bergheim/global-menu-keys
    "yp" 'password-store-copy
    "ip" 'password-store-generate
    "iP" 'password-store-generate-no-symbols))

(use-package pass
  :unless bergheim/container-mode-p)

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
    "att" '((lambda () (interactive) (multishell-pop-to-shell nil (expand-file-name default-directory))) :which-key "shell")
    "atT" '((lambda () (interactive) (multishell-pop-to-shell '(4))) :which-key "new shell"))
  :config
  ;; don't ask for history confirmation on quit
  (remove-hook 'kill-buffer-query-functions #'multishell-kill-buffer-query-function))

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
          (message "Attaching to tmux pane %s" pane-target))))))
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
  :custom
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

(use-package shr
  :ensure nil
  :custom
  ;; (toggle-truncate-lines 1)
  (shr-max-width 120))

(use-package proced
  :ensure nil
  :commands proced
  :general
  (bergheim/global-menu-keys
    "ap" '(proced :which-key "Proced"))
  :hook
  (proced-post-display . hl-line-mode)
  :custom
  (proced-auto-update-flag 'visible)
  (proced-auto-update-interval 2)
  (proced-goal-attribute nil) ;; don't move cursor to args when navigating
  (proced-show-remote-processes t) ;; enable TRAMP support
  (proced-enable-color-flag t)
  (proced-format 'custom)
  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid tree pcpu rss start state (args comm))))

;; pastebin stuff
(use-package 0x0
  :unless bergheim/container-mode-p
  :after general

  :general
  (bergheim/global-menu-keys
    "ys" '(:ignore t :which-key "Share")
    "yss" '(0x0-dwim :which-key "Dwim")
    "ysp" '(0x0-popup :which-key "Text")
    "ysf" '(0x0-upload-file :which-key "File")))

(use-package elfeed
  :unless bergheim/container-mode-p
  :after general
  :commands elfeed
  :init
  (setq elfeed-db-directory (bergheim/get-and-ensure-data-dir "elfeed/db/")
        elfeed-enclosure-default-dir (bergheim/get-and-ensure-data-dir "elfeed/enclosures/"))

  :hook
  (elfeed-search . (lambda () (setq-local display-line-numbers nil)))
  (elfeed-show . (lambda () (setq-local display-line-numbers nil)))

  :general
  (:keymaps 'elfeed-search-mode-map
   :states 'normal
   "d" #'bergheim/elfeed-by-domain
   "C" #'bergheim/elfeed-by-domain)

  :config
  (defun bergheim/elfeed-by-domain ()
    "Filter Elfeed search results to show only entries from the domain of the currently selected feed."
    (interactive)
    (let* ((entry (or (elfeed-search-selected :single)
                      (user-error "No entry selected")))
           (feed (elfeed-entry-feed entry))
           (feed-url (elfeed-feed-url feed))
           (url-host (when feed-url
                       (url-host (url-generic-parse-url feed-url)))))
      (unless url-host
        (user-error "Unable to determine feed's domain"))
      (elfeed-search-set-filter (format "=%s" url-host))))

  (defhydra bergheim/hydra-elfeed (:foreign-keys run)
    "filter"
    ("a" (elfeed-search-set-filter "@6-months-ago")            "All")
    ("d" (elfeed-search-set-filter "@6-months-ago +dev")       "Development")
    ("e" (elfeed-search-set-filter "@6-months-ago +emacs")     "Emacs")
    ("*" (elfeed-search-set-filter "@6-months-ago +star")      "Starred")
    ("r" (elfeed-search-set-filter "@6-months-ago -unread")      "Read")
    ("u" (elfeed-search-set-filter "@6-months-ago +unread")      "Unread")
    ;; ("m" (bergheim/elfeed-toggle-starred)                                    "Star")
    ("m" (lambda () (interactive) (elfeed-search-toggle-all 'star))                                    "Star")
    ("t" (elfeed-search-set-filter "@1-day-ago")               "Today")
    ("q" nil                                                   "quit" :color blue))

  ;; (transient-define-prefix bergheim/elfeed-transient ()
  ;;   "Elfeed Transient"
  ;;   ["Elfeed Filters"
  ;;    ("e" "emacs"       (lambda () (interactive) (elfeed-search-set-filter "@6-months-ago +emacs")) :transient t)
  ;;    ("d" "dev"   (lambda () (interactive) (elfeed-search-set-filter "@6-months-ago +dev")))
  ;;    ("*" "Starred"     (lambda () (interactive) (elfeed-search-set-filter "@6-months-ago +star")))
  ;;    ;; ("M" "Mark"        elfeed-toggle-star)
  ;;    ("a" "All"         (lambda () (interactive) (elfeed-search-set-filter "@6-months-ago")))
  ;;    ("t" "Today"       (lambda () (interactive) (elfeed-search-set-filter "@1-day-ago")))
  ;;    ["General"
  ;;     ;; ("Q" "Quit Elfeed" bjm/elfeed-save-db-and-bury)
  ;;     ;; ("q" "quit" nil)
  ;;     ]]
  ;;   )
  )

;; from https://github.com/skeeto/elfeed/issues/466#issuecomment-1275327427
(define-advice elfeed-search--header (:around (oldfun &rest args))
  (if elfeed-db
      (apply oldfun args)
    "No database loaded yet"))

(use-package elfeed-org
  :after elfeed
  :demand
  :init
  (setq rmh-elfeed-org-files (list (expand-file-name "elfeed/elfeed.org" org-directory)))
  :config
  (elfeed-org))

(use-package elfeed-protocol
  :after elfeed
  :demand
  :general
  (:keymaps 'elfeed-search-mode-map
   :states 'normal
   "gr" #'bergheim/elfeed-refresh)
  :init
  (defun bergheim/elfeed-refresh ()
    (interactive)
    (mark-whole-buffer)
    (cl-loop for entry in (elfeed-search-selected)
             do (elfeed-untag-1 entry 'unread))
    (elfeed-search-update--force)
    (elfeed-protocol-fever-reinit "https://tsb@thomasbergheim.com/rss"))
  :config
  (setq elfeed-use-curl t)
  ;; nextcloud
  ;; (setq elfeed-protocol-feeds '(("owncloud+https://tsb@cloud.thomasbergheim.com"
  ;;                                :password (password-store-get "websites/cloud.thomasbergheim.com/tsb"))))
  ;; (setq elfeed-protocol-enabled-protocols '(owncloud))
  ;; (setq elfeed-protocol-owncloud-star-tag 'star)

  ;; miniflux / fever
  (setq elfeed-protocol-fever-update-unread-only nil)
  (setq elfeed-protocol-fever-fetch-category-as-tag nil)
  (setq elfeed-protocol-feeds '(("fever+https://tsb@thomasbergheim.com/rss"
                                 :api-url "https://thomasbergheim.com/rss/fever/"
                                 :password (password-store-get "mycloud/miniflux/fever"))))
  (setq elfeed-protocol-enabled-protocols '(fever))

  ;; (defvar elfeed-protocol-orig-feeds nil
  ;;   "Store original content of `elfeed-feeds'.")
  ;; (defadvice elfeed (after configure-elfeed-feeds activate)
  ;;   "Make elfeed-org autotags rules works with elfeed-protocol."
  ;;   (setq
  ;;    elfeed-protocol-orig-feeds elfeed-protocol-feeds
  ;;    elfeed-protocol-feeds (list
  ;;                           (list "fever+https://tsb@thomasbergheim.com/rss"
  ;;                                 :api-url "https://thomasbergheim.com/rss/fever/"
  ;;                                 :password (password-store-get "mycloud/miniflux/fever")
  ;;                                 :autotags  elfeed-protocol-orig-feeds)))
  ;;   (elfeed-update))


  ;; enable elfeed-protocol
  (elfeed-protocol-enable))

;; (use-package elfeed-goodies
;;   :after elfeed
;;   :demand
;;   :config
;;   (elfeed-goodies/setup))

(use-package smudge
  :unless bergheim/container-mode-p
  :custom
  (smudge-oauth2-client-secret bergheim/spotify/client-secret)
  (smudge-oauth2-client-id bergheim/spotify/client-id)
  (smudge-player-use-transient-map t)
  (smudge-transport 'connect)
  (smudge-player-status-refresh-interval 0)
  (smudge-api-locale "nb_NO")
  (smudge-api-country "NO")
  (smudge-status-location nil)
  :config
  ;; A hydra for controlling spotify.
  (defhydra hydra-spotify (:hint nil)
    "
^Search^                  ^Control^               ^Manage^
^^^^^^^^-----------------------------------------------------------------
_t_: Track               _SPC_: Play/Pause        _+_: Volume up
_m_: My Playlists        _n_  : Next Track        _-_: Volume down
_f_: Featured Playlists  _p_  : Previous Track    _x_: Mute
_u_: User Playlists      _r_  : Repeat            _d_: Device
^^                       _s_  : Shuffle           _q_: Quit
"
    ("t" smudge-track-search :exit t)
    ("m" smudge-my-playlists :exit t)
    ("f" smudge-featured-playlists :exit t)
    ("u" smudge-user-playlists :exit t)
    ("SPC" smudge-controller-toggle-play :exit nil)
    ("n" smudge-controller-next-track :exit nil)
    ("p" smudge-controller-previous-track :exit nil)
    ("r" smudge-controller-toggle-repeat :exit nil)
    ("s" smudge-controller-toggle-shuffle :exit nil)
    ("+" smudge-controller-volume-up :exit nil)
    ("-" smudge-controller-volume-down :exit nil)
    ("x" smudge-controller-volume-mute-unmute :exit nil)
    ("d" smudge-select-device :exit nil)
    ("q" quit-window "quit" :color blue)))

;; nicked from https://codeberg.org/alternateved/dotfiles/src/branch/main/emacs/.config/emacs/init.el
(use-package erc
  :ensure t
  :after consult
  :autoload erc-buffer-list
  :init
  (setq erc-hide-list
        '("JOIN" "PART" "QUIT" "NICK" "MODE"  ; standard events
          "324"                               ; channel mode
          "329"                               ; channel creation time
          "332" "333"                         ; topic and topic setter
          "353" "366"                         ; names list and end of names
          "477"                               ; channel not available
          "305" "306"                         ; away status
          "328"                               ; channel URL
          "250" "251" "252" "253" "254" "255" ; server stats
          "265" "266"                         ; local/global users
          "401" "404" "405" "406"             ; various errors
          "471" "473" "474" "475"             ; channel errors
          "476")                              ; bad channel mask
        erc-track-exclude-types erc-hide-list)
  :hook
  ;; (erc-mode . erc-spelling-mode)
  (erc-mode . erc-notifications-mode)
  (erc-mode . (lambda ()
                (setq-local orderless-matching-styles '(orderless-literal-prefix)
                            confirm-kill-processes nil
                            corfu-auto-prefix 3)
                (if (featurep 'jinx)
                    (jinx-mode 1))
                (erc-fill-wrap-mode 1)
                (setq-local completion-at-point-functions
                            '(cape-emoji
                              erc-complete-word-at-point))
                (display-line-numbers-mode 0)))
  (erc-status-sidebar-mode . (lambda () (display-line-numbers-mode 0)))
  (speedbar-mode . (lambda () (display-line-numbers-mode 0)))
  :custom
  (erc-autojoin-channels-alist bergheim/irc-channels)

  (erc-autojoin-timing 'ident)
  (erc-autojoin-delay 5)
  (erc-fill-static-center 14)
  (erc-fool-highlight-type 'all)

  ;;;; Logging
  (erc-log-channels-directory (expand-file-name "logs/erc/channels/" bergheim/home-dir))
  (erc-log-write-after-insert t)
  (erc-log-write-after-send t)
  (erc-save-buffer-on-part t)
  (erc-save-queries-on-quit t)

  (erc-fools bergheim/irc-fools)
  (erc-header-line-format nil)
  (erc-log-insert-log-on-open t)
  (erc-insert-timestamp-function 'erc-insert-timestamp-left)
  (erc-timestamp-only-if-changed-flag t)
  (erc-interpret-mirc-color t)
  (erc-join-buffer 'buffer)
  ;; (erc-nicks-contrast-range '(1 . 100))
  (erc-nick "tsb")
  (erc-prompt (format ">"))
  (erc-prompt-for-password nil)
  (erc-prompt "⟩")

  :general
  (bergheim/global-menu-keys
    "ai" '(:ignore t :which-key "irc (erc)")
    "aii" '(bergheim/erc-connect :which-key "init")
    "aiq" '(erc-quit-server :which-key "quit")
    "aib" '(bergheim/consult-erc-buffer :which-key "buffers"))
  (bergheim/localleader-keys
    :states '(normal visual)
    :keymaps 'erc-mode-map
    ;; TODO erc-imenu-mode
    "b" '(bergheim/consult-erc-buffer :which-key "channels")
    "c" 'erc-bufbar-mode
    "s" '(bergheim/erc-swoop-nick :which-key "swoop")
    "d" '(bergheim/erc-open-or-capture-user-note-denote :which-key "denote")
    "n" 'erc-nickbar-mode)
  (:states 'normal
   :keymaps 'erc-mode-map
   "A" (lambda ()
         (interactive)
         (goto-char (point-max))
         (evil-append 0)))
  (:states 'insert
   :keymaps 'erc-mode-map
   "M-h" (lambda ()
           (interactive)
           (evil-normal-state)
           (evil-window-left 1))
   "M-l" (lambda ()
           (interactive)
           (evil-normal-state)
           (evil-window-right 1))
   "C-k" 'erc-previous-command
   "C-j" 'erc-next-command
   "M-h" 'evil-window-left
   "M-l" 'evil-window-right
   "C-u" #'evil-change-whole-line)
  :config
  (erc-log-mode 1)
  (if (< emacs-major-version 30)
      (use-package erc-hl-nicks)
    (add-to-list 'erc-modules 'nicks))

  (add-to-list 'erc-modules 'scrolltobottom)

  (defun bergheim/erc-buffer-connected-p (buffer)
    "Check if ERC BUFFER is connected."
    (with-current-buffer buffer
      (and (derived-mode-p 'erc-mode)
           (erc-server-process-alive)
           erc-server-connected)))

  (defun bergheim/erc-connected-p ()
    "Check if any ERC buffer is connected."
    (seq-some #'bergheim/erc-buffer-connected-p (erc-buffer-list)))

  (defun bergheim/erc-connect ()
    "Open ERC in a dedicated frame and show specified channels."
    (interactive)
    (unless (bergheim/erc-connected-p)
      (erc :server bergheim/irc-server
           :port 6667
           :nick bergheim/irc-nick
           :user bergheim/irc-username
           :password (password-store-get "apps/soju")))
    ;; create or switch to erc frame
    (let* ((frame-name "erc")
           (target-frame
            (or (car (seq-filter
                      (lambda (frame)
                        (and (frame-live-p frame)
                             (string= frame-name (frame-parameter frame 'name))))
                      (frame-list)))
                (make-frame `((name . ,frame-name))))))
      (select-frame-set-input-focus target-frame)
      (delete-other-windows) ;; Ensure any existing splits are removed
      (split-window-right)
      ;; LOL
      (run-with-timer
       3 nil
       (lambda ()
         (when (get-buffer bergheim/irc-channel-a)
           (switch-to-buffer bergheim/irc-channel-a))
         (other-window 1)
         (when (get-buffer bergheim/irc-channel-b)
           (switch-to-buffer bergheim/irc-channel-b))))))

  (autoload 'erc-buffer-list "erc")
  (defvar erc-buffer-source
    `(:name     "ERC"
      :hidden   t
      :narrow   ?e
      :category buffer
      :state    ,#'consult--buffer-state
      :items    ,(lambda () (mapcar #'buffer-name (erc-buffer-list)))))

  (add-to-list 'consult-buffer-sources 'erc-buffer-source 'append)

  (defun bergheim/consult-erc-buffer ()
    "Consult ERC buffers directly."
    (interactive)
    (let ((erc-buffer-source-visible (copy-sequence erc-buffer-source)))
      ;; Temporarily set :hidden to nil for this buffer source
      (setf (plist-get erc-buffer-source-visible :hidden) nil)
      (let ((consult-buffer-sources (list erc-buffer-source-visible)))
        (consult-buffer))))

  (defun bergheim/erc-swoop-nick ()
    "Search for nick in the current ERC buffer, prepopulated with nicks."
    (interactive)
    (let* ((nicks (bergheim/erc-collect-nicks))
           (nick (completing-read "Choose nick: " nicks nil t)))
      (consult-line (format "<%s>" (regexp-quote nick)))))

  (defun bergheim/erc-open-or-capture-user-note-roam-simple ()
    "Search for nick in the current ERC buffer, prepopulated with nicks."
    (interactive)
    (let* ((nicks (bergheim/erc-collect-nicks))
           (nick (completing-read "Choose nick: " nicks nil t)))
      (org-roam-node-find nil (concat nick "-" (symbol-name (erc-network))))))

  (defun bergheim/erc-open-or-capture-user-note-denote ()
    "Open or capture information in a denote note for an IRC user in the current network."
    (interactive)
    (require 'denote)
    (let* ((nicks (bergheim/erc-collect-nicks))
           (nick (completing-read "Choose nick: " nicks nil t))
           (network (symbol-name (erc-network)))
           (slugified-title (denote-sluggify 'title nick))
           (keywords `("irc" ,network))
           (selected-text (when (use-region-p)
                            (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))))
           (choice (completing-read "Action: " '("Open note" "Capture info") nil t))
           (file-regex (format ".*%s.*%s"
                               (regexp-quote slugified-title)
                               (string-join (mapcar (lambda (kw)
                                                      (denote-sluggify 'keyword kw))
                                                    keywords) ".*")))
           (existing-file (car (denote-directory-files file-regex))))

      (if existing-file
          (find-file-other-window existing-file)
        (if (one-window-p)
            (split-window-right))
        (other-window 1)
        (denote slugified-title keywords nil nil nil 'person))
      (when (string-equal choice "Capture info")
        (goto-char (point-max))
        (insert (format "\n** %s\n" (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (when selected-text
          (insert (format "#+begin_quote\n%s\n#+end_quote\n" selected-text)))
        (save-buffer)
        (when (featurep 'evil)
          (evil-insert-state)))))

  ;; ;; bergheim/erc-collect-nicks seems more useful since that is only active users
  ;; (defun bergheim/erc-nicks-from-channel ()
  ;;   "Get a list of nicks from the current ERC channel."
  ;;   (let (nicks)
  ;;     (when (bound-and-true-p erc-channel-users)
  ;;       (maphash (lambda (nick _)
  ;;                  (push nick nicks))
  ;;                erc-channel-users))
  ;;     nicks))

  (defun bergheim/erc-collect-nicks ()
    "Collect active/visible nicks from the current ERC buffer."
    (let (nicks)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                "<\\(@?[A-Za-z0-9_]+\\)> "
                nil t)
          (push (match-string 1) nicks)))
      nicks)))
