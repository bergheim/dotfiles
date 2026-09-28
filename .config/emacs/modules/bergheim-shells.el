;;; bergheim-shells.el --- Shell, eshell, ghostel, and compilation config -*- lexical-binding: t; -*-
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

(defun bergheim/comint-send-input-or-complete ()
  "Accept a selected corfu candidate, else RET as this mode wants it.
Popup with no selection: quit the popup, do nothing else.
No popup: agent-shell newline (M-RET sends), shell sends."
  (interactive)
  (cond
   ((and (bound-and-true-p completion-in-region-mode)
         (bound-and-true-p corfu--candidates)
         (>= corfu--index 0))
    (corfu--update)
    (corfu-complete))
   ((and (bound-and-true-p completion-in-region-mode)
         (bound-and-true-p corfu--candidates))
    (corfu-quit))
   ((derived-mode-p 'agent-shell-mode)
    (newline))
   (t (comint-send-input))))

(defun bergheim/comint-tab ()
  "Insert completion-preview if shown, else cycle corfu, else complete at point."
  (interactive)
  (cond
   ((bound-and-true-p completion-preview-active-mode)
    (completion-preview-insert))
   ((and (bound-and-true-p completion-in-region-mode)
         (bound-and-true-p corfu--candidates))
    (corfu-next 1))
   (t (completion-at-point))))

(defun bergheim/comint-slash ()
  "Zsh-like /: accept the visible preview, then ensure exactly one trailing slash."
  (interactive)
  (when (bound-and-true-p completion-preview-active-mode)
    (completion-preview-insert))
  (unless (eq (char-before) ?/)
    (insert "/")))

(with-eval-after-load 'corfu
  (add-to-list 'corfu-continue-commands #'bergheim/comint-send-input-or-complete)
  (add-to-list 'corfu-continue-commands #'bergheim/comint-tab)
  (add-to-list 'corfu-continue-commands #'bergheim/comint-slash))

(defun bergheim/comint-history ()
  "Insert a command from the shell history, most recent first."
  (interactive)
  (goto-char (point-max))
  (consult-history))

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
   "C-r" #'bergheim/comint-history
   "RET" (lambda ()
           (interactive)
           (if (comint-after-pmark-p)
               (comint-send-input)
             (evil-ret)))
   "<return>" (lambda ()
                (interactive)
                (if (comint-after-pmark-p)
                    (comint-send-input)
                  (evil-ret))))
  (:states 'insert
   :keymaps 'shell-mode-map
   "TAB" #'bergheim/comint-tab
   "/" #'bergheim/comint-slash
   "C-r" #'bergheim/comint-history
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
    "atx" '(bergheim/ghostel-tmux :which-key "tmux session")
    "bs" '(bergheim/switch-to-shell :which-key "shells")
    "ps" '((lambda ()
             (interactive)
             (other-window-prefix)
             (project-shell)) :which-key "shell")
    "p!" '(project-shell-command :which-key "shell command"))
  :hook
  (shell-mode . bergheim/setup-shell)
  ;; this should improve how current directories are tracked
  (comint-output-filter-functions . comint-osc-process-output)
  :config
  (setq comint-check-proc nil)
  (setq confirm-kill-processes nil)

  ;; in 31, this causes Cannot syntax-propertize because of narrowing!
  (setq shell-fontify-input-enable nil)
  (advice-add 'shell-mode :after
              (lambda ()
                (remove-hook 'kill-buffer-query-functions
                             'comint-kill-buffer-query-function t)))
  (let ((zsh (or (executable-find "zsh") "/bin/zsh")))
    (setq shell-file-name zsh
          explicit-shell-file-name zsh
          explicit-zsh-args '("-i")
          shell-completion-execonly nil))

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

    ;; Dir complete adds /; files add nothing (no space). / itself will not double.
    (setq-local comint-completion-addsuffix '("/" . ""))

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
    (if-let* ((shell-buffers (seq-filter (lambda (buf)
                                           (with-current-buffer buf
                                             (derived-mode-p 'shell-mode 'eshell-mode 'term-mode 'ghostel-mode)))
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
        (message "No command found on this line.")))))

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

  (defun bergheim/eshell-get-old-input ()
    "Return the eshell input from start-of-input to point.
Unlike the built-in `eshell-get-old-input', this only returns what
has been typed up to point — used by the consult helpers below (and
the affe one in bergheim-nav.el) that want to grab a partial command line."
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

  (defun eshell/find-file-with-consult ()
    "Find files from your current dir args"
    (interactive)
    (let* ((input (bergheim/eshell-get-old-input))
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
    (let* ((current-input (bergheim/eshell-get-old-input))
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
    (let* ((current-input (bergheim/eshell-get-old-input))
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
                                (define-key eshell-mode-map (kbd "C-c t") 'eshell/find-file-with-consult))))

(defun bergheim/ghostel-here ()
  "Open a new Ghostel in another window, using the current local or remote directory."
  (interactive)
  (other-window-prefix)
  (ghostel '(4)))

(defun bergheim/ghostel-home ()
  "Open a new Ghostel terminal in the local home directory."
  (interactive)
  (let ((default-directory (expand-file-name "~/" "/")))
    (ghostel '(4))))

(defun bergheim/ghostel-tmux-prefix ()
  "Return to terminal input and forward the tmux prefix key."
  (interactive)
  (ghostel-semi-char-mode)
  (goto-char (or (ghostel-cursor-point) (point-max)))
  (evil-insert-state)
  (ghostel--send-event))

(defun bergheim/ghostel-tmux ()
  "Pick or create a tmux session on the current host in another window."
  (interactive)
  (require 'ghostel)
  (let* ((host (or (file-remote-p default-directory 'host) (system-name)))
         ;; ((NAME ID) ...); a failed listing (no server yet) means no sessions
         (sessions (with-temp-buffer
                     (when (eq 0 (process-file "tmux" nil t nil "list-sessions"
                                               "-F" "#{session_name}\t#{session_id}"))
                       (mapcar (lambda (line) (split-string line "\t"))
                               (split-string (buffer-string) "\n" t)))))
         (choice (completing-read (format "Tmux session on %s (or new name): " host)
                                  sessions))
         ;; Attach by id: a name containing . or : cannot be used as a target
         (target (cadr (assoc choice sessions))))
    ;; tmux interprets ; and # even in separate argv elements
    (when (and (not target) (string-match-p "\\`[[:space:]]*\\'\\|[.:;#[:cntrl:]]" choice))
      (user-error "New session name must be non-blank, without . : ; # or control characters"))
    (let ((buffer (generate-new-buffer (format "*tmux: %s@%s*" choice host))))
      (with-current-buffer buffer
        (ghostel-mode)
        (setq-local ghostel-buffer-name-function nil))
      (other-window-prefix)
      (pop-to-buffer buffer)
      (ghostel-exec buffer "tmux" (if target
                                      (list "attach-session" "-t" target)
                                    (list "new-session" "-A" "-s" choice))))))

(use-package ghostel
  :ensure (:wait t)
  :commands (ghostel ghostel-project)
  :custom
  (ghostel-buffer-name-function #'ghostel-buffer-name-by-directory)
  :general
  (bergheim/global-menu-keys
    "atg" '(bergheim/ghostel-home :which-key "ghostel home")
    "atG" '(ghostel-project :which-key "ghostel project"))
  (:keymaps 'ghostel-semi-char-mode-map
   :states 'insert
   "M-p" #'ghostel--send-event
   "M-n" #'ghostel--send-event))

(use-package evil-ghostel
  :after (ghostel evil)
  :custom
  (evil-ghostel-escape 'evil)
  :hook (ghostel-mode . evil-ghostel-mode)
  :config
  (evil-define-motion bergheim/ghostel-beginning-of-input ()
    "Move after a recognized prompt, or to the beginning of an output line."
    :type exclusive
    (ghostel-beginning-of-input-or-line))
  :general
  (:keymaps 'evil-ghostel-mode-map
   :states '(normal insert)
   "C-SPC" #'bergheim/ghostel-tmux-prefix
   "C-@" #'bergheim/ghostel-tmux-prefix)
  (:keymaps 'evil-ghostel-mode-map
   :states '(normal visual operator)
   "0" #'bergheim/ghostel-beginning-of-input)
  ;; Raw char mode's higher-priority escape map still owns M-RET.
  (:keymaps 'evil-ghostel-mode-map
   :states '(normal visual insert emacs)
   "M-RET" #'bergheim/ghostel-here
   "M-<return>" #'bergheim/ghostel-here)
  (bergheim/localleader-keys
    :states '(normal visual)
    :keymaps 'evil-ghostel-mode-map
    "n" '(bergheim/ghostel-here :which-key "new here")
    "h" '(bergheim/ghostel-home :which-key "new at home")
    "p" '(ghostel-project :which-key "project terminal")
    "b" '(ghostel-list-buffers :which-key "switch terminal")
    "t" '(bergheim/ghostel-tmux :which-key "attach/create tmux session")
    "[" '(ghostel-previous :which-key "previous terminal")
    "]" '(ghostel-next :which-key "next terminal")
    "r" '(rename-buffer :which-key "rename buffer")
    "c" '(ghostel-copy-mode :which-key "copy mode (freeze output)")
    "y" '(ghostel-copy-all :which-key "copy all scrollback")
    "v" '(ghostel-yank-pop :which-key "paste from kill ring")
    "f" '(ghostel-find-file-at-point :which-key "open file/link")
    "j" '(:ignore t :which-key "jump")
    "jn" '(ghostel-next-prompt :which-key "next prompt")
    "jp" '(ghostel-previous-prompt :which-key "previous prompt")
    "jl" '(ghostel-next-hyperlink :which-key "next link")
    "jL" '(ghostel-previous-hyperlink :which-key "previous link")
    "i" '(:ignore t :which-key "input mode")
    "is" '(ghostel-semi-char-mode :which-key "semi-char (default)")
    "il" '(ghostel-line-mode :which-key "line editing")
    "ie" '(ghostel-emacs-mode :which-key "Emacs (live scrollback)")
    "ic" '(ghostel-char-mode :which-key "raw char (M-RET exits)")
    "iq" '(ghostel-readonly-exit :which-key "exit read-only")
    "s" '(:ignore t :which-key "send")
    "sk" '(ghostel-send-next-key :which-key "literal next key")
    "sc" '(ghostel-send-C-c :which-key "interrupt (C-c)")
    "sz" '(ghostel-send-C-z :which-key "suspend (C-z)")
    "x" '(:ignore t :which-key "screen")
    "xc" '(ghostel-clear :which-key "clear screen (keep history)")
    "xC" '(ghostel-clear-scrollback :which-key "DELETE screen + scrollback")
    "xr" '(ghostel-force-redraw :which-key "redraw")))

(use-package ghostel-eshell
  :ensure nil
  :hook (eshell-load . ghostel-eshell-visual-command-mode))

;; Shell-mode only, not global — agent-shell is comint too.
(use-package ghostel-comint
  :ensure nil
  :hook (shell-mode . ghostel-comint-mode))

(use-package term-keys
  :ensure (:host github :repo "CyberShadow/term-keys")
  :demand
  :config
  (term-keys-mode t))

;;; bergheim-shells.el ends here
