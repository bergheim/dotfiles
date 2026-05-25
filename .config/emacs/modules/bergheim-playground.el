
(defun bergheim/dired-set-as-wallpaper (darkmode)
  "Sets FILE to the current wallpaper"
  (interactive "P")

  (if-let* ((file (dired-file-name-at-point))
            (swaysock (car (file-expand-wildcards "/run/user/*/sway-ipc.*.sock")))
            (dest (expand-file-name
                   (if (bergheim//system-dark-mode-enabled-p) "~/Pictures/wallpapers/active/dark/primary.jpg"
                     "~/Pictures/wallpapers/active/light/primary.jpg"))))
      (progn
        (copy-file file dest t)
        (when swaysock
          (let ((process-environment (cons (format "SWAYSOCK=%s" swaysock) 
                                           process-environment)))
            (call-process "swaymsg" nil nil nil
                          (format "exec swaybg -m fill -i %s" dest))
            (message "Wallpaper set to %s" dest)
            )))
    (warn "Unable to find a file")))





;;;;;;;;;;;;;; erc

;; 1) define the minor mode and its keymap
(defvar bergheim/erc-unified-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'bergheim/erc-unified-goto)
    map)
  "Keymap for `bergheim/erc-unified-mode'.")

(define-derived-mode bergheim/erc-unified-mode fundamental-mode "ERC-Unified"
  "Major mode for the unified ERC log buffer."
  :group 'erc
  (setq-local buffer-read-only t)
  (visual-line-mode 1)
  (use-local-map bergheim/erc-unified-mode-map))

;; 2) the jump command
(defun bergheim/erc-unified-visit ()
  "In an `erc-unified' buffer, jump to the same message in its channel buffer.
Searches from the bottom of the channel buffer backward for the exact text."
  (interactive)
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
         ;; match “[time] #chan nick:
         message…”
         (re   "^\\[.*?\\] \\([^ ]+\\) [^:]+: \\(.*\\)$")
         (chan (and (string-match re line) (match-string 1 line)))
         (msg  (and (string-match re line) (match-string 2 line)))
         (buf  (and chan (get-buffer chan))))
    (unless (and buf msg)
      (user-error "Cannot parse channel or message"))
    ;; switch to channel buffer in another window (splits if needed)
    (switch-to-buffer-other-window buf)
    ;; go to bottom and search backward for the exact message text
    (goto-char (point-max))
    (if (re-search-backward (regexp-quote msg) nil t)
        (goto-char (match-beginning 0))
      (message "ERC[%s]: \"%s\" not found, at bottom" chan msg)
      (goto-char (point-max)))))

;; 3) whenever erc-log-match makes the "erc-unified" buffer, turn on our mode
(define-advice erc-log-match-make-buffer
    (:filter-return (buf) bergheim/mark-unified)
  "If the new buffer is called \"erc-unified\", enable `bergheim/erc-unified-mode`."
  (when (and buf (string= (buffer-name buf) "erc-unified"))
    (with-current-buffer buf
      (bergheim/erc-unified-mode)))
  buf)



(erc :server "burial.tb"
     :port 6667
     :user"tsb"
     :password "AfgrXhh9JEbf7WgLzivOo4Tp")

(erc-open "burial.tb"
          6667
          "tsb"
          "tsb"
          t
          "AfgrXhh9JEbf7WgLzivOo4Tp")


(use-package image-slicing
  :demand
  :ensure (:host github :repo "ginqi7/image-slicing"))

(defun bergheim/eww-save-to-denote ()
  "Save the current EWW page content to a new Denote note."
  (interactive)
  (let* ((title (plist-get eww-data :title))
         (url (plist-get eww-data :url))
         (eww-buffer (current-buffer))
         (readable-content (progn
                             (eww-readable)
                             (string-trim (buffer-string))))
         (formatted-content (replace-regexp-in-string
                             "^\\* " "- " readable-content)))
    
    (denote title '("eww"))
    
    (with-current-buffer (current-buffer)
      (save-excursion
        (goto-char (point-max))
        (insert "\n\nSource: " url "\n\n")
        (insert "#+begin_quote\n")
        (insert formatted-content)
        (insert "\n#+end_quote")))))


(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq mode major-mode)
          (push buf buffer-mode-matches))))
    buffer-mode-matches))


(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))


(defun bergheim/eww-select-link ()
  "Select and open a link from all links in the current eww buffer."
  (interactive)
  (let ((links '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "https?://[^\s-]+" nil t)
        (let ((url (match-string 0))
              (context (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position))))
          (push (cons (format "%s [%s]" context url) url) links))))
    (if links
        (let* ((choice (completing-read "Open link: " links))
               (url (cdr (assoc choice links))))
          (eww url))
      (message "No links found in buffer"))))

(defun bergheim/gptel-summarize-buffer ()
  "Test gptel-request with a pirate system prompt, insert response in current buffer."
  (interactive)
  (let ((waiting-point (point))
        (buffer (current-buffer))
        (buffer-content (buffer-string))
        (system-prompt (condition-case err
                           (with-temp-buffer
                             (insert-file-contents "/home/tsb/.config/emacs/prompts/compactor.md")
                             (buffer-string))
                         (error (format "Error reading prompt file: %s" err)))))
    (insert "\n\n[⏳ Waiting for response...]")
    (gptel-request buffer-content
                   :system system-prompt
                   :callback `(lambda (response info)
                                (with-current-buffer ,buffer
                                  (save-excursion
                                    (goto-char ,waiting-point)
                                    (delete-char ,(length "\n\n
[⏳ Waiting for response...]"))))
                                (let ((response-buffer (generate-new-buffer "*GPTel Response*")))
                                  (with-current-buffer response-buffer
                                    (if (stringp response)
                                        (insert response)
                                      (insert (format "Request failed: %s" (plist-get info :status))))
                                    (goto-char (point-min))
                                    (org-mode))
                                  (switch-to-buffer-other-window response-buffer))))))

(defun bergheim/kill-compilation-process-dwim ()
  "Kill running compilation process without confirmation.
If multiple project compilation buffers exist, choose interactively.
Otherwise kill the single buffer's process."
  (interactive)
  (let ((compilation-buffers (bergheim/get-project-compilation-buffers)))
    (cond
     ((= (length compilation-buffers) 0)
      (message "No compilation buffers found"))

     ((= (length compilation-buffers) 1)
      (let* ((buffer (cadar compilation-buffers))
             (process (get-buffer-process buffer)))
        (if process
            (progn
              (delete-process process)
              (message "Killed process in %s" (buffer-name buffer)))
          (message "No process running in %s" (buffer-name buffer)))))

     (t
      (let* ((candidates (mapcar (lambda (item)
                                   (let ((name (car item))
                                         (status (caddr item)))
                                     (format "%s [%s]" name status)))
                                 compilation-buffers))
             (choice (completing-read "Kill process in: " candidates nil t))
             (buffer-name (car (split-string choice " \\[")))
             (buffer (get-buffer buffer-name))
             (process (get-buffer-process buffer)))
        (if process
            (progn
              (delete-process process)
              (message "Killed process in %s" buffer-name))
          (message "No process running in %s" buffer-name)))))))






(defun bergheim/sanitize-compile-command (command)
  "Sanitize COMMAND for use in buffer names.
Replace non-alphanumeric characters with dashes, collapse multiple dashes."
  (let ((sanitized (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" command)))
    ;; Trim leading/trailing dashes
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
         ;; Pass the command to get unique buffer name
         (compilation-buffer-name-function 
          (lambda (_mode) (bergheim/project-compilation-buffer-name command)))
         (existing-buffer (get-buffer (bergheim/project-compilation-buffer-name command))))
    
    (if (and existing-buffer
             (with-current-buffer existing-buffer
               (and (derived-mode-p 'compilation-mode 'comint-mode)
                    (get-buffer-process existing-buffer))))
        ;; Recompile existing
        (with-current-buffer existing-buffer
          (if (derived-mode-p 'comint-mode)
              (progn
                (pop-to-buffer existing-buffer)
                (message "Already running in comint mode. Switch to buffer."))
            (recompile)))
      ;; New compilation
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



(defun bergheim/shr-urlify (start url &optional title)
  "Make region between START and point into a link to URL.
This is a replacement for `shr-urlify' that doesn't add [N] annotations."
  (message "asdf")
  )

(advice-add 'shr-urlify :override #'bergheim/shr-urlify)







(defun bergheim/flymake-hide-columns ()
  "Remove file and backend columns from flymake diagnostics buffer."
  (when (and (derived-mode-p 'flymake-project-diagnostics-mode)
             tabulated-list-format)
    ;; Create new format with only line, col, message
    (let ((new-format (make-vector 3 nil))
          (old-format tabulated-list-format))
      (aset new-format 0 (aref old-format 1))  ; Line 
      (aset new-format 1 (aref old-format 2))  ; Col
      (aset new-format 2 (aref old-format 3))  ; Message (assuming type is actually message)
      (setq tabulated-list-format new-format)
      (tabulated-list-init-header)
      (tabulated-list-print t))))

(add-hook 'flymake-project-diagnostics-mode-hook #'bergheim/flymake-hide-columns)


(defun bergheim/debug-flymake-format ()
  "Show the actual column format."
  (interactive)
  (when tabulated-list-format
    (message "Columns: %S" (seq-map (lambda (col) (car col)) tabulated-list-format))))


(defun bergheim/flymake-hide-columns ()
  "Show only the message column in flymake diagnostics buffer."
  (when (and (derived-mode-p 'flymake-project-diagnostics-mode)
             tabulated-list-format)
    ;; Create new format with only the message column
    (let ((new-format (make-vector 1 nil))
          (old-format tabulated-list-format))
      (aset new-format 0 (list "Message" 80 t))  ; Just the message with proper width
      (setq tabulated-list-format new-format)
      (tabulated-list-init-header)
      (tabulated-list-print t))))


(defun bergheim/flymake-hide-columns ()
  "Show only the message column in flymake diagnostics buffer."
  (when (and (derived-mode-p 'flymake-project-diagnostics-mode)
             tabulated-list-format)
    ;; Create new format with only the message column
    (let ((new-format (make-vector 1 nil))
          (old-format tabulated-list-format)
          (message-col (copy-sequence (aref old-format 5))))  ; Copy the Message column
      ;; Modify the width in the copied column
      (setcar (cdr message-col) 80)  ; Set width to 80
      (aset new-format 0 message-col)
      (setq tabulated-list-format new-format)
      (tabulated-list-init-header)
      (tabulated-list-print t))))

(defun bergheim/flymake-hide-columns ()
  "Show only the message column in flymake diagnostics buffer."
  (when (and (derived-mode-p 'flymake-project-diagnostics-mode)
             tabulated-list-format)
    ;; Get the message column and fix its width
    (let ((message-col (copy-sequence (aref tabulated-list-format 5))))
      (setcar (cdr message-col) 80)  ; Set width to 80
      ;; Create new format with only the message column
      (setq tabulated-list-format (vector message-col))
      (tabulated-list-init-header)
      (tabulated-list-print t))))

(defun bergheim/flymake-hide-columns ()
  "Show only message column by modifying entries data."
  (message "bergheim/flymake-hide-columns called!")
  (when (and (derived-mode-p 'flymake-project-diagnostics-mode)
             tabulated-list-format)
    (message "Inside flymake diagnostics mode, modifying columns...")
    ;; Set format to only show message
    (setq tabulated-list-format [("Message" 80 t)])
    
    ;; Modify the entries to only contain message data  
    (setq tabulated-list-entries
          (mapcar (lambda (entry)
                    (list (car entry)  ; Keep the ID
                          (vector (aref (cadr entry) 5))))  ; Only message column data
                  tabulated-list-entries))
    
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (message "Columns modified!")))



(defun bergheim/flymake-remove-backend-column ()
  "Remove Backend and Col columns from flymake project diagnostics."
  (when (eq major-mode 'flymake-project-diagnostics-mode)
    (let* ((remove-cols '("Backend" "Col"))
           (indices (cl-loop for col across tabulated-list-format
                             for i from 0
                             when (cl-some (lambda (name) 
                                             (string-match-p name (car col)))
                                           remove-cols)
                             collect i))
           (orig-entries tabulated-list-entries))
      (setq tabulated-list-format
            (cl-map 'vector
                    (lambda (col)
                      (if (string-match-p "File" (car col))
                          (cons (car col) (cons 10 (cddr col)))
                        col))
                    (cl-remove-if (lambda (col)
                                    (cl-some (lambda (name) 
                                               (string-match-p name (car col)))
                                             remove-cols))
                                  tabulated-list-format)))
      (setq tabulated-list-entries
            (lambda ()
              (mapcar (lambda (entry)
                        (list (car entry)
                              (vconcat 
                               (cl-loop for item across (cadr entry)
                                        for i from 0
                                        unless (member i indices)
                                        collect item))))
                      (if (functionp orig-entries)
                          (funcall orig-entries)
                        orig-entries))))
      (tabulated-list-init-header)
      (tabulated-list-print t))))
(define-key evil-normal-state-map (kbd "K") #'eldoc-print-current-symbol-info)
