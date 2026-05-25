;;; bergheim-diagnostics.el --- Flymake and related diagnostics tooling -*- lexical-binding: t; -*-

(use-package flymake
  :ensure nil
  :defer t
  :hook
  (flymake-project-diagnostics-mode . bergheim/flymake-remove-backend-column)
  :general
  (:keymaps 'prog-mode-map
   "M-RET" #'bergheim/flymake-jump-to-current-error)
  (general-def
    :states '(normal visual motion)
    "ge" (lambda () (interactive) (consult-flymake t))
    "gE" #'flymake-show-project-diagnostics)

  (general-def
    :states '(normal visual motion)
    :keymaps 'override
    "]e" #'bergheim/flymake-goto-next-error-project
    "[e" #'bergheim/flymake-goto-prev-error-project)
  :config
  (defun bergheim/flymake-goto-next-error-project (&optional n)
    "Go to next flymake error across project buffers, skipping same line."
    (interactive "p")
    (let* ((n (or n 1))
           (project (project-current))
           (all-diags (and project (flymake--project-diagnostics project)))
           (sorted (sort (copy-sequence all-diags)
                         (lambda (a b)
                           (let ((buf-a (flymake-diagnostic-buffer a))
                                 (buf-b (flymake-diagnostic-buffer b)))
                             (if (eq buf-a buf-b)
                                 (< (flymake-diagnostic-beg a)
                                    (flymake-diagnostic-beg b))
                               (string< (buffer-name buf-a)
                                        (buffer-name buf-b)))))))
           (current-buf (current-buffer))
           (current-line (line-number-at-pos))
           (next-diag (if (> n 0)
                          (cl-find-if (lambda (d)
                                        (let ((dbuf (flymake-diagnostic-buffer d))
                                              (dline (with-current-buffer (flymake-diagnostic-buffer d)
                                                       (line-number-at-pos (flymake-diagnostic-beg d)))))
                                          (or (not (eq dbuf current-buf))
                                              (> dline current-line))))
                                      sorted)
                        (cl-find-if (lambda (d)
                                      (let ((dbuf (flymake-diagnostic-buffer d))
                                            (dline (with-current-buffer (flymake-diagnostic-buffer d)
                                                     (line-number-at-pos (flymake-diagnostic-beg d)))))
                                        (or (not (eq dbuf current-buf))
                                            (< dline current-line))))
                                    (reverse sorted)))))
      (if next-diag
          (progn
            (switch-to-buffer (flymake-diagnostic-buffer next-diag))
            (goto-char (flymake-diagnostic-beg next-diag))
            (pulse-momentary-highlight-one-line))
        (message "No more diagnostics in project"))))

  (defun bergheim/flymake-goto-prev-error-project (&optional n)
    "Go to previous flymake error across project buffers."
    (interactive "p")
    (bergheim/flymake-goto-next-error-project (- (or n 1))))

  (defun bergheim/flymake-remove-backend-column ()
    "Remove Backend and Col columns from flymake project diagnostics."
    (when (eq major-mode 'flymake-project-diagnostics-mode)
      (visual-line-mode 1)  ;; wrap long lines
      (let* ((remove-cols '("Backend" "Col"))
             (filename-width 10)
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
                            (cons (car col) (cons filename-width (cddr col)))
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
  (defun bergheim/flymake-jump-to-current-error ()
    "Jump to the error for current line in flymake diagnostics buffer."
    (interactive)
    (let ((current-line (line-number-at-pos))
          (current-file (buffer-file-name))
          (original-window (selected-window))
          (diag-buf (seq-find (lambda (buf)
                                (string-match "\\*Flymake diagnostics for" (buffer-name buf)))
                              (buffer-list))))
      (unless current-file
        (user-error "Buffer has no associated file"))
      (unless diag-buf
        (flymake-show-project-diagnostics)
        (setq diag-buf (seq-find (lambda (buf)
                                   (string-match "\\*Flymake diagnostics for" (buffer-name buf)))
                                 (buffer-list))))
      (when diag-buf
        (pop-to-buffer diag-buf)
        (goto-char (point-min))
        (let ((filename (file-name-nondirectory current-file)))
          (if (re-search-forward (format "^%s\\s-+%d\\s-" filename current-line) nil t)
              (progn
                (beginning-of-line)
                (pulse-momentary-highlight-one-line (point)))  ; Visual feedback
            (progn
              (select-window original-window)  ; Go back if no error found
              (message "No error on line %d" current-line)))))))

  (add-to-list 'display-buffer-alist
               '("\\*Flymake.*diagnostics.*\\*"
                 (display-buffer-at-bottom)
                 (window-height . 0.3)))
  (setq flymake-temporary-file-directory (bergheim/get-and-ensure-data-dir "flymake")))

;;; bergheim-diagnostics.el ends here
