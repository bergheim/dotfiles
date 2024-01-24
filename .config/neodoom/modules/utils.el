;;; bergheim-utils.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

;; TODO: rename this to bootstrap and move the others here out
(defun bergheim/toggle-scratch-buffer ()
  "Toggle the *scratch* buffer: show, hide, or switch to it based on its current state."
  (interactive)
  (let ((scratch-buffer (get-buffer-create "*scratch*")))
    (cond
     ;; hide if active
     ((eq (current-buffer) scratch-buffer)
      (bury-buffer))
     ;; switch to it if visible
     ((get-buffer-window scratch-buffer t)
      (pop-to-buffer scratch-buffer))
     ;; else open in current
     (t
      (switch-to-buffer scratch-buffer)))))

(defun bergheim/open-dirvish-current-project ()
  "Open dirvish in the root directory of the current project."
  (interactive)
  (let ((project-root (project-root (project-current t))))
    (if project-root
        (dirvish project-root)
      (message "No project found!"))))

(defun bergheim/copy-current-buffer-file ()
  "Copy the current buffer's file to a specified location."
  (interactive)
  (if buffer-file-name
      (let ((destination (read-file-name "Copy to: ")))
        (copy-file buffer-file-name destination t)
        (find-file destination)
        (message "File copied to: %s" destination))
    (message "No file is associated with this buffer.")))

(defun bergheim/next-file ()
  "Switch to the next file in the current directory."
  (interactive)
  (bergheim/next-in-directory 1))

(defun bergheim/prev-file ()
  "Switch to the previous file in the current directory."
  (interactive)
  (bergheim/next-in-directory -1))

(defun bergheim/next-in-directory (arg)
  "Move to the next or previous file in the current directory (and wrap around)."
  (let* ((current-file (buffer-file-name))
         (all-entries (directory-files (file-name-directory current-file) t))
         (files (seq-filter #'file-regular-p all-entries)) ; Filter out directories
         (position (cl-position current-file files :test 'string=))
         (num-files (length files))
         (next-pos (mod (+ position arg) num-files))) ; Wrap around using `mod`
    (find-file (nth next-pos files))))

(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :config
  (setq jinx-completion-method 'vertico))

(defvar bergheim/jinx-languages
  '("en_US" "nb" "fr_FR" "de_DE"))

(defun bergheim/jinx-language-sort (cands)
  (let ((langs (seq-intersection cands bergheim/jinx-languages)))
    (vertico-sort-history-alpha langs)))

(defun jinx--add-to-abbrev (overlay word)
  "Add abbreviation to `global-abbrev-table'.
The misspelled word is taken from OVERLAY.  WORD is the corrected word."
  (let ((abbrev (buffer-substring-no-properties
                 (overlay-start overlay)
                 (overlay-end overlay))))
    (message "Abbrev: %s -> %s" abbrev word)
    (define-abbrev global-abbrev-table abbrev word)))

(advice-add 'jinx--correct-replace :before #'jinx--add-to-abbrev)

;; (use-package unicode-fonts
;;   :ensure t
;;   :config
;;   (unicode-fonts-setup))

(use-package expand-region
  :ensure t
  :defer t
  :bind ("M-e" . er/expand-region))


(defun bergheim/call-with-universal-arg (fn)
  (lambda ()
    (interactive)
    (let ((current-prefix-arg 4))
      (call-interactively fn))))


(use-package iedit
  :config
  (general-define-key
   :states '(normal visual)
   "gR" 'iedit-mode))
(provide 'bergheim-utils)
;;; bergheim-utils.el ends here
