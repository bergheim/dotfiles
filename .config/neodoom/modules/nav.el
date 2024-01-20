;;; nav.el --- Description -*- lexical-binding: t; -*-

(use-package consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :config
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  (when (locate-library "org-roam")
    (consult-notes-org-roam-mode)))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         ("C-c C-c" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package dired
  :elpaca nil
  :after general
  :init
  (defun bergheim//executables-in-path ()
    "Retrieve a list of all executable files in `exec-path'."
    (let ((files_in_path))
      (dolist (dir exec-path)
        (when (and dir (file-exists-p dir))
          (let ((files (directory-files dir t)))
            (dolist (file files)
              (when (and (file-executable-p file)
                         (not (file-directory-p file)))
                (push (file-name-nondirectory file) files_in_path))))))
      files_in_path))

  (defun bergheim/open-file (arg)
    "Open the current file in 'dired-mode' with an application.
With a universal argument, it allows entering the application to use."
    (interactive "P")
    (let* ((file (dired-get-filename nil t))
           (command (if arg
                        (completing-read "Open current file with: " (bergheim//executables-in-path))
                      "xdg-open")))
      (if file
          (start-process command nil command file)
        (message "No file on this line"))))

  (setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
        ;; don't prompt to revert, just do it
        ;; dired-auto-revert-buffer #'dired-buffer-stale-p
        ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Ask whether destination dirs should get created when copying/removing files.
        dired-create-destination-dirs 'ask
        ;; Where to store image caches
        image-dired-dir (concat bergheim/cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        ;; Screens are larger nowadays, we can afford slightly larger thumbnails
        image-dired-thumb-size 150)
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "h"   #'dired-up-directory
   "l"   #'dired-find-file))

(use-package dirvish
  :ensure t
  :demand t
  :after general
  ;; :commands (dirvish)
  :config
  (setq dirvish-emerge-mode t)
  (add-hook 'dirvish-setup-hook  #'dirvish-emerge-mode)
  (dirvish-override-dired-mode)
  (setq dirvish-cache-dir (concat bergheim/cache-dir "dirvish/"))
  (setq dirvish-attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size))
  (setq dirvish-subtree-state-style 'nerd)

  (setq dirvish-emerge-groups '(("Recent files" (predicate . recent-files-today))
                                ("Documents" (extensions "pdf" "tex" "bib" "epub" "doc"))
                                ("Video" (extensions "mp4" "mkv" "webm"))
                                ("Pictures" (extensions "jpg" "png" "svg" "gif"))
                                ("Audio" (extensions "mp3" "flac" "wav" "ape" "aac"))
                                ("Archives" (extensions "gz" "rar" "zip"))))

  (setq dirvish-quick-access-entries '(("h" "~/" "Home")
                                       ("d" "~/Downloads/" "Downloads")
                                       ("D" "~/dev" "Development")
                                       ("e" "~/.config/neodoom/" "Emacs user directory")))

  :general
  (:keymaps 'dirvish-mode-map
   :states 'normal
   "?"   #'dirvish-dispatch
   "b"   #'dirvish-history-jump
   "q"   #'dirvish-quit
   "a"   #'dirvish-quick-access
   "C"   #'dired-create-directory
   "f"   #'dirvish-fd
   "F"   #'dirvish-fd-ask
   ;; this is hilarious. yes, I do in fact want to copy it as kill
   "y"   #'dired-copy-filename-as-kill
   "Y"   #'dirvish-copy-file-name
   "p"   #'dirvish-yank-menu
   "o"   #'dired-sort-toggle-or-edit
   "O"   #'dirvish-quicksort
   "s"   #'dirvish-narrow
   "S"   #'dirvish-fd-ask
   "TAB" #'dirvish-subtree-toggle
   "C-h" #'dirvish-history-go-backward
   "C-l" #'dirvish-history-go-forward)

  (bergheim/localleader-keys
   :keymaps 'dirvish-mode-map

   "b" '(bergheim/open-file :which-key "Open")
   "B" `(,(bergheim/call-with-universal-arg #'bergheim/open-file) :which-key "Open With")
   "c" '(dired-create-empty-file :which-key "Create empty file")
   "C" '(dired-create-directory :which-key "Create directory")
   "e" '(gnus-dired-attach :which-key "Attach to email")
   "E" '(dirvish-emerge-mode :which-key "toggle emerge")
   "f" '(dirvish-layout-toggle :which-key "toggle fullscreen")
   "i" '(dirvish-file-info-menu :which-key "file info menu")
   "j" '(consult-dir :which-key "switch dir")
   "s" '(dirvish-setup-menu :which-key "setup menu")
   "m" '(dirvish-mark-menu :which-key "mark menu")
   "o" '((lambda ()
           (interactive)
           (call-interactively 'bergheim/org-attach-dired-to-subtree))
         :which-key "Copy to org")
   "O" `(,(bergheim/call-with-universal-arg #'bergheim/org-attach-dired-to-subtree)
         :which-key "Move to org")))

(defun bergheim/delete-current-file ()
  "Delete the current file and kill its buffer, after asking for confirmation."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (when (and current-file
               (file-exists-p current-file)
               (yes-or-no-p (format "Really delete file %s? " current-file)))
      (delete-file current-file)
      (kill-buffer))))

(use-package affe
  :ensure t)

;;; nav.el ends here
