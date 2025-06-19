;;; nav.el --- Description -*- lexical-binding: t; -*-

(use-package avy
  :demand t
  :after evil
  :custom
  (avy-timeout-seconds 0.4)
  ;; this is needed if we want to allow actions on single matches
  ;; (avy-single-candidate-jump nil)
  :config
  ;; this allows us to go back. strange the evil version does not do this..
  (defadvice evil-avy-goto-char-timer (around bergheim/save-position activate)
    (evil-set-jump)
    ad-do-it)

  (general-define-key
   :states '(normal visual)
   "M-d" 'evil-avy-goto-char-timer
   "g SPC" 'evil-avy-goto-char-timer
   "gu" 'avy-resume)
  ;; FIXME: too broad; this messes up the surround operator, say ds(
  ;; (general-define-key
  ;;  :states 'operator
  ;;  "z" 'evil-avy-goto-char-2
  ;;  "x" 'evil-avy-goto-char-2
  ;;  "s" 'evil-avy-goto-char-2-below
  ;;  "S" 'evil-avy-goto-char-2-above)
  )

(use-package link-hint
  :ensure t
  :defer t
  :general
  (:states 'normal
   "gl" #'link-hint-open-link
   "gL" #'link-hint-copy-link))

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
  :after consult
  :bind (("C-c C-d" . consult-dir)
         :map vertico-map
         ("M-d" . consult-dir)
         ("M-f" . consult-dir-jump-file)
         ("C-c C-d" . consult-dir)
         ("C-c C-f" . consult-dir-jump-file))
  :general
  (bergheim/global-menu-keys
    "RET" '(consult-dir :which-key "Bookmarks and history"))
  :config
  ;; prefer recent files/dirs to static things
  (setq consult-dir-sources
        '(consult-dir--source-recentf
          consult-dir--source-bookmark
          consult-dir--source-default
          consult-dir--source-project
          consult-dir--source-tramp-local)))

(use-package dired
  :ensure nil
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
        dired-use-ls-dired t ;; parse names reliably
        dired-clean-confirm-killing-deleted-buffers nil
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
   "M-<return>" #'bergheim/dired-leave-for-shell
   "h"   #'dired-up-directory
   "l"   #'dired-find-file))

(use-package dirvish
  :after dired
  :config
  ;; (add-hook 'dirvish-setup-hook  #'dirvish-emerge-mode)
  (setq dirvish-emerge-mode nil
        dired-listing-switches "-alGh --group-directories-first"
        dired-omit-files
        (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
                (seq bol "." (not (any "."))) ;; dot-files
                (seq "~" eol)))               ;; backup-files
        dirvish-cache-dir (expand-file-name "dirvish/" bergheim/cache-dir)
        dirvish-attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-subtree-state-style 'nerd
        dirvish-epub-thumbnailer-program "gnome-epub-thumbnailer"

        dirvish-emerge-groups '(("Recent files" (predicate . recent-files-today))
                                ("Documents" (extensions "pdf" "tex" "bib" "epub" "doc"))
                                ("Video" (extensions "mp4" "mkv" "webm"))
                                ("Pictures" (extensions "jpg" "png" "svg" "gif"))
                                ("Audio" (extensions "mp3" "flac" "wav" "ape" "aac"))
                                ("Archives" (extensions "gz" "rar" "zip")))

        dirvish-quick-access-entries '(("h" "~/" "Home")
                                       ("d" "~/Downloads/" "Downloads")
                                       ("D" "~/dev" "Development")
                                       ("e" "~/.config/emacs/" "Emacs user directory")))
  (dirvish-override-dired-mode)
  ;; see https://github.com/alexluigit/dirvish/issues/188
  (evil-make-overriding-map dirvish-mode-map 'normal)
  :general
  (:keymaps 'dirvish-mode-map
   :states 'normal
   "?"   #'dirvish-dispatch
   "H"   #'dirvish-history-jump
   "q"   #'dirvish-quit
   "a"   #'dirvish-quick-access
   "C"   #'dired-create-directory
   "s"   #'dirvish-fd-ask
   "S"   #'dirvish-fd
   "v"   'dirvish-vc-menu
   "f"   #'dirvish-narrow ;; "filter"
   ;; this is hilarious. yes, I do in fact want to copy it as kill
   "y"   #'dired-copy-filename-as-kill
   "Y"   #'dirvish-copy-file-name
   "p"   #'dirvish-yank-menu
   "o"   #'dired-sort-toggle-or-edit
   "O"   #'dirvish-quicksort
   "TAB" #'dirvish-subtree-toggle
   "C-<return>" #'bergheim/dired-return-path
   "M-<return>" #'bergheim/dired-leave-for-shell
   "C-h" #'dirvish-history-go-backward
   "C-l" #'dirvish-history-go-forward
   "C-M-k" #'dirvish-emerge-previous-group
   "C-M-j" #'dirvish-emerge-next-group)

  (:keymaps 'dirvish-mode-map
   :states '(normal visual)
   "G" #'end-of-buffer
   "gg" #'beginning-of-buffer)

  (bergheim/localleader-keys
    :keymaps 'dirvish-mode-map

    "x" '(bergheim/open-file :which-key "Open")
    "X" `(,(bergheim/call-with-universal-arg #'bergheim/open-file) :which-key "Open With")
    "c" '(dired-create-empty-file :which-key "Create empty file")
    "C" '(dired-create-directory :which-key "Create directory")
    "e" '(gnus-dired-attach :which-key "Attach to email")
    "E" '(dirvish-emerge-mode :which-key "toggle emerge")
    "f" '(dirvish-layout-toggle :which-key "toggle fullscreen")
    "i" '(dirvish-file-info-menu :which-key "file info menu")
    "j" '(consult-dir :which-key "switch dir")
    "s" '(dirvish-setup-menu :which-key "setup menu")
    "m" '(dirvish-mark-menu :which-key "mark menu")
    "w" '(bergheim/dired-set-as-wallpaper :which-key "wallpaper")
    "o" `(,(lambda ()
             (interactive)
             (call-interactively 'bergheim/org-attach-dired-to-subtree))
          :which-key "Copy to org")
    "O" `(,(bergheim/call-with-universal-arg #'bergheim/org-attach-dired-to-subtree)
          :which-key "Move to org"))

  (defun bergheim/dired-leave-for-shell ()
    "Quit dired and open shell in the current directory."
    (interactive)
    (let ((current-dir default-directory))
      (dirvish-quit)
      (multishell-pop-to-shell)
      (comint-send-string (current-buffer) (concat "cd " current-dir "\n"))
      (evil-insert 0)
      ))

  (defun bergheim/dired-leave-for-eshell ()
    "Quit dirvish and open eshell in the current directory."
    (interactive)
    (let ((current-dir default-directory))
      (dirvish-quit)
      (eshell)
      (eshell/cd current-dir)
      (eshell-send-input))))

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
  :ensure t
  :config
  (setq affe-find-command "fd --color=never --hidden --follow --exclude .git --exclude node_modules --regex"))

(defun bergheim/consult-browse-links ()
  "Find and open URLs in the current buffer.

Search through lines in the current buffer containing URLs.
Prompt the user to select a line, then open the chosen URL in the
default web browser. Navigate forward through the buffer if a prefix
argument is given, otherwise navigate backward."
  (interactive)
  (let ((vertico-sort-function nil)
        candidates
        (search-fn (if current-prefix-arg 're-search-forward 're-search-backward))
        (url-regex "\\(https?://[^][:space:]\n]+\\)"))
    (save-excursion
      (goto-char (if current-prefix-arg (point-min) (point-max)))
      (while (funcall search-fn url-regex nil t)
        (let ((url (match-string-no-properties 0))
              (line (string-trim (thing-at-point 'line t)))
              (pos (point)))
          (push (cons line (cons url pos)) candidates)))

      (unless candidates ; Abort if no candidates found
        (user-error "No URLs found"))

      (let ((selected nil))
        (consult--read
         (mapcar #'car candidates)
         :prompt "Lines with URLs: "
         :lookup (lambda (user-query cands narrow input)
                   (when-let ((entry (assoc user-query candidates)))
                     (goto-char (cdr (cdr entry)))
                     (cons user-query t)))
         :require-match t
         :state (lambda (action cand)
                  (when (eq action 'return)
                    (setq selected (car cand)))))

        (when selected
          (when-let (url (car (cdr (assoc selected candidates))))
            (message "Opening URL: %s" url)
            (browse-url url)))))))
;;; nav.el ends here
