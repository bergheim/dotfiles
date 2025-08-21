;; -*- lexical-binding: t; -*-

(use-package denote
  :ensure
  :after org
  :init
  (setq denote-directory (expand-file-name "denote" org-directory))
  :custom
  (denote-known-keywords '("emacs" "journal"))
  (denote-date-prompt-use-org-read-date t)
  (denote-backlinks-show-context t)
  :hook
  (dired-mode . denote-dired-mode-in-directories)
  :config
  (setq denote-dired-directories-include-subdirectories t
        denote-dired-directories
        (list denote-directory
              (expand-file-name "data" denote-directory)
              (expand-file-name "data" org-directory)))

  (defun my-denote-tmr ()
    (tmr "5" "Write focused now.."))
  (add-hook 'denote-journal-hook 'my-denote-tmr)
  ;; (add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe)
  (denote-rename-buffer-mode 1)

  (setq denote-templates
        `((default . "")
          (person . ,(concat "* Contact Info\n"
                             "- Name: \n"
                             "- Website: \n"
                             "- Github: \n"
                             "- Email: \n"
                             "\n"
                             "* About\n"
                             "* Notes and Quotes\n"))))

  (defun bergheim/denote-new-journal-entry ()
    "Create a new journal entry and enter writer mode"
    (interactive)
    (siren-tab-bar-switch-to-or-create-tab "journal")
    (let ((entry-today (denote-journal--entry-today)))
      (if entry-today
          (denote-open-or-create (car entry-today))
        (denote-journal-new-entry)))
    (bergheim/write-mode t)
    (goto-char (point-max))
    (delete-trailing-whitespace)
    (insert "\n* [" (format-time-string "%H:%M") "] ")
    (evil-insert 0))

  (defun bergheim/denote-last-journal-entry ()
    "Open the newest entry"
    (interactive)
    (let* ((all-entries (directory-files denote-journal-directory t "^[^.]"))
           (files (seq-filter #'file-regular-p all-entries)))
      (when files
        (siren-tab-bar-switch-to-or-create-tab "journal")
        (find-file (expand-file-name
                    (car (last (sort files 'string<))) denote-journal-directory))
        (bergheim/write-mode t))))

  :general
  (bergheim/global-menu-keys
    "n" '(:ignore t :which-key "Denote")
    "na" '(denote-add-links :which-key "Add all inks")
    "nb" '(denote-find-backlink :which-key "Show backlinks")
    "nB" '(denote-backlinks :which-key "Show backlinks")
    "nd" '(denote :which-key "New note")
    "nf" '(denote-open-or-create :which-key "Find")
    ;;"ndj" '(denote-journal-extras-new-or-existing-entry :which-key "Journal")
    "ne" '(denote-org-extract-org-subtree :which-key "Extract from node")
    "nh" '(denote-org-link-to-heading :which-key "Link to heading")
    "ni" '(:ignore t :which-key "Insert")
    "nib" '(denote-org-dblock-insert-backlinks :which-key "backlinks")
    "nif" '(denote-org-dblock-insert-files :which-key "files")
    "nil" '(denote-org-dblock-insert-links :which-key "links")
    "nj" '(:ignore t :which-key "Journal")
    "njj" '(bergheim/denote-new-journal-entry :which-key "New journal")
    "njl" '(bergheim/denote-last-journal-entry :which-key "Last journal entry")
    "njb" '((lambda () (interactive) (find-file denote-journal-directory)) :which-key "Browse journals")
    "nL" '(denote-find-link :which-key "Show links")
    "nl" '(denote-link-or-create :which-key "Link")
    "nn" '(denote-open-or-create :which-key "Open/create")
    "nr" '(denote-rename-file-using-front-matter :which-key "Rename")
    "nr" '(denote-rename-file :which-key "Rename")
    "nR" '(denote-rename-file-signature :which-key "Rename signature")
    "ns" '(consult-notes-search-in-all-notes :which-key "Search")
    "nS" '(denote-grep :which-key "Grep")))

(use-package denote-journal
  :after denote
  :demand)

(use-package denote-org
  :after denote
  :demand)

(use-package denote-journal-capture
  :after denote-journal
  :demand)

(use-package denote-menu
  :after denote)

(use-package consult-denote
  :after denote)

;; TODO: see https://lucidmanager.org/productivity/denote-explore/
;; (use-package denote-explore
;;   :after denote)
