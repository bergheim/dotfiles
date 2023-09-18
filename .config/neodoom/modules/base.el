
(use-package git-auto-commit-mode
  :ensure t)

(use-package esup
  :ensure t
  ;; To prevent any graphical interface to pop-up.
  :custom (esup-depth 0))

(use-package elfeed
  :ensure t
  :commands elfeed
  :init
  (setq elfeed-db-directory (concat bergheim/cache-dir "elfeed/db/")
        elfeed-enclosure-default-dir (concat bergheim/cache-dir "elfeed/enclosures/"))
  :config
  ;; (setq elfeed-search-filter "@2-week-ago ")
        ;; elfeed-show-entry-switch #'pop-to-buffer
        ;; elfeed-show-entry-delete #'+rss/delete-pane
        ;; shr-max-image-proportion 0.8)
  (make-directory elfeed-db-directory t))

(use-package elfeed-org
  :ensure t
  :after elfeed
  :preface
  (setq rmh-elfeed-org-files (list "elfeed.org"))
  :config
  (elfeed-org))

(use-package elfeed-goodies
  :ensure t
  :after elfeed
  :config
  (elfeed-goodies/setup))

(use-package minibuffer
  :after general
  :config
  (general-define-key
   :keymaps 'minibuffer-local-map
   "C-w" 'backward-kill-word
   "C-u" 'backward-kill-sentence))

(use-package restclient
  :ensure t
  :defer t)
