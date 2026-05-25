;;; bergheim-feeds.el --- elfeed RSS reader -*- lexical-binding: t; -*-

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
    (elfeed-protocol-fever-reinit bergheim/elfeed-fever-url))
  :config
  (setq elfeed-use-curl t)
  ;; miniflux / fever
  (setq elfeed-protocol-fever-update-unread-only nil)
  (setq elfeed-protocol-fever-fetch-category-as-tag nil)
  (setq elfeed-protocol-feeds
        `((,(concat "fever+" bergheim/elfeed-fever-url)
           :api-url ,bergheim/elfeed-fever-api-url
           :password ,(password-store-get bergheim/elfeed-fever-password-store-key))))
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

;;; bergheim-feeds.el ends here
