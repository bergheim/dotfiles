;;; bergheim-browser.el --- In-Emacs web browsing (eww, w3m, shr) -*- lexical-binding: t; -*-

(use-package shr
  :ensure nil
  :custom
  ;; (toggle-truncate-lines 1)
  (shr-max-width 120))

(use-package eww
  :ensure nil
  :general
  (bergheim/global-menu-keys
    "bw" '(bergheim/consult-browser-buffer :which-key "browser buffers")
    "sw" '(eww :which-key "web search"))
  :hook
  (eww-after-render . bergheim/eww-rename-buffer)
  ;; for some reason we must do this after to get the keys set up
  (eww-mode . (lambda ()
                (setq line-spacing 0) ;; this makes images not have gaps
                (bergheim/localleader-keys
                  :states '(normal visual)
                  :keymaps 'local
                  "b" '(bergheim/consult-browser-buffer :which-key "browser buffers")
                  "f" '(link-hint-open-link :which-key "follow link")
                  "F" '(bergheim/link-hint-open-background :which-key "open link background")
                  "x" '(eww-browse-with-external-browser :which-key "open external browser")
                  "w" '(bergheim/eww-open-link-w3m :which-key "open in w3m")
                  "r" '(bergheim/eww-select-link :which-key "grep links")
                  "y" '(eww-copy-page-url :which-key "copy link")
                  "r" '(eww-reload :which-key "reload")
                  "s" '(eww-view-source :which-key "view source")
                  "h" '(eww-list-histories :which-key "tab history")
                  "i" '(eww-toggle-images :which-key "toggle images")
                  "o" '(bergheim/eww-open-link-w3m :which-key "open in w3m")
                  "d" '(eww-download :which-key "download"))

                (evil-define-key 'normal eww-mode-map
                  (kbd "gs") 'eww-view-source
                  (kbd "gy") 'link-hint-copy-link
                  (kbd "gf") 'link-hint-open-link
                  (kbd "gF") 'bergheim/link-hint-open-background
                  (kbd "M-RET") 'bergheim/open-link-background
                  (kbd "]t") 'eww-buffer-show-next
                  (kbd "[t") 'eww-buffer-show-previous)))
  :config
  (advice-add 'eww-back-url :after (lambda (&rest _) (bergheim/eww-rename-buffer)))
  (advice-add 'eww-forward-url :after (lambda (&rest _) (bergheim/eww-rename-buffer)))
  (setq eww-search-prefix "https://search.ts.glvortex.net/search?q="
        eww-display-inline-images t
        ;; eww-blocked-urls '("*.js" "*.css" "*.png" "*.jpg" "*.gif")
        )

  (defun bergheim/open-link-background (&optional url)
    "Open link in background buffer (context-aware).
If URL is provided, use that. Otherwise get link at point."
    (interactive)
    (let ((target-url (or url
                          (cond
                           ((eq major-mode 'eww-mode)
                            (or (get-text-property (point) 'shr-url)
                                (eww-suggested-uris)))
                           ((eq major-mode 'w3m-mode)
                            (w3m-anchor))
                           (t (user-error "Not in a supported browser mode"))))))
      (when target-url
        ;; Handle case where target-url is a link-hint plist
        (when (and (listp target-url) (plist-get target-url :args))
          (setq target-url (plist-get target-url :args)))

        (cond
         ((eq major-mode 'eww-mode)
          (save-window-excursion (eww target-url 4))
          (message "Opened in background (EWW): %s" target-url))
         ((eq major-mode 'w3m-mode)
          (save-window-excursion (w3m target-url t))
          (message "Opened in background (W3M): %s" target-url))
         (t (user-error "Not in a supported browser mode"))))))

  (defun bergheim/link-hint-open-background ()
    "Use avy to select and open a link in background for current browser."
    (interactive)
    (let* ((link-hint-types (link-hint--valid-types :open))
           (links (link-hint--get-links))
           link)
      (when links
        (setq link (link-hint--process links))
        (when link
          (bergheim/open-link-background link)))))

  (defun bergheim/eww-open-link-w3m ()
    "Open current eww page in w3m."
    (interactive)
    (let ((url (plist-get eww-data :url)))
      (message "EWW URL: %s" url)  ; debug
      (when url
        (w3m url)
        (message "Opened current page in w3m: %s" url))))

  (defun bergheim/eww-rename-buffer ()
    "Rename EWW buffer to include page title."
    (let ((title (plist-get eww-data :title)))
      (when title
        (rename-buffer (format "*eww - %s*" title) t))))

  (defun bergheim/consult-browser-buffer ()
    "Switch between browser buffers using `consult'."
    (interactive)
    (let ((candidates
           (seq-filter
            (lambda (buf)
              (with-current-buffer buf
                (memq major-mode '(eww-mode w3m-mode))))
            (buffer-list))))
      (if candidates
          (switch-to-buffer
           (consult--read (mapcar #'buffer-name candidates)
                          :prompt "Browser buffers: "))
        (user-error "No browser buffers found"))))

  (defun bergheim/eww-select-link ()
    "Select and open a link from all links in the current eww buffer."
    (interactive)
    (let ((links '()))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when-let ((url (get-text-property (point) 'shr-url)))
            (let ((text (buffer-substring-no-properties
                         (point)
                         (next-single-property-change (point) 'shr-url nil (point-max)))))
              (push (cons (format "%s → %s" (string-trim text) url) url) links)))
          (goto-char (next-single-property-change (point) 'shr-url nil (point-max)))))
      (if links
          (let* ((choice (completing-read "Open link: " links))
                 (url (cdr (assoc choice links))))
            (eww url))
        (message "No links found in buffer")))))

(use-package w3m
  :defer t
  :commands (w3m w3m-search bergheim/w3m-browse-or-search)
  :general
  (bergheim/global-menu-keys
    "sW" '(bergheim/w3m-browse-or-search :which-key "w3m history/search"))
  :hook
  (w3m-display . bergheim/w3m-rename-buffer)
  (w3m-mode . (lambda ()
                (setq line-spacing 0) ;; this makes images not have gaps
                (evil-define-key 'normal w3m-mode-map
                  (kbd "gs") 'w3m-view-source
                  (kbd "gy") 'link-hint-copy-link
                  (kbd "gf") 'link-hint-open-link
                  (kbd "gF") 'bergheim/link-hint-open-background
                  (kbd "M-RET") 'bergheim/open-link-background
                  (kbd "]t") 'w3m-next-buffer
                  (kbd "[t") 'w3m-previous-buffer)))
  :config
  (require 'w3m-hist)
  (require 'w3m-search)
  (bergheim/localleader-keys
    :states '(normal visual)
    :keymaps 'w3m-mode-map
    "b" '(bergheim/consult-browser-buffer :which-key "browser buffers")
    "f" '(link-hint-open-link :which-key "follow link")
    "F" '(bergheim/link-hint-open-background :which-key "open link background")
    "x" '((lambda () (interactive) (browse-url-default-browser w3m-current-url)) :which-key "open external browser")
    "e" '(bergheim/w3m-open-link-eww :which-key "open in eww")
    "y" '(link-hint-copy-link :which-key "copy link")
    "r" '(w3m-reload-this-page :which-key "reload")
    "s" '(w3m-view-source :which-key "view source")
    "H" '(w3m-db-history :which-key "search history")
    "h" '(w3m-history :which-key "tab history")
    "i" '(w3m-toggle-inline-images :which-key "toggle images")
    "d" '(w3m-download :which-key "download")
    "t" '(w3m-copy-buffer :which-key "new tab")
    "T" '(w3m-delete-buffer :which-key "close tab")
    "o" '(bergheim/w3m-open-link-eww :which-key "open in eww"))

  (add-to-list 'w3m-search-engine-alist
               '("searxng" "https://search.ts.glvortex.net/search?q=%s" utf-8))
  (setq w3m-search-default-engine "searxng"
        w3m-use-cookies nil
        w3m-confirm-leaving-secure-page nil
        w3m-default-display-inline-images t
        w3m-toggle-inline-images-permanently t)

  (defun bergheim/w3m-open-link-eww ()
    "Open current w3m page in eww."
    (interactive)
    (let ((url (format "%s" w3m-current-url)))
      (when (and url (not (string-empty-p url)))
        (eww url)
        (message "Opened current page in eww: %s" url))))

  (defun bergheim/w3m-browse-or-search ()
    "Browse to URL from history or search for input."
    (interactive)
    (let* ((history-entries (bergheim/w3m-get-history-urls))
           (choice (completing-read "Open URL: " history-entries nil nil)))
      (if (member choice history-entries)
          (w3m choice)
        (w3m-search w3m-search-default-engine choice))))

  (defun bergheim/w3m-get-history-urls ()
    "Extract URLs from w3m arrived database."
    (unless (and (boundp 'w3m-arrived-db) w3m-arrived-db)
      (w3m-arrived-setup))  ; Initialize the database
    (let ((urls '()))
      (when (boundp 'w3m-arrived-db)
        (maphash (lambda (url _) (push url urls)) w3m-arrived-db))
      (delete-dups (reverse urls))))

  ;; FIXME this does not need a function
  (defun bergheim/w3m-rename-buffer (&optional _url)
    "Rename w3m buffer to include page title."
    (let ((title (w3m-current-title)))
      (when title
        (rename-buffer (format "*w3m - %s*" title) t)))))

;; FIXME: fucks up mu4e view
(use-package image-slicing
  :demand
  :ensure (:host github :repo "ginqi7/image-slicing")
  :general
  (bergheim/global-menu-keys
    "tu" #'bergheim/toggle-line-spacing-for-images)
  :config
  (defvar bergheim/original-line-spacing nil
    "Store the original line-spacing value before setting to 0.")

  (defun bergheim/toggle-line-spacing-for-images ()
    "Toggle line-spacing between 0 and whatever it was before."
    (interactive)
    (if (and (numberp line-spacing) (= line-spacing 0))
        (setq line-spacing bergheim/original-line-spacing)
      (setq bergheim/original-line-spacing line-spacing)
      (setq line-spacing 0))
    (redisplay))
  ;; (setopt line-spacing 0) ;; without this we get gaps

  ;; Only enable in eww-mode, not globally
  (defun bergheim/enable-image-slicing ()
    "Enable image-slicing for this buffer."
    (setq-local shr-external-rendering-functions
                '((img . image-slicing-tag-img)))
    (image-slicing-mode 1))

  (add-hook 'eww-mode-hook #'bergheim/enable-image-slicing))

;;; bergheim-browser.el ends here
