;;; helpers.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

(defun bergheim/mu4e--msg-get-account (msg)
  "Retrieve the top-level directory (account) from the :maildir field of MSG."
  (let* ((maildir (mu4e-message-field msg :maildir))
         (account (and maildir (car (split-string maildir "/" t)))))
    (or account "")))


(defun bergheim/mu4e--msg-get-modified-maildir (msg)
  "Retrieve the maildir after the top-level directory (account) from the :maildir field of MSG."
  (let* ((maildir (mu4e-message-field msg :maildir))
         (maildir-parts (and maildir (split-string maildir "/" t))))
    (if (> (length maildir-parts) 1)
        (concat "/" (mapconcat 'identity (cdr maildir-parts) "/"))
      maildir)))

(defun bergheim/mu4e--clean-string-for-mu (subject)
  "Clean up the subject before sending it to mu4e.

This is because xapian has a lot of characters that will break the search. I could not find docs on
it, and this mess has just evolved over time.."

  (let* (;; remove any "Re: ", "Fwd: " etc
         (subject (replace-regexp-in-string "^\\(\\ca\\{2,3\\}: ?\\)+" "" subject))
         ;; remove characters that make mu unhappy and breaks xapian keywords
         (subject (replace-regexp-in-string "’" "'" subject))
         (subject (replace-regexp-in-string "[^[:alnum:]_'\.]" " " subject))
         ;; (subject (replace-regexp-in-string "[[:nonascii:]]" " " subject))
         ;; (subject (replace-regexp-in-string "[[:punct:]]" " " subject))
         ;; (subject (replace-regexp-in-string "[[:graph:]]" " " subject))
         ;; remove more breaking xapian keywords
         (subject (replace-regexp-in-string "[-:]" " " subject))
         ;; apparently surrounding 's is a nono
         (subject (replace-regexp-in-string "\\('\\)[[:blank:]]" "" subject nil nil 1))
         (subject (replace-regexp-in-string "[[:blank:]]\\('\\)" "" subject nil nil 1))
         ;; xapian matches 10.10 but not foo.bar
         (subject (replace-regexp-in-string "[^[:digit:]]\\(\\\.+\\)" " " subject nil nil 1))
         (subject (replace-regexp-in-string "[[:digit:]]\\(\\\.+\\)" "\\\\." subject nil nil 1)))

    (s-trim subject)))

(defun bergheim/mu4e--pattern-match-subject (subject &optional only-match)
  "Match the SUBJECT for anything interesting, and return that.
If ONLY-MATCH is non-nil, only return if anything actually matched

Used to quickly match similar messages"

  (let ((match
         (or (seq-some (lambda (r) (and (string-match-p r subject)
                                        (concat "subject:/" (s-trim (string-replace " " "\\ " (string-replace "\\" "\\\\" r))) "/")))
                       bergheim/email-dwim-subjects)
             (unless only-match
               subject))))
    match))

;; TODO: look over these..
(defun bergheim/mu4e--get-account (maildir)
  (when (string-match "^/\\(\\w+\\)/" maildir)
    (match-string 1 maildir)))

(defun bergheim/mu4e--get-account-email (maildir)
  (let ((account (bergheim/mu4e--get-account maildir)))
    (pcase account
      ("gmail" bergheim/gmail/email)
      ("glvortex" bergheim/glvortex/email))))

(cl-defun bergheim/utils--get-domain (email &optional full-domain)
  "Get the main domain of an email address"
  (let* ((domain (cadr (split-string email "@")))
         (parts (reverse (split-string domain "\\."))))

    ;; FIXME: do not always use full domain
    (if (or current-prefix-arg full-domain)
        domain
      (format "%s.%s" (nth 1 parts) (nth 0 parts)))))

(defun bergheim/mu4e--addressed-to-me ()
  "Generate a query for mails sent to me specifically.

With \\[universal-argument], search all emails where I am a recipient"
  (let ((keyword "to:%s"))
    (when current-prefix-arg
      (setq keyword "recip:%s"))
    (mapconcat
     (lambda (address) (format keyword address))
     (mu4e-personal-addresses) " OR ")))

(defun bergheim/mu4e-open-message-in-webclient (msg)
  (let* ((maildir (mu4e-message-field msg :maildir))
         (msgid (mu4e-message-field msg :message-id))
         (subject (mu4e-message-field msg :subject))
         (to (plist-get (car (mu4e-message-field-at-point :to)) :email))
         (from (plist-get (car (mu4e-message-field-at-point :from)) :email))
         (account (bergheim/mu4e--get-account maildir)))

    (pcase account
      ("neptune" (let ((query (format "SUBJECT:(%s) %s %s" subject to from)))
                   ;; ms could not care less about standards, as always. you
                   ;; can't even produce a link to a search, let alone to an
                   ;; email without crawling through their enterprise API
                   ;; "offerings". I am amazed this even works over IMAP
                   ;; so uh, paste this into the searchbox in outlook I guess
                   (kill-new query)
                   (browse-url "https://outlook.office.com/mail/")))
      ("gmail"
       (let ((url (concat "https://mail.google.com/mail/u/0/#search/rfc822msgid%3A"
                          (url-encode-url msgid))))
         (start-process "" nil "chromium" url)))
      ("glvortex"
       ;; I am not seing any Message-Id or anything else in the headers on
       ;; ProtonMail - I assume they have filtered it
       (browse-url (format "https://mail.proton.me/u/0/all-mail#keyword=%s&from=%s&to=%s"
                           ;; protonmail does not allow searches for [ ] etc so strip them
                           (url-encode-url (replace-regexp-in-string "\\W" " " subject))
                           (url-encode-url from)
                           (url-encode-url to))))
      (_ (display-warning :warning (format "Account \"%s\" based on dir \"%s\"not found!" account maildir))))))

(defun bergheim/org-subtree-to-mu4e ()
  "Send the current subtree to mu4e and promote it to level 1"
  (interactive)
  (org-copy-subtree)
  (+mu4e/compose)
  (unless (derived-mode-p 'org-msg-edit-mode)
    (org-msg-edit-mode))
  (org-msg-goto-body)
  (save-excursion
    (yank)
    (insert ?\n))

  ;; don't keep the subtree in the kill-ring
  (when kill-ring
    (setq kill-ring (cdr kill-ring)))

  ;; not really elegant but it gets the job done..
  (let* ((heading-level (- (car (org-heading-components)) 1)))
    (cl-loop repeat heading-level do (org-promote-subtree))))

;; TODO: use this instead of macros?
(defun bergheim/mail-search (query)
  "Perform a mu4e query"
  (interactive)
  (mu4e t)
  (mu4e-search-bookmark query))

;; FIXME: this does not really work since the view loads twice
;; (defun bergheim/mu4e--headers-goto-bottom ()
;;   "Move point to the last email in the mu4e-headers buffer.

;; Since the headers display is async, and we only want this for some functions,
;; remove the hook after invocation"
;;   (with-current-buffer (get-buffer "*mu4e-headers*")
;;     (goto-char (point-max))
;;     (remove-hook 'mu4e-headers-found-hook #'bergheim/mu4e--headers-goto-bottom)
;;     (mu4e-headers-prev)))

;;;; iterate all emails in a list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is a bit nuts, but it works! opens a link on a predefined spot from the entire list
;; its a lot of async stuff going on, and I couldn't get the hooks to work..
;; TODO: add iterate- to the ones that wo
(defun bergheim/mu4e--iterate-wait-for-email-body (callback)
  "Wait until the email body is rendered, then execute CALLBACK."
  (let ((max-attempts 10))
    (unless (eq major-mode 'mu4e-view-mode)
      (error "Not in mu4e-view-mode"))
    (run-with-timer
     0.1 nil
     (lambda ()
       (unless (or (zerop max-attempts)
                   (bergheim/mu4e--iterate-email-body-rendered-p))
         (setq max-attempts (1- max-attempts))
         (bergheim/mu4e--iterate-wait-for-email-body callback))
       (when (bergheim/mu4e--iterate-email-body-rendered-p)
         (funcall callback))))))

(defun bergheim/mu4e--iterate-email-body-rendered-p ()
  "Check if the email body is rendered in mu4e."
  (or (text-property-any (point-min) (point-max) 'shr-url nil)
      (text-property-any (point-min) (point-max) 'mu4e-url nil)))

(defvar bergheim/mu4e--iterate-current-navigate-fn nil
  "Current function used to navigate links in mu4e message view.")

(defun bergheim/mu4e--process-single-message ()
  "Process the current mu4e message based on bergheim/current-navigate-fn."
  (bergheim/mu4e--iterate-wait-for-email-body
   (lambda ()
     (funcall bergheim/mu4e--iterate-current-navigate-fn)
     (shr-browse-url)
     (remove-hook 'mu4e-view-mode-hook 'bergheim/mu4e--process-single-message)
     (if (mu4e-view-headers-next)
         (progn
           (setq bergheim/mu4e--iterate-current-navigate-fn bergheim/mu4e--iterate-current-navigate-fn)
           (add-hook 'mu4e-view-mode-hook 'bergheim/mu4e--process-single-message))
       (mu4e-view-quit)))))

(defun bergheim/mu4e--iterate-open-links (&optional navigate-function)
  "Visit each mu4e message, open the first link, and move to the next message."
  (interactive)
  (setq bergheim/mu4e--iterate-current-navigate-fn (or navigate-function 'bergheim/mu4e-navigate-first-link))
  (when (eq major-mode 'mu4e-headers-mode)
    (add-hook 'mu4e-view-mode-hook 'bergheim/mu4e--process-single-message)
    (mu4e-headers-view-message)))

(defun bergheim/mu4e--navigate-first-link ()
  "Navigate to the first link in mu4e message view and open it."
  (shr-next-link))

(defun bergheim/mu4e--navigate-second-to-last-links ()
  "Navigate to the second-to-last link in mu4e message view and open it."
  (goto-char (point-max))
  (shr-previous-link))

(defun bergheim/mu4e-open-first-links (&optional navigate-function)
  "Visit each mu4e message, open the first link, and move to the next message."
  (interactive)
  (bergheim/mu4e--iterate-open-links 'bergheim/mu4e--navigate-first-link))

(defun bergheim/mu4e-open-second-last-links ()
  "Visit each mu4e message, open the second to last link, and move to the next message."
  (interactive)
  (bergheim/mu4e--iterate-open-links 'bergheim/mu4e--navigate-second-to-last-links))

;; nicked from doom
(defun +mu4e-part-selectors (parts)
  "Generate selection strings for PARTS."
  (if parts
      (let (partinfo labeledparts maxfnamelen fnamefmt maxsizelen sizefmt)
        (dolist (part parts)
          (push (list :index (car part)
                      :mimetype (if (and (string= "text/plain" (caaddr part))
                                         (alist-get 'charset (cdaddr part)))
                                    (format "%s (%s)"
                                            (caaddr part)
                                            (alist-get 'charset (cdaddr part)))
                                  (caaddr part))
                      :type (car (nth 5 part))
                      :filename (cdr (assoc 'filename (assoc "attachment" (cdr part))))
                      :size (file-size-human-readable (with-current-buffer (cadr part) (buffer-size)))
                      :part part)
                partinfo))
        (setq maxfnamelen (apply #'max 7 (mapcar (lambda (i) (length (plist-get i :filename))) partinfo))
              fnamefmt (format " %%-%ds  " maxfnamelen)
              maxsizelen (apply #'max (mapcar (lambda (i) (length (plist-get i :size))) partinfo))
              sizefmt (format "%%-%ds " maxsizelen))
        (dolist (pinfo partinfo)
          (push (cons (concat (propertize (format "%-2s " (plist-get pinfo :index)) 'face '(bold font-lock-type-face))
                              (when (featurep 'nerd-icons)
                                (nerd-icons-icon-for-file (or (plist-get pinfo :filename) "")))
                              (format fnamefmt (or (plist-get pinfo :filename)
                                                   (propertize (plist-get pinfo :type) 'face '(italic font-lock-doc-face))))
                              (format sizefmt (propertize (plist-get pinfo :size) 'face 'font-lock-builtin-face))
                              (propertize (plist-get pinfo :mimetype) 'face 'font-lock-constant-face))
                      (plist-get pinfo :part))
                labeledparts))
        labeledparts)))

(defun +mu4e-view-select-mime-part-action ()
  "Select a MIME part, and perform an action on it."
  (interactive)
  (let ((labeledparts (+mu4e-part-selectors (mu4e--view-gather-mime-parts))))
    (if labeledparts
        (mu4e-view-mime-part-action
         (cadr (assoc (completing-read "Select part: " (mapcar #'car labeledparts))
                      labeledparts)))
      (user-error (mu4e-format "No parts found")))))

(defun bergheim/mu4e-compose-email (arg)
  "Compose an email. Use universal argument to compose in only plain text mode."
  (interactive "P")
  (let ((org-msg-default-alternatives (if arg
                                          '((new           . (text))
                                            (reply-to-html . (text))
                                            (reply-to-text . (text)))
                                        '((new           . (text html))
                                          (reply-to-html . (text html))
                                          (reply-to-text . (text))))))
    (mu4e-compose-new)))

(defun bergheim/mu4e-reply-email (arg)
  "Compose a reply to an email. Use universal argument to compose in only plain text mode."
  (interactive "P")
  (call-interactively 'org-store-link)
  (let ((org-msg-default-alternatives (if arg
                                          '((new           . (text))
                                            (reply-to-html . (text))
                                            (reply-to-text . (text)))
                                        '((new           . (text html))
                                          (reply-to-html . (text html))
                                          (reply-to-text . (text))))))
    (mu4e-compose-reply)))

;;; helpers.el ends here
