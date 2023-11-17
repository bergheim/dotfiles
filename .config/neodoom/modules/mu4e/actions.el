;;; actions.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim


(add-to-list 'mu4e-marks
             '(spam
               :char       "X"
               :prompt     "spam"
               :dyn-target (lambda (target msg)
                             (with-mu4e-context-vars (mu4e-context-determine msg nil)
                                 mu4e-spam-folder))
               :action      (lambda (docid msg target)
                              (mu4e--server-move docid (mu4e--mark-check-target target) "+S-u-N"))))

(mu4e~headers-defun-mark-for spam)
(mu4e--view-defun-mark-for spam)
;; based on https://github.com/djcb/mu/issues/1136#issuecomment-486177435
(setf (alist-get 'trash mu4e-marks)
      (list :char '("d" . "▼")
            :prompt "dtrash"
            :dyn-target (lambda (target msg)
                          (mu4e-get-trash-folder msg))
            :action (lambda (docid msg target)
                      ;; Here's the main difference to the regular trash mark,
                      ;; no +T before -N so the message is not marked as
                      ;; IMAP-deleted:
                      (mu4e--server-move docid (mu4e--mark-check-target target) "+S-u-N"))))

(defun bergheim/mu4e-refile-as-spam (msg)
  (if (eq major-mode 'mu4e-headers-mode)
      (mu4e-headers-mark-for-spam)
    (mu4e-view-mark-for-spam)))

;; TODO: see if this works when sending works again
;; message-id is apparently not generated on the server..? which sounds strange
;; (add-hook 'message-send-mail-hook (lambda () (message "hello") (org-store-link nil)))

(defun bergheim/mu4e-read-later (msg)
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "el")
  (if (eq major-mode 'mu4e-headers-mode)
      (mu4e-headers-mark-for-refile)
    (mu4e-view-mark-for-refile)))

(defun bergheim/mu4e-follow-up (msg)
  (interactive)
  (call-interactively 'org-store-link)
  ;; TODO: No hooks for `org-capture' I think? I want to capture, then refile on completion
  (let ((mu4e-headers-advance-after-mark nil))
    (if (eq major-mode 'mu4e-headers-mode)
        (mu4e-headers-mark-for-refile)
      (mu4e-view-mark-for-refile)))
  (org-capture nil "ef"))

(defun bergheim/mu4e-store-link-to-query ()
  (interactive)
  (let ((mu4e-org-link-query-in-headers-mode t))
    (call-interactively 'org-store-link)))

;; TODO in general, lower-case should match /Inbox/, upper-case should mean everything but /Trash/
(setq mu4e-headers-actions (delete '("show this thread" . mu4e-action-show-thread) mu4e-headers-actions))
(setq mu4e-headers-actions (delete '("capture message" . mu4e-action-capture-message) mu4e-headers-actions))
(add-to-list 'mu4e-headers-actions '("adwim" . bergheim/mu4e-search-dwim) t)
(add-to-list 'mu4e-headers-actions '("Around" . bergheim/mu4e-search-around-message) t)
(add-to-list 'mu4e-headers-actions '("browser" . bergheim/mu4e-open-message-in-webclient) t)
(add-to-list 'mu4e-headers-actions '("capture message" . mu4e-action-capture-message) t)
(add-to-list 'mu4e-headers-actions '("Capture contact" . mu4e-action-add-org-contact) t)
(add-to-list 'mu4e-headers-actions '("domain" . bergheim/mu4e-search-from-domain-all) t)
(add-to-list 'mu4e-headers-actions '("Domain inbox" . bergheim/mu4e-search-from-domain) t)
(add-to-list 'mu4e-headers-actions '("email" . bergheim/mu4e-search-from-address) t)
(add-to-list 'mu4e-headers-actions '("follow up" . bergheim/mu4e-follow-up) t)
(add-to-list 'mu4e-headers-actions '("junk" . bergheim/mu4e-refile-as-spam) t)
(add-to-list 'mu4e-headers-actions '("later" . bergheim/mu4e-read-later) t)
(add-to-list 'mu4e-headers-actions '("mail list" . bergheim/mu4e-search-from-mail-list) t)
(add-to-list 'mu4e-headers-actions '("Me" . bergheim/mu4e-search-to-me) t)
(add-to-list 'mu4e-headers-actions '("name" . bergheim/mu4e-search-from-name) t)
(add-to-list 'mu4e-headers-actions '("Narrow to sender" . bergheim/mu4e-narrow-to-sender) t)
(add-to-list 'mu4e-headers-actions '("subject" . bergheim/mu4e-search-this-subject) t)
(add-to-list 'mu4e-headers-actions '("thread" . mu4e-action-show-thread) t)

(setq mu4e-view-actions (delete '("View in browser" . mu4e-action-view-in-browser) mu4e-view-actions))
(setq mu4e-view-actions (delete '("show this thread" . mu4e-action-show-thread) mu4e-view-actions))
(setq mu4e-view-actions (delete '("capture message" . mu4e-action-capture-message) mu4e-view-actions))
(add-to-list 'mu4e-view-actions '("adwim" . bergheim/mu4e-search-dwim) t)
(add-to-list 'mu4e-view-actions '("Around" . bergheim/mu4e-search-around-message) t)
(add-to-list 'mu4e-view-actions '("browser" . bergheim/mu4e-open-message-in-webclient) t)
(add-to-list 'mu4e-view-actions '("capture message" . mu4e-action-capture-message) t)
(add-to-list 'mu4e-view-actions '("domain" . bergheim/mu4e-search-from-domain-all) t)
(add-to-list 'mu4e-view-actions '("Domain inbox" . bergheim/mu4e-search-from-domain) t)
(add-to-list 'mu4e-view-actions '("email" . bergheim/mu4e-search-from-address) t)
(add-to-list 'mu4e-view-actions '("follow up" . bergheim/mu4e-follow-up) t)
(add-to-list 'mu4e-view-actions '("junk" . bergheim/mu4e-refile-as-spam) t)
(add-to-list 'mu4e-view-actions '("later" . bergheim/mu4e-read-later) t)
(add-to-list 'mu4e-view-actions '("mail list" . bergheim/mu4e-search-from-mail-list) t)
(add-to-list 'mu4e-view-actions '("Me" . bergheim/mu4e-search-to-me) t)
(add-to-list 'mu4e-view-actions '("name" . bergheim/mu4e-search-from-name) t)
(add-to-list 'mu4e-view-actions '("subject" . bergheim/mu4e-search-this-subject) t)
(add-to-list 'mu4e-view-actions '("thread" . mu4e-action-show-thread) t)

;;; actions.el ends here
