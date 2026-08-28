;;; style.el --- mu4e header columns and read-option tweaks -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

(setq mu4e-read-option-use-builtin nil
      ;; use vertico
      mu4e-completing-read-function 'completing-read)

(add-to-list 'mu4e-header-info-custom
             '(:account .
               (:name "Account"
                :shortname "Account"
                :help "Account the message belongs to"
                :function bergheim/mu4e--msg-get-account)))

(add-to-list 'mu4e-header-info-custom
             '(:shortened-maildir .
               (:name "Maildir"
                :shortname "Maildir"
                :help "Modified Maildir"
                :function bergheim/mu4e--msg-get-modified-maildir)))

(defvar bergheim/mu4e-headers-fields-detailed
  '((:account . 8)
    (:human-date . 14)
    (:shortened-maildir . 8)
    (:to . 22)
    (:from . 20)
    (:subject))
  "Full headers columns: account, date, maildir, to, from, subject.")

(defvar bergheim/mu4e-headers-fields-focused
  '((:human-date . 14)
    (:from-or-to . 22)
    (:subject))
  "Compact headers columns: date, from-or-to, subject.")

(setq mu4e-headers-fields bergheim/mu4e-headers-fields-detailed)

(defun bergheim/mu4e-toggle-header-columns ()
  "Switch headers between detailed and subject-focused columns."
  (interactive)
  (setq mu4e-headers-fields
        (if (equal mu4e-headers-fields bergheim/mu4e-headers-fields-focused)
            bergheim/mu4e-headers-fields-detailed
          bergheim/mu4e-headers-fields-focused))
  (mu4e-search-rerun))

;;; style.el ends here
