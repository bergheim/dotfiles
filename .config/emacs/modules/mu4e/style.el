;;; style.el --- Description -*- lexical-binding: t; -*-
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

;;; viewer.el ends here
