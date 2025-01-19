;;; accounts.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

(setq mu4e-contexts
      (list
       (make-mu4e-context
        :name "private"
        :enter-func (lambda () (mu4e-message "Switch to Personal Context"))
        :match-func (lambda (msg)
                      (when msg
                        (string-match-p "^/glvortex" (mu4e-message-field msg :maildir))))
        :vars `(
                (user-full-name         . ,bergheim/glvortex/name)
                (user-mail-address      . ,bergheim/glvortex/email)
                ;; (message-signature . ,bergheim/glvortex/signature)
                (org-msg-signature      . ,bergheim/glvortex/signature-html)

                (mu4e-compose-format-flowed . t)

                (mu4e-sent-folder   . "/glvortex/Sent")
                (mu4e-trash-folder  . "/glvortex/Trash")
                (mu4e-drafts-folder . "/glvortex/Drafts")
                (mu4e-refile-folder . bergheim/mu4e-refile-mail)
                (mu4e-spam-folder   . "/glvortex/Spam")

                (mu4e-maildir-shortcuts  . (("/glvortex/Inbox"   . ?i)
                                            ("/glvortex/Sent"    . ?s)
                                            ("/glvortex/Trash"   . ?t)
                                            ("/glvortex/Drafts"  . ?d)
                                            ("/glvortex/Archive" . ?a)
                                            ))))
       (make-mu4e-context
        :name "gmail"
        :match-func (lambda (msg)
                      (when msg
                        (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
        :vars `(
                (user-full-name         . ,bergheim/gmail/name)
                (user-mail-address      . ,bergheim/gmail/email)
                ;; (message-signature      . ,bergheim/gmail/signature)
                (org-msg-signature      . ,bergheim/gmail/signature-html)

                (mu4e-compose-format-flowed . t)

                (mu4e-sent-folder   . "/gmail/Sent")
                (mu4e-trash-folder  . "/gmail/Trash")
                (mu4e-drafts-folder . "/gmail/Drafts")
                (mu4e-refile-folder . bergheim/mu4e-refile-mail)
                (mu4e-spam-folder   . "/gmail/Spam")

                (mu4e-maildir-shortcuts  . (("/gmail/Inbox"  . ?i)
                                            ("/gmail/Sent"   . ?s)
                                            ("/gmail/Trash"  . ?t)
                                            ("/gmail/Drafts" . ?d)
                                            ))))

       (make-mu4e-context
        :name "ntnu"
        :match-func (lambda (msg)
                      (when msg
                        (string-match-p "^/ntnu" (mu4e-message-field msg :maildir))))
        :vars `(
                (user-full-name         . ,bergheim/ntnu/name)
                (user-mail-address      . ,bergheim/ntnu/email)
                ;; (message-signature      . ,bergheim/ntnu/signature)
                (org-msg-signature      . ,bergheim/ntnu/signature-html)

                (mu4e-compose-format-flowed . t)

                (mu4e-sent-folder   . "/ntnu/Sent")
                (mu4e-trash-folder  . "/ntnu/Trash")
                (mu4e-drafts-folder . "/ntnu/Drafts")
                (mu4e-refile-folder . bergheim/mu4e-refile-mail)
                (mu4e-spam-folder   . "/ntnu/Spam")

                (mu4e-maildir-shortcuts  . (("/ntnu/Inbox"  . ?i)
                                            ("/ntnu/Sent"   . ?s)
                                            ("/ntnu/Trash"  . ?t)
                                            ("/ntnu/Drafts" . ?d)
                                            ))))))

;;; accounts.el ends here
