;;; accounts.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

(setq mu4e-contexts
      (list
       (make-mu4e-context
        :name "glvortex"
        :enter-func (lambda () (mu4e-message "Switch to Personal Context"))
        :match-func
         (lambda (msg)
           (when msg
             (mu4e-message-contact-field-matches msg :to "@glvortex.net$")))

        :vars `((user-full-name         . ,bergheim/glvortex/name)
                (user-mail-address      . ,bergheim/glvortex/email)
                ;; (user-mail-address      . "thisis-ignored@gmail.com")
                ;; (message-signature . ,bergheim/glvortex/signature)
                (org-msg-signature      . ,bergheim/glvortex/signature-html)

                (mu4e-compose-format-flowed . t)

                (mu4e-sent-folder   . "/mailbox/Sent")
                (mu4e-trash-folder  . "/mailbox/Trash")
                (mu4e-drafts-folder . "/mailbox/Drafts")
                (mu4e-refile-folder . bergheim/mu4e-refile-mail)
                (mu4e-spam-folder   . "/mailbox/Spam")

                (mu4e-maildir-shortcuts  . (("/mailbox/Inbox"   . ?i)
                                            ("/mailbox/Sent"    . ?s)
                                            ("/mailbox/Trash"   . ?t)
                                            ("/mailbox/Drafts"  . ?d)
                                            ("/mailbox/Archive" . ?a)))))

       (make-mu4e-context
        :name "personal"
        :match-func
         (lambda (msg)
           (when msg
             (mu4e-message-contact-field-matches msg :to "@thomasbergheim.com")))

        :vars `((user-full-name         . ,bergheim/personal/name)
                (user-mail-address      . ,bergheim/personal/email)
                ;; (message-signature      . ,bergheim/personal/signature)
                (org-msg-signature      . ,bergheim/personal/signature-html)

                (mu4e-compose-format-flowed . t)

                (mu4e-sent-folder   . "/mailbox/Sent")
                (mu4e-trash-folder  . "/mailbox/Trash")
                (mu4e-drafts-folder . "/mailbox/Drafts")
                ;; TODO is this correct? do we need ,
                (mu4e-refile-folder . bergheim/mu4e-refile-mail)
                (mu4e-spam-folder   . "/mailbox/Spam")

                (mu4e-maildir-shortcuts  . (("/mailbox/Inbox"  . ?i)
                                            ("/mailbox/Sent"   . ?s)
                                            ("/mailbox/Trash"  . ?t)
                                            ("/mailbox/Drafts" . ?d)))))

       (make-mu4e-context
        :name "gmail"
        :match-func
         (lambda (msg)
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
        :match-func
         (lambda (msg)
           (when msg
             (mu4e-message-contact-field-matches msg :to bergheim/ntnu/email)))

        :vars `((user-full-name         . ,bergheim/ntnu/name)
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
                                            ("/ntnu/Drafts" . ?d)))))))

;;; accounts.el ends here
