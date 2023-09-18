;;; accounts.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

(setq mu4e-contexts
      (list
       (make-mu4e-context
        :name "work"
        :match-func (lambda (msg)
                      (when msg
                        (string-match-p "^/neptune" (mu4e-message-field msg :maildir))))
        :vars `((user-full-name         . ,bergheim/neptune/name)
                (user-mail-address      . ,bergheim/neptune/email )
                (mu4e-compose-signature . ,bergheim/neptune/signature)
                (org-msg-signature      . ,bergheim/neptune/signature-html)

                (mu4e-compose-format-flowed . t)

                (mu4e-sent-folder   . "/neptune/Sent")
                (mu4e-trash-folder  . "/neptune/Trash")
                (mu4e-drafts-folder . "/neptune/Drafts")
                (mu4e-refile-folder . "/neptune/Archive")
                (mu4e-spam-folder   . "/neptune/Spam")

                (mu4e-maildir-shortcuts  . (("/neptune/Inbox"   . ?i)
                                            ("/neptune/Sent"    . ?s)
                                            ("/neptune/Trash"   . ?t)
                                            ("/neptune/Drafts"  . ?d)
                                            ("/neptune/Archive" . ?a)
                                            ))))
       (make-mu4e-context
        :name "gmail"
        :match-func (lambda (msg)
                      (when msg
                        (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
        :vars `(
                (user-full-name         . ,bergheim/gmail/name)
                (user-mail-address      . ,bergheim/gmail/email)
                (mu4e-compose-signature . ,bergheim/gmail/signature)
                (org-msg-signature      . ,bergheim/gmail/signature-html)

                (mu4e-compose-format-flowed . t)

                (mu4e-sent-folder   . "/gmail/Sent")
                (mu4e-trash-folder  . "/gmail/Trash")
                (mu4e-drafts-folder . "/gmail/Drafts")
                (mu4e-refile-folder . "/gmail/Archives")
                (mu4e-spam-folder   . "/gmail/Spam")

                (mu4e-maildir-shortcuts  . (("/gmail/Inbox"  . ?i)
                                            ("/gmail/Sent"   . ?s)
                                            ("/gmail/Trash"  . ?t)
                                            ("/gmail/Drafts" . ?d)
                                            ))))
       (make-mu4e-context
        :name "private"
        :enter-func (lambda () (mu4e-message "Switch to Personal Context"))
        :match-func (lambda (msg)
                      (when msg
                        (string-match-p "^/glvortex" (mu4e-message-field msg :maildir))))
        :vars `(
                (user-full-name         . ,bergheim/glvortex/name)
                (user-mail-address      . ,bergheim/glvortex/email)
                (mu4e-compose-signature . ,bergheim/glvortex/signature)
                (org-msg-signature      . ,bergheim/glvortex/signature-html)

                (mu4e-compose-format-flowed . t)

                (mu4e-sent-folder   . "/glvortex/Sent")
                (mu4e-trash-folder  . "/glvortex/Trash")
                (mu4e-drafts-folder . "/glvortex/Drafts")
                (mu4e-refile-folder . "/glvortex/Archive")
                (mu4e-spam-folder   . "/glvortex/Spam")

                (mu4e-maildir-shortcuts  . (("/glvortex/Inbox"   . ?i)
                                            ("/glvortex/Sent"    . ?s)
                                            ("/glvortex/Trash"   . ?t)
                                            ("/glvortex/Drafts"  . ?d)
                                            ("/glvortex/Archive" . ?a)
                                            ))))
       ))

;;; accounts.el ends here
