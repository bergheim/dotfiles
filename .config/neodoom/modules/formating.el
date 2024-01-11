;;; formating.el --- Description -*- lexical-binding: t; -*-
;;

(use-package apheleia
  :config
  ;; temp fix while apheleia does not support this ts mode directly
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist)
        'prettier)

  (apheleia-global-mode +1))

(provide 'formating)
;;; formating.el ends here
