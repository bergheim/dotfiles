;;; formating.el --- Description -*- lexical-binding: t; -*-

(use-package apheleia
  :init
  (apheleia-global-mode 1)
  :config
  (dolist (mode '(css-mode
                  css-ts-mode
                  js-json-mode
                  js-mode
                  json-mode
                  json-ts-mode
                  js-ts-mode
                  tsx-ts-mode
                  typescript-mode
                  typescript-ts-mode))
    (setf (alist-get mode apheleia-mode-alist) 'biome)))

(provide 'formating)
;;; formating.el ends here
