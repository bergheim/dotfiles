;;; bergheim-viewers.el --- PDF, Info, and systemd file viewers -*- lexical-binding: t; -*-

(use-package pdf-tools
  :unless bergheim/container-mode-p
  :defer t
  :config
  (pdf-tools-install :no-query))

(use-package systemd
  :unless bergheim/container-mode-p
  :mode (("\\.service\\'" . systemd-mode)
         ("\\.socket\\'" . systemd-mode)
         ("\\.timer\\'" . systemd-mode)
         ("\\.target\\'" . systemd-mode)
         ("\\.mount\\'" . systemd-mode)
         ("\\.automount\\'" . systemd-mode)
         ("\\.slice\\'" . systemd-mode)
         ("\\.path\\'" . systemd-mode)))

(use-package info
  :ensure nil
  :general
  (:states 'normal
   :keymaps 'Info-mode-map
   "H" 'Info-history-back
   "L" 'Info-history-forward
   "o" 'Info-history
   "J" 'Info-menu
   ;; "l" 'Info-follow-nearest-node
   "u" 'Info-up
   "C-n" 'Info-next
   "C-p" 'Info-prev
   "d" 'Info-directory)

  ;; TODO: when searching (s/S) n/N does not work. should be bound to `Info-search-next' etc
  (bergheim/localleader-keys
    :states '(normal visual)
    :keymaps 'Info-mode-map
    "a" '(Info-apropos :which-key "apropos")
    "d" '(Info-directory :which-key "directory")
    "i" '(Info-virtual-index :which-key "index")
    "h" '(Info-history :which-key "history")
    "m" '(Info-menu :which-key "menu items")
    "s" '(consult-info :which-key "search")))

;;; bergheim-viewers.el ends here
