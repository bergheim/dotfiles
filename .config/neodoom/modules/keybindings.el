;;; keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim
;;
;; Author: Thomas Bergheim
;; Maintainer: Thomas Bergheim
;; Created: September 16, 2023
;; Modified: September 16, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bergheim/dotfiles
;; Package-Requires: ((emacs "29.1"))
;;


;; https://www.masteringemacs.org/article/mastering-key-bindings-emacs
;; Generally, all keys prefixed with C-c ? (where ? is a single character) are reserved for you, and you alone
;; The other set of reserved keys are the F-keys from F5 and onwards. The other two prefix keys reserved to you are hyper and super



(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3)
  (setq which-key-idle-secondary-delay 0.3))

(use-package general
  :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"

   "b" '(:ignore t :which-key "Buffers")
   "bb" '(switch-to-buffer :which-key "Switch")
   "bd" '(evil-delete-buffer :which-key "Delete")
   "bn" '(evil-buffer-new :which-key "New")

   "f" '(:ignore t :which-key "Files")
   "ff" '(find-file :which-key "Find file")
   "fr" '(consult-recent-file :which-key "Recent files")
   "fR" '(rename-visited-file :which-key "Rename file")
   "fs" '(save-buffer :which-key "Save buffer")

   "h" '(:ignore t :which-key "Help")
   "hv" '(helpful-variable :which-key "Variable")
   "hf" '(helpful-callable :which-key "Function")
   "hk" '(helpful-key :which-key "Key")
   "ha" '(apropos :which-key "Apropos")
   "hk" '(describe-mode :which-key "Desibe mode")
   "hr" '(bergheim/reload-init-file :which-key "Reload")

   "o" '(:ignore t :which-key "Org Mode")
   "od" '((lambda (&optional arg) (interactive) (org-agenda arg "d")) :which-key "Orgmode Dashboard")

   "p" '(:ignore t :which-key "Project")
   "pa" '(projectile-add-known-project :which-key "Add project")
   "pf" '(projectile-find-file :which-key "Find file")
   ;; TODO: make this switch persps
   "pp" '(projectile-switch-project :which-key "Switch project")
   "pu" '(projectile-discover-projects-in-search-path :which-key "Update projects")

   "m" '(:ignore t :which-key "Mode specific")

   "q" '(:ignore t :which-key "Quit")
   "qq" '(save-buffers-kill-terminal :which-key "Quit")
   "qr" '(restart-emacs :which-key "Restart")
   )

  (general-define-key
   :states '(normal insert)
   "M-h" #'evil-window-left
   "M-j" #'evil-window-down
   "M-k" #'evil-window-up
   "M-l" #'evil-window-right
   "M-H" #'+evil/window-move-left
   "M-J" #'+evil/window-move-down
   "M-K" #'+evil/window-move-up
   "M-L" #'+evil/window-move-right
   "M-\\" #'evil-window-vsplit
   "M-]" #'evil-window-split
   "M-DEL" #'evil-window-delete
   ;; "M-<backspace>" #'+workspace/close-window-or-workspace
   "M-S-<backspace>" #'kill-current-buffer
   "M-o" #'evil-window-next
   "M-f" #'maximize-window
   "M-F" #'winner-undo)
  )


(general-create-definer bergheim/emacs-lisp-keys
  :prefix "SPC m"
  :states '(normal visual emacs)
  :keymaps 'emacs-lisp-mode-map)

(bergheim/emacs-lisp-keys
 "e" '(:ignore t :which-key "Eval")
 "e d" '(eval-defun :which-key "eval last defun")
 "e e" '(eval-last-sexp :which-key "eval last sexp")
 "e b" '(eval-last-sexp :which-key "eval buffer")
 "e r" '(elisp-eval-region-or-buffer :which-key "eval region or buffer"))

(provide 'keybindings)
;;; keybindings.el ends here
