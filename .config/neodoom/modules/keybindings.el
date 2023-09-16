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
   "b" '(switch-to-buffer :which-key "Buffers")
   "f" '(:ignore t :which-key "Files")
   "ff" '(find-file :which-key "Find file")
   "fs" '(save-buffer :which-key "Save buffer")

   "h" '(:ignore t :which-key "Help")
   "hv" '(helpful-variable :which-key "Variable")
   "hf" '(helpful-callable :which-key "Function")
   "ha" '(apropos :which-key "Apropos")

   "m" '(:ignore t :which-key "Mode specific")
   "q" '(:ignore t :which-key "Quit")
   "qq" '(save-buffers-kill-terminal :which-key "Quit")
   "qr" '(restart-emacs :which-key "Restart")
   ))

(general-create-definer bergheim/emacs-lisp-keys
  :prefix "SPC m"
  :states '(normal visual emacs)
  :keymaps 'emacs-lisp-mode-map)

(bergheim/emacs-lisp-keys
 "e" '(:ignore t :which-key "Eval")
 "e d" '(eval-last-sexp :which-key "eval last sexp")
 "e b" '(eval-last-sexp :which-key "eval buffer")
 "e r" '(elisp-eval-region-or-buffer :which-key "eval region or buffer"))

(provide 'keybindings)
;;; keybindings.el ends here
