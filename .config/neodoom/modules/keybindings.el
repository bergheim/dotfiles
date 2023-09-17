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
  ;; :after evil-org-agenda
  :config
  ;; TODO: make this work with reloads
  ;; (general-unbind :prefix "SPC")

  (general-def
    :keymaps '(evil-normal-state-map org-agenda-mode-map)
    "C-S-u" 'universal-argument
    "C-u" 'evil-scroll-up)


  ;; (general-unbind 'org-agenda-mode-map
  ;;   "SPC")
  ;; (general-create-definer my-leader-def
  ;;   :prefix "SPC")
  ;; (my-leader-def
  ;;  :states '(normal visual motion)
  ;;  :prefix "SPC"
  ;;  :keymaps 'org-agenda-mode-map
  ;;  "bb" '(switch-to-buffer :which-key "Switch"))

  ;; (general-def 'emacs org-agenda-mode-map
  ;;  ;; :states 'motion
  ;;  :prefix "SPC"
  ;;  ;; :keymaps '(emacs evil-normal-state-map org-agenda-mode-map)
  ;;  :non-normal-prefix "M-SPC"
  ;;  "b" '(:ignore t :which-key "Buffers"))

  (general-define-key
   :states '(normal visual insert emacs motion)
   :prefix "SPC"
   :keymaps '(global-map org-agenda-mode-map)
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

   "g" '(:ignore t :which-key "git")
   "gg" '(magit :which-key "magit")

   "h" '(:ignore t :which-key "Help")
   "ha" '(apropos :which-key "Apropos")
   "hb" '(embark-bindings :which-key "Bindings")
   "hf" '(helpful-callable :which-key "Function")
   "hk" '(helpful-key :which-key "Key")
   "hm" '(describe-mode :which-key "Describe mode")
   "hr" '(bergheim/reload-init-file :which-key "Reload")
   "ht" '(consult-theme :which-key "Switch theme")
   "hv" '(helpful-variable :which-key "Variable")

   "m" '(:ignore t :which-key "Mode specific")

   "o" '(:ignore t :which-key "Org Mode")
   "od" '((lambda (&optional arg) (interactive) (org-agenda arg "d")) :which-key "Orgmode Dashboard")

   "p" '(:ignore t :which-key "Project")
   "pa" '(projectile-add-known-project :which-key "Add project")
   "pf" '(projectile-find-file :which-key "Find file")
   ;; TODO: make this switch persps
   "pp" '(projectile-switch-project :which-key "Switch project")
   "pu" '(projectile-discover-projects-in-search-path :which-key "Update projects")

   ;; TODO: add lots more consult stuff like kill-ring etc
   "s" '(:ignore t :which-key "Search")
   "sa" '(consult-org-agenda :which-key "org agenda")
   "sb" '(consult-line-multi :which-key "open buffers")
   "sh" '(consult-org-heading :which-key "org heading")
   "sh" '(consult-recent-file :which-key "recent files")
   "sI" '(consult-imenu-multi :which-key "imenu items in all buffers")
   "si" '(consult-imenu :which-key "imenu items")
   "sm" '(consult-man :which-key "man")
   "sp" '(consult-project-buffer :which-key "project buffers")
   "ss" '(consult-line :which-key "buffer")


   "q" '(:ignore t :which-key "Quit")
   "qq" '(save-buffers-kill-terminal :which-key "Quit")
   "qr" '(restart-emacs :which-key "Restart")

   "w" '(:ignore t :which-key "Window")
   "ws" '(evil-window-split :which-key "split horizontally")
   "wv" '(evil-window-vsplit :which-key "split vertically")
   "wd" '(evil-window-delete :which-key "delete window")
   )


  (defun my-define-common-keys (&optional keymap states)
    (let ((target-keymap (or keymap 'global)))
      (general-define-key
       :states (or states '(normal insert))
       :keymaps target-keymap
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
       "M-F" #'winner-undo
       )))

  (my-define-common-keys)

  ;; ;; Apply to org-agenda-mode-map for normal, insert, and visual states:
  ;; (my-define-common-keys 'org-agenda-mode-map '(normal insert visual))

  (my-define-common-keys '(org-mode-map org-agenda-mode-map) 'motion)


  ;; (general-define-key
  ;;  :states 'motion
  ;;  :keymaps 'org-agenda-mode-map
  ;;  :prefix "SPC"
  ;;  :non-normal-prefix "M-SPC"

  ;;  "b" '(:ignore t :which-key "Buffers")
  ;;  "bb" '(switch-to-buffer :which-key "Switch"))


  (general-define-key
   :states '(normal insert visual)
   :keymaps 'org-mode-map
   "M-h" #'evil-window-left
   "M-j" #'evil-window-down
   "M-k" #'evil-window-up
   "M-l" #'evil-window-right

   "C-M-h" #'org-metaleft
   "C-M-j" #'org-metadown
   "C-M-k" #'org-metaup
   "C-M-l" #'org-metaright

   "C-M-S-h" #'org-shiftmetaleft
   "C-M-S-j" #'org-shiftmetadown
   "C-M-S-k" #'org-shiftmetaup
   "C-M-S-l" #'org-shiftmetaright))

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
