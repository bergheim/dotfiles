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

(use-package general
  :config
  ;; TODO: make this work with reloads
  ;; (general-unbind :prefix "SPC")

  (general-create-definer bergheim/global-menu-keys
    :states '(normal visual insert motion emacs)
    :prefix "SPC"
    :keymaps 'override
    :non-normal-prefix "M-SPC")

  (general-create-definer bergheim/global-evil-keys
    :states '(normal visual motion operator)
    :keymaps 'override)

  (general-create-definer bergheim/emacs-lisp-keys
    :prefix "SPC m"
    :states '(normal visual emacs)
    :keymaps 'emacs-lisp-mode-map)

  (general-def
    :keymaps '(evil-normal-state-map org-agenda-mode-map)
    "C-S-u" 'universal-argument
    "C-u" 'evil-scroll-up)

  (bergheim/global-menu-keys
   "a" '(:ignore t :which-key "Applications")
   "ad" '(dirvish :which-key "Dirvish")
   "ae" '(elfeed :which-key "Elfeed")

   "b" '(:ignore t :which-key "Buffers")
   "bb" '(consult-buffer :which-key "Switch")
   "bd" '(evil-delete-buffer :which-key "Delete")
   "bn" '(evil-buffer-new :which-key "New")

   "f" '(:ignore t :which-key "Files")
   "ff" '(find-file :which-key "Find file")
   "fr" '(consult-recent-file :which-key "Recent files")
   "fR" '(rename-visited-file :which-key "Rename file")
   "fs" '(save-buffer :which-key "Save buffer")

   "g" '(:ignore t :which-key "git")
   "g g" '(magit :which-key "magit")
   "g o" 'browse-at-remote
   "g l" 'browse-at-remote-kill
   "g y" 'browse-at-remote-kill
   "g s" 'magit-stage-file

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

   "u" '(universal-argument :which-key "Universal argument")

   "q" '(:ignore t :which-key "Quit")
   "qq" '(save-buffers-kill-terminal :which-key "Quit")
   "qr" '(bergheim/restart-emacs :which-key "Restart")

   "w" '(:ignore t :which-key "Workspace and windows")
   "ws" '(evil-window-split :which-key "split horizontally")
   "wv" '(evil-window-vsplit :which-key "split vertically")
   "wd" '(evil-window-delete :which-key "delete window")

   "wb" '(:ignore t :which-key "Burly")
   "wbf" '(burly-bookmark-frames :which-key "Bookmark frames")
   "wbw" '(burly-bookmark-windows :which-key "Bookmark windows and frames")
   "wbo" '(burly-open-bookmark :which-key "Open a bookmark")
   "wbl" '(burly-open-last-bookmark :which-key "Open last bookmark")
   "wu" '(winner-undo :which-key "Winner undo")
   "wr" '(winner-redo :which-key "Winner redo")
   "wm" '(bergheim/zoom-window :which-key "Maximize window")

   "wB" '(:ignore t :which-key "Bufler")
   "wBb" '(bufler :which-key "Show buffers")
   "wBs" '(bufler-switch-buffer :which-key "Switch to buffer"))

  (bergheim/global-evil-keys
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

  ;; (general-define-key
  ;;  :states '(normal visual)
  ;;  "gc" 'evil-commentary
  ;;  "gy" 'evil-commentary-yank
  ;;  "s" 'evil-surround-region)
  ;;

  (bergheim/emacs-lisp-keys
   "e" '(:ignore t :which-key "Eval")
   "e d" '(eval-defun :which-key "eval last defun")
   "e e" '(eval-last-sexp :which-key "eval last sexp")
   "e b" '(eval-last-sexp :which-key "eval buffer")
   "e r" '(elisp-eval-region-or-buffer :which-key "eval region or buffer"))
  )
