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

;; death to C-g
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(global-set-key [escape] 'keyboard-quit)
(with-eval-after-load 'mu4e
  (define-key mu4e-minibuffer-search-query-map [escape] 'abort-recursive-edit))

(let ((keymaps '(minibuffer-local-map minibuffer-local-ns-map minibuffer-local-completion-map minibuffer-local-must-match-map)))
  (dolist (map keymaps)
    (define-key (symbol-value map) [escape] 'abort-recursive-edit)))

;; https://www.masteringemacs.org/article/mastering-key-bindings-emacs
;; Generally, all keys prefixed with C-c ? (where ? is a single character) are reserved for you, and you alone
;; The other set of reserved keys are the F-keys from F5 and onwards. The other two prefix keys reserved to you are hyper and super



(use-package general
  :config
  (general-evil-setup)
  (defvar bergheim/localleader-map (make-sparse-keymap)
    "Keymap for 'SPC m'")

  (general-create-definer bergheim/global-menu-keys
    :states '(normal visual insert motion emacs)
    :prefix "SPC"
    :keymaps 'override
    :non-normal-prefix "M-SPC")

  (general-create-definer bergheim/localleader-keys
    :prefix "SPC m"
    :states '(normal visual emacs)
    :keymaps 'bergheim/localleader-map)

  (general-create-definer bergheim/global-evil-keys
    :states '(normal visual motion operator)
    :keymaps 'override)

  (general-create-definer bergheim/emacs-lisp-keys
    :prefix "SPC m"
    :states '(normal visual emacs)
    :keymaps 'emacs-lisp-mode-map)

  (general-def :keymaps '(vertico-active-map consult-preview-keymap embark-general-map)
    [escape] 'keyboard-quit)

  (general-def :keymaps 'transient-map
    [escape] 'transient-quit-one)

  ;; jk exits normal mode
  (general-imap "j"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "k" 'evil-normal-state))

  (general-def
    :keymaps '(evil-normal-state-map org-agenda-mode-map global-map)
    "C-S-u" 'universal-argument
    "C-u" 'evil-scroll-up)

  (bergheim/global-menu-keys
    "RET" '(consult-bookmark :which-key "Bookmarks")
    "/" '(bergheim/consult-ripgrep-with-selection :which-key "Grep project")
    "'" '(vertico-repeat :which-key "Repeat search")
    "`" '(bergheim/toggle-scratch-buffer :which-key "Scratch buffer")
    "SPC" '(find-file :which-key "Find file")

    "a" '(:ignore t :which-key "Apps")
    "ad" '(dirvish :which-key "Dirvish")
    "ae" '(elfeed :which-key "Elfeed")
    "at" '(treemacs :which-key "Treemacs")

    "b" '(:ignore t :which-key "Buffers")
    "bb" '(bergheim/consult-project-or-buffer :which-key "Switch")
    "bB" '(consult-buffer :which-key "Switch")

    "bd" '(evil-delete-buffer :which-key "Delete")
    "bn" '(evil-buffer-new :which-key "New")
    "bS" '(scratch-buffer :which-key "switch to scratch")
    "bs" '(lambda () (interactive) (switch-to-buffer-other-window "*scratch*")
            :which-key "switch to scratch")
    "bm" '(bookmark-set :which-key "Bookmark set")
    "bj" '(bookmark-jump :which-key "Bookmark jump")
    "bu" '(undo-tree-visualize :which-key "Undo tree")

    "c" '(:ignore t :which-key "Code")
    "cr" '(xref-find-references :which-key "Find references")
    "ce" '(consult-flymake :which-key "Search errors")
    "cE" '(flymake-show-buffer-diagnostics :which-key "Show errors")
    
    "d" '(:ignore t :which-key "dotfiles")
    "dd" (lambda () (interactive) (magit-status "/yadm::") :which-key "Switch")
    "df" '(bergheim/find-in-dotfiles :which-key "Find a file")
    "db" '(bergheim/browse-dotfiles :which-key "Browse")

    "f" '(:ignore t :which-key "Files")
    "fc" '(bergheim/copy-current-buffer-file :which-key "Copy file")
    "fd" '(dirvish :which-key "Dirvish")
    "fD" '(bergheim/delete-current-file :which-key "Delete file")
    "ff" '(find-file :which-key "Find file")
    "fr" '(consult-recent-file :which-key "Recent files")
    "fR" '(rename-visited-file :which-key "Rename file")
    "fs" '(save-buffer :which-key "Save buffer")

    "g" '(:ignore t :which-key "git")
    "g g" '(magit :which-key "magit")
    "g o" 'browse-at-remote
    "g l" 'magit-log-buffer-file
    "g y" 'browse-at-remote-kill
    "g s" 'magit-stage-buffer-file

    "h" '(:ignore t :which-key "Help")
    "ha" '(apropos :which-key "Apropos")
    "hb" '(embark-bindings :which-key "Bindings")
    "hf" '(helpful-callable :which-key "Function")
    "hk" '(helpful-key :which-key "Key")
    "hm" '(describe-mode :which-key "Describe mode")
    "hr" '(bergheim/reload-init-file :which-key "Reload")
    "ht" '(consult-theme :which-key "Switch theme")
    "hv" '(helpful-variable :which-key "Variable")

    "i" '(:ignore t :which-key "Insert")
    "ir" '(consult-register :which-key "Register")

    "m" `(,bergheim/localleader-map :which-key "Local leader")

    "p" '(:ignore t :which-key "Project")
    "pb" '(consult-project-buffer :which-key "buffers")
    "pd" '(project-find-dir :which-key "Find dir")
    "pe" '(project-or-external-find-file :which-key "External Find file")
    "pf" '(project-find-file :which-key "Find file")
    "pF" '(affe-find :which-key "Async find file")
    "ps" '(bergheim/consult-ripgrep-with-selection :which-key "Grep project")
    "pt" '(treemacs :which-key "Treemacs")
    "pp" '(project-switch-project :which-key "Switch project")

    ;; TODO: add lots more consult stuff like kill-ring etc
    "s" '(:ignore t :which-key "Search")
    "sa" '(consult-org-agenda :which-key "org agenda")
    "sb" '(consult-line-multi :which-key "open buffers")
    "sd" '((lambda () (interactive) (bergheim/consult-ripgrep-with-selection ".")) :which-key "Current directory")
    "sD" '((lambda () (interactive) 
             (let ((current-prefix-arg (prefix-numeric-value '(4)))) 
               (call-interactively 'bergheim/consult-ripgrep-with-selection)))
           :which-key "Other dir")
    "sA" '(affe-grep :which-key "Async grep")
    
    "sf" '(consult-fd :which-key "Search File")
    "sF" '(consult-find :which-key "Search all files")
    "sh" '(consult-isearch-history :which-key "Search history")
    "sr" '(consult-recent-file :which-key "recent files")
    "si" '(consult-imenu-multi :which-key "imenu items in all buffers")
    "sI" '(consult-imenu :which-key "imenu items")
    "sk" '(consult-kmacro :which-key "kmacro")
    "sm" '(consult-global-mark :which-key "global marks")
    "sM" '(consult-man :which-key "man")
    "sn" '(consult-focus-lines :which-key "narrow view")
    "so" '(consult-outline :which-key "outline")
    ;; todo remember consult-kmacro
    "sp" '(consult-project-buffer :which-key "project buffers")
    "ss" '(consult-line :which-key "buffer")
    "sS" '(consult-line-multi :which-key "open buffers")

    "t" '(:ignore t :which-key "Toggle")
    "t f" '(apheleia-global-mode :which-key "Toggle formatting")
    "t i" '(berhgeim/eglot-inlay-hints-toggle :which-key "Toggle inlay hints")
    "t p" '(popper-toggle-type :which-key "Toggle popup")
    "t w" '(writeroom-mode :which-key "Writerroom")
    "t z" '(bergheim/present-mode :which-key "Present")

    "u" '(universal-argument :which-key "Universal argument")

    "q" '(:ignore t :which-key "Quit")
    "qq" '(save-buffers-kill-terminal :which-key "Quit")
    "qr" '(bergheim/restart-emacs :which-key "Restart")

    "w" '(:ignore t :which-key "Workspace and windows")
    "w=" '(balance-windows :which-key "Balance")
    "ws" '(evil-window-split :which-key "split horizontally")
    "wv" '(evil-window-vsplit :which-key "split vertically")
    "wd" '(evil-window-delete :which-key "delete window")

    "wb" '(:ignore t :which-key "Burly")
    "wbf" '(burly-bookmark-frames :which-key "Bookmark frames")
    "wbw" '(burly-bookmark-windows :which-key "Bookmark windows and frames")
    "wbo" '(burly-open-bookmark :which-key "Open a bookmark")
    "wbl" '(burly-open-last-bookmark :which-key "Open last bookmark")
    "wu" '(winner-undo :which-key "Winner undo")
    "wU" '(winner-redo :which-key "Winner redo")
    "wr" '(evil-window-rotate-upwards :which-key "Rotate upwards")
    "wR" '(evil-window-rotate-downwards :which-key "Rotate downwards")
    "wm" '(bergheim/zoom-window :which-key "Maximize window")

    "wB" '(:ignore t :which-key "Bufler")
    "wBb" '(bufler :which-key "Show buffers")
    "wBs" '(bufler-switch-buffer :which-key "Switch to buffer"))

  (bergheim/global-evil-keys
    "M-h" #'evil-window-left
    "M-j" #'evil-window-down
    "M-k" #'evil-window-up
    "M-l" #'evil-window-right

            ;; TODO: do these work?
            "M-H" #'windmove-left
            "M-J" #'windmove-down
            "M-K" #'windmove-up
            "M-L" #'windmove-right

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
