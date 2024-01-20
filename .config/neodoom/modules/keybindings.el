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

(with-eval-after-load 'general
  (general-def :keymaps '(vertico-active-map consult-preview-keymap embark-general-map)
    [escape] 'keyboard-quit)

  (general-def :keymaps 'transient-map
    [escape] 'transient-quit-one)

  ;; jk exits normal mode
  (general-imap "j"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "k" 'evil-normal-state))

  (general-define-key
   :states 'visual
   "u" 'undo-tree-undo
   "C-r" 'undo-tree-redo)

  (general-define-key
   :states '(normal visual)

   "C-=" 'global-text-scale-adjust
   "C--" '(lambda () (interactive) (global-text-scale-adjust -1))
   "C-0" '(lambda () (interactive) (bergheim/set-font-based-on-frame-resolution))

   "[b" 'previous-buffer
   "]b" 'next-buffer
   "]e" 'flymake-goto-next-error
   "[e" 'flymake-goto-prev-error
   "]e" 'flymake-goto-next-error
   "[n" 'bergheim/prev-file
   "]n" 'bergheim/next-file
   "[s" 'jinx-prev
   "]s" 'jinx-next
   "z=" 'jinx-correct-nearest
   "[t" 'tab-bar-switch-to-prev-tab
   "]t" 'tab-bar-switch-to-next-tab
   "[T" 'tab-bar-move-tab-backward
   "]T" 'tab-bar-move-tab)

  (general-def
    :keymaps '(evil-normal-state-map org-agenda-mode-map global-map)
    "C-S-u" 'universal-argument
    "C-u" 'evil-scroll-up
    "M-r" #'vertico-repeat
    ;; I sometimes want to use C-h for other things..
    "C-c h" 'help-command)

  (general-def
    :keymaps '(evil-motion-state-map)
    "gD" #'bergheim/evil-goto-definition-other-window)

  (bergheim/global-menu-keys
    "RET" '(consult-bookmark :which-key "Bookmarks")
    "/" '(bergheim/consult-ripgrep-with-selection :which-key "Grep project")
    "'" '(vertico-repeat :which-key "Repeat search")
    "`" '(bergheim/toggle-scratch-buffer :which-key "Scratch buffer")
    ;; "SPC" '(bergheim/consult-project-or-buffer :which-key "Find stuff")
    ;; "SPC" '(consult-buffer :which-key "Switch")
    "SPC" '(execute-extended-command :which-key "Run")

    "TAB" '(:ignore t :which-key "Tabs")
    "TAB TAB" '(tab-switch :which-key "switch to a tab")
    "TAB SPC" '(tab-bar-switch-to-recent-tab :which-key "switch to recent tab")
    "TAB d" '(tab-bar-detach-tab :which-key "detach tab")
    "TAB c" '(tab-close :which-key "close tab")
    "TAB g" '(tab-bar-change-tab-group :which-key "change group")
    "TAB G" '(tab-bar-close-group-tabs :which-key "close group")
    "TAB j" '(tab-next :which-key "next tab")
    "TAB k" '(tab-previous :which-key "previous tab")
    "TAB n" '(siren-tab-bar-switch-to-or-create-tab :which-key "new tab")
    "TAB p" '(bergheim/open-or-switch-to-project-tab :which-key "Switch project")
    "TAB r" '(tab-bar-rename-tab :which-key "Rename tab")
    "TAB x" '(tab-close :which-key "close tab")

    "a" '(:ignore t :which-key "Apps")
    "ad" '((lambda () (interactive) (dirvish "~/")) :which-key "Dirvish")
    "ae" '(elfeed :which-key "Elfeed")
    "at" '(treemacs :which-key "Treemacs")

    "b" '(:ignore t :which-key "Buffers")
    "bb" '(bergheim/consult-project-or-buffer :which-key "Switch")
    "bB" '(consult-buffer :which-key "Switch")

    "bd" '(kill-current-buffer :which-key "Delete")
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
    "fb" '(dirvish :which-key "Browse")
    "fc" '(bergheim/copy-current-buffer-file :which-key "Copy file")
    "fd" '(dirvish :which-key "Dirvish")
    "fD" '(bergheim/delete-current-file :which-key "Delete file")
    "ff" '(find-file :which-key "Find file")
    "fF" '(consult-find :which-key "Find file")
    "fh" '(dirvish-history-jump :which-key "Dirvish history")
    "fr" '(consult-recent-file :which-key "Recent files")
    "fR" '(rename-visited-file :which-key "Rename file")
    "fo" '(browse-url-of-buffer :which-key "Browse file")
    "fs" '(save-buffer :which-key "Save buffer")

    "g" '(:ignore t :which-key "git")
    "g g" '(magit :which-key "magit")
    "g o" 'browse-at-remote
    "g l" 'magit-log-buffer-file
    "g f" 'magit-find-file
    "g y" 'browse-at-remote-kill
    "g s" 'magit-stage-buffer-file

    "h" '(:ignore t :which-key "Help")
    "ha" '(apropos :which-key "Apropos")
    "hb" '(embark-bindings :which-key "Bindings")
    "hc" '(describe-char :which-key "Char")
    "hd" '(:ignore t :which-key "Debug")
    "he" '(view-echo-area-messages t :which-key "Echo area")
    "hdd" '(toggle-debug-on-error t :which-key "Debug")
    "hdq" '(toggle-debug-on-quit t :which-key "Debug on quit")
    "hf" '(helpful-callable :which-key "Function")
    "hk" '(helpful-key :which-key "Key")
    "hl" '(find-library :which-key "Library")
    "hm" '(describe-mode :which-key "Describe mode")

    "hp" '(:ignore t :which-key "Packages")
    "hpp" '(elpaca-manager :which-key "Packages")
    "hpb" '(elpaca-browse :which-key "Browse")
    "hpi" '(elpaca-info :which-key "Info")
    "hpf" '(elpaca-fetch-all :which-key "Fetch")
    "hpm" '(elpaca-merge-all :which-key "Merge")

    "hr" '(bergheim/reload-init-file :which-key "Reload")
    "ht" '(consult-theme :which-key "Switch theme")
    "hv" '(helpful-variable :which-key "Variable")

    "i" '(:ignore t :which-key "Insert")
    "ir" '(consult-register :which-key "Register")
    "ie" '(emoji-search :which-key "Emojiii")
    "iu" '(insert-char :which-key "Unicode")
    "ip" '(consult-yank-pop :which-key "kill-ring")
    "m" `(,bergheim/localleader-map :which-key "Local leader")

    "n" '(:ignore t :which-key "Notes")
    "nd" '(:ignore t :which-key "Denote")
    "ndd" '(denote :which-key "Denote")
    "ndl" '(denote-link :which-key "Link")
    "ns" '(consult-notes-search-in-all-notes :which-key "Search")

    "p" '(:ignore t :which-key "Project")
    "pb" '(consult-project-buffer :which-key "buffers")
    "pd" '(project-find-dir :which-key "Find dir")
    "pe" '(project-or-external-find-file :which-key "External Find file")
    "pf" '(project-find-file :which-key "Find file")
    "pF" '(affe-find :which-key "Async find file")
    "ps" '(bergheim/consult-ripgrep-with-selection :which-key "Grep project")
    "pt" '(treemacs :which-key "Treemacs")
    "pp" '(bergheim/open-or-switch-to-project-tab :which-key "Switch project")
    "pP" '((lambda () (interactive) (bergheim/open-or-switch-to-project-tab t)) :which-key "Switch project")

    ;; TODO: add lots more consult stuff like kill-ring etc
    "s" '(:ignore t :which-key "Search")
    "sa" '(consult-org-agenda :which-key "org agenda")
    "sb" '(consult-line-multi :which-key "open buffers")
    "sd" '(bergheim/consult-ripgrep-with-selection :which-key "Current directory")
    "sD" '(bergheim/consult-ripgrep-with-selection-other-dir :which-key "Other dir")
    "sA" '(affe-grep :which-key "Async grep")
    
    "sf" '(consult-fd :which-key "Search File")
    "sF" '(consult-find :which-key "Search all files")
    "sh" '(consult-isearch-history :which-key "Search history")
    "sr" '(consult-recent-file :which-key "recent files")
    "si" '(consult-imenu :which-key "imenu items")
    "sI" '(consult-imenu-multi :which-key "imenu items in all buffers")
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
    "t l" '(jinx-languages :which-key "Languages")
    "t p" '(popper-toggle-type :which-key "Toggle popup")
    "t s" '(jinx-mode :which-key "Spelling")
    "t v" '(bergheim/toggle-visual-fluff :which-key "Toggle visual helpers")
    "t w" '(bergheim/write-mode :which-key "Writer room")
    "t z" '(bergheim/present-mode :which-key "Present")

    "u" '(universal-argument :which-key "Universal argument")

    "q" '(:ignore t :which-key "Quit")
    "qf" '(delete-frame :which-key "Close frame")
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

    "y" '(:ignore t :which-key "Yanking"))

  (bergheim/global-evil-keys
    "M-h" #'evil-window-left
    "M-j" #'evil-window-down
    "M-k" #'evil-window-up
    "M-l" #'evil-window-right

    "M-H" #'evil-window-move-far-left
    "M-J" #'evil-window-move-far-bottom
    "M-K" #'evil-window-move-far-top
    "M-L" #'evil-window-move-far-right

    "M-\\" #'evil-window-vsplit
    "M-]" #'evil-window-split

    "M-DEL" #'evil-window-delete
    ;; "M-<backspace>" #'+workspace/close-window-or-workspace
    "M-S-<backspace>" #'kill-current-buffer

    "M-o" #'evil-window-next
    "M-f" #'maximize-window
    "M-F" #'winner-undo

    "M-1" '(lambda () (interactive) (tab-bar-select-tab 1))
    "M-2" '(lambda () (interactive) (tab-bar-select-tab 2))
    "M-3" '(lambda () (interactive) (tab-bar-select-tab 3))
    "M-4" '(lambda () (interactive) (tab-bar-select-tab 4))
    "M-5" '(lambda () (interactive) (tab-bar-select-tab 5))
    "M-6" '(lambda () (interactive) (tab-bar-select-tab 6))
    "M-t" 'siren-tab-bar-switch-to-or-create-tab
    "M-w" 'tab-close)

  ;; (general-define-key
  ;;  :states '(normal visual)
  ;;  "gc" 'evil-commentary
  ;;  "gy" 'evil-commentary-yank
  ;;  "s" 'evil-surround-region)
  ;;
  )
