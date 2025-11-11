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
;; well actually no, as this messes up remote ssh setups
;; (define-key key-translation-map (kbd "ESC") (kbd "C-g"))
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

  (general-define-key
   :states 'visual
   "u" 'evil-undo
   "C-r" 'evil-redo)

  (general-define-key
   :states '(normal visual emacs)
   "C-M-<return>" '((lambda ()
                      (interactive)
                      (if (equal (buffer-name) "*dashboard*")
                          (shell)
                        (multishell-pop-to-shell nil (expand-file-name default-directory))))
                    :which-key "shell")
   "C-=" 'global-text-scale-adjust
   "C--" '(lambda () (interactive) (global-text-scale-adjust -1))

   "[b" 'previous-buffer
   "]b" 'next-buffer
   "]e" 'flymake-goto-next-error
   "[e" 'flymake-goto-prev-error
   "]e" 'flymake-goto-next-error
   "[n" 'bergheim/prev-file
   "]n" 'bergheim/next-file
   "[s" 'jinx-previous
   "]s" 'jinx-next
   "z=" 'jinx-correct-nearest
   "z\\" 'jinx-correct-nearest
   "[t" 'tab-bar-switch-to-prev-tab
   "]t" 'tab-bar-switch-to-next-tab
   "[T" 'tab-bar-move-tab-backward
   "]T" 'tab-bar-move-tab)

  (general-def
    :keymaps '(evil-normal-state-map org-agenda-mode-map global-map)
    "C-S-u" 'universal-argument
    "C-u" 'evil-scroll-up
    "M-r" #'vertico-repeat-select
    ;; I sometimes want to use C-h for other things..
    "C-c h" 'help-command)

  (general-def
    :keymaps '(evil-motion-state-map)
    "gD" #'bergheim/evil-goto-definition-other-window)

  (general-def
    :keymaps 'elpaca-log-mode-map
    :states 'normal
    "M-RET" 'elpaca-log-view-diff
    "gx" 'elpaca-ui-browse-package
    "gd" 'elpaca-log-view-diff)

  (bergheim/global-menu-keys
    "/" '(bergheim/consult-ripgrep-with-selection :which-key "Grep project")
    "'" '(vertico-repeat :which-key "Repeat search")
    "æ" '(vertico-repeat :which-key "Repeat search")
    "\"" '(vertico-repeat-select :which-key "Repeat a search")
    ";" '(embark-dwim :which-key "Embark DWIM")
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
    "TAB J" '(tab-bar-move-tab :which-key "move left")
    "TAB K" '(tab-bar-move-tab-backward :which-key "move right")
    "TAB n" '(siren-tab-bar-switch-to-or-create-tab :which-key "new tab")
    "TAB p" '(bergheim/open-or-switch-to-project-tab :which-key "Switch project")
    "TAB r" '(tab-bar-rename-tab :which-key "Rename tab")
    "TAB x" '(tab-close :which-key "close tab")
    "TAB 1" '((lambda () (interactive) (tab-select 1)) :which-key "Tab 1")
    "TAB 2" '((lambda () (interactive) (tab-select 2)) :which-key "Tab 2")
    "TAB 3" '((lambda () (interactive) (tab-select 3)) :which-key "Tab 3")
    "TAB 4" '((lambda () (interactive) (tab-select 4)) :which-key "Tab 4")
    "TAB 5" '((lambda () (interactive) (tab-select 5)) :which-key "Tab 5")
    "TAB 6" '((lambda () (interactive) (tab-select 6)) :which-key "Tab 6")

    "a" '(:ignore t :which-key "Apps")
    "ac" '(bergheim/open-calendar :which-key "Calendar")
    "aC" '((lambda () (interactive) (find-file bergheim/calendar/nextcloud/local)) :which-key "Calendar org")
    "af" `(,(bergheim/call-with-universal-arg #'dirvish) :which-key "Files home")
    "aF" '(dired :which-key "Files")
    "ae" '(elfeed :which-key "Elfeed")
    "am" '(:ignore t :which-key "Spotify")
    "amh" '(hydra-spotify/body :which-key "Hydra")
    "amm" '(smudge-my-playlists :which-key "Search my lists")
    "amp" '(smudge-playlist-search :which-key "Search playlists")
    "amt" '(smudge-track-search :which-key "Search track")
    "at" '(:ignore t :which-key "Terminals")
    "aw" '(treemacs :which-key "Treemacs workspace")

    "b" '(:ignore t :which-key "Buffers")
    "ba" '(consult-buffer :which-key "All buffers")
    "bb" '(bergheim/switch-to-relevant-buffer :which-key "buffers")
    "bB" '(consult-buffer :which-key "All buffers")

    "bc" '(clone-indirect-buffer-other-window :which-key "clone")
    "bd" '(kill-current-buffer :which-key "Delete")
    "bn" '(evil-buffer-new :which-key "New")
    "bS" '((lambda () (interactive) (switch-to-buffer-other-window "*scratch*"))
           :which-key "scratch other window")
    "bm" '(bookmark-set :which-key "Bookmark set")
    "be" '(bookmark-bmenu-list :which-key "Bookmark edit")
    "bj" '(bookmark-jump :which-key "Bookmark jump")
    "bf" '(consult-bookmark :which-key "Bookmark find")
    "bl" '(ibuffer :which-key "ibuffer")

    "c" '(:ignore t :which-key "Code")
    "cb" '(eldoc-doc-buffer :which-key "Eldoc other buffer")

    "cd" '(:ignore t :wk "Debug (dape)")
    "cdb" '(dape-breakpoint-toggle :wk "Toggle Breakpoint")
    "cdB" '(dape-breakpoint-remove-all :wk "Delete All Breakpoints")
    "cdc" '(dape-continue :wk "Continue")
    "cdd" '(dape :wk "Start")
    "cde" '(dape-evaluate-expression :wk "Evaluate")
    "cdE" '(dape-breakpoint-expression :wk "Dape Breakpoint Expression")
    "cdI" '(dape-info :wk "Info")
    "cdi" '(dape-step-in :wk "Step In")
    "cdk" '(dape-kill :wk "Kill Session")
    "cdl" '(dape-breakpoint-log :wk "Dape Breakpoint Log")
    "cdn" '(dape-next :wk "Next")
    "cdo" '(dape-step-out :wk "Step Out")
    "cdq" '(dape-quit :wk "Quit Debugger")
    "cdr" '(dape-repl :wk "REPL")
    "cdR" '(dape-restart :wk "Restart")
    "cds" '(dape-select-stack :wk "Select Stack")
    "cdw" '(dape-watch-dwim :wk "Watch DWIM")

    "cd<" '(dape-stack-select-down nil :wf "Dape Stack Down")
    "cd>" '(dape-stack-select-up nil :wf "Dape Stack Up")

    "cD" '(xref-find-definitions :which-key "Find definitions")
    ;; "cD" '(xref-find-definitions-other-window :which-key "Find definitions other")
    "cr" '(xref-find-references :which-key "Find references")
    ;; TODO are these useful?
    ;; "cR" '(xref-query-replace-in-results :which-key "Rename")
    ;; "cR" '(xref-find-references-and-replace :which-key "Rename")
    "ce" '(consult-flymake :which-key "Search errors")
    "cE" '(flymake-show-buffer-diagnostics :which-key "Show errors")
    
    "d" '(:ignore t :which-key "dotfiles")
    "dd" (lambda () (interactive) (magit-status "/yadm::") :which-key "Magit")
    "df" '(bergheim/find-in-dotfiles :which-key "Find a file")
    "ds" '(bergheim/search-in-dotfiles :which-key "Search for a file")
    "db" '(bergheim/browse-dotfiles :which-key "Browse")
    "de" '((lambda () (interactive) (consult-ripgrep "~/.config/emacs")) :which-key "Grep emacs")
    "dg" '((lambda () (interactive) (consult-ripgrep "~/.config/")) :which-key "Grep config")

    "f" '(:ignore t :which-key "Files")
    "fb" '(dirvish :which-key "Browse")
    "fc" '(bergheim/copy-current-buffer-file :which-key "Copy file")
    "fd" '(dirvish :which-key "Dirvish")
    "fD" '(bergheim/delete-current-file :which-key "Delete file")
    ;; TODO: make like rg fd/fD = fd, sd/sD = rg
    "ff" '(find-file :which-key "Find file")
    "fF" '(consult-fd :which-key "Fd file")
    ;; "fd" '(consult-fd :which-key "Find file (fast)")

    "fh" '(dirvish-history-jump :which-key "Dirvish history")
    "fr" '(consult-recent-file :which-key "Recent files")
    "fR" '(rename-visited-file :which-key "Rename file")
    "fo" '(browse-url-of-buffer :which-key "Browse file")
    "fs" '(save-buffer :which-key "Save buffer")
    "fS" `(,(bergheim/call-with-universal-arg #'save-some-buffers) :which-key "Save all buffers")
    "fy" '(+default/yank-buffer-path :which-key "Yank file path")

    "g" '(:ignore t :which-key "git")
    "g b" '(magit-blame :which-key "blame")
    "g g" '(magit :which-key "magit")
    "g o" 'forge-browse
    "g l" `(,(bergheim/call-with-universal-arg #'magit-log-buffer-file) :which-key "log buffer file")
    "g L" `(,(bergheim/call-with-universal-arg #'magit-log-all) :which-key "Log project")
    "g f" 'magit-find-file
    "g y" 'forge-copy-url-at-point-as-kill
    "g s" 'magit-stage-buffer-file

    "h" '(:ignore t :which-key "Help")
    "ha" '(apropos :which-key "Apropos")
    "hb" '(embark-bindings :which-key "Bindings")
    "hc" '(describe-char :which-key "Char")
    "he" '(view-echo-area-messages t :which-key "Echo area")
    "hw" '((lambda ()
             (interactive)
             (switch-to-buffer-other-window "*Warnings*"))
           :which-key "Warnings")

    "hd" '(nil t :which-key "Debug")
    "hda" '(debugger-quit :which-key "Abort debugging")
    "hdb" '(debug-on-entry :which-key "Debug function")
    "hdB" '(edebug-on-entry :which-key "Edebug function")
    "hdc" '(cancel-debug-on-entry :which-key "Cancel debug function")
    "hdC" '(edebug-cancel-on-entry :which-key "Cancel edebug function")
    "hdd" '(toggle-debug-on-error t :which-key "Debug")
    "hdq" '(toggle-debug-on-quit t :which-key "Debug on quit")
    "hdw" '(debug-watch t :which-key "Debug on watch")
    "hdW" '(cancel-debug-watch t :which-key "Cancel debug on watch")

    "hf" '(helpful-callable :which-key "Callable function")
    "hF" '(helpful-function :which-key "Function")
    "hi" '(consult-info :which-key "Search info")
    "hI" '(info :which-key "Info")
    "hk" '(helpful-key :which-key "Key")
    "hy" '(describe-keymap :which-key "Keymap")
    "hl" '(find-library :which-key "Library")
    "hm" '(describe-mode :which-key "Describe mode")
    "hP" '(describe-package :which-key "Describe package")

    "hp" '(:ignore t :which-key "Packages")
    "hpp" '(elpaca-manager :which-key "Packages")
    "hpb" '(elpaca-browse :which-key "Browse")
    "hpi" '(elpaca-info :which-key "Info")
    "hpf" '(elpaca-fetch-all :which-key "Fetch")
    "hpm" '(elpaca-merge-all :which-key "Merge")

    "hr" '(bergheim/reload-init-file :which-key "Reload")
    "ht" '(consult-theme :which-key "theme")
    "hg" '(fontaine-set-preset :which-key "glyphs")
    "hv" '(helpful-variable :which-key "Variable")
    "hs" '(helpful-symbol :which-key "Symbol")
    "hx" '(helpful-command :which-key "Command")

    "hzs" '(profiler-start   :which-key "Start profiler")
    "hzr" '(profiler-report  :which-key "Show report")
    "hzq" '(profiler-stop    :which-key "Stop profiler")

    "i" '(:ignore t :which-key "Insert")
    "ir" '(consult-register :which-key "Register")
    "ie" '(emoji-search :which-key "Emojiii")
    "iE" '(emoji-insert :which-key "Emojiii")
    "ik" '(karakeep-dwim :which-key "Send to Karakeep")
    "iK" `(,(bergheim/call-with-universal-arg  #'karakeep-dwim) :which-key "Send to Karakeep list")
    "iu" '(insert-char :which-key "Unicode")
    "iy" '(consult-yank-pop :which-key "kill-ring")

    "j" '(:ignore t :which-key "AI")

    "m" `(,bergheim/localleader-map :which-key "Local leader")

    "p" '(:ignore t :which-key "Project")
    "pb" '(consult-project-buffer :which-key "buffers")
    "pd" '(project-find-dir :which-key "Find dir")
    "pe" '(project-or-external-find-file :which-key "External Find file")
    "pf" '(project-find-file :which-key "Find file")
    "pF" '(bergheim/project-find-file-other-window :which-key "Find file other window")
    ;; "pF" '(affe-find :which-key "Async find file")
    "ps" '(bergheim/consult-ripgrep-with-selection :which-key "Grep project")
    "pt" '(project-shell :which-key "Shell")
    "pT" '(project-eshell :which-key "Eshell")
    "pp" '(bergheim/open-or-switch-to-project-tab :which-key "Switch project")
    "pP" '((lambda () (interactive) (bergheim/open-or-switch-to-project-tab t)) :which-key "Switch project")
    "pw" '(treemacs :which-key "Treemacs workspace")

    ;; TODO: add lots more consult stuff like kill-ring etc
    "s" '(:ignore t :which-key "Search")
    "sa" '(consult-org-agenda :which-key "org agenda")
    "sb" '(consult-line-multi :which-key "open buffers")
    "sB" `(,(bergheim/call-with-universal-arg  #'consult-line-multi) :which-key "all buffers")
    "sd" '(bergheim/consult-ripgrep-with-selection-current-dir :which-key "Current directory")
    "sD" '(bergheim/consult-ripgrep-with-selection-other-dir :which-key "Other dir")
    "sA" '(affe-grep :which-key "Async grep")
    "sf" '(consult-fd :which-key "Search files")
    "sF" '(bergheim/consult-fd-other-dir :which-key "Search files in dir")
    
    "sh" '(consult-isearch-history :which-key "Search history")
    "sr" '(consult-recent-file :which-key "recent files")
    "si" '(consult-imenu :which-key "imenu items")
    "sI" '(consult-imenu-multi :which-key "imenu items all buffers")
    "sk" '(consult-kmacro :which-key "kmacro")
    "sl" '(bergheim/avy-goto-link :which-key "open URL")
    "sL" `(,(bergheim/call-with-universal-arg  #'bergheim/consult-browse-links) :which-key "list and open URL")
    "sm" '(consult-global-mark :which-key "global marks")
    "sM" '(consult-man :which-key "man")
    "sn" '(consult-focus-lines :which-key "narrow view")
    "so" '(consult-outline :which-key "outline")
    ;; todo remember consult-kmacro
    "ss" '(consult-line :which-key "buffer")
    "sS" '(consult-line-multi :which-key "open buffers")

    "t" '(:ignore t :which-key "Toggle")
    "t f" '(apheleia-global-mode :which-key "Toggle formatting")
    "t i" '(bergheim/eglot-inlay-hints-toggle :which-key "Toggle inlay hints")
    "t L" '(jinx-languages :which-key "Languages")
    "t n" '(display-line-numbers-mode :which-key "Toggle line numbers")

    ;; toggle-truncate-lines?
    "t l" '(visual-line-mode :which-key "Visual lines")
    "t g" '(zoom-mode :which-key "Golden ratio")
    "t p" '(popper-toggle-type :which-key "Toggle popup")
    "t s" '(jinx-mode :which-key "Spelling")
    "t o" '(org-toggle-link-display :which-key "Org links")
    "t v" '(bergheim/toggle-visual-fluff :which-key "Toggle visual helpers")
    "t s" '(window-toggle-side-windows :which-key "Sidewindows")
    "t t" '(toggle-frame-tab-bar the :which-key "Tab bar")
    "t w" '(bergheim/write-mode :which-key "Writer room")
    "t z" '(bergheim/present-mode :which-key "Present")

    "r" '(:ignore t :which-key "Run")
    "re" '(eval-expression :which-key "Expression")

    "u" '(universal-argument :which-key "Universal argument")

    "q" '(:ignore t :which-key "Quit")
    "qf" '(delete-frame :which-key "Close frame")
    "qq" '(save-buffers-kill-terminal :which-key "Quit")
    "qr" '(bergheim/restart-emacs :which-key "Restart")
    "qs" '(bergheim/save-desktop :which-key "Save session")
    "qS" '((lambda () (interactive) (desktop-auto-save 1) :which-key "Autosave session"))
    "ql" '(bergheim/load-desktop :which-key "Load session")

    "w" '(:ignore t :which-key "Workspace and windows")
    "w=" '(balance-windows :which-key "Balance")
    "ws" '(evil-window-split :which-key "split horizontally")
    "wS" '(bergheim/swap-window-with :which-key "swap windows")
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
    "ww" '(activities-resume :which-key "Resume activity")

    "wB" '(:ignore t :which-key "Bufler")
    "wBb" '(bufler :which-key "Show buffers")
    "wBs" '(bufler-switch-buffer :which-key "Switch to buffer")

    "y" '(:ignore t :which-key "Yanking"))

  (bergheim/global-evil-keys
    "M-h" #'evil-window-left
    "M-j" #'evil-window-down
    "M-k" #'evil-window-up
    "M-l" #'evil-window-right

    "M-H" #'evil-window-move-far-left
    "M-J" #'evil-window-move-very-bottom
    "M-K" #'evil-window-move-very-top
    "M-L" #'evil-window-move-far-right

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

  (general-define-key
   :states '(insert normal visual motion operator emacs)
   "M-\\" #'evil-window-vsplit
   "M-]" #'evil-window-split
   "M-DEL" #'evil-window-delete
   "C-M-<backspace>" #'kill-current-buffer)

  ;; (general-define-key
  ;;  :states '(normal visual)
  ;;  "gc" 'evil-commentary
  ;;  "gy" 'evil-commentary-yank
  ;;  "s" 'evil-surround-region)
  ;;
  )
