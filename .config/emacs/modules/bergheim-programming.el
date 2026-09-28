;;; bergheim-programming.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim
;;
;; Author: Thomas Bergheim
;; Maintainer: Thomas Bergheim
;; Created: September 18, 2023
;; Modified: September 18, 2023

(use-package ediff
  :ensure nil
  :config
  (setq ediff-show-clashes-only t)
  ;; open diffs horizontally in the current frame
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

(use-package editorconfig
  :ensure nil ;; part of emacs 30
  :config
  (editorconfig-mode 1))

(use-package treesit
  :ensure nil
  :config
  (setq treesit-font-lock-level 4))

(use-package emacs
  :ensure nil
  :config
  (electric-pair-mode t)
  :custom
  (treesit-enabled-modes t)
  (treesit-auto-install-grammar 'always)
  (xref-search-program 'ripgrep)
  (grep-command "rg -nS --no-heading ")
  (grep-use-null-device nil))

(use-package dumb-jump
  :ensure t
  :init
  ;; autoloaded; the package itself loads on the first xref lookup
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  ;; should use `consult-xref`?
  ;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq dumb-jump-prefer-searcher 'rg))

;; used by vimish-fold
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode))

(use-package smartparens
  :disabled
  :demand
  :config
  ;; lisp pairs: no '' / `' pairing in elisp etc.
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  ;; (general-define-key
  ;;  :states 'normal
  ;;  :keymaps 'smartparens-mode-map
  ;;  "H" 'sp-backward-sexp
  ;;  "L" 'sp-forward-sexp
  ;;  "K" 'sp-backward-up-sexp
  ;;  "J" 'sp-down-sexp
  ;;  "C-M-l" 'sp-forward-slurp-sexp
  ;;  "C-M-h" 'sp-backward-barf-sexp
  ;;  "C-M-j" 'sp-forward-barf-sexp
  ;;  "C-M-k" 'sp-backward-slurp-sexp)

  ;; Prefer sp-comment in lisp-y buffers (respects sexps); fall back to
  ;; evil-commentary elsewhere. Currently defined but unbound — uncomment
  ;; the binding below (or rebind gcc) to actually use it.
  (defun bergheim/conditional-comment ()
    (interactive)
    (if (and (bound-and-true-p smartparens-mode)
             (fboundp 'sp-comment))
        (call-interactively 'sp-comment)
      (call-interactively 'evil-commentary)))
  ;; (general-define-key
  ;;  :states 'normal
  ;;  :keymaps 'evil-commentary-mode-map
  ;;  "gcc" #'bergheim/conditional-comment)
  )

(use-package evil-smartparens
  :disabled
  :after smartparens
  ;; :config
  ;; (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  :hook (emacs-lisp-mode . evil-smartparens-mode))

;; Modal structural editing for sexps that fits evil's mindset:
;; `symex-mode-interface' enters a transient state where h/j/k/l traverse
;; sexps, ( / ) slurp/barf, x deletes, c changes, etc. — whole-sexp objects,
;; evil-style keys. See https://countvajhula.github.io/symex.el/
(use-package symex
  :disabled
  :ensure t
  :after (evil general)
  :custom
  (symex-modal-backend 'evil)
  :general
  ;; Enter symex on demand via the leader — adjust prefix to taste.
  ;; "ms" reads as modal-structural; move it under your "l"isp prefix etc.
  ;; if you have one.
  (bergheim/global-menu-keys
    "ms" '(symex-mode-interface :which-key "symex"))
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode
                          scheme-mode clojure-mode racket-mode)
         . symex-mode)
  :config
  ;; Two soft conflicts to be aware of when you actually enable symex:
  ;;
  ;; 1) evil-snipe-override-mode owns s/S globally. If you want `s' in lisp
  ;;    buffers to enter symex (most ergonomic), turn snipe-override off
  ;;    locally there — e.g.:
  ;;    (add-hook 'emacs-lisp-mode-hook
  ;;              (lambda () (turn-off-evil-snipe-override-mode)))
  ;;
  ;; 2) evil-smartparens (above) is a different structural paradigm; if you
  ;;    commit to symex, consider dropping the evil-smartparens hook for the
  ;;    same modes so the two don't tug at the same keys.
  )

(use-package paredit
  :disabled
  :after general
  :hook (emacs-lisp-mode . (lambda ()
                             (setq-local evil-move-beyond-eol t)
                             (paredit-mode))))

;; this is pretty active
(use-package enhanced-evil-paredit
  :disabled
  :after paredit
  :config
  (general-define-key
   :states 'normal
   :keymaps 'paredit-mode-map
   "H" 'paredit-backward
   "J" 'paredit-forward-down
   ;; "M-J" 'paredit-forward-up
   "K" 'paredit-backward-up
   ;; "K" 'paredit-backward-up
   "L" 'paredit-forward
   "C-M-l" 'paredit-forward-slurp-sexp
   "C-M-h" 'paredit-forward-barf-sexp
   "C-M-j" 'paredit-backward-barf-sexp
   "C-M-k" 'paredit-backward-slurp-sexp)
  :hook (paredit-mode . #'enhanced-evil-paredit))

;; emacs lisp debuggers
(use-package emacs
  :ensure nil
  :general
  (:keymaps 'emacs-lisp-mode-map
   "<C-return>" 'eval-defun)

  (bergheim/localleader-keys
    :states 'normal
    :keymaps 'emacs-lisp-mode-map

    "d" '(:ignore t :which-key "debug")
    "de" '(edebug-defun             :which-key "edebug defun")
    "da" '(edebug-all-defs          :which-key "edebug all defs (buffer)")
    "di" '(edebug-on-entry          :which-key "edebug on entry")
    "dI" '(cancel-edebug-on-entry   :which-key "Cancel edebug on entry")

    "dt" '(toggle-debug-on-error    :which-key "toggle debug-on-error")
    "dq" '(toggle-debug-on-quit     :which-key "toggle debug-on-quit")

    "dd" '(debug-on-entry           :which-key "native debug on entry")
    "dD" '(cancel-debug-on-entry    :which-key "Cancel native debug on entry")
    "dw" '(debug-watch              :which-key "native debug watch variable")
    "dW" '(cancel-debug-watch       :which-key "Cancel native debug watch")

    "e" '(:ignore t :which-key "eval")
    "eb" '(eval-buffer              :which-key "buffer")
    "ed" '(eval-defun               :which-key "defun")
    "ee" '(eval-last-sexp           :which-key "last sexp")
    "es" '(eval-last-sexp           :which-key "last sexp")
    "er" '(eval-region              :which-key "region")
    "el" '(eval-print-last-sexp     :which-key "print last sexp")
    "ep" '(pp-eval-last-sexp        :which-key "pprint last sexp")
    "eP" '(pp-eval-defun            :which-key "pprint defun")

    "c" '(check-parens              :which-key "Check parens")
    "p" '(paredit-mode              :which-key "Toggle paredit")

    "m" '(pp-macroexpand-last-sexp  :which-key "Macroexpand last sexp")
    "M" '(macroexpand-all           :which-key "Macroexpand all")))

(use-package edebug
  :ensure nil
  :hook (edebug-mode . bergheim/setup-edebug-keys)
  :config
  (defun bergheim/setup-edebug-keys ()
    "Setup edebug keybindings that work better with evil."
    (general-define-key
     :keymaps 'edebug-mode-map
     ;; TODO make sure these are the same
     ;; "n" 'edebug-next-mode
     ;; "s" 'edebug-step-mode
     ;; "g" 'edebug-go-mode
     ;; "q" 'top-level
     ;; "c" 'edebug-continue-mode
     "v" 'evil-visual-char
     "w" 'edebug-where
     "V" 'edebug-view-outside
     "E" 'edebug-eval-last-sexp)))

(use-package markdown-mode
  :init
  ;; (setq markdown-command "pandoc -f markdown -t html")
  (setq markdown-command "markdown"))

(use-package typescript-ts-mode
  :ensure nil
  :custom (typescript-ts-mode-indent-offset 4))

(use-package sxhkdrc-mode)

;; see https://github.com/abicky/nodejs-repl.el for options
(use-package nodejs-repl
  :ensure t
  :commands (nodejs-repl))

;; ;; Enable repeat mode for more ergonomic `dape' use
;; (use-package repeat
;;   :config
;;   (repeat-mode))

(use-package mise
  :disabled
  :config
  (global-mise-mode))

;;; bergheim-programming.el ends here
