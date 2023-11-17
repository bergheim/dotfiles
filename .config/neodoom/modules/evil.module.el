;;; evil.module.el --- Description -*- lexical-binding: t; -*-

(use-package evil
  :config
  (setq evil-want-fine-undo t)
  (setq evil-want-C-u-scroll t)
  (defun bergheim/evil-search-symbol-forward ()
    "Search forward for the entire symbol under cursor, or fall back to word search."
    (interactive)
    (let ((symbol (thing-at-point 'symbol t)))
      (if symbol
          (evil-search symbol t t)
        (evil-ex-search-word-forward))))

  :general
  (evil-normal-state-map
   "*" 'bergheim/evil-search-symbol-forward))


(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org
  :ensure t
  :after org
  :hook
  (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; much better node matching
(use-package evil-matchit
  :ensure t
  :after evil
  :config
  (global-evil-matchit-mode 1))

;; gl/gL text-object char
(use-package evil-lion
  :ensure t
  :after evil
  :config
  (evil-lion-mode))

;; TODO add this and keybindings
;; (use-package evil-textobj-tree-sitter
;;   :ensure t)

;; gc  evil-commentary
;; gy  evil-commentary-yank
;; s-/ evil-commentary-line
;;     evil-commentary-yank-line
(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Just use evil-avy-goto-char-2?
;; use `z' or `x' in operator mode
;; (use-package evil-snipe
;;   :ensure t
;;   :after evil
;;   :config
;;   (setq evil-snipe-scope 'whole-visible)
;;   (push '(?\[ "[[{(]") evil-snipe-aliases)
;;   (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
;;   (evil-snipe-mode +1)
;;   ;; I find `s' (and to a lesser degree `S') pretty useless tbh, so just override it
;;   (evil-snipe-override-mode +1))

(use-package vimish-fold
  :ensure t
  :after evil)

(use-package evil-vimish-fold
  :ensure t
  :after vimish-fold
  :init
  (setq evil-vimish-fold-mode-lighter " ⮒")
  (setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
  :config
  (global-evil-vimish-fold-mode))

(use-package evil-owl
  :ensure t
  :after evil
  :config
  (setq evil-owl-max-string-length 500)
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)))
  (evil-owl-mode))

;; posframe is.. kinda cool I guess? but it does not contain all the text..
;; (use-package posframe
;;   :ensure t
;;   :config
;;   (setq posframe-width 80))

;; (use-package evil-owl
;;   :ensure t
;;   :config
;;   (setq evil-owl-display-method 'posframe
;;         evil-owl-extra-posframe-args '(:width 50 :height 20)
;;         evil-owl-max-string-length 50)
;;   (evil-owl-mode))


;;; evil.module.el ends here
