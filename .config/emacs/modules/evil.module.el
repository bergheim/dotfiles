;;; evil.module.el --- Description -*- lexical-binding: t; -*-

(use-package evil-collection
  :demand t
  :after evil
  :config
  (setq evil-collection-want-unimpaired-p nil)
  (evil-collection-init))

(use-package evil-escape
  :after evil
  :demand t
  :config
  (setq-default evil-escape-delay 0.2)
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-excluded-major-modes
                '(magit-status-mode
                  magit-diff-mode
                  magit-revision-mode
                  magit-log-mode))
  (setq-default evil-escape-excluded-states '(visual))
  (evil-escape-mode))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () (evil-org-mode))))

(use-package evil-org-agenda
  :ensure nil
  :demand
  :after org-agenda
  :config
  (evil-org-agenda-set-keys))

;; much better node matching
(use-package evil-matchit
  :ensure t
  :after evil
  :config
  (global-evil-matchit-mode 1))

;; gl/gL text-object char
(use-package evil-lion
  :after (evil general)
  :demand 10
  :config
  (general-define-key
   :states '(normal visual)
   :keymaps 'prog-mode-map
   "gl" 'evil-lion-left
   "gL" 'evil-lion-right))

;; FIXME this doesn't work anymore atm..
(use-package evil-textobj-tree-sitter
  :after evil
  :demand t
  :config
  ;; example of custom text object
  ;; (define-key evil-outer-text-objects-map "m" (evil-textobj-tree-sitter-get-textobj "import"
  ;;                                               '((python-mode . [(import_statement) @import])
  ;;                                                 (go-mode . [(import_spec) @import])
  ;;                                                 (rust-mode . [(use_declaration) @import]))))
  (define-key evil-outer-text-objects-map "f" (cons "evil-outer-function" (evil-textobj-tree-sitter-get-textobj "function.outer")))
  (define-key evil-inner-text-objects-map "f" (cons "evil-inner-function" (evil-textobj-tree-sitter-get-textobj "function.inner")))
  (define-key evil-outer-text-objects-map "c" (cons "evil-outer-class" (evil-textobj-tree-sitter-get-textobj "class.outer")))
  (define-key evil-inner-text-objects-map "c" (cons "evil-inner-class" (evil-textobj-tree-sitter-get-textobj "class.inner")))
  (define-key evil-outer-text-objects-map "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key evil-inner-text-objects-map "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key evil-outer-text-objects-map "i" (cons "evil-outer-conditional-loop" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer"))))
  (define-key evil-inner-text-objects-map "i" (cons "evil-inner-conditional-loop" (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner"))))
  (define-key evil-inner-text-objects-map "a" (cons "evil-inner-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.inner")))
  (define-key evil-outer-text-objects-map "a" (cons "evil-outer-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.outer")))

  (define-key evil-normal-state-map (kbd "]a") (cons "goto-parameter-start" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "parameter.inner"))))
  (define-key evil-normal-state-map (kbd "[a") (cons "goto-parameter-start" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "parameter.inner" t))))
  (define-key evil-normal-state-map (kbd "]A") (cons "goto-parameter-end" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "parameter.inner" nil t))))
  (define-key evil-normal-state-map (kbd "[A") (cons "goto-parameter-end" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "parameter.inner" t t))))
  (define-key evil-normal-state-map (kbd "]f") (cons "goto-function-start" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer") (reposition-window)))))
  (define-key evil-normal-state-map (kbd "[f") (cons "goto-function-start" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer" t) (reposition-window)))))
  (define-key evil-normal-state-map (kbd "]F") (cons "goto-function-end" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t) (reposition-window)))))
  (define-key evil-normal-state-map (kbd "[F") (cons "goto-function-end" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer" t t) (reposition-window))))))

;; gc  evil-commentary
;; gy  evil-commentary-yank
;; s-/ evil-commentary-line
;;     evil-commentary-yank-line
(use-package evil-commentary
  :demand t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :ensure t
  :demand t
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Just use evil-avy-goto-char-2?
;; use `z' or `x' in operator mode
(use-package evil-snipe
  :after evil
  :demand
  :custom
  (evil-snipe-smart-case t)
  (evil-snipe-scope 'whole-visible)
  (evil-snipe-auto-scroll nil)
  (evil-snipe-tab-increment t)
  :config
  ;; s[ to search brackets
  (push '(?\[ "[[{(]") evil-snipe-aliases)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  (evil-snipe-mode +1)
  ;; `s' (and to a lesser degree `S') is pretty useless.
  ;; and `cl` and `cc` does the same, so override it
  (evil-snipe-override-mode +1))

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

;; (use-pacage evil-cleverparens
;;   :ensure t
;;   :after evil
;;   :hook (emacs-lisp-mode . evil-cleverparens-mode)
;;   :config
;;   (setq evil-cleverparens-use-additional-bindings t
;;         evil-cleverparens-use-additional-movement-keys t
;;         evil-cleverparens-use-s-and-S nil))

(defun bergheim/evil-goto-definition-other-window ()
  "Open the definition in another window."
  (interactive)
  (let ((currentBuf (current-buffer))
        (currentPos (point-marker)))
    (evil-goto-definition)
    (let ((targetPos (point))
          (targetBuf (current-buffer)))
      (switch-to-buffer currentBuf)
      (if (one-window-p)
          (split-window-horizontally))
      (other-window 1)
      (switch-to-buffer targetBuf)
      (goto-char targetPos))))

;; (use-package lispyville
;;   :ensure t
;;   :after evil
;;   ;; :defer t
;;   :init
;;   (general-add-hook '(emacs-lisp-mode-hook lisp-mode-hook) #'lispyville-mode)
;;   :config
;;   (setq lispyville-key-theme '(operators c-w additional
;;                                          additional-movement slurp/barf-cp
;;                                          text-objects)))
;; (use-package iedit
;;   :after lispyville
;;   :config
;;   (define-key iedit-mode-keymap (kbd "C-;") nil))  ; this is used by embark

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

;; this package is not updated in 7 years, and aside from some misplaced matches
;; from time to time, it still does the job
;; at some point to be replaced by https://github.com/noctuid/things.el
(use-package evil-textobj-anyblock
  :after evil
  :demand t
  :config
  (evil-define-text-object my-evil-textobj-anyblock-inner-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "'")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count nil)))

  (evil-define-text-object my-evil-textobj-anyblock-a-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "'")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count t)))

  (evil-define-text-object my-evil-textobj-anyblock-inner-bracket
    (count &optional beg end type)
    "Select text enclosed by the nearest matching pair of brackets."
    (let ((evil-textobj-anyblock-blocks
           '(("(" . ")")
             ("{" . "}")
             ("\\[" . "\\]"))))
      (evil-textobj-anyblock--make-textobj beg end type count nil)))

  (evil-define-text-object my-evil-textobj-anyblock-a-bracket
    (count &optional beg end type)
    "Select text, including the nearest matching pair of brackets."
    (let ((evil-textobj-anyblock-blocks
           '(("(" . ")")
             ("{" . "}")
             ("\\[" . "\\]"))))
      (evil-textobj-anyblock--make-textobj beg end type count t)))

  (define-key evil-inner-text-objects-map "q" 'my-evil-textobj-anyblock-inner-quote)
  (define-key evil-outer-text-objects-map "q" 'my-evil-textobj-anyblock-a-quote)

  (define-key evil-inner-text-objects-map "b" 'my-evil-textobj-anyblock-inner-bracket)
  (define-key evil-outer-text-objects-map "b" 'my-evil-textobj-anyblock-a-bracket)

  (define-key evil-inner-text-objects-map "g" 'evil-textobj-anyblock-inner-block)
  (define-key evil-outer-text-objects-map "g" 'evil-textobj-anyblock-a-block)

  ;; can do some fancy movements as well wanted
  ;; (define-key evil-motion-state-map "b" 'evil-textobj-anyblock-backward-any-block-start)

  ;; only count some of these as blocks in lisp
  (add-hook 'lisp-mode-hook
            (lambda ()
              (setq-local evil-textobj-anyblock-blocks
                          '(("(" . ")")
                            ("{" . "}")
                            ("\\[" . "\\]")
                            ("\"" . "\""))))))

;;; evil.module.el ends here
