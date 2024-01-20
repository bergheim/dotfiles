;;; completion.el --- Description -*- lexical-binding: t; -*-

(use-package emacs
  :elpaca nil
  :config
  (setq enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
  (setq completion-cycle-threshold 1)                  ; TAB cycles candidates
  (setq completions-detailed t)                        ; Show annotations
  (setq tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
  (setq completion-styles '(basic initials substring)) ; Different styles to match input to candidates

  (setq completion-auto-help 'always)                  ; Open completion always; `lazy' another option
  (setq completions-max-height 20)                     ; This is arbitrary
  (setq completions-detailed t)
  (setq completions-format 'one-column)
  (setq completions-group t)
  (setq completion-auto-select 'second-tab)            ; Much more eager
  ;; (setq completion-auto-select t)                      ; See `C-h v completion-auto-select' for more possible values

  (keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)) ; TAB acts more like how it does in the shell

(use-package avy
  :demand t
  :after evil
  :custom
  (avy-timeout-seconds 0.3)
  :config

  ;; this allows us to go back. strange the evil version does not do this..
  (defadvice evil-avy-goto-char-timer (around bergheim/save-position activate)
    (evil-set-jump)
    ad-do-it)

  (general-define-key
   :states '(normal visual)
   "M-d" 'evil-avy-goto-char-timer
   "g SPC" 'evil-avy-goto-char-timer
   "gu" 'avy-resume)
   ;; FIXME: too broad; this messes up the surround operator, say ds(
  ;; (general-define-key
  ;;  :states 'operator
  ;;  "z" 'evil-avy-goto-char-2
  ;;  "x" 'evil-avy-goto-char-2
  ;;  "s" 'evil-avy-goto-char-2-below
  ;;  "S" 'evil-avy-goto-char-2-above)
  )


;; right click from your keyboard
(use-package embark
  :demand t
  :after avy
  :bind (("C-c a" . embark-act)
         ("C-'" . embark-act)
         ("C-c b" . embark-bindings)
         ("C-;" . embark-dwim))
  :init
  ;; use embark to search the help menu
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; if you still want which-key for general..
  ;; (setq embark-act-pre-display-hook
  ;;       (lambda () (which-key--hide-popup-ignore-command))
  ;;       embark-post-action-hook
  ;;       (lambda () (which-key--show-popup which-key--buffer))
  ;;       prefix-help-command 'which-key-show-top-level)

  ;; Add the option to run embark when using avy
  (defun bedrock/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; FIXME: this does not work
  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; lots of more filtering options for completing-read
(use-package consult
  ;; Other good things to bind: consult-ripgrep, consult-line-multi,
  ;; consult-history, consult-outline
  :bind (("M-p" . consult-yank-pop) ; yes yes. I am an evil heretic
         ("C-s" . consult-line)     ; orig. isearch
         :map minibuffer-local-map
         ("M-s" . consult-history)  ;; orig. next-matching-history-element
         ("M-r" . consult-history))
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

(use-package consult-todo :after consult)

(defun bergheim/consult-ripgrep-with-selection (&optional dir)
  "Run `consult-ripgrep' with the current visual selection as the initial input.
If called interactively with a prefix argument, prompt for DIR, otherwise use the default behavior of `consult-ripgrep`."
  (interactive
   (list (when current-prefix-arg
           (read-directory-name "Search Directory: "))))
  (let ((initial (when (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end)))))
    (consult-ripgrep (or dir (consult--project-root)) initial)))

(defun bergheim/consult-ripgrep-with-selection-other-dir ()
  "Invoke `bergheim/consult-ripgrep-with-selection` with a chosen directory."
  (interactive)
  (let ((dir (read-directory-name "Search Directory: ")))
    (bergheim/consult-ripgrep-with-selection dir)))

(defun bergheim/consult-project-or-buffer ()
  "Call `consult-project-buffer` if in a project, otherwise `consult-buffer`."
  (interactive)
  (if (project-current)
      (consult-project-buffer)
    (consult-buffer)))

;; TODO: make sure this works on multiple implementations
(use-package xref
  :elpaca nil
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(defun sort-directories-first (files)
  (setq files (vertico-sort-history-alpha files))
  (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
         (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

;; Minibuffer completion
(use-package vertico
  :demand t
  :ensure t
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  ;; save the state so we can `vertico-repeat'
  :hook (minibuffer-setup . vertico-repeat-save)
  :general
  (general-def :keymaps 'vertico-map
    "C-h" 'vertico-directory-up
    "C-j" 'vertico-next
    "C-k" 'vertico-previous
    "C-M-j" 'vertico-next-group
    "C-M-k" 'vertico-previous-group
    "C-l" 'vertico-directory-enter)
  :config
  (setq vertico-sort-function #'vertico-sort-history-alpha
        vertico-multiform-commands '((consult-imenu-multi buffer indexed)
                                     (jinx-languages (vertico-sort-function . bergheim/jinx-language-sort))
                                     (mu4e-headers-action grid)
                                     (mu4e-view-action grid)
                                     ;; (execute-extended-command buffer)
                                     ;; (t posframe)
                                     )
        vertico-multiform-categories '((symbol (vertico-sort-function . vertico-sort-alpha))
                                       (jinx grid (vertico-grid-annotate . 20))
                                       ;; (consult-grep buffer)
                                       (file (vertico-sort-function . sort-directories-first)))
        vertico-scroll-margin 4
        vertico-count 10
        vertico-resize 'grow-only
        vertico-cycle t))

;; these look ice and all but I dunno..
;; (use-package vertico-posframe
;;   :ensure t
;;   :config
;;   (setq vertico-posframe-parameters
;;         '((left-fringe . 32)
;;           (top-fringe . 32)
;;           (bottom-fringe . 32)
;;           (right-fringe . 32)))
;;   (vertico-posframe-mode 1))

;; (use-package mini-frame
;;   :custom
;;   (custom-set-variables
;;    '(mini-frame-show-parameters
;;      '((top . 10)
;;        (width . 0.7)
;;        (left . 0.5))))
;;   :config
;;   (mini-frame-mode))

;; FIXME: this is so terrible..
(add-to-list 'load-path (expand-file-name "extensions/" bergheim/config-dir ))
(use-package vertico-directory
  :elpaca nil
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; this loads it up into `other-buffer'.
;; like that crazy japanese guys vim extension years ago! de..polete?
;; (require 'vertico-buffer)
;; (use-package vertico-buffer
;;   :ensure nil
;;   :after vertico
;;   :config
;;   (vertico-buffer-mode))

;; Add descriptions to completion
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;; COmpletion in Region FUnction (code completion)
(use-package corfu
  :init
  ;; (setq corfu-max-width 150)
  ;; (setq corfu-max-height 35)
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("S-SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("M-n" . nil)
        ("M-p" . nil))
  :custom
  (corfu-auto t) ;; enable auto completion
  (corfu-cycle t)
  ;; if we have applied the separator, never quit
  ;; (corfu-quit-no-match 'separator)
  (corfu-auto-prefix 2)) ;; min chars

;; Part of corfu
(use-package corfu-popupinfo
  :elpaca nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode))

(use-package dabbrev
  :elpaca nil
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; combine completion at point functions. if the name cape was not clear
(use-package cape
  :demand t
  :config
  ;; globally available CAPE completions (with lower priority)
  (add-hook 'completion-at-point-functions #'cape-dabbrev 80)
  (add-hook 'completion-at-point-functions #'cape-file 80)
  (add-hook 'completion-at-point-functions #'cape-emoji 80))

(defun bergheim/org-mode-setup-corfu ()
  (add-to-list 'completion-at-point-functions 'org-block-capf)
  (add-to-list 'completion-at-point-functions #'cape-tex 80)
  ;; TODO: set up a dict and enable this
  ;; (add-to-list 'completion-at-point-functions #'cape-dict 80)
  (add-to-list 'completion-at-point-functions #'cape-keyword 80))

(add-hook 'org-mode-hook #'bergheim/org-mode-setup-corfu)

;; Orderless: powerful completion style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  ;; this has a bunch of other things set up.. so just set everything from orderless
  (completion-category-defaults nil)
  (completion-category-overrides '(file (styles basic partial-completion)))
  (orderless-affix-dispatch-alist
   '((37 . char-fold-to-regexp)       ; %
     (33 . orderless-without-literal) ; !
     (44 . orderless-initialism)      ; ,
     (61 . orderless-literal)         ; =
     (126 . orderless-flex)           ; ~
     (43 . orderless-prefix)))        ; +
  ;; don't add rarely used things here, use dispatchers instead
  (orderless-matching-styles '(orderless-literal orderless-prefixes orderless-regexp)))

;; TODO: use this? I want MRU and then alphanumeric sorting
;; (use-package vertico-prescient
;;   :ensure t
;;   :after vertico
;;   :config
;;   (vertico-prescient-mode))

(use-package org-block-capf
  :elpaca (:host github :repo "xenodium/org-block-capf"))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :after corfu
  :config
  (setq kind-icon-default-style
        '(:padding -1 :stroke 0 :margin 0 :radius 0 :height 0.5 :scale 1.0))
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package tempel
  :after corfu
  ;; Require trigger prefix before template name when completing.
  ;; :custom (tempel-trigger-prefix "!")
  :bind (("C-c t" . tempel-expand)))

(defun tempel-setup-capf ()
  ;; Add the Tempel Capf to `completion-at-point-functions'.
  ;; `tempel-expand' only triggers on exact matches. Alternatively use
  ;; `tempel-complete' if you want to see all matches, but then you
  ;; should also configure `tempel-trigger-prefix', such that Tempel
  ;; does not trigger too often when you don't expect it. NOTE: We add
  ;; `tempel-expand' *before* the main programming mode Capf, such
  ;; that it will be tried first.
  (setq-local completion-at-point-functions
              (cons #'tempel-complete
                    completion-at-point-functions)))

(add-hook 'conf-mode-hook 'tempel-setup-capf)
(add-hook 'prog-mode-hook 'tempel-setup-capf)
(add-hook 'text-mode-hook 'tempel-setup-capf)

(use-package tempel-collection
  :after tempel)

;; `~' returns home
(defun bergheim/find-file-recognize-home (orig-fun &rest args)
  (let ((input (car args)))
    (if (and input (string-equal input "~/~"))
        (apply orig-fun "~" (cdr args))
      (apply orig-fun args))))

(advice-add 'find-file :around #'bergheim/find-file-recognize-home)
(provide 'completion)
;;; completion.el ends here
