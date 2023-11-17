;;; completion.el --- Description -*- lexical-binding: t; -*-

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

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

(use-package avy
  :ensure t
  :demand t
  :after evil
  :config
  ;; :bind (("C-c j" . avy-goto-line)
  ;;        ("C-c u"   . avy-goto-char-timer)))
  (general-define-key
   :states '(normal visual)
   "s" 'evil-avy-goto-char-2)

  (general-define-key
   :states 'operator
   "z" 'evil-avy-goto-char-2
   "x" 'evil-avy-goto-char-2))

;; right click from your keyboard
(use-package embark
  :ensure t
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
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; lots of more filtering options for completing-read
(use-package consult
  :ensure t
  ;; Other good things to bind: consult-ripgrep, consult-line-multi,
  ;; consult-history, consult-outline
  :bind (("C-x b" . consult-buffer) ; orig. switch-to-buffer
         ;; TODO: add this to keybindings.el
         ("M-y" . consult-yank-pop) ; orig. yank-pop
         ("C-s" . consult-line))    ; orig. isearch
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<")
  ;; (setq consult-xref-display-action nil)
  (consult-customize
   consult-xref
   :display 'vertico))

(defun bergheim/consult-ripgrep-with-selection (&optional dir)
  "Run `consult-ripgrep' with the current visual selection as the initial input.
If called interactively with a prefix argument, prompt for DIR, otherwise use the default behavior of `consult-ripgrep`."
  (interactive
   (list (when current-prefix-arg
           (read-directory-name "Search Directory: "))))
  (let ((initial (when (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end)))))
    (consult-ripgrep (or dir (consult--project-root)) initial)))

(defun bergheim/consult-project-or-buffer ()
  "Call `consult-project-buffer` if in a project, otherwise `consult-buffer`."
  (interactive)
  (if (project-current)
      (consult-project-buffer)
    (consult-buffer)))


;; TODO: make sure this works on multiple implementations
(use-package xref
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

;; Minibuffer completion
(use-package vertico
  :ensure t
  :demand t
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode)
  ;; save the state so we can `vertico-repeat'
  :hook (minibuffer-setup . vertico-repeat-save)
  :general
  (general-def :keymaps 'vertico-map
    "C-h" 'vertico-directory-up
    "C-j" 'vertico-next
    "C-k" 'vertico-previous
    "C-l" 'vertico-directory-enter
    ;; "C-h" 'vertico-directory-enter
    )
  :config
  (setq vertico-scroll-margin 4
        vertico-count 20
        vertico-resize nil
        vertico-cycle t))

;; FIXME: this is so terrible..
(add-to-list 'load-path (expand-file-name "extensions/" bergheim/config-dir ))
(require 'vertico-directory)
(use-package vertico-directory
  :ensure nil
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
  :ensure t
  :init
  (marginalia-mode))

;; COmpletion in Region FUnction (code completion)
(use-package corfu
  :ensure t
  :init
  ;; (setq corfu-max-width 150)
  ;; (setq corfu-max-height 35)
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("M-n" . nil)
        ("M-p" . nil))
  :custom
  (corfu-auto t) ;; enable auto completion
  ;; (corfu-auto-delay 0)
  ;; (corfu-auto-prefix 0)
  ;; (completion-styles '(basic))
  )

;; Part of corfu
(use-package corfu-popupinfo
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
  :ensure t
  :config
  (corfu-terminal-mode))

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless))
  ;; (setq completion-styles '(orderless basic))
  ;; ;; (setq orderless-matching-styles '(orderless-initialism)))
  ;; (setq orderless-matching-styles '(orderless-regexp orderless-literal orderless-initialism))
  )

;; TODO: use this? I want MRU and then alphanumeric sorting
;; (use-package vertico-prescient
;;   :ensure t
;;   :after vertico
;;   :config
;;   (vertico-prescient-mode))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (setq kind-icon-default-style
        '(:padding -1 :stroke 0 :margin 0 :radius 0 :height 0.5 :scale 1.0))
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(use-package tempel
  :ensure t
  :custom
  ;; Require trigger prefix before template name when completing.
  (tempel-trigger-prefix "!")
  :bind (("C-c t" . tempel-expand))
  :config
  (with-eval-after-load 'tempel
    (define-key tempel-map (kbd "M-n") 'tempel-next)
    (define-key tempel-map (kbd "M-p") 'tempel-previous))
  :init
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
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package tempel-collection
  :after tempel
  :ensure t)

;; `~' returns home
(defun bergheim/find-file-recognize-home (orig-fun &rest args)
  (let ((input (car args)))
    (if (and input (string-equal input "~/~"))
        (apply orig-fun "~" (cdr args))
      (apply orig-fun args))))

(advice-add 'find-file :around #'bergheim/find-file-recognize-home)
(provide 'completion)
;;; completion.el ends here
