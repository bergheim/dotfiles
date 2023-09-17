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
;(setq completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; For a fancier built-in completion option, try ido-mode or fido-mode. See also
;; the file extras/base.el
;(fido-vertical-mode)
;(setq icomplete-delay-completions-threshold 4000)

(use-package avy
  :ensure t
  :demand t
  :bind (("C-c j" . avy-goto-line)
         ("C-c u"   . avy-goto-char-timer)))

;; right click from your keyboard
(use-package embark
  :ensure t
  :demand t
  :after avy
  :bind (("C-c a" . embark-act))
  :bind (("C-c b" . embark-bindings))
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

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))

(use-package embark-consult
  :ensure t)

;; lots of more filtering options for completing-read
(use-package consult
  :ensure t
  ;; Other good things to bind: consult-ripgrep, consult-line-multi,
  ;; consult-history, consult-outline
  :bind (("C-x b" . consult-buffer) ; orig. switch-to-buffer
         ("M-y" . consult-yank-pop) ; orig. yank-pop
         ("C-s" . consult-line))    ; orig. isearch
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

;; Minibuffer completion
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode))

;; lets try this. also look at buffer etc - https://github.com/minad/vertico/blob/main/extensions/
(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

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
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous))
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
  (setq completion-styles '(orderless)))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package eshell
  :bind (("C-r" . consult-history)))

(provide 'completion)
;;; completion.el ends here