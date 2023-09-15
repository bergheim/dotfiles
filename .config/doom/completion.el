;;; completion.el --- All the completion stuff -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim
;;
;; Author: Thomas Bergheim
;; Maintainer: Thomas Bergheim
;; Created: April 12, 2023
;; Modified: April 12, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/bergheim/dotfiles
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  All the completion stuff
;;
;;; Code:

;; COmpletion in Region FUnction
(use-package! corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  ;; (setq corfu-auto-delay 0) ;; Happens also without this, but makes it easier to notice the issue
  ;; (setq corfu-auto-prefix 1) ;; Happens also without this, but makes it easier to notice the issue
  ;; (setq corfu-quit-no-match t) ;; Happens also without this, but makes it easier to notice the issue
  ;; show helpful docs after some idle time
  (corfu-popupinfo-mode)
  (global-corfu-mode))

(use-package! orderless
  :ensure t
  :config
  ;; (setq completion-styles '(orderless flex))
  ;; (setq completion-styles '(orderless basic))
  (setq completion-styles '(orderless basic))
  ;; (setq orderless-matching-styles '(orderless-initialism)))
  (setq orderless-matching-styles '(orderless-regexp orderless-literal orderless-initialism)))
  ;; (setq orderless-matching-styles '(orderless-flex orderless-regexp orderless-literal orderless-initialism)))
;; (setq company-search-regexp-function 'regexp-quote)

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)
;; Enable indentation+completion using the TAB key.
(setq tab-always-indent 'complete)

;; Add extensions
(use-package! cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("M-p p p" . completion-at-point) ;; capf
         ("M-p p t" . complete-tag)        ;; etags
         ("M-p p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-p p h" . cape-history)
         ("M-p p f" . cape-file)
         ("M-p p k" . cape-keyword)
         ("M-p p s" . cape-symbol)
         ("M-p p a" . cape-abbrev)
         ("M-p p l" . cape-line)
         ("M-p p w" . cape-dict)
         ("M-p p \\" . cape-tex)
         ("M-p p _" . cape-tex)
         ("M-p p ^" . cape-tex)
         ("M-p p &" . cape-sgml)
         ("M-p p r" . cape-rfc1345))

  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

;; Use Dabbrev with Corfu!
(use-package! dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))


(provide 'completion)
;;; completion.el ends here
