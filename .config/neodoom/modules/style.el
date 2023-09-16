;;; style.el --- Description -*- lexical-binding: t; -*-
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
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


;; Note: height = px * 100
(set-face-attribute 'default nil :font "Ubuntu Mono" :height 120)
(load-theme 'modus-vivendi t)
(show-paren-mode 1) ;; Visualize matching parens

(use-package ef-themes
  :ensure t
  :config
  (ef-themes-select 'ef-cyprus))

(provide 'style)
;;; style.el ends here
