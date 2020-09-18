;;; ~/.config/doom/+org.el -*- lexical-binding: t; -*-

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; (setq org-directory "~/org/")

(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
