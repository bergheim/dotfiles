;;; bergheim-writing.el --- Spell-check, thesaurus, prose tooling -*- lexical-binding: t; -*-

(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :config
  (setq jinx-completion-method 'vertico
        jinx-languages "en_US nb_NO"))

(defvar bergheim/jinx-languages
  '("en_US" "nb_NO" "fr_FR" "de_DE"))

(defun bergheim/jinx-language-sort (cands)
  (let ((langs (seq-intersection cands bergheim/jinx-languages)))
    (vertico-sort-history-alpha langs)))

(defun jinx--add-to-abbrev (overlay word)
  "Add abbreviation to `global-abbrev-table'.
The misspelled word is taken from OVERLAY.  WORD is the corrected word."
  (let ((abbrev (buffer-substring-no-properties
                 (overlay-start overlay)
                 (overlay-end overlay))))
    (message "Abbrev: %s -> %s" abbrev word)
    (define-abbrev global-abbrev-table abbrev word)))

(advice-add 'jinx--correct-replace :before #'jinx--add-to-abbrev)

(use-package powerthesaurus
  :after embark
  :general
  (bergheim/global-menu-keys
    "st" '(powerthesaurus-lookup-dwim :which-key "Search thesaurus"))
  :bind
  (:map embark-general-map
   ("D" . bergheim/embark-powerthesaurus))
  :config
  (defun bergheim/embark-powerthesaurus ()
    "Use Powerthesaurus to find synonyms for the word at point."
    (interactive)
    (let ((word (thing-at-point 'word t)))
      (if word
          (powerthesaurus-lookup word :definitions)
        (message "No word at point")))))

;;; bergheim-writing.el ends here
