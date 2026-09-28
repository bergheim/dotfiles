;;; babel.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Thomas Bergheim

;; Declare languages without loading them: plain `setq' skips the defcustom
;; :set, so no ob-* file is required at startup. The list still feeds the
;; language prompts in commands.el.
(setq org-babel-load-languages
      '((emacs-lisp . t)
        (C . t)
        (calc . t)
        (shell . t)
        (sql . t)
        (js . t)
        (go . t)
        (rust . t)
        (python . t)
        (ruby . t)
        (elixir . t)
        (typescript . t)
        (verb . t)))

(defun bergheim/org-babel-load-languages-once (&rest _)
  "Load every language in `org-babel-load-languages' on first execution."
  (advice-remove 'org-babel-execute-src-block #'bergheim/org-babel-load-languages-once)
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(advice-add 'org-babel-execute-src-block :before #'bergheim/org-babel-load-languages-once)

(setq python-indent-offset 4
      org-babel-default-header-args:python '((:results . "output"))
      org-babel-default-header-args:ruby '((:results . "output")))

;; ob-typescript expects typescript-mode; support tsx as well
(unless (fboundp 'typescript-mode)
  (defalias 'typescript-mode 'typescript-ts-mode))
(add-to-list 'org-src-lang-modes '("tsx" . typescript))
(defalias 'org-babel-execute:tsx 'org-babel-execute:typescript)

;; Installed only; loaded by the advice above.
(use-package ob-go)
(use-package ob-rust)
(use-package ob-elixir)
(use-package ob-typescript)
(use-package verb)

;; (provide 'langs)
