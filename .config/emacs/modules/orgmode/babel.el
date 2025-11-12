(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (calc . t)
   (shell . t)
   (sql . t)
   (js . t)))

(use-package ob-go
  :demand
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((go . t)))))

(use-package ob-rust
  :demand
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((rust . t)))))

(use-package ob-python
  :ensure nil
  :demand
  :after org
  :commands (org-babel-execute:python)
  :custom
  (python-indent-offset 4)
  :config
  (setq org-babel-default-header-args:python
        '((:results . "output")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((python . t)))))

(use-package ob-ruby
  :ensure nil
  :demand
  :after org
  :commands (org-babel-execute:ruby)
  :config
  (setq org-babel-default-header-args:ruby
        '((:results . "output")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((ruby . t)))))

(use-package ob-elixir
  :demand
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((elixir . t)))))

(use-package ob-typescript
  :after org
  :demand t
  :config
  ;; Assuming ob-typescript expects typescript-mode, we might need to
  ;; temporarily alias typescript-ts-mode to typescript-mode.
  (unless (fboundp 'typescript-mode)
    (defalias 'typescript-mode 'typescript-ts-mode))

  ;; support tsx as well
  (add-to-list 'org-src-lang-modes '("tsx" . typescript))
  (defalias 'org-babel-execute:tsx 'org-babel-execute:typescript)

  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((typescript . t)))))

;; (provide 'langs)
