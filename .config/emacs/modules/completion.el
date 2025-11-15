;;; completion.el --- Description -*- lexical-binding: t; -*-

(defun bergheim/disable-pgtk-im (&rest _args)
  ;; this enables S-SPC again, at the expense of input methods
  ;; see https://lists.gnu.org/archive/html/bug-gnu-emacs/2021-07/msg00071.html
  (when (fboundp 'pgtk-use-im-context)
    (pgtk-use-im-context nil)))

(use-package emacs
  :ensure nil
  :config
  (add-hook 'after-make-frame-functions #'bergheim/disable-pgtk-im)
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

;; right click from your keyboard
(use-package embark
  :demand t
  :bind (("C-c a" . embark-act)
         ("C-c A" . embark-act-all)
         ("C-c d" . embark-dwim)
         ("C-c e" . embark-export)
         ("C-'" . embark-act)
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

  ;; TODO: make a selection take precedence..? that could support spaces..
  (defun bergheim/embark-open-with (file)
    "Open the current file in 'dired-mode' with an application of your choosing."
    (interactive "sOpen externally: ")
    (unless (string-match-p "\\`[a-z]+://" file)
      (setq file (expand-file-name file)))
    (when-let ((command (completing-read "Open current file with: "
                                         (bergheim//executables-in-path))))
      (start-process command nil command file)))

  ;; TODO make buffers optional then bind this to "C-c g" or something to be
  ;; called directly from the minibuffer
  (defun bergheim/grep-selected-buffers (buffers)
    "Swoop across the selected BUFFERS"
    (interactive)
    (consult-line-multi (list :include buffers)))

  :bind
  (:map embark-general-map
   ("g" . #'bergheim/grep-selected-buffers))
  :config
  (with-eval-after-load 'avy
    ;; why not call embark on avy targets
    (defun avy-action-embark (pt)
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0))))
      t)
    (setf (alist-get ?' avy-dispatch-alist) 'avy-action-embark))

  ;; force C-' in org-mode as well
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-'") #'embark-act))
  (when (featurep 'evil)
    (evil-collection-embark-setup))

  ;; there are so many commands in embark one might as well have just added all of M-x..
  (defvar my-embark-file-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map embark-file-map)
      (define-key map "b" nil)  ; remove byte-compile file
      (define-key map "R" nil)  ; remove byte-compile dir
      (define-key map "m" nil)  ; chmod
      (define-key map "\\" nil)  ; remove recent removal
      (define-key map "\+" nil)  ; remove make dir
      (define-key map "s" nil)  ; make symlink
      map))
  ;; I give up..
  (add-to-list 'embark-keymap-alist '(file . my-embark-file-map))

  (setq embark-confirm-act-all nil)
  (add-to-list 'embark-multitarget-actions #'bergheim/grep-selected-buffers)
  (define-key embark-buffer-map (kbd "g") #'bergheim/grep-selected-buffers)
  (define-key embark-file-map (kbd "X") #'bergheim/embark-open-with))

(use-package embark-consult
  :after (embark consult)
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; lots of more filtering options for completing-read
(use-package consult
  :demand
  ;; Other good things to bind: consult-ripgrep, consult-line-multi,
  ;; consult-history, consult-outline
  :bind (("M-p" . consult-yank-pop) ; yes yes. I am an evil heretic
         ("C-s" . consult-line)     ; orig. isearch
         :map minibuffer-local-map
         ("M-s" . consult-history)  ;; orig. next-matching-history-element
         ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (defun consult-toggle-preview ()
    "Previews candidate in vertico buffer, unless it's a consult command"
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim)))))
  :config
  (setopt consult-fd-args '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
                            "--full-path --color=never --hidden"))
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<")
  (setq consult-preview-key 'any)
  (setq consult-buffer-list-function #'consult--frame-buffer-list)
  (with-eval-after-load 'consult-xref
    ;; nicked from https://github.com/minad/consult/issues/1015#issuecomment-2107283203
    ;; TODO remove after move to emacs 30
    (el-patch-defun consult-xref--candidates ()
      "Return xref candidate list."
      (let ((root (consult--project-root)))
        (mapcar (lambda (xref)
                  (let* ((loc (xref-item-location xref))
                         (group (el-patch-swap (if (fboundp 'xref--group-name-for-display) ;;  <----- patch here
                                                   ;; This function is available in xref 1.3.2
                                                   (xref--group-name-for-display
                                                    (xref-location-group loc) root)
                                                 (xref-location-group loc))
                                               (xref-location-group loc)))
                         (cand (consult--format-file-line-match
                                group
                                (or (xref-location-line loc) 0)
                                (xref-item-summary xref))))
                    (add-text-properties
                     0 1 `(consult-xref ,xref consult--prefix-group ,group) cand)
                    cand))
                (funcall consult-xref--fetcher)))))

  :general
  (:keymaps 'vertico-map
   "C-SPC" #'consult-toggle-preview))

(use-package consult-todo :after consult)

(use-package consult-recoll
  :after consult
  :general
  (bergheim/global-menu-keys
    "sR" '(consult-recoll :which-key "Recoll"))
  :custom
  (consult-recoll-group-by-mime t)
  (consult-recoll-embark-setup)
  :config
  (defun bergheim/open-with-mu4e (file &optional _page)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (if (re-search-forward "^Message-Id: <\\([^>]+\\)>$" nil t)
          (progn
            (message "Extracted Message-Id: %s" (match-string 1))
            (mu4e-view-message-with-message-id (match-string 1)))
        (message "Error: Message-Id not found in %s" file))))

  (add-to-list 'consult-recoll-open-fns '("message/rfc822" . bergheim/open-with-mu4e)))


(defun bergheim/consult-ripgrep-with-selection (&optional dir)
  "Run `consult-ripgrep' with the current visual selection as the initial input.
If called interactively with a prefix argument, prompt for DIR, otherwise use the default behavior of `consult-ripgrep`."
  (interactive
   (list (when current-prefix-arg
           (read-directory-name "Search Directory: "))))
  (let ((initial (when (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end)))))
    (consult-ripgrep (or dir (consult--project-root)) initial)))

(defun bergheim/consult-ripgrep-with-selection-current-dir ()
  "Run `consult-ripgrep' with the current visual selection as the initial input from current directory."
  (interactive)
  (bergheim/consult-ripgrep-with-selection "."))

(defun bergheim/consult-ripgrep-with-selection-other-dir ()
  "Invoke `bergheim/consult-ripgrep-with-selection` with a chosen directory."
  (interactive)
  (let ((dir (read-directory-name "Search Directory: ")))
    (bergheim/consult-ripgrep-with-selection dir)))

(defun bergheim/consult-fd-other-dir ()
  "Invoke `bergheim/consult-ripgrep-with-selection` with a chosen directory."
  (interactive)
  (let ((dir (read-directory-name "Search Directory: ")))
    (consult-fd dir)))

(defun bergheim/switch-to-relevant-buffer ()
  "Generate a buffer list that you can switch to based on the context."

  (interactive)
  (cond
   ((project-current)
    (consult-project-buffer))
   ((and (fboundp 'activities-switch-buffer) (activities-current))
    (call-interactively 'activities-switch-buffer))
   ((fboundp 'beframe-switch-buffer)
    (call-interactively 'beframe-switch-buffer))
   (t
    (consult-buffer))))

(use-package xref
  :ensure nil
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(defun sort-directories-first (files)
  (setq files (vertico-sort-history-alpha files))
  (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
         (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

;; Minibuffer completion
(use-package vertico
  :demand
  :ensure
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion regex flex))
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
    "C-l" 'vertico-insert
    "C-," 'embark-select
    "C-M-j" 'vertico-next-group
    "C-M-k" 'vertico-previous-group
    "C-S-H" 'helpful-key)

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

;; these look nice and all but I dunno..
;; (use-package vertico-posframe
;;   :ensure t
;;   :after vertico
;;   :config
;;   (setq vertico-posframe-parameters
;;         '((left-fringe . 32)
;;           (top-fringe . 32)
;;           (bottom-fringe . 32)
;;           (right-fringe . 32)))
;;   (vertico-posframe-mode 1))

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
  :init
  (marginalia-mode))

;; COmpletion in Region FUnction (code completion)
(use-package corfu
  :init
  ;; (setq corfu-max-width 150)
  ;; (setq corfu-max-height 35)
  (global-corfu-mode)
  :bind ("C-c p" . cape-prefix-map)
  (:map corfu-map
   ("S-SPC" . corfu-insert-separator)
   ("BACKTAB" . corfu-previous)
   ([backtab] . corfu-previous)
   ("S-TAB" . corfu-previous)
   ("TAB" . corfu-next)
   ("C-n" . corfu-next)
   ("C-p" . corfu-previous)
   ("C-j" . corfu-next)
   ("C-k" . corfu-previous)
   ("M-n" . nil)
   ("M-p" . nil))
  :custom
  ;; use `cape-dict' instead
  (text-mode-ispell-word-completion nil)
  (corfu-auto t) ;; enable auto completion
  (corfu-cycle t)
  ;; if we have applied the separator, never quit
  ;; (corfu-quit-no-match 'separator)
  ;; TODO add a timer here or increase max chars
  (corfu-auto-prefix 3)) ;; min chars

(use-package corfu-popupinfo
  ;; Part of corfu
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :general
  (corfu-map
   "C-h" 'corfu-popupinfo-documentation)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

(use-package corfu-history
  :ensure nil
  :after corfu
  :hook
  (corfu-mode-hook . corfu-history-mode))

(use-package corfu-echo
  :ensure nil
  :after corfu
  :hook
  (corfu-mode-hook . corfu-echo-mode))

(use-package corfu-info
  :ensure nil
  :after corfu
  :unless (display-graphic-p)
  :after corfu
  :general
  (corfu-map
   "C-h" 'corfu-info-documentation))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (< emacs-major-version 31)
  :unless (featurep 'tty-child-frames)
  :unless (display-graphic-p)
  :hook
  (corfu-mode-hook . corfu-terminal-mode))

;; (use-package corfu-doc-terminal
;;   :after corfu-terminal
;;   :ensure (:host "https://codeberg.org/akib/emacs-corfu-doc-terminal.git"))

(use-package dabbrev
  :ensure nil
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :custom
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;; combine completion at point functions. if the name cape was not clear
(use-package cape
  :demand t
  :bind ("C-c p" . cape-prefix-map)
  :config
  ;; globally available CAPE completions (with lower priority)
  (add-hook 'completion-at-point-functions (cape-capf-super #'cape-dabbrev
                                                            #'cape-dict))
  (add-hook 'completion-at-point-functions #'cape-file)
  ;; (add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  (dolist (hook '(text-mode-hook markdown-mode-hook org-mode-hook erc-mode-hook))
    (add-hook hook
              (lambda ()
                (add-hook 'completion-at-point-functions #'cape-emoji 80 t))))

  (defun bergheim/org-mode-setup-corfu ()
    "Configure CAPFs for Org buffers."
    (when buffer-file-name
      (setq-local completion-at-point-functions
                  (list #'tempel-complete
                        #'cape-tex       ;; expands \
                        #'org-block-capf ;; expands <
                        #'cape-elisp-block
                        (cape-capf-super #'cape-dabbrev
                                         #'cape-dict
                                         #'cape-keyword)
                        #'cape-emoji))))   ;; expands :

  (add-hook 'org-mode-hook #'bergheim/org-mode-setup-corfu))

;; Orderless: powerful completion style
(use-package orderless
  :after vertico
  :config
  ;; from https://github.com/minad/consult/wiki
  (defun +orderless--consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if (boundp 'consult--tofu-regexp)
        (concat consult--tofu-regexp "*\\'")
      "\\'"))

  ;; Recognizes the following patterns:
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-consult-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--consult-suffix))))))

  (setq orderless-style-dispatchers (list #'+orderless-consult-dispatch
                                          #'orderless-kwd-dispatch
                                          #'orderless-affix-dispatch))
  :custom
  (completion-styles '(orderless basic))
  ;; this has a bunch of other things set up.. so just set everything from orderless
  (completion-category-defaults nil)
  (completion-category-overrides '(file (styles basic partial-completion)))
  ;; don't add rarely used things here, use dispatchers instead
  (orderless-matching-styles '(orderless-literal orderless-prefixes orderless-regexp)))

;; TODO: use this? I want MRU and then alphanumeric sorting
;; (use-package vertico-prescient
;;   :ensure t
;;   :after vertico
;;   :config
;;   (vertico-prescient-mode))

(use-package org-block-capf
  :ensure (:host github :repo "xenodium/org-block-capf"))

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
  :bind (("M-n" . tempel-complete)
         ("C-c t" . tempel-complete)
         :map tempel-map
         ("M-n" . tempel-next)
         ("M-p" . tempel-previous)
         ("C-l" . tempel-next)
         ("C-h" . tempel-previous)
         ("C-j" . tempel-next)
         ("C-k" . tempel-previous)
         ("<tab>" . tempel-next)
         ("<backtab>" . tempel-previous))
  :custom
  (tempel-path (concat bergheim/config-dir "templates/tempel/*"))
  :init

  (defun tempel-setup-capf ()
    ;; put this at the end and rely more on a trigger char
    (setq-local completion-at-point-functions
                (append (remove #'tempel-complete completion-at-point-functions)
                        (list #'tempel-complete)))
    (setq-local corfu-auto-trigger "/"
                completion-at-point-functions
                (cons (cape-capf-trigger #'tempel-complete ?/)
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

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
