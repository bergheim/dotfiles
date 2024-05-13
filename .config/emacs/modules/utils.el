;;; bergheim-utils.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

;; TODO: rename this to bootstrap and move the others here out
(defun bergheim/toggle-scratch-buffer ()
  "Toggle the *scratch* buffer: show, hide, or switch to it based on its current state."
  (interactive)
  (let ((scratch-buffer (get-buffer-create "*scratch*")))
    (cond
     ;; hide if active
     ((eq (current-buffer) scratch-buffer)
      (bury-buffer))
     ;; switch to it if visible
     ((get-buffer-window scratch-buffer t)
      (pop-to-buffer scratch-buffer))
     ;; else open in current
     (t
      (switch-to-buffer scratch-buffer)))))

(defun bergheim/open-dirvish-current-project ()
  "Open dirvish in the root directory of the current project."
  (interactive)
  (let ((project-root (project-root (project-current t))))
    (if project-root
        (dirvish project-root)
      (message "No project found!"))))

(defun bergheim/copy-current-buffer-file ()
  "Copy the current buffer's file to a specified location."
  (interactive)
  (if buffer-file-name
      (let ((destination (read-file-name "Copy to: ")))
        (copy-file buffer-file-name destination t)
        (find-file destination)
        (message "File copied to: %s" destination))
    (message "No file is associated with this buffer.")))

(defun bergheim/next-file ()
  "Switch to the next file in the current directory."
  (interactive)
  (bergheim/next-in-directory 1))

(defun bergheim/prev-file ()
  "Switch to the previous file in the current directory."
  (interactive)
  (bergheim/next-in-directory -1))

(defun bergheim/next-in-directory (arg)
  "Move to the next or previous file in the current directory (and wrap around)."
  (let* ((current-file (buffer-file-name))
         (all-entries (directory-files (file-name-directory current-file) t))
         (files (seq-filter #'file-regular-p all-entries)) ; Filter out directories
         (position (cl-position current-file files :test 'string=))
         (num-files (length files))
         (next-pos (mod (+ position arg) num-files))) ; Wrap around using `mod`
    (find-file (nth next-pos files))))

(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :config
  (setq jinx-completion-method 'vertico))

(defvar bergheim/jinx-languages
  '("en_US" "nb" "fr_FR" "de_DE"))

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

;; (use-package unicode-fonts
;;   :ensure t
;;   :config
;;   (unicode-fonts-setup))

(use-package expand-region
  :ensure t
  :defer t
  :bind ("M-e" . er/expand-region))

;; .csv parser
(use-package pcsv)

(use-package transient)

(defun bergheim/call-with-universal-arg (fn)
  (lambda ()
    (interactive)
    (let ((current-prefix-arg 4))
      (call-interactively fn))))

(use-package emacs-everywhere
  :config
  ;; I always want this to open centered, not where my cursor might happen to be
  (setq emacs-everywhere-init-hooks
        (delq 'emacs-everywhere-set-frame-position emacs-everywhere-init-hooks))
  :custom
  (emacs-everywhere-frame-parameters
   '((name . "floating emacs-everywhere")
     (width . 80)
     (height . 30))))

(use-package iedit
  :config
  (general-define-key
   :states '(normal visual)
(use-package iedit
  :demand t
  :general
  (:states '(normal visual)
   "gR" 'iedit-mode))
;; redefines the silly indent of keyword lists
;; before
;;   (:foo bar
;;         :baz qux)
;; after
;;   (:foo bar
;;    :baz qux)
(eval-after-load "lisp-mode"
  '(defun lisp-indent-function (indent-point state)
     "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
     (let ((normal-indent (current-column))
           (orig-point (point)))
       (goto-char (1+ (elt state 1)))
       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
       (cond
        ;; car of form doesn't seem to be a symbol, or is a keyword
        ((and (elt state 2)
              (or (not (looking-at "\\sw\\|\\s_"))
                  (looking-at ":")))
         (if (not (> (save-excursion (forward-line 1) (point))
                     calculate-lisp-indent-last-sexp))
             (progn (goto-char calculate-lisp-indent-last-sexp)
                    (beginning-of-line)
                    (parse-partial-sexp (point)
                                        calculate-lisp-indent-last-sexp 0 t)))
         ;; Indent under the list or under the first sexp on the same
         ;; line as calculate-lisp-indent-last-sexp.  Note that first
         ;; thing on that line has to be complete sexp since we are
         ;; inside the innermost containing sexp.
         (backward-prefix-chars)
         (current-column))
        ((and (save-excursion
                (goto-char indent-point)
                (skip-syntax-forward " ")
                (not (looking-at ":")))
              (save-excursion
                (goto-char orig-point)
                (looking-at ":")))
         (save-excursion
           (goto-char (+ 2 (elt state 1)))
           (current-column)))
        (t
         (let ((function (buffer-substring (point)
                                           (progn (forward-sexp 1) (point))))
               method)
           (setq method (or (function-get (intern-soft function)
                                          'lisp-indent-function)
                            (get (intern-soft function) 'lisp-indent-hook)))
           (cond ((or (eq method 'defun)
                      (and (null method)
                           (> (length function) 3)
                           (string-match "\\`def" function)))
                  (lisp-indent-defform state indent-point))
                 ((integerp method)
                  (lisp-indent-specform method state
                                        indent-point normal-indent))
                 (method
                  (funcall method indent-point state)))))))))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local lisp-indent-function #'lisp-indent-function)))


(provide 'bergheim-utils)
;;; bergheim-utils.el ends here
