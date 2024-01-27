;;; commands.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim


(defun bergheim/org-copy-url-only ()
  "Copy the URL (without description) of an Org-mode link under the cursor to the clipboard."
  (interactive)
  (let ((link (org-element-context)))
    (if (eq (org-element-type link) 'link)
        (let ((url (org-element-property :raw-link link)))
          (kill-new url)
          (message "URL copied: %s" url))
      (message "No link at point."))))

;; FIXME: this does not list items in the latest order
;; TODO: verify that, and add a shortcut
(defun bergheim/org-recent-tasks-in-buffer ()
  "Lists the recently worked on tasks for this buffer"
  (interactive)
  (let ((vertico-sort-function nil)
        (data (seq-uniq
               (mapcar
                (lambda (elem)
                  (cons (org-element-property :raw-value elem) elem))
                (org-ql-query
                  :select #'element-with-markers
                  ;; :from (org-agenda-files)
                  :from (buffer-file-name)
                  :where '(ts :from -31 :to today)
                  :order-by '(reverse date)))
               ;; only take the most recently clocked item
               (lambda (a b) (string-equal
                              (car a)
                              (car b))))))
    (org-goto-marker-or-bmk
     (org-element-property
      :org-marker
      (cdr
       (assoc
        (completing-read "TODO: " data)
        data
        #'string-equal))))))

(defun bergheim/org-mru-goto ()
  (interactive)
  (bergheim/vertico--without-sorting 'org-mru-clock-goto (org-mru-clock--completing-read)))

(defun bergheim/org-mru-clock-in ()
  (interactive)
  (bergheim/vertico--without-sorting 'org-mru-clock-in))

(defun bergheim/org-open-attachments ()
  "Open an attachment of the current outline node using xdg-open"
  (interactive)
  (if (member "ATTACH" (org-get-tags))
      (let ((attach-dir (expand-file-name (concat org-attach-id-dir (bergheim/~id-get-or-generate)))))
        ;; TODO with universal argument, make dir
        (message "Attach me %s" attach-dir)
        (if attach-dir
            (if IS-MAC
                (call-process "open" nil 0 nil attach-dir)
              (browse-url-xdg-open attach-dir))
          (error "No attachment directory exist")))
    (message "No attachments enabled")))

;; Idea taken from org-attach-dired-to-subtree
(cl-defun bergheim/org-attach-dired-to-subtree (files &optional (attach-method 'cp))
  "Attach FILES to current Org heading.

Attaches to heading at point in most recently selected Org buffer
using ATTACH-METHOD (interactively, with prefix, move, otherwise
copy).  Interactively, FILES is the file at point or all marked
files.  To be called in a `dired' buffer, or one in a major mode
derived from `dired-mode'."
  (interactive
   (list (dired-get-marked-files) (when current-prefix-arg 'mv)))
  (unless (derived-mode-p 'dired-mode)
    (user-error "This command must be triggered in a `dired-derived' buffer"))
  (let ((dirvish-buffer (current-buffer))
        (current-org-heading)
        ;; `buffer-list' is MRU-ordered, so pick the first Org buffer we find.
        (org-node-target
         (or (cl-loop for buffer in (buffer-list)
                      when (eq 'org-mode (buffer-local-value 'major-mode buffer))
                      return buffer)
             (user-error "Can't attach to subtree.  No window displaying an Org buffer"))))
    (with-current-buffer org-node-target
      (setq current-org-heading (substring-no-properties (org-get-heading)))
      (when (yes-or-no-p (format "%s selected files to %S?"
                                 (if (eq 'mv attach-method)
                                     "Move"
                                   "Copy")
                                 current-org-heading))
        (dolist (file files)
          (org-attach-attach file nil attach-method))))
    (when (eq 'mv attach-method)
      ;; Revert the Dired buffer to show that the file is moved
      (revert-buffer)
      (message (format "Files moved to %S" current-org-heading)))))

;; FIXME: this goes past "today"
(defun bergheim/org-agenda-recent-changes (&optional tags)
  (interactive)
  (let (filter from)
    (if current-prefix-arg
        (setq from (org-read-date nil))
      (setq from -7))
    (when tags
      (setq filter `(:discard (:not (:tag ,tags)))))
    (org-ql-search (org-agenda-files)
      `(ts :from ,from :to today)
      :title "Recent Items"
      :super-groups `(,filter
                      (:name "Work" :auto-bergheim/clocked-or-created t)
                      (:discard (:anything t))))))

;; TODO maybe just bin this
(defun bergheim/org-agenda-new-items ()
  (interactive)
  (org-ql-search (org-agenda-files)
    '(ts :from -7 :to today)
    :title "Recent Items"
    :sort '(todo priority date)
    :where '(and (property "CREATED")
                 ;;            (string-match "issue" (org-entry-get (point) "CUSTOM_ID"))))
                 :super-groups '((:auto-ts t)))))

(defun bergheim/org-agenda-work-items ()
  (interactive)
  (org-ql-search (org-agenda-files)
    '(not (done))
    :title "Work related tasks"
    :super-groups '((:name "Important tasks"
                     :discard (:not (:tag ("@work" "planet9")))
                     :priority ("A" "B"))
                    (:name "Needs refiling"
                     :tag "REFILE"
                     :order 7)
                    (:name "Habits"
                     :habit t
                     :order 3)
                    (:todo "WAITING"
                     :order 6)
                    (:priority "A" :order 1)
                    (:priority "B" :order 2)
                    (:priority "C" :order 2))))

;; TODO learn this
(defun bergheim/org-agenda-mark-done-and-add-followup ()
  "Mark the current TODO as done and add another task after it.
       Creates it at the same level as the previous task, so it's better to use
       this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-todo "DONE")
  (org-agenda-switch-to)
  (if (member "@work" (org-get-tags))
      (org-capture 0 "wtt")
    (org-capture 0 "ptt")))

(defun bergheim/org-agenda-toggle-work ()
  (interactive)
  (pcase (get 'work 'state)
    ('show-work (progn (org-agenda-redo)
                       (org-agenda-filter-apply '("+@work") 'tag)
                       (put 'work 'state 'show-private)))
    ('show-private (progn (org-agenda-redo)
                          (org-agenda-filter-apply '("-@work") 'tag)
                          (put 'work 'state 'show-everything)))
    (_ (progn (org-agenda-redo)
              (org-agenda-filter-apply '() 'tag)
              (put 'work 'state 'show-work)))))

(defun bergheim/org-move-up-header ()
  "Go to the root of the current heading if not at the beginning.
   If at the beginning, move up one level or if not, up one sibling."
  (interactive)
  (condition-case nil
      (org-up-element)
    (error (org-previous-visible-heading 1))))

(defun bergheim/org-move-down-header ()
  "Move down to the next subheading if available, otherwise to the next sibling."
  (interactive)
  (condition-case nil
      (org-down-element)
    (error (org-next-visible-heading 1))))


;; stolen straight from doom
(defun +org/dwim-at-point (&optional arg)
  "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- citation: follow it
- headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
  subtree; update statistics cookies/checkboxes and ToCs.
- clock: update its time.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- timestamp: open an agenda view for the time-stamp date/range at point.
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- src block: execute it
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive "P")
  (if (button-at (point))
      (call-interactively #'push-button)
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        ((or `citation `citation-reference)
         (org-cite-follow context arg))

        (`headline
         (cond ((memq (bound-and-true-p org-goto-map)
                      (current-active-maps))
                (org-goto-ret))
               ((and (fboundp 'toc-org-insert-toc)
                     (member "TOC" (org-get-tags)))
                (toc-org-insert-toc)
                (message "Updating table of contents"))
               ((string= "ARCHIVE" (car-safe (org-get-tags)))
                (org-force-cycle-archived))
               ((or (org-element-property :todo-type context)
                    (org-element-property :scheduled context))
                (org-todo
                 (if (eq (org-element-property :todo-type context) 'done)
                     (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                         'todo)
                   'done))))
         ;; Update any metadata or inline previews in this subtree
         (org-update-checkbox-count)
         (org-update-parent-todo-statistics)
         (when (and (fboundp 'toc-org-insert-toc)
                    (member "TOC" (org-get-tags)))
           (toc-org-insert-toc)
           (message "Updating table of contents"))
         (let* ((beg (if (org-before-first-heading-p)
                         (line-beginning-position)
                       (save-excursion (org-back-to-heading) (point))))
                (end (if (org-before-first-heading-p)
                         (line-end-position)
                       (save-excursion (org-end-of-subtree) (point))))
                (overlays (ignore-errors (overlays-in beg end)))
                (latex-overlays
                 (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                             overlays))
                (image-overlays
                 (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                             overlays)))
           (+org--toggle-inline-images-in-subtree beg end)
           (if (or image-overlays latex-overlays)
               (org-clear-latex-preview beg end)
             (org--latex-preview-region beg end))))

        (`clock (org-clock-update-time-maybe))

        (`footnote-reference
         (org-footnote-goto-definition (org-element-property :label context)))

        (`footnote-definition
         (org-footnote-goto-previous-reference (org-element-property :label context)))

        ((or `planning `timestamp)
         (org-follow-timestamp-link))

        ((or `table `table-row)
         (if (org-at-TBLFM-p)
             (org-table-calc-current-TBLFM)
           (ignore-errors
             (save-excursion
               (goto-char (org-element-property :contents-begin context))
               (org-call-with-arg 'org-table-recalculate (or arg t))))))

        (`table-cell
         (org-table-blank-field)
         (org-table-recalculate arg)
         (when (and (string-empty-p (string-trim (org-table-get-field)))
                    (bound-and-true-p evil-local-mode))
           (evil-change-state 'insert)))

        (`babel-call
         (org-babel-lob-execute-maybe))

        (`statistics-cookie
         (save-excursion (org-update-statistics-cookies arg)))

        ((or `src-block `inline-src-block)
         (org-babel-execute-src-block arg))

        ((or `latex-fragment `latex-environment)
         (org-latex-preview arg))

        (`link
         (let* ((lineage (org-element-lineage context '(link) t))
                (path (org-element-property :path lineage)))
           (if (or (equal (org-element-property :type lineage) "img")
                   (and path (image-type-from-file-name path)))
               (+org--toggle-inline-images-in-subtree
                (org-element-property :begin lineage)
                (org-element-property :end lineage))
             (org-open-at-point arg))))

        (`paragraph
         (+org--toggle-inline-images-in-subtree))

        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
           (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

        (_
         (if (or (org-in-regexp org-ts-regexp-both nil t)
                 (org-in-regexp org-tsr-regexp-both nil  t)
                 (org-in-regexp org-link-any-re nil t))
             (call-interactively #'org-open-at-point)
           (+org--toggle-inline-images-in-subtree
            (org-element-property :begin context)
            (org-element-property :end context))))))))

;;; commands.el ends here
