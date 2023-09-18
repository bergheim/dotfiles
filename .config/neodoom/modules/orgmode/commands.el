;;; commands.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim


;; TODO: is this used?
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

(defun bergheim/org--open-attachments ()
  "Open an attachment of the current outline node using xdg-open"
  (interactive)
  (let ((attach-dir (concat org-attach-id-dir (bergheim/~id-get-or-generate))))
    ;; TODO with universal argument, make dir
    (if attach-dir
        (if IS-MAC
            (call-process "open" nil 0 nil attach-dir)
          (browse-url-xdg-open attach-dir))
      (error "No attachment directory exist"))))

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
  (message (format "arg %s" current-prefix-arg))
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
        (let ((org-attach-method attach-method))
          (dolist (file files)
            (org-attach-attach file)))))
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

;;; commands.el ends here
