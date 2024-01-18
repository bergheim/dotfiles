;;; style.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

(defun org-capture-select-template-prettier (&optional keys)
  "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
  (let ((org-capture-templates
         (or (org-contextualize-keys
              (org-capture-upgrade-templates org-capture-templates)
              org-capture-templates-contexts)
             '(("t" "Task" entry (file+headline "" "Tasks")
                "* TODO %?\n  %u\n  %a")))))
    (if keys
        (or (assoc keys org-capture-templates)
            (error "No capture template referred to by \"%s\" keys" keys))
      (org-mks org-capture-templates
               "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
               "Template key: "
               `(("q" ,(concat (nerd-icons-octicon "nf-oct-stop" :face `nerd-icons-red :v-adjust 0.01) "\tAbort")))))))

(advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

;; TODO: still needed?
;; indent items that span lines in the agenda. it's not perfect but it's better (divinedominion)
(defun ct/org-agenda-set-wrap-prefix ()
  (setq-local wrap-prefix "                          "))
(add-hook 'org-agenda-mode-hook #'ct/org-agenda-set-wrap-prefix)

;; indent by default
(add-hook 'org-mode-hook 'org-indent-mode)

;; (setq org-agenda-prefix-format
;;       (quote
;;        ((agenda . "%-12c%?-18t% s")
;;         (timeline . "% s")
;;         (todo . "%-20c")
;;         (tags . "%-22c")
;;         (search . "%-12c"))))


;; thanks tecosaur
(defun +doct-icon-declaration-to-icon (declaration)
  "Convert :icon declaration to icon"
  (let ((name (pop declaration))
        (set  (intern (concat "nerd-icons-" (plist-get declaration :set))))
        (face (intern (concat "nerd-icons-" (plist-get declaration :color))))
        (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
    (apply set `(,name :face ,face :v-adjust ,v-adjust))))

(defun +doct-iconify-capture-templates (groups)
  "Add declaration's :icon to each template group in GROUPS."
  (let ((templates (doct-flatten-lists-in groups)))
    (setq doct-templates
          (mapcar (lambda (template)
                    (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                (spec (plist-get (plist-get props :doct) :icon)))
                      (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                     "\t"
                                                     (nth 1 template))))
                    template)
                  templates))))

(setq doct-after-conversion-functions '(+doct-iconify-capture-templates))

;;; style.el ends here
