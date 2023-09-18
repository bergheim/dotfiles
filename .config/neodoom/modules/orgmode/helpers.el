;;; helpers.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim

;; TODO: should this be interactive? if so, rename
(defun bergheim/~id-get-or-generate()
  "Returns the ID property if set or generates and returns a new one if not set.
 The generated ID is stripped off potential progress indicator cookies and
 sanitized to get a slug. Furthermore, it is prepended with an ISO date-stamp
 if none was found before."
  (interactive)
  (when (not (org-id-get))
    (progn
      (let* (
             (my-heading-text (nth 4 (org-heading-components)));; retrieve heading string
             (my-heading-text (replace-regexp-in-string "\\(\\[[0-9]+%\\]\\)" "" my-heading-text));; remove progress indicators like "[25%]"
             (my-heading-text (replace-regexp-in-string "\\(\\[[0-9]+/[0-9]+\\]\\)" "" my-heading-text));; remove progress indicators like "[2/7]"
             (my-heading-text (replace-regexp-in-string "\\(\\[#[ABC]\\]\\)" "" my-heading-text));; remove priority indicators like "[#A]"
             (my-heading-text (replace-regexp-in-string "\\[\\[\\(.+?\\)\\]\\[" "" my-heading-text t));; removes links, keeps their description and ending brackets
             ;;                      (my-heading-text (replace-regexp-in-string "[<\\[][12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)[>\\]]" "" my-heading-text t));; removes day of week and time from date- and time-stamps (doesn't work somehow)
             (my-heading-text (replace-regexp-in-string "<[12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)>" "" my-heading-text t));; removes day of week and time from active date- and time-stamps
             (my-heading-text (replace-regexp-in-string "\\[[12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)\\]" "" my-heading-text t));; removes day of week and time from inactive date- and time-stamps
             (new-id (bergheim/~generate-sanitized-alnum-dash-string my-heading-text));; get slug from heading text
             (my-created-property (assoc "CREATED" (org-entry-properties))) ;; nil or content of CREATED time-stamp
             )
        (when (not (string-match "[12][0-9][0-9][0-9]-[01][0-9]-[0123][0-9]-.+" new-id))
          ;; only if no ISO date-stamp is found at the beginning of the new id:
          (if my-created-property (progn
                                    ;; prefer date-stamp of CREATED property (if found):
                                    (setq my-created-datestamp (substring (org-entry-get nil "CREATED" nil) 1 11)) ;; returns "2021-12-16" or nil (if no CREATED property)
                                    (setq new-id (concat my-created-datestamp "-" new-id))
                                    )
            ;; use today's date-stamp if no CREATED property is found:
            (setq new-id (concat (format-time-string "%Y-%m-%d-") new-id))))
        (org-set-property "ID" new-id)
        )
      )
    )
  (kill-new (concat "id:" (org-id-get)));; put ID in kill-ring
  (org-id-get);; retrieve the current ID in any case as return value
  )

;; this will create unique ids that are easy to read as well. works great.
;; nicked from https://github.com/novoid/dot-emacs/blob/master/config.org
(defun bergheim/~generate-sanitized-alnum-dash-string(str)
  "Returns a string which contains only a-zA-Z0-9 with single dashes
 replacing all other characters in-between them.

 Some parts were copied and adapted from org-hugo-slug
 from https://github.com/kaushalmodi/ox-hugo (GPLv3)."
  (let* (;; Remove "<FOO>..</FOO>" HTML tags if present.
         (str (replace-regexp-in-string "<\\(?1:[a-z]+\\)[^>]*>.*</\\1>" "" str))
         ;; Remove URLs if present in the string.  The ")" in the
         ;; below regexp is the closing parenthesis of a Markdown
         ;; link: [Desc](Link).
         (str (replace-regexp-in-string (concat "\\](" ffap-url-regexp "[^)]+)") "]" str))
         ;; Replace "&" with " and ", "." with " dot ", "+" with
         ;; " plus ".
         (str (replace-regexp-in-string
               "&" " and "
               (replace-regexp-in-string
                "\\." " dot "
                (replace-regexp-in-string
                 "\\+" " plus " str))))
         (str (replace-regexp-in-string "æ" "ae" str nil))
         (str (replace-regexp-in-string "ø" "ue" str nil))
         (str (replace-regexp-in-string "å" "oe" str nil))
         ;; Replace all characters except alphabets, numbers and
         ;; parentheses with spaces.
         (str (replace-regexp-in-string "[^[:alnum:]()]" " " str))
         ;; Remove leading and trailing whitespace.
         (str (replace-regexp-in-string "\\(^[[:space:]]*\\|[[:space:]]*$\\)" "" str))
         ;; Replace 2 or more spaces with a single space.
         (str (replace-regexp-in-string "[[:space:]]\\{2,\\}" " " str))
         ;; Replace parentheses with double-hyphens.
         (str (replace-regexp-in-string "\\s-*([[:space:]]*\\([^)]+?\\)[[:space:]]*)\\s-*" " -\\1- " str))
         ;; Remove any remaining parentheses character.
         (str (replace-regexp-in-string "[()]" "" str))
         ;; Replace spaces with hyphens.
         (str (replace-regexp-in-string " " "-" str))
         ;; Remove leading and trailing hyphens.
         (str (replace-regexp-in-string "\\(^[-]*\\|[-]*$\\)" "" str)))
    str))

(defun bergheim/org-clock-status ()
  "Return the org time status - including any pomodoro activity"
  (if (and (featurep 'org-pomodoro) (org-pomodoro-active-p))
      (cl-case org-pomodoro-state
        (:pomodoro
         (format "Pomo: %d minutes - %s" (/ (org-pomodoro-remaining-seconds) 60) org-clock-heading))
        (:short-break
         (format "Short break time: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
        (:long-break
         (format "Long break time: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
        (:overtime
         (format "Overtime! %d minutes" (/ (org-pomodoro-remaining-seconds) 60))))
    (if (org-clocking-p)
        (format "%s - %s" (org-duration-from-minutes (org-clock-get-clocked-time)) org-clock-heading)
      "")))


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



(defun bergheim/vertico--without-orderless (fn &rest args)
  (let ((completion-styles '(partial-completion)))
    (apply fn args)))

(defun bergheim/vertico--without-sorting (fn &rest args)
  (let ((vertico-sort-function nil))
    (apply fn args)))

;;; helpers.el ends here
