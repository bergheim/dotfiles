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


(defun bergheim/vertico--without-orderless (fn &rest args)
  (let ((completion-styles '(partial-completion)))
    (apply fn args)))

(defun bergheim/vertico--without-sorting (fn &rest args)
  (let ((vertico-sort-function nil))
    (apply fn args)))

;; nicked from doom again!

;;;###autoload
(defvar +org-capture-frame-parameters
  `((name . "org-capture")
    (width . 70)
    (height . 25)
    (transient . t)
    ,@(when IS-LINUX
        `((window-system . ,(if (boundp 'pgtk-initialized) 'pgtk 'x))
          (display . ,(or (getenv "WAYLAND_DISPLAY")
                          (getenv "DISPLAY")
                          ":0"))))
    ,(if IS-MAC '(menu-bar-lines . 1)))
  "TODO")

;;;###autoload
(defun +org-capture-frame-p (&rest _)
  "Return t if the current frame is an org-capture frame opened by
`+org-capture/open-frame'."
  (and (equal (alist-get 'name +org-capture-frame-parameters)
              (frame-parameter nil 'name))
       (frame-parameter nil 'transient)))

;;;###autoload
(defun +org-capture/open-frame (&optional initial-input key)
  "Opens the org-capture window in a floating frame that cleans itself up once
you're done. This can be called from an external shell script."
  (interactive)
  (when (and initial-input (string-empty-p initial-input))
    (setq initial-input nil))
  (when (and key (string-empty-p key))
    (setq key nil))
  (let* ((frame-title-format "")
         (frame (if (+org-capture-frame-p)
                    (selected-frame)
                  (make-frame +org-capture-frame-parameters))))
    (select-frame-set-input-focus frame)  ; fix MacOS not focusing new frames
    (with-selected-frame frame
      (require 'org-capture)
      (condition-case ex
          (letf! ((#'pop-to-buffer #'switch-to-buffer))
            (switch-to-buffer (doom-fallback-buffer))
            (let ((org-capture-initial initial-input)
                  org-capture-entry)
              (when (and key (not (string-empty-p key)))
                (setq org-capture-entry (org-capture-select-template key)))
              (funcall +org-capture-fn)))
        ('error
         (message "org-capture: %s" (error-message-string ex))
         (delete-frame frame))))))


;;; helpers.el ends here
