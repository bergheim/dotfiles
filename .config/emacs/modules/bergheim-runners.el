;;; bergheim-runners.el --- Isolated desktop runner commands -*- lexical-binding: t; -*-

(elpaca-wait)
(require 'nerd-icons)
(require 'vertico-buffer)
(require 'savehist)

(setq vertico-buffer-display-action '(display-buffer-same-window)
      savehist-file (expand-file-name "runners-history" bergheim/cache-dir)
      savehist-additional-variables '(bergheim/cliphist-history
                                      bergheim/apps-history
                                      bergheim/pass-history))

(vertico-buffer-mode 1)
(savehist-mode 1)

(keymap-set vertico-map "<escape>" #'abort-minibuffers)

;; Runners only do completing-read. Corfu-auto in the same minibuffer is a
;; second completion UI on Super+c.
(setq corfu-auto nil)
(when (fboundp 'global-corfu-mode)
  (global-corfu-mode -1))

(bergheim/apply-system-theme)

(defvar bergheim/runner-frames nil
  "Alist of runner kind to frame.")

(defvar bergheim/runner--busy nil
  "Non-nil while a runner completing-read is on screen.")

(defvar bergheim/cliphist--timer nil)

(defvar bergheim/runner-frame-fraction 0.6
  "Runner frame size, as a fraction of its monitor's work area.")

(defvar bergheim/cliphist-history nil)

(defvar bergheim/apps-history nil)

(defvar bergheim/pass-history nil)

(defvar bergheim/pass-clear-timer nil)

(defvar bergheim/apps-icon-height 32)

(defvar bergheim/apps-image-cache (make-hash-table :test 'equal))

(defun bergheim/runner-on-sway-p ()
  "Return non-nil when the runner daemon belongs to Sway."
  (or (string= (getenv "XDG_CURRENT_DESKTOP") "sway")
      (and (getenv "SWAYSOCK") t)))

(defun bergheim/runner-frame-name (kind)
  (format "emacs-runner-%s" kind))

(defun bergheim/runner-frame (kind)
  "Return the reusable runner frame for KIND."
  (let ((frame (alist-get kind bergheim/runner-frames)))
    (unless (frame-live-p frame)
      (setq frame
            (make-frame-on-display
             (or (getenv "WAYLAND_DISPLAY") (getenv "DISPLAY"))
             `((name . ,(bergheim/runner-frame-name kind))
               (visibility . ,(bergheim/runner-on-sway-p))
               (fullscreen . 0)
               (undecorated . t)
               (tab-bar-lines . 0)
               (vertical-scroll-bars . nil)
               (horizontal-scroll-bars . nil))))
      ;; `workarea' is in device pixels, `set-frame-size' in logical ones.
      (let* ((monitor (frame-monitor-attributes frame))
             (scale (or (cdr (assq 'scale-factor monitor)) 1))
             (fraction (/ bergheim/runner-frame-fraction scale)))
        (pcase-let ((`(,_x ,_y ,width ,height) (cdr (assq 'workarea monitor))))
          (set-frame-size frame
                          (round (* fraction width))
                          (round (* fraction height))
                          t)))
      (with-selected-frame frame
        (spacious-padding-mode 1)
        (bergheim/runner-set-font))
      (setf (alist-get kind bergheim/runner-frames) frame))
    frame))

(when (and (boundp 'bergheim/runner-frame)
           (frame-live-p bergheim/runner-frame))
  (delete-frame bergheim/runner-frame t)
  (setq bergheim/runner-frame nil))

(defun bergheim/runner-set-font ()
  "Set up the runner frame's font.
The preset has to be installed after `bergheim/fontaine-on-frame', which
rebuilds `fontaine-presets'."
  (bergheim/fontaine-on-frame)
  (setf (alist-get 'runner fontaine-presets)
        `(:default-height ,(round (* 100 bergheim/font-base))
          :default-family "Iosevka Nerd Font"
          :fixed-pitch-family "Iosevka Nerd Font"
          :variable-pitch-family "Iosevka Nerd Font Propo"
          :line-spacing (0.2 . 0.2)))
  (fontaine-set-preset 'runner))

(defun bergheim/with-runner-frame (kind fn)
  "Show the KIND runner frame, call FN, then hide the frame."
  (unless bergheim/runner--busy
    (let ((bergheim/runner--busy t)
          (frame (bergheim/runner-frame kind))
          (title (format "^%s$" (bergheim/runner-frame-name kind))))
      (bergheim/apply-system-theme)
      (unwind-protect
          (with-selected-frame frame
            (let ((buf (get-buffer-create " *runner*")))
              (with-current-buffer buf
                (setq-local mode-line-format nil)
                (setq-local cursor-type nil))
              (switch-to-buffer buf))
            (if (bergheim/runner-on-sway-p)
                (call-process "swaymsg" nil nil nil
                              (format "[title=\"%s\"] scratchpad show, move position center" title))
              (make-frame-visible frame))
            (select-frame-set-input-focus frame)
            (funcall fn))
        (when (frame-live-p frame)
          (if (bergheim/runner-on-sway-p)
              (call-process "swaymsg" nil nil nil
                            (format "[title=\"%s\"] move scratchpad" title))
            (make-frame-invisible frame)))))))

(defun bergheim/cliphist-start ()
  "Schedule the clipboard picker outside the emacsclient request."
  (unless bergheim/runner--busy
    (when (timerp bergheim/cliphist--timer)
      (cancel-timer bergheim/cliphist--timer))
    (setq bergheim/cliphist--timer
          (run-at-time 0 nil #'bergheim/cliphist)))
  nil)

(defun bergheim/cliphist ()
  "Pick a cliphist entry and copy it."
  (interactive)
  (bergheim/with-runner-frame
   'clip
   (lambda ()
     (let* ((entries (process-lines "cliphist" "list"))
            (choices (mapcar (lambda (entry)
                               (cons (string-trim-left entry "[^\t]+\t") entry))
                             entries))
            (table (lambda (str pred action)
                     (if (eq action 'metadata)
                         '(metadata (display-sort-function . identity)
                                    (cycle-sort-function . identity))
                       (complete-with-action action choices str pred))))
            ;; Vertico's count overlay sits on the prompt's first
            ;; character and picks up its face, so keep the icon off
            ;; that spot or the digits render in the icon font.
            (prompt (concat " "
                            (nerd-icons-mdicon "nf-md-clipboard_clock_outline")
                            "  Clipboard: "))
            (choice (completing-read prompt table nil t nil
                                     'bergheim/cliphist-history))
            (entry (cdr (assoc-string choice choices))))
       (unless entry
         (user-error "Clipboard entry disappeared: %S" choice))
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (insert entry)
         (unless (zerop (call-process-region
                         (point-min) (point-max) "cliphist" t t nil "decode"))
           (user-error "cliphist decode failed"))
         (unless (zerop (call-process-region
                         (point-min) (point-max) "wl-copy" nil nil))
           (user-error "wl-copy failed")))))))

(defun bergheim/apps-start ()
  "Schedule the app runner outside the emacsclient request."
  (run-at-time 0 nil #'bergheim/apps)
  nil)

(defun bergheim/apps-image (path)
  "Return a cached image for PATH, or nil."
  (or (gethash path bergheim/apps-image-cache)
      (when (and path (file-readable-p path))
        (when-let* ((img (ignore-errors
                          (create-image path nil nil
                                        :height bergheim/apps-icon-height
                                        :ascent 'center))))
          (puthash path img bergheim/apps-image-cache)
          img))))

(defvar bergheim/apps-rows nil)

(defun bergheim/apps-list-bin ()
  (expand-file-name "~/local/bin/xdg-apps-list"))

(defun bergheim/apps-refresh ()
  "Reload desktop entries and realize their icons on the apps frame."
  (let ((theme (if (bergheim//system-dark-mode-enabled-p)
                   "breeze-dark"
                 "breeze"))
        (frame (bergheim/runner-frame 'apps))
        rows)
    (with-selected-frame frame
      (dolist (line (process-lines (bergheim/apps-list-bin) theme))
        (pcase (split-string line "\t")
          (`(,id ,name ,icon)
           (bergheim/apps-image icon)
           (push (list name id icon) rows))
          (`(,id ,name)
           (push (list name id "") rows)))))
    (setq bergheim/apps-rows (nreverse rows))))

(defun bergheim/apps-data ()
  (or bergheim/apps-rows (bergheim/apps-refresh)))

(defun bergheim/apps ()
  "Launch an XDG application from a completing-read with icons."
  (interactive)
  (let* ((rows (bergheim/apps-data))
         (icons (make-hash-table :test 'equal))
         (choices (mapcar (lambda (row)
                            (pcase-let ((`(,name ,id ,icon) row))
                              (when (and icon (not (string-empty-p icon)))
                                (puthash name icon icons))
                              (cons name id)))
                          rows))
         (affix (lambda (cands)
                  (mapcar (lambda (c)
                            (let ((img (bergheim/apps-image (gethash c icons))))
                              (list c
                                    (if img
                                        (concat (propertize " " 'display img) " ")
                                      "   ")
                                    "")))
                          cands)))
         (table (lambda (str pred action)
                  (if (eq action 'metadata)
                      `(metadata (affixation-function . ,affix)
                                 (display-sort-function . identity)
                                 (cycle-sort-function . identity)
                                 (category . bergheim-app))
                    (complete-with-action action choices str pred))))
         (prompt (concat " "
                         (nerd-icons-mdicon "nf-md-application_outline")
                         "  Run: ")))
    (bergheim/with-runner-frame
     'apps
     (lambda ()
       (let* ((choice (completing-read prompt table nil t nil
                                       'bergheim/apps-history))
              (id (cdr (assoc-string choice choices))))
         (unless id
           (user-error "App disappeared: %S" choice))
         (call-process (bergheim/apps-list-bin) nil 0 nil "--bump" id)
         (call-process "gtk-launch" nil 0 nil id)
         (setq bergheim/apps-rows nil)
         (run-at-time 0.1 nil #'bergheim/apps-refresh))))))

(defun bergheim/pass-start ()
  "Schedule the pass picker outside the emacsclient request."
  (run-at-time 0 nil #'bergheim/pass)
  nil)

(defun bergheim/pass-entries ()
  "Return password-store paths, without decrypting."
  (let* ((dir (expand-file-name
               (or (getenv "PASSWORD_STORE_DIR") "~/.password-store")))
         (dir (file-name-as-directory dir))
         (prefix-len (length dir)))
    (mapcar (lambda (file)
              (string-remove-suffix ".gpg" (substring file prefix-len)))
            (directory-files-recursively dir "\\.gpg\\'"))))

(defun bergheim/pass-parse (text)
  "Parse a `pass show' payload into :password :user :url."
  (let* ((lines (split-string text "\n"))
         (password (or (car lines) ""))
         user url)
    (let ((case-fold-search t))
      (dolist (line (cdr lines))
        (cond
         ((string-match-p "\\`otpauth://" line) nil)
         ((string-match "\\`\\(login\\|user\\|username\\):[[:space:]]*\\(.*\\)\\'" line)
          (setq user (match-string 2 line)))
         ((string-match "\\`url:[[:space:]]*\\(.*\\)\\'" line)
          (setq url (match-string 1 line))))))
    (list :password password :user user :url url)))

(defun bergheim/pass-copy (text)
  "Copy TEXT to the clipboard and clear it after 10 seconds."
  (when (timerp bergheim/pass-clear-timer)
    (cancel-timer bergheim/pass-clear-timer))
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max) "wl-copy" nil nil))
  (setq bergheim/pass-clear-timer
        (run-at-time 10 nil (lambda ()
                              (call-process "wl-copy" nil nil nil "--clear")))))

(defun bergheim/pass-type (user password)
  "Type USER, Tab, PASSWORD into the focused window via wtype stdin."
  (let ((wtype (executable-find "wtype")))
    (unless wtype
      (user-error "wtype not found"))
    (sit-for 0.2)
    (with-temp-buffer
      (insert user)
      (call-process-region (point-min) (point-max) wtype nil nil nil "-"))
    (call-process wtype nil nil nil "-k" "Tab")
    (with-temp-buffer
      (insert password)
      (call-process-region (point-min) (point-max) wtype nil nil nil "-"))))

(defun bergheim/pass-show (entry)
  (with-temp-buffer
    (unless (zerop (call-process "pass" nil t nil "show" entry))
      (user-error "pass show failed"))
    (buffer-string)))

(defun bergheim/pass ()
  "Pick a password-store entry, then copy or type a field."
  (interactive)
  (let* ((entries (bergheim/pass-entries))
         (prompt (concat " "
                         (nerd-icons-mdicon "nf-md-key_variant")
                         "  Pass: "))
         pending-type)
    (bergheim/with-runner-frame
     'pass
     (lambda ()
       (let* ((entry (completing-read prompt entries nil t nil
                                      'bergheim/pass-history))
              (parsed (bergheim/pass-parse (bergheim/pass-show entry)))
              (password (plist-get parsed :password))
              (user (plist-get parsed :user))
              (url (plist-get parsed :url))
              (actions
               (delq nil
                     (list (and (not (string-empty-p password))
                                (cons "Password" (list 'copy password)))
                           (and user (not (string-empty-p user))
                                (cons (format "Username (%s)" user)
                                      (list 'copy user)))
                           (and url (not (string-empty-p url))
                                (cons (format "URL (%s)" url)
                                      (list 'copy url)))
                           (and user password
                                (not (string-empty-p user))
                                (not (string-empty-p password))
                                (cons "Type username and password"
                                      (list 'type user password))))))
              (choice (completing-read " Action: " actions nil t))
              (spec (cdr (assoc-string choice actions))))
         (pcase spec
           (`(copy ,text) (bergheim/pass-copy text))
           (`(type ,u ,p) (setq pending-type (list u p)))
           (_ (user-error "Unknown action"))))))
    (when pending-type
      (apply #'bergheim/pass-type pending-type))))

(bergheim/runner-frame 'clip)
(bergheim/runner-frame 'apps)
(bergheim/runner-frame 'pass)
(run-with-idle-timer 0.5 nil #'bergheim/apps-refresh)

(provide 'bergheim-runners)
