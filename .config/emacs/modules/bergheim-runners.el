;;; bergheim-runners.el --- Isolated desktop runner commands -*- lexical-binding: t; -*-

(elpaca-wait)
(require 'vertico-buffer)
(require 'savehist)

(setq vertico-buffer-display-action '(display-buffer-same-window)
      savehist-file (expand-file-name "runners-history" bergheim/cache-dir)
      savehist-additional-variables '(bergheim/cliphist-history))

(vertico-buffer-mode 1)
(savehist-mode 1)

(keymap-set vertico-map "<escape>" #'abort-minibuffers)

(load-theme (if (bergheim//system-dark-mode-enabled-p)
                bergheim/theme-dark
              bergheim/theme-light)
            t)

(defvar bergheim/runner-frame nil)
(defvar bergheim/cliphist-history nil)

(defun bergheim/runner-on-sway-p ()
  "Return non-nil when the runner daemon belongs to Sway."
  (or (string= (getenv "XDG_CURRENT_DESKTOP") "sway")
      (and (getenv "SWAYSOCK") t)))

(defun bergheim/runner-frame ()
  "Return the reusable runner frame."
  (unless (frame-live-p bergheim/runner-frame)
    (setq bergheim/runner-frame
          (make-frame-on-display
           (or (getenv "WAYLAND_DISPLAY") (getenv "DISPLAY"))
           `((name . "emacs-runner")
             (visibility . ,(bergheim/runner-on-sway-p))
             (fullscreen . 0)
             (undecorated . t)
             (tab-bar-lines . 0)
             (vertical-scroll-bars . nil)
             (horizontal-scroll-bars . nil)
             (width . (text-pixels . 784))
             (height . (text-pixels . 600))))))
  bergheim/runner-frame)

(bergheim/runner-frame)

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

(defun bergheim/cliphist-start ()
  "Schedule the clipboard picker outside the emacsclient request."
  (run-at-time 0 nil #'bergheim/cliphist)
  nil)

(defun bergheim/cliphist ()
  "Pick a cliphist entry and copy it."
  (interactive)
  (let ((frame (bergheim/runner-frame)))
    (unwind-protect
        (with-selected-frame frame
          (if (bergheim/runner-on-sway-p)
              (call-process "swaymsg" nil nil nil
                            "[title=\"^emacs-runner$\"] scratchpad show, move position center")
            (make-frame-visible frame))
          (select-frame-set-input-focus frame)
          ;; Takes its colours from the selected frame, so re-apply here.
          (spacious-padding-mode 1)
          (bergheim/runner-set-font)
          (let* ((entries (process-lines "cliphist" "list"))
                 (choices (mapcar (lambda (entry)
                                    (cons (string-trim-left entry "[^\t]+\t") entry))
                                  entries))
                 (table (lambda (str pred action)
                          (if (eq action 'metadata)
                              '(metadata (display-sort-function . identity)
                                         (cycle-sort-function . identity))
                            (complete-with-action action choices str pred))))
                 (choice (completing-read "Clipboard: " table nil t nil
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
                (user-error "wl-copy failed")))))
      (when (frame-live-p frame)
        (if (bergheim/runner-on-sway-p)
            (call-process "swaymsg" nil nil nil
                          "[title=\"^emacs-runner$\"] move scratchpad")
          (make-frame-invisible frame))))))

(provide 'bergheim-runners)
