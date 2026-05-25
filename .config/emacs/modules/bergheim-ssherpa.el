;;; bergheim-ssherpa.el --- Carry display intent back to your laptop over SSH  -*- lexical-binding: t; -*-

;; Author: Thomas Bergheim
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, processes
;; URL: https://github.com/bergheim/ssherpa.el

;;; Commentary:
;;
;; Carries display intent from a remote Emacs back to the laptop you
;; are physically sitting at.  Run Emacs on a powerful machine back
;; home and SSH in from anywhere — coffee shop, train, hotel.  When
;; something needs to be opened (PDF, image, link), ssherpa scp's the
;; file across a reverse SSH tunnel and tells the laptop to open it
;; natively.
;;
;; The point: your laptop stays cool and quiet, and its battery lasts
;; a workday instead of two hours.  Heavy compute stays at home; only
;; the things that actually want a screen come to you.
;;
;; Setup
;; -----
;;
;; 1. On the laptop, add to ~/.ssh/config under the host you SSH into:
;;
;;        RemoteForward 25509 localhost:<laptop-sshd-port>
;;
;;    This forwards port 25509 on the remote back to the laptop's
;;    sshd, so the remote can ssh "back" to you.
;;
;; 2. After SSHing in and starting Emacs, run M-x `ssherpa-connect'
;;    to tag the current frame.  (Tip: wrap emacsclient so it
;;    --evals this for you when $SSH_CLIENT is set.)
;;
;; 3. Route opens through `ssherpa-open':
;;
;;        (setq browse-url-browser-function #'ssherpa-open)
;;
;; 4. Customize `ssherpa-user' / `ssherpa-port' / `ssherpa-dest-dir' /
;;    `ssherpa-opener' to match your laptop.
;;
;; Limitations: requires SSH (mosh has no port forwarding); the laptop
;; must run sshd and have the chosen opener installed.

;;; Code:

(defgroup ssherpa nil
  "Carry display intent back to your laptop over SSH."
  :group 'convenience
  :prefix "ssherpa-")

(defcustom ssherpa-user (user-login-name)
  "SSH user on the laptop (the machine physically in front of you)."
  :type 'string
  :group 'ssherpa)

(defcustom ssherpa-port 25509
  "Reverse-tunnel port on the remote that routes back to the laptop's sshd.
Match this to the `RemoteForward' entry in the laptop's ~/.ssh/config."
  :type 'integer
  :group 'ssherpa)

(defcustom ssherpa-dest-dir "~/tmp"
  "Directory on the laptop where transferred files land.
Kept out of /tmp for privacy (bills, receipts, etc.)."
  :type 'string
  :group 'ssherpa)

(defcustom ssherpa-opener "systemd-run --user --quiet --pipe xdg-open"
  "Command on the laptop to open URLs/files.
Wrapped in systemd-run by default so the spawned process inherits
the user manager's graphical session env (WAYLAND_DISPLAY etc.).
On macOS hosts, use \"open\" instead."
  :type 'string
  :group 'ssherpa)

;;;###autoload
(defun ssherpa-connect ()
  "Tag the current frame so `ssherpa-open' will route through SSH."
  (interactive)
  (set-frame-parameter nil 'ssherpa-connected t))

;;;###autoload
(defun ssherpa-disconnect (&optional frame)
  "Clear the ssherpa tag on FRAME (or the current frame)."
  (interactive)
  (set-frame-parameter frame 'ssherpa-connected nil))

(defun ssherpa-connected-p ()
  "Return non-nil if the current frame is tagged for ssherpa routing."
  (frame-parameter nil 'ssherpa-connected))

(defun ssherpa--run (program args label)
  "Run PROGRAM with ARGS synchronously; log non-zero exit under LABEL."
  (with-temp-buffer
    (let ((rc (apply #'call-process program nil t nil args)))
      (unless (zerop rc)
        (message "ssherpa %s failed (rc=%d): %s"
                 label rc (string-trim (buffer-string)))))))

;;;###autoload
(defun ssherpa-open (url &optional _new-window)
  "Open URL on the laptop via the reverse SSH tunnel.
Falls back to `browse-url-default-browser' when the current frame
is not tagged via `ssherpa-connect'.  Local files (URL begins with /)
are scp'd to `ssherpa-dest-dir' first."
  (interactive (browse-url-interactive-arg "URL: "))
  (if (not (ssherpa-connected-p))
      (browse-url-default-browser url)
    (let* ((port (number-to-string ssherpa-port))
           (target (format "%s@localhost" ssherpa-user))
           (open-target
            (if (string-prefix-p "/" url)
                ;; Quote only the filename so the dir's ~ still expands remotely.
                (let ((dest (format "%s/%s"
                                    ssherpa-dest-dir
                                    (shell-quote-argument
                                     (file-name-nondirectory url)))))
                  (ssherpa--run
                   "scp" (list "-P" port url (format "%s:%s" target dest))
                   "scp")
                  dest)
              (shell-quote-argument url))))
      (ssherpa--run
       "ssh" (list "-p" port target
                   (format "%s %s" ssherpa-opener open-target))
       "open"))))

(provide 'bergheim-ssherpa)
;;; bergheim-ssherpa.el ends here
