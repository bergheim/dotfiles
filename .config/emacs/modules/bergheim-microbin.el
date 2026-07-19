;;; bergheim-microbin.el --- MicroBin paste client -*- lexical-binding: t -*-

(require 'cl-lib)

(defgroup bergheim/microbin nil
  "Self-hosted MicroBin client."
  :group 'tools)

(defcustom bergheim/microbin-host "https://share.glvortex.net"
  "Base URL of the MicroBin instance.  No trailing slash."
  :type 'string)

(defcustom bergheim/microbin-default-view 'raw
  "Which URL form to copy / message after a successful upload.
`raw'    — /raw/<id>     (raw bytes; the 0x0 replacement)
`pretty' — /upload/<id>  (the syntax-highlighted browser view)

MicroBin only exposes those two routes for a pasta.  Earlier
versions of this file also mentioned a `/url/<id>' route — that
path returns a 404-body page; it does not exist."
  :type '(choice (const raw) (const pretty)))

(defcustom bergheim/microbin-mode-syntax-alist
  '((emacs-lisp-mode      . "lisp")
    (lisp-interaction-mode . "lisp")
    (lisp-mode            . "lisp")
    (scheme-mode          . "lisp")
    (clojure-mode         . "lisp")
    (sh-mode              . "sh")
    (bash-ts-mode         . "sh")
    (c-mode               . "c")
    (c-ts-mode            . "c")
    (c++-mode             . "cpp")
    (c++-ts-mode          . "cpp")
    (csharp-mode          . "cs")
    (csharp-ts-mode       . "cs")
    (erlang-mode          . "erl")
    (go-mode              . "go")
    (go-ts-mode           . "go")
    (haskell-mode         . "hs")
    (html-mode            . "html")
    (mhtml-mode           . "html")
    (web-mode             . "html")
    (java-mode            . "java")
    (java-ts-mode         . "java")
    (js-mode              . "js")
    (js-ts-mode           . "js")
    (js2-mode             . "js")
    (typescript-mode      . "js")
    (typescript-ts-mode   . "js")
    (tsx-ts-mode          . "js")
    (json-mode            . "json")
    (json-ts-mode         . "json")
    (js-json-mode         . "json")
    (kotlin-mode          . "kt")
    (lua-mode             . "lua")
    (pascal-mode          . "pas")
    (php-mode             . "php")
    (python-mode          . "py")
    (python-ts-mode       . "py")
    (ess-r-mode           . "r")
    (r-mode               . "r")
    (ruby-mode            . "rb")
    (ruby-ts-mode         . "rb")
    (rust-mode            . "rs")
    (rust-ts-mode         . "rs")
    (rustic-mode          . "rs")
    (scala-mode           . "sc")
    (swift-mode           . "swift")
    (xml-mode             . "xml")
    (nxml-mode            . "xml")
    (yaml-mode            . "yaml")
    (yaml-ts-mode         . "yaml"))
  "Map major mode → MicroBin `syntax_highlight' code.
Lookup tries exact match first, then `derived-mode-p' for each entry.
Buffers whose mode isn't matched fall back to `\"auto\"' (MicroBin
guesses).  Set the cdr to \"none\" to force plain text for a mode."
  :type '(alist :key-type symbol :value-type string))

(defun bergheim/microbin--syntax-for-mode (mode)
  "Return MicroBin syntax_highlight code for MODE, or \"auto\"."
  (or (cdr (assq mode bergheim/microbin-mode-syntax-alist))
      (cl-loop for (parent . code) in bergheim/microbin-mode-syntax-alist
               when (and (fboundp 'provided-mode-derived-p)
                         (provided-mode-derived-p mode parent))
               return code)
      "auto"))

(defcustom bergheim/microbin-password-function nil
  "Zero-arg function returning the uploader password as a string. "
  :type '(choice (const :tag "Not configured" nil) function))

(defun bergheim/microbin--password ()
  "Call `bergheim/microbin-password-function' and validate the result."
  (unless (functionp bergheim/microbin-password-function)
    (user-error "Set `bergheim/microbin-password-function' to a 0-arg function"))
  (let ((pw (funcall bergheim/microbin-password-function)))
    (unless (and (stringp pw) (not (string-empty-p pw)))
      (user-error "`bergheim/microbin-password-function' returned no password"))
    pw))

(defun bergheim/microbin--view-url (pasta-url view)
  "Rewrite a /upload/<id> URL into the requested VIEW form.
`pasta-url' is the redirect target from MicroBin and is already
the `pretty' (highlighted-HTML) view; only `raw' needs rewriting."
  (pcase view
    ('raw    (replace-regexp-in-string "/upload/" "/raw/" pasta-url t t))
    ('pretty pasta-url)
    (_       pasta-url)))

(defun bergheim/microbin--curl (extra-args)
  "POST to MicroBin with EXTRA-ARGS appended after the auth field.
Return the redirect target URL (string)."
  (let* ((pw  (bergheim/microbin--password))
         (url (concat bergheim/microbin-host "/upload"))
         (args (append (list "--silent" "--show-error"
                             "--output" null-device
                             "--write-out" "%{redirect_url}"
                             "--form-string" (concat "uploader_password=" pw))
                       extra-args
                       (list url)))
         (out (with-temp-buffer
                (let ((rc (apply #'call-process "curl" nil t nil args)))
                  (unless (zerop rc)
                    (user-error "curl exited %s: %s" rc (buffer-string))))
                (string-trim (buffer-string)))))
    (cond
     ((string-match-p "/upload/[^/]+\\'" out) out)
     ((string-match-p "/incorrect\\'" out)
      (user-error "MicroBin: rejected (bad uploader password?)"))
     ((string-empty-p out)
      (user-error "MicroBin: no redirect from server"))
     (t (user-error "MicroBin: unexpected response %s" out)))))

(defun bergheim/microbin--finish (pasta-url view)
  "Rewrite PASTA-URL to VIEW, copy to kill-ring, echo, return."
  (let ((u (bergheim/microbin--view-url pasta-url view)))
    (kill-new u)
    (message "MicroBin → %s  (copied)" u)
    u))

(defun bergheim/microbin--view-for (prefix-arg)
  "Pick the view based on PREFIX-ARG.
No prefix → `bergheim/microbin-default-view'.
C-u       → toggle to the other view."
  (cond
   ((consp prefix-arg)
    (if (eq bergheim/microbin-default-view 'raw) 'pretty 'raw))
   (t bergheim/microbin-default-view)))

;;;###autoload
(defun bergheim/microbin-upload-region (start end &optional prefix-arg)
  "Upload region START..END to MicroBin as a text pasta.
Syntax highlighting is inferred from the buffer's major mode via
`bergheim/microbin-mode-syntax-alist'."
  (interactive "r\nP")
  (let ((tmp    (make-temp-file "microbin-"))
        (syntax (bergheim/microbin--syntax-for-mode major-mode)))
    (unwind-protect
        (progn
          (write-region start end tmp nil 'quiet)
          (bergheim/microbin--finish
           (bergheim/microbin--curl
            (list "-F" (concat "content=<" tmp)
                  "-F" (concat "syntax_highlight=" syntax)))
           (bergheim/microbin--view-for prefix-arg)))
      (ignore-errors (delete-file tmp)))))

;;;###autoload
(defun bergheim/microbin-upload-buffer (&optional prefix-arg)
  "Upload the whole current buffer as a text pasta."
  (interactive "P")
  (bergheim/microbin-upload-region (point-min) (point-max) prefix-arg))

;;;###autoload
(defun bergheim/microbin-upload-dwim (&optional prefix-arg)
  "Upload active region if any, otherwise the whole buffer."
  (interactive "P")
  (if (use-region-p)
      (bergheim/microbin-upload-region (region-beginning) (region-end) prefix-arg)
    (bergheim/microbin-upload-buffer prefix-arg)))

;;;###autoload
(defun bergheim/microbin-upload-dwim-pretty (&optional prefix-arg)
  "Like `bergheim/microbin-upload-dwim' but copy the highlighted /url/ link."
  (interactive "P")
  (let ((bergheim/microbin-default-view 'pretty))
    (bergheim/microbin-upload-dwim prefix-arg)))

;;;###autoload
(defun bergheim/microbin-upload-buffer-pretty (&optional prefix-arg)
  "Like `bergheim/microbin-upload-buffer' but copy the highlighted /url/ link."
  (interactive "P")
  (let ((bergheim/microbin-default-view 'pretty))
    (bergheim/microbin-upload-buffer prefix-arg)))

;;;###autoload
(defun bergheim/microbin-upload-region-pretty (start end &optional prefix-arg)
  "Like `bergheim/microbin-upload-region' but copy the highlighted /url/ link."
  (interactive "r\nP")
  (let ((bergheim/microbin-default-view 'pretty))
    (bergheim/microbin-upload-region start end prefix-arg)))

;;;###autoload
(defun bergheim/microbin-upload-file (file &optional prefix-arg)
  "Upload FILE as a binary attachment (lands at /file/<id>).
No syntax_highlight applied — attachments use a different view."
  (interactive "fFile to upload: \nP")
  (bergheim/microbin--finish
   (bergheim/microbin--curl (list "-F" (concat "file=@" (expand-file-name file))))
   (bergheim/microbin--view-for prefix-arg)))

(provide 'bergheim-microbin)
;;; bergheim-microbin.el ends here
