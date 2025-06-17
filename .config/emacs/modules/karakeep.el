;; -*- lexical-binding: t; -*-

(defgroup karakeep nil
  "Send bookmarks to Karakeep."
  :group 'external)

(defcustom karakeep-api-url "http://localhost:3000/api/v1"
  "Karakeep API endpoint."
  :type 'string
  :group 'karakeep)

(defcustom karakeep-api-token "YourApiKey"
  "Authorization token for Karakeep."
  :type 'string
  :group 'karakeep)

(customize-set-variable karakeep-api-url (password-store-get "homelab/karakeep/url"))
(customize-set-variable karakeep-api-token (password-store-get "homelab/karakeep/api-emacs"))

(defun karakeep--get-bookmarks-url ()
  "Get the bookmarks API endpoint."
  (concat karakeep-api-url "/bookmarks"))

(defun karakeep--get-lists-url ()
  "Get the lists API endpoint."
  (concat karakeep-api-url "/lists"))

(defun karakeep--send-request (payload &optional list-id)
  "Send PAYLOAD to Karakeep API.
PAYLOAD should be an alist that will be JSON-encoded.
LIST-ID is an optional list identifier to add the item to."
  (let* ((json-string (json-encode payload))
         (json-payload (string-as-unibyte
                        (encode-coding-string json-string 'utf-8)))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json; charset=utf-8")
            ("Authorization" . ,(concat "Bearer " karakeep-api-token))))
         (url-request-data json-payload)
         (callback (when list-id
                     (lambda (bookmark-id success-p)
                       (when (and bookmark-id success-p)
                         (karakeep--add-bookmark-to-list bookmark-id list-id))))))
    (url-retrieve (karakeep--get-bookmarks-url)
                  (lambda (status)
                    (karakeep--handle-response status callback)))))

(defun karakeep--handle-response (status &optional callback)
  "Handle the response from Karakeep API.
STATUS is the response status from url-retrieve.
CALLBACK is optional - if provided, called with (bookmark-id success-p) as arguments."
  (goto-char url-http-end-of-headers)
  (let* ((response (string-trim (buffer-substring-no-properties (point) (point-max))))
         (parsed-response (ignore-errors (json-parse-string response))))
    (cond
     ((and parsed-response (eq (gethash "alreadyExists" parsed-response) t))
      (let* ((bookmark-id (gethash "id" parsed-response))
             (content (gethash "content" parsed-response))
             (url (gethash "url" content)))
        (message "ℹ️ Already exists in Karakeep%s" (if url (format ": %s" url) ""))
        (when callback (funcall callback bookmark-id t))))
     ((and parsed-response (gethash "id" parsed-response))
      (let* ((bookmark-id (gethash "id" parsed-response))
             (content (gethash "content" parsed-response))
             (url (gethash "url" content)))
        (message "✅ Sent to Karakeep%s" (if url (format ": %s" url) ""))
        (when callback (funcall callback bookmark-id t))))
     (t
      (message "❌ Karakeep error: %s" (or response "Unknown error"))
      (when callback (funcall callback nil nil))))
    (kill-buffer (current-buffer))))

(defun karakeep--add-bookmark-to-list (bookmark-id list-id)
  "Add BOOKMARK-ID to LIST-ID using PUT request."
  (let* ((url (format "%s/%s/bookmarks/%s"
                      (karakeep--get-lists-url)
                      list-id bookmark-id))
         (url-request-method "PUT")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " karakeep-api-token)))))
    (url-retrieve url (lambda (status)
                        (when (plist-get status :error)
                          (display-warning 'karakeep
                                           "Bookmark saved but failed to add to list"
                                           :warning))
                        (kill-buffer (current-buffer))))))


(defun karakeep--get-org-link-at-point ()
  "Extract the Org-mode link at point."
  (let ((context (org-element-context)))
    (when (eq (car context) 'link)
      (let ((url (org-element-property :raw-link context))
            (desc (org-element-contents context)))
        (list url (if desc (org-trim (org-no-properties (car desc))) ""))))))

(defun karakeep--get-link-at-point ()
  "Extract link at point from various contexts."
  (cond
   ;; either org..
   ((and (derived-mode-p 'org-mode)
         (karakeep--get-org-link-at-point)))

   ;; ..or whatever URL we can find
   ((thing-at-point 'url)
    (list (thing-at-point 'url) ""))))

(defun karakeep--fetch-lists-sync ()
  "Fetch lists from Karakeep API synchronously.
Returns the parsed JSON response or nil on error."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " karakeep-api-token))))
         (buffer (url-retrieve-synchronously (karakeep--get-lists-url))))
    (when buffer
      (with-current-buffer buffer
        (goto-char url-http-end-of-headers)
        (let* ((response (string-trim
                          (buffer-substring-no-properties (point) (point-max))))
               (parsed-response (ignore-errors (json-parse-string response))))
          (kill-buffer buffer)
          parsed-response)))))

(defun karakeep--parse-lists (api-response)
  "Parse the API response and return a list of (display-name . id) pairs.
API-RESPONSE is the hash table returned from the API."
  (when-let ((lists-array (gethash "lists" api-response)))
    (mapcar (lambda (list-hash)
              (let ((name (gethash "name" list-hash))
                    (icon (gethash "icon" list-hash))
                    (id (gethash "id" list-hash)))
                (cons (format "%s %s" icon name)  ; Display as "🎮 gaming"
                      id)))                       ; Store the ID
            (append lists-array nil))))           ; Convert vector to list

(defun karakeep--select-list ()
  "Fetch lists and prompt user to select one.
Returns the selected list ID or nil if cancelled/failed."
  (let* ((raw-lists (karakeep--fetch-lists-sync))
         (parsed-lists (karakeep--parse-lists raw-lists)))
    (when parsed-lists
      (let ((selection (completing-read "Select Karakeep list: " parsed-lists)))
        (cdr (assoc selection parsed-lists))))))

(defun karakeep-send-link (&optional list-id)
  "Send the link at point to Karakeep."
  (interactive)
  (if-let* ((link-data (karakeep--get-link-at-point))
            (url (car link-data))
            (title (cadr link-data)))
      (karakeep--send-request `(("type" . "link")
                                ("url" . ,url)
                                ("title" . ,title))
                              list-id)
    (message "⚠️ No valid link at point.")))

(defun karakeep-send-region (&optional list-id)
  "Send the currently active region (marked text) to Karakeep as a 'text' type."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (karakeep--send-request `(("type" . "text")
                                  ("text" . ,text))
                                list-id))
    (message "⚠️ No active region (marked text).")))

;; TODO: send tags from elfeed to karakeep!
(defun karakeep-send-elfeed-entry (&optional list-id)
  "Star the current Elfeed entry and send it to Karakeep."
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :single)))
         (url (when entry (elfeed-entry-link entry)))
         (title (when entry (elfeed-entry-title entry))))
    (if (not entry)
        (message "⚠️ No entry selected.")
      ;; Add star tag to the entry
      (elfeed-tag entry 'star)
      (when (eq major-mode 'elfeed-search-mode)
        (elfeed-search-update-entry entry))
      ;; Send to Karakeep
      (karakeep--send-request `(("type" . "link")
                                ("url" . ,url)
                                ("title" . ,title))
                              list-id))))

(defun karakeep-send-eww-page (&optional list-id)
  "Send the current EWW page to Karakeep."
  (interactive)
  (let* ((url (eww-current-url))
         (title (plist-get eww-data :title)))
    (when url
      (karakeep--send-request `(("type" . "link")
                                ("url" . ,url)
                                ("title" . ,title))
                              list-id))))

;;;###autoload
(defun karakeep-dwim (&optional arg)
  "Send content to Karakeep based on context.
With universal argument, prompt for list selection."
  (interactive "P")
  (let ((list-id (when arg (karakeep--select-list))))
    (when (or (not arg) list-id)  ; Proceed if no arg, or if list was selected
      (cond
       ;; Elfeed context
       ((or (eq major-mode 'elfeed-show-mode)
            (eq major-mode 'elfeed-search-mode))
        (karakeep-send-elfeed-entry list-id))

       ((eq major-mode 'eww-mode)
        (karakeep-send-eww-page list-id))

       ((use-region-p)
        (karakeep-send-region list-id))

       ;; Links - should probably not be the fallback but good enough for now
       (t
        (karakeep-send-link list-id))))))

(provide 'karakeep-send)
