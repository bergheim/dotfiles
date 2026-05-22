;;; contactor.el --- Sync contacts between org and vCard -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Thomas Bergheim

(defcustom bergheim/contactor-sync-directory
  (expand-file-name "~/.local/share/vdirsyncer-testing/contacts/contacts/")
  "Directory where vCard files are stored for syncing."
  :type 'directory
  :group 'bergheim)

(defcustom bergheim/contactor-file
  (expand-file-name "~/org/contacts.org")
  "Full path to the contacts org file."
  :type 'file
  :group 'bergheim)

(defcustom bergheim/contactor-auto-commit t
  "Automatically commit vCard changes to git after export."
  :type 'boolean
  :group 'bergheim)

(defconst bergheim/contactor--timestamp-format-org "[%Y-%m-%d %a %H:%M:%S]"
  "Org-mode timestamp format for metadata (LAST_MODIFIED, LAST_SYNCED, etc).")

(defconst bergheim/contactor--timestamp-format-vcard "%Y-%m-%dT%H:%M:%SZ"
  "vCard REV timestamp format (ISO 8601 extended with UTC).")

(defun bergheim/contactor--timestamp-now-org ()
  "Return current timestamp in org format."
  (format-time-string bergheim/contactor--timestamp-format-org))

(defun bergheim/contactor--timestamp-now-vcard ()
  "Return current timestamp in vCard REV format (UTC)."
  (format-time-string bergheim/contactor--timestamp-format-vcard nil t))

(defun bergheim/contactor-run-tests ()
  "Run all vCard conversion tests."
  (interactive)
  (require 'contactor-test)
  (ert "bergheim/test-\\(vcard\\|org\\|merge\\|export\\|import\\|neo\\).*"))
;; (ert "bergheim/test-\\(neo\\).*"))

(defun bergheim/contactor-import (vcard-dir contacts-file)
  "Import all vCards from VCARD-DIR to CONTACTS-FILE."
  (let ((contacts '())
        (updated 0)
        (added 0)
        (conflicts '()))
    ;; Collect all vCards
    (dolist (vcard-file (directory-files vcard-dir t "\\.vcf$"))
      (with-temp-buffer
        (insert-file-contents vcard-file)
        (let ((contact (bergheim/contactor--vcard-to-contact (buffer-string))))
          (when (assoc-default "UID" contact)
            (push contact contacts)))))
    
    ;; Safety: an empty vcard dir almost always means the sync failed or the
    ;; path is wrong, NOT that you suddenly have zero contacts. Bail before we
    ;; touch the org buffer or its metadata.
    (when (null contacts)
      (user-error "No vCards found in %s — aborting (sync may have failed)" vcard-dir))

    ;; Check for conflicts before updating
    (with-current-buffer (find-file-noselect contacts-file)
      (dolist (contact contacts)
        (let* ((uid (assoc-default "UID" contact))
               (marker (when uid (bergheim/contactor--find-by-uid uid))))
          (when (and marker (bergheim/contactor--has-conflict-p marker contact))
            (push (list :uid uid :name (assoc-default "FN" contact)) conflicts))))
      
      (when conflicts
        (unless (yes-or-no-p (format "%d contacts have local modifications. Import anyway and overwrite? %s"
                                     (length conflicts)
                                     (mapconcat (lambda (c) (plist-get c :name)) conflicts ", ")))
          (user-error "Import cancelled")))
      
      ;; Proceed with import
      (when (= (buffer-size) 0)
        (insert "#+TITLE: Contacts\n")
        (insert "#+STARTUP: showeverything\n\n"))

      (dolist (contact contacts)
        (let* ((uid (assoc-default "UID" contact))
               (marker (when uid (bergheim/contactor--find-by-uid uid))))
          (if marker
              (progn
                (goto-char marker)
                (bergheim/contactor--update-org-heading contact)
                (org-set-property "LAST_MODIFIED" (bergheim/contactor--timestamp-now-org))
                (setq updated (1+ updated)))
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (string-trim-right (bergheim/contactor--contact-to-org contact)))
            (insert "\n")
            (goto-char (point-max))
            (re-search-backward "^\\* ")
            (org-set-property "LAST_MODIFIED" (bergheim/contactor--timestamp-now-org))
            (setq added (1+ added)))))
      
      (save-buffer)
      
      ;; Sort and update metadata
      (goto-char (point-min))
      (when (re-search-forward "^\\* " nil t)
        (goto-char (line-beginning-position))
        (let ((beg (point))
              (end (point-max)))
          (save-restriction
            (narrow-to-region beg end)
            (goto-char (point-min))
            (sort-subr nil 
                       'outline-next-heading
                       'outline-end-of-subtree
                       (lambda ()
                         (when (looking-at org-complex-heading-regexp)
                           (match-string-no-properties 4))))))
        (save-buffer))
      
      (let ((timestamp (bergheim/contactor--timestamp-now-org))
            ;; Count the contacts actually present in the file, not just the
            ;; ones touched this run — this is the baseline export-precheck
            ;; uses to detect a suspicious drop, so it must reflect reality.
            (total-contacts (let ((n 0))
                              (org-map-entries
                               (lambda () (when (org-entry-get nil "UID") (setq n (1+ n))))
                               nil nil)
                              n)))
        (bergheim/contactor--update-metadata
         :last-synced timestamp
         :contact-count total-contacts)
        (bergheim/contactor--update-top-modified)
        (save-buffer)))
    
    (message "Synced contacts: %d added, %d updated" added updated)))

;; (defun bergheim/contactor-sync ()
;;   "Sync vdirsyncer then import to org-contacts."
;;   (interactive)
;;   (message "NOT Syncing with Nextcloud...")
;;   ;; (shell-command "vdirsyncer sync nextcloud_contacts")
;;   (bergheim/contactor-import
;;    bergheim/contactor-sync-directory
;;    (expand-file-name "contacts.org" org-directory))
;;   (when (get-file-buffer (expand-file-name "contacts.org" org-directory))
;;     (with-current-buffer (get-file-buffer (expand-file-name "contacts.org" org-directory))
;;       (revert-buffer t t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bergheim/contactor-export-dirty ()
  "Export all contacts modified since last export/sync."
  (interactive)
  ;; Safety net: refuse a bulk export if the file looks wrong (never synced,
  ;; stale sync, or a suspicious drop in contact count). Can be overridden
  ;; interactively, but defaults to aborting so we never silently nuke vCards.
  (let ((check (bergheim/contactor--export-precheck)))
    (unless (or (plist-get check :safe)
                (yes-or-no-p (format "Export safety check failed: %s  Export anyway? "
                                     (plist-get check :reason))))
      (user-error "Export aborted: %s" (plist-get check :reason))))
  (let ((dirty (bergheim/contactor--find-dirty))
        (exported 0)
        (skipped 0))
    (dolist (contact-info dirty)
      (message "Dirty: %s" contact-info)
      (goto-char (plist-get contact-info :marker))
      (condition-case err
          (progn
            (bergheim/contactor-export-at-point)
            (setq exported (1+ exported)))
        (error 
         (message "Failed to export %s: %s" 
                  (plist-get contact-info :name) err)
         (setq skipped (1+ skipped)))))
    (message "Exported %d contacts, skipped %d" exported skipped)))

(defun bergheim/contactor-sync ()
  "Bidirectional sync: import from vCards, then export dirty org contacts."
  (interactive)
  (message "Importing from vCards...")
  (condition-case err
      (bergheim/contactor-impo
       bergheim/contactor-sync-directory
       (expand-file-name "contacts.org" org-directory))
    (user-error 
     (message "Import cancelled: %s" (error-message-string err))))
  
  (message "Exporting modified contacts...")
  (with-current-buffer (find-file-noselect 
                        (expand-file-name "contacts.org" org-directory))
    (bergheim/contactor-export-dirty)
    (save-buffer))
  (message "Sync complete"))


(defun bergheim/contactor--find-by-uid (uid)
  "Find org heading with matching UID property. Returns marker or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((pos (org-find-property "UID" uid)))
      (when pos
        (goto-char pos)
        (point-marker)))))

(defun bergheim/contactor--vcard-encode-value (value)
  "Encode value for vCard output - reverse of decode."
  (setq value (replace-regexp-in-string "\\\\" "\\\\\\\\" value nil t))
  (setq value (replace-regexp-in-string "," "\\\\," value))
  (setq value (replace-regexp-in-string ";" "\\\\;" value))
  value)

(defun bergheim/contactor-export-at-point ()
  "Export contact at point to vCard file in vdirsyncer directory.
Only writes if content has changed."
  (interactive)
  (let* ((contact (bergheim/contactor--org-to-contact))
         (uid (assoc-default "UID" contact))
         (name (assoc-default "FN" contact)))
    (unless uid
      (user-error "Contact has no UID"))
    (let* ((vcard-dir (expand-file-name bergheim/contactor-sync-directory))
           (vcard-file (bergheim/contactor--find-vcard-by-uid vcard-dir uid)))
      
      (unless vcard-file
        (user-error "No vCard file found for UID: %s" uid))
      
      ;; Parse existing vCard file
      (let ((vcard-contact (with-temp-buffer
                             (insert-file-contents vcard-file)
                             (bergheim/contactor--vcard-to-contact (buffer-string)))))
        
        ;; Check for external changes (REV mismatch)
        (let ((org-rev (assoc-default "REV" contact))
              (vcard-rev (assoc-default "REV" vcard-contact)))
          (when (and org-rev vcard-rev (not (equal org-rev vcard-rev)))
            (unless (yes-or-no-p (format "vCard file for %s has different REV (external change?). Really overwrite? " name))
              (user-error "Export cancelled"))))

        ;; we don't care if it updates remote - we still tried to export
        (org-back-to-heading t)
        (org-set-property "LAST_EXPORTED" (bergheim/contactor--timestamp-now-org))
        ;; Check if content actually changed
        (if (bergheim/contactor--changed-p contact vcard-contact)
            (progn
              ;; Update REV timestamp
              (let* ((new-rev (bergheim/contactor--timestamp-now-vcard))
                     (updated-contact (bergheim/contactor--set-field contact "REV" new-rev)))
                ;; Write to vCard file
                (let ((vcard-string (bergheim/contactor--contact-to-vcard updated-contact)))
                  (with-temp-file vcard-file
                    (insert vcard-string)))
                ;; Update org entry completely (PROPERTIES + VCARD with new REV)
                (bergheim/contactor--update-org-heading updated-contact)

                (when bergheim/contactor-auto-commit
                  (bergheim/contactor--git-commit name vcard-file))
                (message "Exported %s to %s (updated)" name vcard-file))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Export: org → vCard tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Export: org → vCard implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bergheim/contactor--update-metadata (&rest args)
  "Update top-level sync metadata in current org buffer.
ARGS is a plist with keys :last-synced, :last-export, :last-modified, :contact-count."
  (save-excursion
    (let ((fields (list (cons "LAST_SYNCED" (plist-get args :last-synced))
                        (cons "LAST_EXPORT" (plist-get args :last-export))
                        (cons "LAST_MODIFIED" (plist-get args :last-modified))
                        (cons "CONTACT_COUNT" (when-let* ((count (plist-get args :contact-count)))
                                                (format "%d" count))))))
      (dolist (field fields)
        (let ((keyword (car field))
              (value (cdr field)))
          (when value
            (goto-char (point-min))
            (if (re-search-forward (format "^#\\+%s: \\(.*\\)$" keyword) nil t)
                ;; Update existing
                (replace-match value nil nil nil 1)
              ;; Insert new - find TITLE or beginning
              (goto-char (point-min))
              (let ((insert-point
                     (if (re-search-forward "^#\\+TITLE:.*$" nil t)
                         (line-beginning-position 2)  ; Start of next line
                       (point-min))))
                (goto-char insert-point)
                (insert (format "#+%s: %s\n" keyword value))))))))))

(defun bergheim/contactor--mark-modified ()
  "Mark current contact as modified with timestamp."
  (org-back-to-heading t)
  (org-set-property "LAST_MODIFIED" (bergheim/contactor--timestamp-now-org)))

(defun bergheim/contactor--find-dirty ()
  "Find all contacts that need export (modified since last export).
Returns list of plists with :uid, :name, :marker."
  (let ((dirty '()))
    (org-map-entries
     (lambda ()
       (let* ((uid (org-entry-get nil "UID"))
              (last-modified (org-entry-get nil "LAST_MODIFIED"))
              (last-exported (org-entry-get nil "LAST_EXPORTED"))
              (name (org-get-heading t t t t)))
         (when (and uid
                    (or (not last-exported)
                        (and last-modified
                             (org-time> last-modified last-exported))))
           (push (list :uid uid 
                       :name name 
                       :marker (point-marker))
                 dirty))))
     nil nil)
    (message "Dirty %s" dirty)
    (nreverse dirty)))

(defun bergheim/contactor--export-precheck ()
  "Run safety checks before export. Returns plist with :safe and :reason."
  (save-excursion
    (goto-char (point-min))
    (let* ((last-synced (when (re-search-forward "^#\\+LAST_SYNCED: \\(.*\\)$" nil t)
                          (match-string 1)))
           (stored-count (when (progn (goto-char (point-min))
                                      (re-search-forward "^#\\+CONTACT_COUNT: \\([0-9]+\\)$" nil t))
                           (string-to-number (match-string 1))))
           (current-count 0))
      
      ;; Count current contacts
      (org-map-entries
       (lambda ()
         (when (org-entry-get nil "UID")
           (setq current-count (1+ current-count))))
       nil nil)
      
      (cond
       ;; Never synced
       ((not last-synced)
        (list :safe nil :reason "No LAST_SYNCED found - have you imported contacts yet?"))
       
       ;; Sync too old (>7 days)
       ((org-time< last-synced 
                   (format-time-string bergheim/contactor--timestamp-format-org
                                       (time-subtract (current-time) (days-to-time 7))))
        (list :safe nil :reason (format "LAST_SYNCED is old (%s) - sync first?" last-synced)))
       
       ;; Too few contacts (< 5 or < 50% of stored)
       ((or (< current-count 5)
            (and stored-count (< current-count (* stored-count 0.5))))
        (list :safe nil 
              :reason (format "Contact count suspicious: was %d, now %d" 
                              (or stored-count 0) current-count)))
       
       ;; All good
       (t (list :safe t :current-count current-count))))))

(defun bergheim/contactor--update-top-modified ()
  "Update top-level LAST_MODIFIED to most recent contact modification."
  (let ((max-modified nil))
    (org-map-entries
     (lambda ()
       (let ((modified (org-entry-get nil "LAST_MODIFIED")))
         (when (and modified 
                    (or (not max-modified)
                        (org-time> modified max-modified)))
           (setq max-modified modified))))
     nil nil)
    
    (when max-modified
      (bergheim/contactor--update-metadata :last-modified max-modified))))

(defun bergheim/contactor--find-vcard-by-uid (vcard-dir uid)
  "Find vCard file in VCARD-DIR containing UID.
Searches directly in vcard-dir, not subdirectories."
  (let ((result nil))
    (dolist (vcard-file (directory-files vcard-dir t "\\.vcf$"))
      (when (and (not result)
                 (with-temp-buffer
                   (insert-file-contents vcard-file)
                   (goto-char (point-min))
                   (re-search-forward (format "^UID:%s$" (regexp-quote uid)) nil t)))
        (setq result vcard-file)))
    result))

(defun bergheim/contactor--changed-p (contact1 contact2)
  "Compare two contacts, ignoring REV timestamp and field order.
Returns t if they differ."
  (let* ((c1-no-rev (seq-remove (lambda (f) (equal (car f) "REV")) contact1))
         (c2-no-rev (seq-remove (lambda (f) (equal (car f) "REV")) contact2))
         ;; Sort both by field name for comparison
         (c1-sorted (sort (copy-sequence c1-no-rev) 
                          (lambda (a b) (string< (car a) (car b)))))
         (c2-sorted (sort (copy-sequence c2-no-rev) 
                          (lambda (a b) (string< (car a) (car b))))))
    (not (equal c1-sorted c2-sorted))))

(defun bergheim/contactor--has-conflict-p (marker new-contact)
  "Check if importing NEW-CONTACT would conflict with org contact at MARKER.
Returns t if contacts differ (prompting user before overwrite)."
  (save-excursion
    (goto-char marker)
    (let ((org-contact (bergheim/contactor--org-to-contact)))
      (bergheim/contactor--changed-p org-contact new-contact))))










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






(defun bergheim/contactor--vcard-to-contact (vcard-string)
  "Parse VCARD-STRING to ordered alist of (FIELD . VALUE) pairs.
Values stored raw (with any escaping intact)."
  (with-temp-buffer
    (insert vcard-string)
    (goto-char (point-min))
    (let ((fields '()))
      (while (re-search-forward "^\\([A-Z0-9.]+\\)\\(;[^:]*\\)?:\\(.*\\)$" nil t)
        (let ((field (match-string 1))
              (params (match-string 2))
              (value (match-string 3)))
          ;; Handle continuation lines
          (forward-line)
          (while (and (not (eobp)) (looking-at "^ \\(.*\\)$"))
            (setq value (concat value (match-string 1)))
            (forward-line))
          ;; Only normalize line endings - NO OTHER DECODING
          (setq value (replace-regexp-in-string "\r" "" value))
          (push (cons (concat field (or params "")) value) fields)))
      (nreverse fields))))


(defun bergheim/contactor--contact-to-vcard (contact)
  "Convert CONTACT alist to vCard string.
Values written raw (no encoding)."
  (let ((lines '()))
    (dolist (field contact)
      (let ((key (car field))
            (value (cdr field)))
        ;; NO ENCODING - write as-is
        (push (concat key ":" value) lines)))
    (concat (string-join (nreverse lines) "\n") "\n")))

(defun bergheim/contactor--vcard-drawer ()
  "Get contents of VCARD drawer at current heading.
Returns the full multi-line content as string."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (or (outline-next-heading) (point-max)))))
      (when (re-search-forward "^[ \t]*:VCARD:[ \t]*$" end t)
        (forward-line)
        (let ((drawer-start (point)))
          (when (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
            (beginning-of-line)
            (string-trim (buffer-substring-no-properties drawer-start (point)))))))))

(defun bergheim/contactor--org-to-contact ()
  "Extract vCard contact representation from org heading at point.
Returns ordered alist of (FIELD . VALUE) pairs.
VCARD drawer is authoritative, properties override specific fields."
  (save-excursion
    (org-back-to-heading t)
    (let* ((heading (org-get-heading t t t t))
           (uid (org-entry-get nil "UID"))
           (email (org-entry-get nil "EMAIL"))
           (birthday (org-entry-get nil "BIRTHDAY"))
           (note (org-entry-get nil "NOTE"))
           (vcard-drawer (bergheim/contactor--vcard-drawer))
           ;; Parse VCARD - keep ALL fields
           (contact (if vcard-drawer
                        (bergheim/contactor--vcard-to-contact
                         (concat "BEGIN:VCARD\n" vcard-drawer "\nEND:VCARD"))
                      '(("BEGIN" . "VCARD")
                        ("VERSION" . "3.0")
                        ("END" . "VCARD")))))
      
      ;; Override specific fields from properties (in place, preserving position)
      (when heading
        (setq contact (bergheim/contactor--set-field contact "FN" heading)))
      
      (when uid
        (setq contact (bergheim/contactor--set-field contact "UID" uid)))
      
      (when email
        (setq contact (bergheim/contactor--set-field contact "EMAIL" email)))
      
      (when birthday
        (let ((bday (bergheim/contactor--org-bday-to-vcard birthday)))
          (when bday
            (setq contact (bergheim/contactor--set-field contact "BDAY" bday)))))
      
      (when note
        (setq contact (bergheim/contactor--set-field contact "NOTE" note)))
      
      contact)))

(defun bergheim/contactor--set-field (contact field value)
  "Set FIELD to VALUE in CONTACT alist.
If field exists, replace it. Otherwise add after VERSION, before END."
  (let* ((without-end (seq-remove (lambda (f) (equal (car f) "END")) contact))
         (end-pair (cons "END" "VCARD"))
         (existing (assoc field without-end)))
    (if existing
        ;; Replace existing field
        (append (mapcar (lambda (f)
                          (if (equal (car f) field)
                              (cons field value)
                            f))
                        without-end)
                (list end-pair))
      ;; Add new field after VERSION
      (let ((result '())
            (added nil))
        (dolist (f without-end)
          (push f result)
          (when (and (not added) (equal (car f) "VERSION"))
            (push (cons field value) result)
            (setq added t)))
        (append (nreverse result) (list end-pair))))))

(defun bergheim/contactor--org-bday-to-vcard (timestamp)
  "Convert org TIMESTAMP to vCard BDAY format.
<1980-12-25 +1y> → 19801225
<12-25 +1y> → --1225"
  (when (string-match "<\\(?:\\([0-9]\\{4\\}\\)-\\)?\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" timestamp)
    (let ((year (match-string 1 timestamp))
          (month (match-string 2 timestamp))
          (day (match-string 3 timestamp)))
      (if year
          (concat year month day)
        (concat "--" month day)))))

(defun bergheim/contactor--generate-id (contact)
  "Generate stable ID for contact from name.
Format: contact-firstname-lastname
Appends -2, -3, etc. if collision detected."
  (if-let* ((fn (assoc-default "FN" contact))
            (sanitized (downcase (bergheim/~generate-sanitized-alnum-dash-string fn)))
            (base-id (concat "contact-" sanitized)))
      (let ((id base-id)
            (counter 2))
        ;; Check for collisions in org file
        (while (with-current-buffer (find-file-noselect bergheim/contactor-file)
                 (org-find-property "ID" id))
          (setq id (format "%s-%d" base-id counter)
                counter (1+ counter)))
        id)
    ;; Fallback to UID if name is missing/weird
    (concat "contact-" (assoc-default "UID" contact))))

(defun bergheim/contactor--contact-to-org (contact)
  "Format CONTACT alist as org heading string.
Decides what goes to PROPERTIES vs VCARD."
  (let* ((fn (assoc-default "FN" contact))
         (uid (assoc-default "UID" contact))
         (id (bergheim/contactor--generate-id contact))
         (email (assoc-default "EMAIL" contact))
         (bday (assoc-default "BDAY" contact))
         (note (assoc-default "NOTE" contact))
         ;; Build raw VCARD content - just key:value, no encoding
         (vcard-content (string-join
                         (mapcar (lambda (field)
                                   (let ((key (car field))
                                         (value (cdr field)))
                                     (unless (member key '("BEGIN" "END"))
                                       (concat key ":" value))))
                                 contact)
                         "\n"))
         ;; Remove empty lines
         (vcard-content (string-join
                         (seq-filter (lambda (line) (not (string-empty-p line)))
                                     (split-string vcard-content "\n"))
                         "\n")))
    (with-temp-buffer
      (org-mode)
      (concat
       "* " fn "\n"
       ":PROPERTIES:\n"
       (format ":ID: %s\n" id)
       (when uid (format ":UID: %s\n" uid))
       (when email (format ":EMAIL: %s\n" email))
       (when bday 
         (format ":BIRTHDAY: <%s +1y>\n" 
                 (bergheim/contactor--vcard-bday-to-org bday)))
       (when note (format ":NOTE: %s\n" note))
       ":END:\n"
       ":VCARD:\n"
       vcard-content "\n"
       ":END:\n\n"))))

(defun bergheim/contactor--vcard-bday-to-org (bday)
  "Convert vCard BDAY to org timestamp format.
19801225 → 1980-12-25
--1225 → 12-25"
  (cond
   ;; YYYYMMDD format
   ((string-match "^\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)$" bday)
    (format "%s-%s-%s" 
            (match-string 1 bday)
            (match-string 2 bday)
            (match-string 3 bday)))
   ;; --MMDD format
   ((string-match "^--\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)$" bday)
    (format "%s-%s"
            (match-string 1 bday)
            (match-string 2 bday)))
   ;; Already in right format
   (t bday)))

(defun bergheim/contactor--update-org-heading (contact)
  "Update org heading at point from CONTACT, preserve
 body content and extra VCARD fields."
  (save-excursion
    (org-back-to-heading t)
    (let* ((fn (assoc-default "FN" contact))
           (uid (assoc-default "UID" contact))
           (email (assoc-default "EMAIL" contact))
           (bday (assoc-default "BDAY" contact))
           (note (assoc-default "NOTE" contact))
           (rev (assoc-default "REV" contact))
           (heading-start (point))
           ;; Read existing VCARD to preserve extra fields
           (existing-vcard (bergheim/contactor--vcard-drawer))
           (existing-contact (when existing-vcard
                               (bergheim/contactor--vcard-to-contact
                                (concat "BEGIN:VCARD\n" existing-vcard "\nEND:VCARD"))))
           ;; Merge: keep fields from existing that aren't in new contact
           (merged-contact (if existing-contact
                               (let ((result contact))
                                 (dolist (field existing-contact)
                                   (let ((key (car field)))
                                     ;; If field exists in existing but not in new, add it
                                     (unless (assoc key contact)
                                       (setq result (append result (list field))))))
                                 result)
                             contact))
           (section-end (save-excursion
                          (or (outline-next-heading) (point-max)))))
      
      ;; Update heading text
      (when (looking-at org-complex-heading-regexp)
        (replace-match fn nil nil nil 4))
      
      ;; Update all properties from heading start
      (goto-char heading-start)
      (when uid (org-set-property "UID" uid))
      (goto-char heading-start)
      (if email
          (org-set-property "EMAIL" email)
        (org-delete-property "EMAIL"))
      (goto-char heading-start)
      (if bday
          (org-set-property "BIRTHDAY" 
                            (format "<%s +1y>" 
                                    (bergheim/contactor--vcard-bday-to-org bday)))
        (org-delete-property "BIRTHDAY"))
      (goto-char heading-start)
      (if note
          (org-set-property "NOTE" note)
        (org-delete-property "NOTE"))
      
      ;; Re-calculate section-end after property changes
      (goto-char heading-start)
      (setq section-end (save-excursion
                          (outline-end-of-subtree)
                          (point)))
      
      ;; Handle VCARD drawer - delete old one
      (goto-char heading-start)
      (when (re-search-forward "^[ \t]*:VCARD:[ \t]*$" section-end t)
        (let ((drawer-start (line-beginning-position)))
          (when (re-search-forward "^[ \t]*:END:[ \t]*$" section-end t)
            (delete-region drawer-start (line-beginning-position 2)))))
      
      ;; Insert new VCARD after properties drawer
      (goto-char heading-start)
      (forward-line 1)
      (when (looking-at org-property-drawer-re)
        (goto-char (match-end 0))
        (unless (bolp) (insert "\n")))
      
      ;; Build VCARD content from merged contact
      (let* ((vcard-content (string-join
                             (mapcar (lambda (field)
                                       (let ((key (car field))
                                             (value (cdr field)))
                                         (unless (member key '("BEGIN" "END"))
                                           (concat key ":" value))))
                                     merged-contact)
                             "\n"))
             (vcard-content-clean (string-join
                                   (seq-filter (lambda (line) (not (string-empty-p line)))
                                               (split-string vcard-content "\n"))
                                   "\n")))
        (insert ":VCARD:\n" vcard-content-clean "\n:END:"))
      
      ;; Ensure exactly one blank line after this entry
      (goto-char heading-start)
      (outline-end-of-subtree)
      (skip-chars-backward " \t\n")
      (delete-region (point) (save-excursion (outline-end-of-subtree) (point)))
      (insert "\n"))))

(defun bergheim/contactor--on-property-changed (property value)
  "Hook to update LAST_MODIFIED when contact properties change."
  (when (and (derived-mode-p 'org-mode)
             (org-entry-get nil "UID")
             (not (member property '("LAST_MODIFIED" "LAST_EXPORTED"))))
    (org-set-property "LAST_MODIFIED" (bergheim/contactor--timestamp-now-org))))

(add-hook 'org-property-changed-functions 
          'bergheim/contactor--on-property-changed)

(defun bergheim/contactor--maybe-export-on-save ()
  "Export contact at point if in contacts file and on a contact heading."
  (when (and (buffer-file-name)
             (string= (buffer-file-name) bergheim/contactor-file)
             (org-entry-get nil "UID"))
    (save-excursion
      (bergheim/contactor-export-at-point))))

(defun bergheim/contactor--setup-auto-export ()
  (when (and buffer-file-name
             (string= buffer-file-name bergheim/contactor-file))
    (add-hook 'after-save-hook #'bergheim/contactor--maybe-export-on-save nil t)))

(add-hook 'org-mode-hook #'bergheim/contactor--setup-auto-export)

(defun bergheim/contactor--git-commit (name vcard-file)
  "Commit changes to VCARD-FILE if in a git repo and auto-commit is enabled."
  (when bergheim/contactor-auto-commit
    (let* ((default-directory (file-name-directory vcard-file))
           (relative-path (file-name-nondirectory vcard-file)))
      (when (file-directory-p (expand-file-name ".git" default-directory))
        (shell-command (format "git add %s && git commit -m 'Update contact: %s'" 
                               (shell-quote-argument relative-path)
                               name))))))

(defun bergheim/contactor-completion-at-point ()
  "Completion-at-point function for contacts."
  (when (and (derived-mode-p 'org-mode)
             (looking-back "@\\([a-zA-Z0-9-]*\\)" (line-beginning-position)))
    (let* ((bounds (cons (match-beginning 1) (match-end 1)))
           (contacts (with-current-buffer (find-file-noselect bergheim/contactor-file)
                       (org-map-entries 
                        (lambda () 
                          (when-let* ((name (org-get-heading t t t t))
                                      (id (or (org-entry-get nil "ID")
                                              (org-id-get-create))))
                            (propertize name 'contact-id id)))
                        nil nil))))
      (list (car bounds) 
            (cdr bounds)
            contacts
            :exit-function
            (lambda (name _status)
              ;; The just-inserted candidate occupies [start,end]. cape-capf-trigger
              ;; usually consumes the leading "@" itself, but if it's still there,
              ;; pull it in too — and ONLY if it's literally an "@", so we can never
              ;; delete a preceding newline (the old (1- start) bug).
              (let* ((end (point))
                     (start (- end (length name))))
                (when (and (> start (point-min))
                           (eq (char-before start) ?@))
                  (setq start (1- start)))
                (delete-region start end)
                (insert (format "[[id:%s][%s]]"
                                (get-text-property 0 'contact-id name)
                                name))))))))

(defun bergheim/contactor-insert-link ()
  "Insert org-id link to contact with completion."
  (interactive)
  (let* ((contacts (with-current-buffer (find-file-noselect bergheim/contactor-file)
                     (org-map-entries 
                      (lambda () 
                        (when-let* ((name (org-get-heading t t t t))
                                    (id (or (org-entry-get nil "ID")
                                            (org-id-get-create))))
                          (cons name id)))
                      nil nil)))
         (choice (completing-read "Contact: " contacts nil t))
         (id (alist-get choice contacts nil nil #'equal)))
    (insert (format " [[id:%s][%s]] " id choice))))

;; use vertico for all
;; (defun bergheim/contactor-goto-backlink ()
;;   "Jump to a backlink of any contact with completion."
;;   (interactive)
;;   (let* ((contacts (with-current-buffer (find-file-noselect bergheim/contactor-file)
;;                      (org-map-entries 
;;                       (lambda () 
;;                         (when-let* ((name (org-get-heading t t t t))
;;                                    (id (org-entry-get nil "ID")))
;;                           (cons name id)))
;;                       nil nil)))
;;          (contact-choice (completing-read "Contact: " contacts nil t))
;;          (id (alist-get contact-choice contacts nil nil #'equal))
;;          (backlinks (org-ql-select (org-agenda-files)
;;                       `(link :target ,id)
;;                       :action '(cons (org-get-heading t t t t) (point-marker)))))
;;     (if backlinks
;;         (let* ((backlink-choice (completing-read "Backlink: " backlinks nil t))
;;                (marker (alist-get backlink-choice backlinks nil nil #'equal)))
;;           (org-goto-marker-or-bmk marker))
;;       (message "No backlinks found for %s" contact-choice))))

(defun bergheim/contactor-goto-backlink ()
  "Show backlinks for a contact with org-ql presentation."
  (interactive)
  (let* ((contacts (with-current-buffer (find-file-noselect bergheim/contactor-file)
                     (org-map-entries 
                      (lambda () 
                        (when-let* ((name (org-get-heading t t t t))
                                    (id (org-entry-get nil "ID")))
                          (cons name id)))
                      nil nil)))
         (contact-choice (completing-read "Contact: " contacts nil t))
         (id (alist-get contact-choice contacts nil nil #'equal)))
    (org-ql-search (org-agenda-files)
      `(link :target ,id)
      :title (format "Backlinks to %s" contact-choice)
      :sort 'date
      :super-groups '((:auto-parent t)))))

(defun bergheim/contactor-compose-mail ()
  "Compose email to contact."
  (interactive)
  (let* ((contacts (with-current-buffer (find-file-noselect bergheim/contactor-file)
                     (org-map-entries 
                      (lambda () 
                        (when-let* ((name (org-get-heading t t t t))
                                    (email (org-entry-get nil "EMAIL")))
                          (format "%s <%s>" name email)))
                      "EMAIL<>\"\""  ;; only contacts without an empty EMAIL
                      nil)))
         (choice (completing-read "Mail to: " contacts nil t))
         (email (alist-get choice contacts nil nil #'equal)))
    (mu4e-compose-new)
    (message-goto-to)
    (insert choice)
    (message-goto-subject)))

(defun bergheim/contactor-find-for-capture ()
  "Prompt for contact and position point for capture."
  (let* ((contacts (with-current-buffer (find-file-noselect bergheim/contactor-file)
                     (org-map-entries 
                      (lambda () 
                        (when-let* ((name (org-get-heading t t t t))
                                    (id (org-entry-get nil "ID")))
                          (cons name id)))
                      nil nil)))
         (choice (completing-read "Contact: " contacts nil t))
         (id (alist-get choice contacts nil nil #'equal)))
    (find-file bergheim/contactor-file)
    (widen)
    (goto-char (org-find-property "ID" id))))

(defun bergheim/contactor-capture-todo ()
  "Capture TODO for a contact."
  (interactive)
  (org-capture nil "pc"))  ;; "pc" = personal contact
;; (let ((org-capture-clock-keep t))  ;; Don't mess with current clock
;;   (org-capture nil "pc")))  ;; "pc" = personal contact

(defun bergheim/contactor--setup-keybindings ()
  "Set up keybindings for contacts.org."
  (when (and buffer-file-name
             (string= buffer-file-name bergheim/contactor-file))
    (general-define-key
     :states '(normal)
     :keymaps 'local
     "g b" 'bergheim/contactor-goto-backlink)))

(add-hook 'org-mode-hook 'bergheim/contactor--setup-keybindings)

(provide 'contactor)
