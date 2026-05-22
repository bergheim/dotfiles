;;; contacts-test.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Thomas Bergheim

(ert-deftest bergheim/test-vcard-birthday ()
  "Test birthday parsing variations."
  (dolist (test '(("BDAY:19801225" "19801225")
                  ("BDAY:1980-12-25" "1980-12-25")
                  ("BDAY:--12-25" "--12-25")))
    (let ((vcard (format "BEGIN:VCARD
VERSION:3.0
FN:Test Person
UID:test-uid
%s
END:VCARD" (car test))))
      (let ((contact (bergheim/contactor--vcard-to-contact vcard)))
        (should (equal (assoc-default "BDAY" contact) (cadr test)))))))

(ert-deftest bergheim/test-vcard-multiple-phones ()
  "Test handling multiple phone numbers."
  (let ((vcard "BEGIN:VCARD
VERSION:3.0
FN:Test Person
UID:test-uid
TEL;TYPE=CELL:+1584073
TEL;TYPE=CELL:1584073
TEL;TYPE=HOME:555-1234
END:VCARD"))
    (let ((contact (bergheim/contactor--vcard-to-contact vcard)))
      ;; First TEL;TYPE=CELL should be returned by assoc-default
      (should (equal (assoc-default "TEL;TYPE=CELL" contact) "+1584073"))
      ;; But both should exist in the alist
      (should (= 2 (length (seq-filter (lambda (f) (equal (car f) "TEL;TYPE=CELL")) contact))))
      (should (string-match-p "TEL;TYPE=HOME:555-1234" 
                              (bergheim/contactor--contact-to-vcard contact))))))

(ert-deftest bergheim/test-vcard-structured-name ()
  "Test N field is preserved."
  (let ((vcard "BEGIN:VCARD
VERSION:3.0
FN:Øyvind Helsøm
N:Helsøm;Øyvind;;;
UID:test-uid
EMAIL:test@example.com
END:VCARD"))
    (let ((contact (bergheim/contactor--vcard-to-contact vcard)))
      (should (equal (assoc-default "FN" contact) "Øyvind Helsøm"))
      (should (equal (assoc-default "N" contact) "Helsøm;Øyvind;;;"))
      (should (equal (assoc-default "EMAIL" contact) "test@example.com")))))

(ert-deftest bergheim/test-vcard-real-world ()
  "Test parsing real-world vCard with many fields."
  (let ((vcard "BEGIN:VCARD
VERSION:3.0
PRODID:-//Thunderbird//EN
UID:0673b677-7130-4a27-bc54-67e239041874
CATEGORIES:coworkers,myContacts
FN:Jane Smith
N:Smith;Jane;;;
ITEM1.EMAIL:jane.smith@example.com
TEL;TYPE=CELL:+1234567890
REV:2025-07-25T20:02:28Z
END:VCARD"))
    (let ((contact (bergheim/contactor--vcard-to-contact vcard)))
      (should (equal (assoc-default "FN" contact) "Jane Smith"))
      (should (equal (assoc-default "ITEM1.EMAIL" contact) "jane.smith@example.com"))
      (should (equal (assoc-default "UID" contact) "0673b677-7130-4a27-bc54-67e239041874"))
      (should (equal (assoc-default "N" contact) "Smith;Jane;;;"))
      (should (equal (assoc-default "PRODID" contact) "-//Thunderbird//EN"))
      (should (equal (assoc-default "CATEGORIES" contact) "coworkers,myContacts"))
      (should (equal (assoc-default "TEL;TYPE=CELL" contact) "+1234567890"))
      (should (equal (assoc-default "REV" contact) "2025-07-25T20:02:28Z")))))

(ert-deftest bergheim/test-vcard-note-with-newlines ()
  "Test that NOTE with \\n stays encoded."
  (let ((vcard "BEGIN:VCARD
VERSION:3.0
FN:Test Person
UID:test-uid
NOTE:Line one\\nLine two\\nLine three
END:VCARD"))
    (let ((contact (bergheim/contactor--vcard-to-contact vcard)))
      (should (equal (assoc-default "NOTE" contact) "Line one\\nLine two\\nLine three")))))

(ert-deftest bergheim/test-find-contact-by-uid ()
  "Test finding contact by UID."
  (with-temp-buffer
    (org-mode)
    (insert "* Alice\n:PROPERTIES:\n:UID: uid-alice\n:END:\n\n")
    (insert "* Bob\n:PROPERTIES:\n:UID: uid-bob\n:END:\n\n")
    (goto-char (point-min))
    (let ((marker (bergheim/contactor--find-by-uid "uid-bob")))
      (should marker)
      (goto-char marker)
      (should (string= "Bob" (org-get-heading t t t t))))
    (should-not (bergheim/contactor--find-by-uid "uid-nonexistent"))))

(ert-deftest bergheim/test-update-removes-missing-properties ()
  "Test that nil properties get removed."
  (with-temp-buffer
    (org-mode)
    (insert "* Test Person\n")
    (insert ":PROPERTIES:\n")
    (insert ":UID: uid-123\n")
    (insert ":EMAIL: test@example.com\n")
    (insert ":BIRTHDAY: <1980-01-01 +1y>\n")
    (insert ":NOTE: Old note\n")
    (insert ":END:\n")
    
    (goto-char (point-min))
    (let ((contact '(("BEGIN" . "VCARD")
                     ("VERSION" . "3.0")
                     ("UID" . "uid-123")
                     ("FN" . "Test Person")
                     ("EMAIL" . "test@example.com")
                     ("END" . "VCARD"))))
      (bergheim/contactor--update-org-heading contact))
    
    (goto-char (point-min))
    (should (string= "test@example.com" (org-entry-get nil "EMAIL")))
    (should-not (org-entry-get nil "BIRTHDAY"))
    (should-not (org-entry-get nil "NOTE"))))

(ert-deftest bergheim/test-update-without-vcard-extra ()
  "Test updating contact that has no
 VCARD."
  (with-temp-buffer
    (org-mode)
    (insert "* Simple Contact\n")
    (insert ":PROPERTIES:\n")
    (insert ":UID: uid-simple\n")
    (insert ":EMAIL: simple@example.com\n")
    (insert ":END:\n")
    (insert "\n** Notes section\n")
    
    (goto-char (point-min))
    (let ((contact '(("BEGIN" . "VCARD")
                     ("VERSION" . "3.0")
                     ("UID" . "uid-simple")
                     ("FN" . "Simple Contact Updated")
                     ("EMAIL" . "updated@example.com")
                     ("END" . "VCARD"))))
      (bergheim/contactor--update-org-heading contact))
    
    (goto-char (point-min))
    (should (re-search-forward "^\\* Simple Contact Updated" nil t))
    (should (string= "updated@example.com" (org-entry-get nil "EMAIL")))
    (should (re-search-forward "^\\*\\* Notes section" nil t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;








(ert-deftest bergheim/test-merge-multiple-contacts ()
  "Test merging multiple contacts with updates and additions."
  (let ((temp-file (make-temp-file "contacts-test" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "#+TITLE: Contacts\n\n")
            (insert "* Alice\n:PROPERTIES:\n:UID: uid-alice\n:EMAIL: alice@old.com\n:END:\n")
            (insert ":VCARD:\n")
            (insert "VERSION:3.0\n")
            (insert "FN:Alice\n")
            (insert "EMAIL:alice@old.com\n")
            (insert ":END:\n")
            (insert "\n** Alice's notes\n")
            (insert "* Bob\n:PROPERTIES:\n:UID: uid-bob\n:EMAIL: bob@example.com\n:END:\n"))
          
          ;; Simulate importing: update Alice, keep Bob, add Charlie
          (with-current-buffer (find-file-noselect temp-file)
            (let ((contacts (list
                             '(("BEGIN" . "VCARD")
                               ("VERSION" . "3.0")
                               ("UID" . "uid-alice")
                               ("FN" . "Alice Updated")
                               ("EMAIL" . "alice@new.com")
                               ("END" . "VCARD"))
                             '(("BEGIN" . "VCARD")
                               ("VERSION" . "3.0")
                               ("UID" . "uid-bob")
                               ("FN" . "Bob")
                               ("EMAIL" . "bob@example.com")
                               ("END" . "VCARD"))
                             '(("BEGIN" . "VCARD")
                               ("VERSION" . "3.0")
                               ("UID" . "uid-charlie")
                               ("FN" . "Charlie")
                               ("EMAIL" . "charlie@example.com")
                               ("END" . "VCARD")))))
              (dolist (contact contacts)
                (let* ((uid (assoc-default "UID" contact))
                       (marker (when uid (bergheim/contactor--find-by-uid uid))))
                  (if marker
                      (progn
                        (goto-char marker)
                        (bergheim/contactor--update-org-heading contact))
                    (goto-char (point-max))
                    (unless (bolp) (insert "\n"))
                    (insert (string-trim-right (bergheim/contactor--contact-to-org contact)))
                    (insert "\n"))))
              (save-buffer))
            
            ;; Verify results
            (goto-char (point-min))
            (should (re-search-forward "^\\* Alice Updated" nil t))
            (should (string= "alice@new.com" (org-entry-get nil "EMAIL")))
            (should (re-search-forward "^\\*\\* Alice's notes" nil t))
            
            (goto-char (point-min))
            (should (re-search-forward "^\\* Bob" nil t))
            
            (goto-char (point-min))
            (should (re-search-forward "^\\* Charlie" nil t))
            (should (string= "charlie@example.com" (org-entry-get nil "EMAIL")))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest bergheim/test-vcard-full-update-cycle ()
  "Test complete vCard update: parse, add body, update vCard, verify changes."
  (let ((vcard-v1 "BEGIN:VCARD
VERSION:3.0
PRODID:-//Thunderbird//EN
UID:test-uid-12345
FN:John Doe
N:Doe;John;;;
BDAY:19800101
TEL;TYPE=CELL:555-1234
EMAIL:john@example.com
END:VCARD")
        (vcard-v2 "BEGIN:VCARD
VERSION:3.0
PRODID:-//Thunderbird//EN
UID:test-uid-12345
FN:Jane Smith
N:Smith;Jane;;;
BDAY:19900202
TEL;TYPE=CELL:555-9999
EMAIL:jane@example.com
END:VCARD"))
    
    (with-temp-buffer
      (org-mode)
      
      ;; Step 1-2: Parse first vCard and insert
      (let ((contact-v1 (bergheim/contactor--vcard-to-contact vcard-v1)))
        (insert (bergheim/contactor--contact-to-org contact-v1)))
      
      ;; Step 3: Add body text
      (goto-char (point-min))
      (re-search-forward "^:END:")
      (forward-line)
      (insert "\n** Personal notes\n")
      (insert "Met at conference 2023.\n")
      (insert "\n** TODO Follow up\n")
      
      ;; Verify initial state
      (goto-char (point-min))
      (should (re-search-forward "^\\* John Doe" nil t))
      (should (string= "<1980-01-01 +1y>" (org-entry-get nil "BIRTHDAY")))
      (should (string-match-p "555-1234" (buffer-string)))
      
      ;; Step 4-5: Update with second vCard
      (goto-char (point-min))
      (let* ((contact-v2 (bergheim/contactor--vcard-to-contact vcard-v2))
             (marker (bergheim/contactor--find-by-uid "test-uid-12345")))
        (should marker)
        (goto-char marker)
        (bergheim/contactor--update-org-heading contact-v2))
      
      ;; Verify updates
      (goto-char (point-min))
      (should (re-search-forward "^\\* Jane Smith" nil t))
      (should (string= "jane@example.com" (org-entry-get nil "EMAIL")))
      (should (string= "<1990-02-02 +1y>" (org-entry-get nil "BIRTHDAY")))
      
      ;; Verify VCARD has new phone
      (goto-char (point-min))
      (should (re-search-forward "TEL;TYPE=CELL:555-9999" nil t))
      
      ;; Search only in the actual contact entry, not the whole buffer
      (goto-char (point-min))
      (re-search-forward "^\\* ")
      (let ((entry-start (line-beginning-position))
            (entry-end (or (save-excursion (outline-next-heading)) (point-max))))
        (goto-char entry-start)
        (should-not (re-search-forward "555-1234" entry-end t)))
      
      ;; Verify body preserved
      (goto-char (point-min))
      (should (re-search-forward "^\\*\\* Personal notes" nil t))
      (should (re-search-forward "Met at conference 2023" nil t))
      (should (re-search-forward "^\\*\\* TODO Follow up" nil t)))))

(ert-deftest bergheim/test-vcard-sync-to-org-idempotent ()
  "Test that syncing multiple times doesn't add extra newlines."
  (let ((temp-file (make-temp-file "contacts-test" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "#+TITLE: Contacts\n\n"))
          
          ;; Add two contacts
          (with-current-buffer (find-file-noselect temp-file)
            (let ((contacts (list

                             '(("BEGIN" . "VCARD")
                               ("VERSION" . "3.0")
                               ("UID" . "uid-alice")
                               ("FN" . "Alice")
                               ("EMAIL" . "alice@example.com")
                               ("TEL;TYPE=CELL" . "555-1111")
                               ("END" . "VCARD"))
                             '(("BEGIN" . "VCARD")
                               ("VERSION" . "3.0")
                               ("UID" . "uid-bob")
                               ("FN" . "Bob")
                               ("EMAIL" . "bob@example.com")
                               ("TEL;TYPE=CELL" . "555-2222")
                               ("END" . "VCARD")))))
              (dolist (contact contacts)
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))
                (insert (string-trim-right (bergheim/contactor--contact-to-org contact)))
                (insert "\n")))
            (save-buffer))
          
          ;; First update - establishes proper formatting
          (with-current-buffer (find-file-noselect temp-file)
            (dolist (uid '("uid-alice" "uid-bob"))
              (let* ((marker (bergheim/contactor--find-by-uid uid))
                     (contact (save-excursion
                                (goto-char marker)
                                (bergheim/contactor--org-to-contact))))
                (goto-char marker)
                (bergheim/contactor--update-org-heading contact)))
            (save-buffer))
          
          (let ((after-first-update (with-temp-buffer
                                      (insert-file-contents temp-file)
                                      (buffer-string))))
            
            ;; Second update - should be idempotent
            (with-current-buffer (find-file-noselect temp-file)
              (dolist (uid '("uid-alice" "uid-bob"))
                (let* ((marker (bergheim/contactor--find-by-uid uid))
                       (contact (save-excursion
                                  (goto-char marker)
                                  (bergheim/contactor--org-to-contact))))
                  (goto-char marker)
                  (bergheim/contactor--update-org-heading contact)))
              (save-buffer))
            
            (let ((after-second-update (with-temp-buffer
                                         (insert-file-contents temp-file)
                                         (buffer-string))))
              
              ;; Second and subsequent updates should be identical
              (should (string= after-first-update after-second-update)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest bergheim/test-vcard-sorting-preserved ()
  "Test that contacts stay sorted after updates."
  (let ((temp-file (make-temp-file "contacts-test" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "#+TITLE: Contacts\n\n"))
          
          ;; Add contacts in random order
          (with-current-buffer (find-file-noselect temp-file)
            (let ((contacts (list
                             '(("BEGIN" . "VCARD")
                               ("VERSION" . "3.0")
                               ("UID" . "uid-zoe")
                               ("FN" . "Zoe")
                               ("EMAIL" . "zoe@example.com")
                               ("END" . "VCARD"))
                             '(("BEGIN" . "VCARD")
                               ("VERSION" . "3.0")
                               ("UID" . "uid-alice")
                               ("FN" . "Alice")
                               ("EMAIL" . "alice@example.com")
                               ("END" . "VCARD"))
                             '(("BEGIN" . "VCARD")
                               ("VERSION" . "3.0")
                               ("UID" . "uid-mike")
                               ("FN" . "Mike")
                               ("EMAIL" . "mike@example.com")
                               ("END" . "VCARD")))))
              (dolist (contact contacts)
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))
                (insert (string-trim-right (bergheim/contactor--contact-to-org contact)))
                (insert "\n")))
            
            ;; Sort - skip to first heading
            (goto-char (point-min))
            (re-search-forward "^\\* ")
            (goto-char (line-beginning-position))
            (sort-subr nil 
                       'outline-next-heading
                       'outline-end-of-subtree
                       (lambda ()
                         (when (looking-at org-complex-heading-regexp)
                           (match-string-no-properties 4))))
            (save-buffer))
          
          ;; Verify sorted order
          (with-current-buffer (find-file-noselect temp-file)
            (goto-char (point-min))
            (should (re-search-forward "^\\* Alice" nil t))
            (should (re-search-forward "^\\* Mike" nil t))
            (should (re-search-forward "^\\* Zoe" nil t))
            
            ;; Update middle contact
            (goto-char (point-min))
            (let ((marker (bergheim/contactor--find-by-uid "uid-mike")))
              (goto-char marker)
              (let ((contact '(("BEGIN" . "VCARD")
                               ("VERSION" . "3.0")
                               ("UID" . "uid-mike")
                               ("FN" . "Mike Updated")
                               ("EMAIL" . "mike.new
@example.com")
                               ("END" . "VCARD"))))
                (bergheim/contactor--update-org-heading contact)))
            (save-buffer))
          
          ;; Verify order still correct after update
          (with-current-buffer (find-file-noselect temp-file)
            (goto-char (point-min))
            (should (re-search-forward "^\\* Alice" nil t))
            (let ((alice-pos (point)))
              (should (re-search-forward "^\\* Mike Updated" nil t))
              (let ((mike-pos (point)))
                (should (< alice-pos mike-pos))
                (should (re-search-forward "^\\* Zoe" nil t))
                (should (< mike-pos (point)))))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest bergheim/test-vcard-multiple-emails ()
  "Test handling multiple email addresses - first to property, rest to VCARD."
  (let ((vcard "BEGIN:VCARD
VERSION:3.0
FN:Test Person
EMAIL;TYPE=WORK:work@example.com
EMAIL;TYPE=HOME:home@example.com
EMAIL;TYPE=OTHER:other@example.com
END:VCARD"))
    (let ((contact (bergheim/contactor--vcard-to-contact vcard)))
      ;; First email should be in first position
      (should (equal (assoc-default "EMAIL;TYPE=WORK" contact) "work@example.com"))
      ;; Other emails should also exist
      (should (assoc-default "EMAIL;TYPE=HOME" contact))
      (should (assoc-default "EMAIL;TYPE=OTHER" contact)
              ))))

(ert-deftest bergheim/test-vcard-email-metadata-preserved ()
  "Test that EMAIL metadata (ITEM, TYPE) is preserved."
  (let ((vcard "BEGIN:VCARD
VERSION:3.0
FN:Test Person
ITEM1.EMAIL;TYPE=INTERNET:test1@example.com
ITEM2.EMAIL;TYPE=INTERNET:test2@example.com
END:VCARD"))
    (let ((contact (bergheim/contactor--vcard-to-contact vcard)))
      ;; Both emails with full metadata should exist
      (should (equal (assoc-default "ITEM1.EMAIL;TYPE=INTERNET" contact) "test1@example.com"))
      (should (equal (assoc-default "ITEM2.EMAIL;TYPE=INTERNET" contact) "test2@example.com")))))

(ert-deftest bergheim/test-vcard-photo-unfolding ()
  "Test that multiline PHOTO field is properly unfolded."
  (let ((vcard "BEGIN:VCARD
VERSION:3.0
FN:Test Person
PHOTO:https//lh3googleusercontent.com/AG6tpzE
 tngNQm7L5n8DGNOnlcx8FZ
END:VCARD"))
    (let ((contact (bergheim/contactor--vcard-to-contact vcard)))
      (should (equal (assoc-default "PHOTO" contact)
                     "https//lh3googleusercontent.com/AG6tpzEtngNQm7L5n8DGNOnlcx8FZ")))))

(ert-deftest bergheim/test-export-special-chars ()
  "Test encoding of special characters."
  (should (equal (bergheim/contactor--vcard-encode-value "Müller, Thomas") "Müller\\, Thomas"))
  (should (equal (bergheim/contactor--vcard-encode-value "test;user") "test\\;user")))

(ert-deftest bergheim/test-export-metadata-initialization ()
  "Test that metadata is added to new org file on first import."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Contacts\n\n")
    (let ((timestamp (bergheim/contactor--timestamp-now-org)))
      (bergheim/contactor--update-metadata 
       :last-synced timestamp
       :contact-count 2)
      
      (goto-char (point-min))
      (should (re-search-forward "^#\\+LAST_SYNCED: " nil t))
      (goto-char (point-min))  ; Reset point!
      (should (re-search-forward "^#\\+CONTACT_COUNT: 2" nil t)))))

(ert-deftest bergheim/test-export-track-contact-modified ()
  "Test that editing contact updates LAST_MODIFIED."
  (with-temp-buffer
    (org-mode)
    (insert "* John Doe\n")
    (insert ":PROPERTIES:\n")
    (insert ":UID: uid-123\n")
    (insert ":EMAIL: john@example.com\n")
    (insert ":END:\n")
    
    ;; Simulate contact modification
    (goto-char (point-min))
    (bergheim/contactor--mark-modified)
    
    (should (org-entry-get nil "LAST_MODIFIED"))))

(ert-deftest bergheim/test-export-find-dirty-contacts ()
  "Test finding contacts that need export."
  (with-temp-buffer
    (org-mode)
    (let ((old-time "[2025-01-15 Wed 10:00]")
          (new-time "[2025-01-15 Wed 14:00]"))
      
      ;; Contact 1: exported after modification (clean)
      (insert "* Alice\n")
      (insert ":PROPERTIES:\n")
      (insert ":UID: uid-alice\n")
      (insert (format ":LAST_MODIFIED: %s\n" old-time))
      (insert (format ":LAST_EXPORTED: %s\n" new-time))
      (insert ":END:\n\n")
      
      ;; Contact 2: modified after export (dirty)
      (insert "* Bob\n")
      (insert ":PROPERTIES:\n")
      (insert ":UID: uid-bob\n")
      (insert (format ":LAST_MODIFIED: %s\n" new-time))
      (insert (format ":LAST_EXPORTED: %s\n" old-time))
      (insert ":END:\n\n")
      
      ;; Contact 3: never exported (dirty)
      (insert "* Charlie\n")
      (insert ":PROPERTIES:\n")
      (insert ":UID: uid-charlie\n")
      (insert (format ":LAST_MODIFIED: %s\n" new-time))
      (insert ":END:\n\n")
      
      (let ((dirty (bergheim/contactor--find-dirty)))
        (should (= (length dirty) 2))
        (should (member "uid-bob" (mapcar (lambda (c) (plist-get c :uid)) dirty)))
        (should (member "uid-charlie" (mapcar (lambda (c) (plist-get c :uid)) dirty)))))))

(ert-deftest bergheim/test-export-precheck-safety ()
  "Test pre-export safety checks."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Contacts\n")
    (insert (format "#+LAST_SYNCED: %s\n" (bergheim/contactor--timestamp-now-org)))  ; FIX: use recent date
    (insert "#+CONTACT_COUNT: 100\n\n")
    
    ;; Only 2 contacts now (was 100)
    (insert "* Alice\n:PROPERTIES:\n:UID: uid-1\n:END:\n\n")
    (insert "* Bob\n:PROPERTIES:\n:UID: uid-2\n:END:\n\n")
    
    (let ((result (bergheim/contactor--export-precheck)))
      (should-not (plist-get result :safe))
      (should (string-match-p "count" (plist-get result :reason))))))

(ert-deftest bergheim/test-export-precheck-no-sync ()
  "Test refusing export when never synced."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Contacts\n\n")
    (insert "* Alice\n:PROPERTIES:\n:UID: uid-1\n:END:\n\n")
    
    (let ((result (bergheim/contactor--export-precheck)))
      (should-not (plist-get result :safe))
      (should (string-match-p "LAST_SYNCED" (plist-get result :reason))))))

(ert-deftest bergheim/test-export-precheck-old-sync ()
  "Test warning when sync is old."
  (with-temp-buffer
    (org-mode)
    (let ((old-date (format-time-string bergheim/contactor--timestamp-format-org
                                        (time-subtract (current-time) 
                                                       (days-to-time 30)))))
      (insert "#+TITLE: Contacts\n")
      (insert (format "#+LAST_SYNCED: %s\n" old-date))
      (insert "#+CONTACT_COUNT: 10\n\n")
      
      (dotimes (i 10)
        (insert (format "* Contact %d\n:PROPERTIES:\n:UID: uid-%d\n:END:\n\n" i i)))
      
      (let ((result (bergheim/contactor--export-precheck)))
        (should-not (plist-get result :safe))
        (should (string-match-p "old" (plist-get result :reason)))))))

(ert-deftest bergheim/test-export-format-vcard ()
  "Test converting org contact back to vCard format."
  (with-temp-buffer
    (org-mode)
    (insert "* John Doe\n")
    (insert ":PROPERTIES:\n")
    (insert ":UID: uid-123\n")
    (insert ":EMAIL: john@example.com\n")
    (insert ":BIRTHDAY: <1980-12-25 +1y>\n")
    (insert ":END:\n")
    (insert ":VCARD:\n")
    (insert "VERSION:3.0\n")
    (insert "N:Doe;John;;;\n")
    (insert "TEL;TYPE=CELL:555-1234\n")
    (insert ":END:\n")
    
    (goto-char (point-min))
    (let* ((contact (bergheim/contactor--org-to-contact))
           (vcard (bergheim/contactor--contact-to-vcard contact)))
      (should (string-match-p "BEGIN:VCARD" vcard))
      (should (string-match-p "UID:uid-123" vcard))
      (should (string-match-p "FN:John Doe" vcard))
      (should (string-match-p "N:Doe;John;;;" vcard))  ; Raw format, no escaping
      (should (string-match-p "TEL;TYPE=CELL:555-1234" vcard))
      (should (string-match-p "END:VCARD" vcard)))))

(ert-deftest bergheim/test-export-update-top-level-modified ()
  "Test updating top-level LAST_MODIFIED from contacts."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Contacts\n")
    (insert "#+LAST_MODIFIED: [2025-01-15 Wed 10:00]\n\n")
    
    (insert "* Alice\n:PROPERTIES:\n:LAST_MODIFIED: [2025-01-15 Wed 12:00]\n:END:\n\n")
    (insert "* Bob\n:PROPERTIES:\n:LAST_MODIFIED: [2025-01-15 Wed 15:30]\n:END:\n\n")
    (insert "* Charlie\n:PROPERTIES:\n:LAST_MODIFIED: [2025-01-15 Wed 11:00]\n:END:\n\n")
    
    (bergheim/contactor--update-top-modified)
    
    (goto-char (point-min))
    (should (re-search-forward "^#\\+LAST_MODIFIED: \\[2025-01-15 Wed 15:30\\]" nil t))))

(ert-deftest bergheim/test-export-birthday-overwrites-vcard-extra ()
  "Test that BIRTHDAY property takes precedence over BDAY in VCARD."
  (with-temp-buffer
    (org-mode)
    (insert "* John Doe\n")
    (insert ":PROPERTIES:\n")
    (insert ":UID: uid-123\n")
    (insert ":BIRTHDAY: <1985-06-15 +1y>\n")
    (insert ":END:\n")
    (insert ":VCARD:\n")
    (insert "VERSION:3.0\n")
    (insert "BDAY:19801225\n")
    (insert ":END:\n")
    
    (goto-char (point-min))
    (let* ((contact
            (bergheim/contactor--org-to-contact))
           (vcard (bergheim/contactor--contact-to-vcard contact)))
      (should (string-match-p "BDAY:19850615" vcard))
      (should-not (string-match-p "19801225" vcard)))))

(ert-deftest bergheim/test-contact-unchanged ()
  "Test that identical org contact and vCard are detected as unchanged."
  (let ((vcard "BEGIN:VCARD
VERSION:3.0
UID:uid-123
FN:John Doe
N:Doe;John;;;

EMAIL:john@example.com
BDAY:19801225
NOTE:Some notes
TEL;TYPE=CELL:555-1234
REV:2025-01-20T10:00:00Z
END:VCARD")
        (temp-file (make-temp-file "test-vcard" nil ".vcf")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert vcard))
          
          (with-temp-buffer
            (org-mode)
            (insert "* John Doe\n")
            (insert ":PROPERTIES:\n")
            (insert ":UID: uid-123\n")
            (insert ":EMAIL: john@example.com\n")
            (insert ":BIRTHDAY: <1980-12-25 +1y>\n")
            (insert ":NOTE: Some notes\n")
            (insert ":REV: 2025-01-20T10:00:00Z\n")
            (insert ":END:\n")
            (insert ":VCARD:\n")
            (insert "VERSION:3.0\n")
            (insert "FN:John Doe\n")
            (insert "N:Doe;John;;;\n")
            (insert "EMAIL:john@example.com\n")
            (insert "BDAY:19801225\n")
            (insert "NOTE:Some notes\n")
            (insert "TEL;TYPE=CELL:555-1234\n")
            (insert "REV:2025-01-20T10:00:00Z\n")
            (insert ":END:\n")
            
            (goto-char (point-min))
            (let ((org-contact (bergheim/contactor--org-to-contact))
                  (vcard-contact (bergheim/contactor--vcard-to-contact vcard)))
              (should-not (bergheim/contactor--changed-p org-contact vcard-contact)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest bergheim/test-contact-changed ()
  "Test that different org contact and vCard are detected as changed."
  (let ((vcard "BEGIN:VCARD
VERSION:3.0
UID:uid-123
FN:John Doe
N:Doe;John;;;
EMAIL:john@example.com
BDAY:19801225
NOTE:Old notes
TEL;TYPE=CELL:555-1234
REV:2025-01-20T10:00:00Z
END:VCARD")
        (temp-file (make-temp-file "test-vcard" nil ".vcf")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert vcard))
          
          (with-temp-buffer
            (org-mode)
            (insert "* John Doe\n")
            (insert ":PROPERTIES:\n")
            (insert ":UID: uid-123\n")
            (insert ":EMAIL: john@example.com\n")
            (insert ":BIRTHDAY: <1980-12-25 +1y>\n")
            (insert ":NOTE: Updated notes here\n")
            (insert ":REV: 2025-01-20T10:00:00Z\n")
            (insert ":END:\n")
            (insert ":VCARD:\n")
            (insert "VERSION:3.0\n")
            (insert "N:Doe;John;;;\n")
            (insert "TEL;TYPE=CELL:555-1234\n")
            (insert ":END:\n")
            
            (goto-char (point-min))
            (let ((org-contact (bergheim/contactor--org-to-contact))
                  (vcard-contact (bergheim/contactor--vcard-to-contact vcard)))
              (should (bergheim/contactor--changed-p org-contact vcard-contact)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest bergheim/test-contact-changed-phone ()
  "Test that phone number change in VCARD is detected."
  (let ((vcard "BEGIN:VCARD
VERSION:3.0
UID:uid-123
FN:John Doe
EMAIL:john@example.com
TEL;TYPE=CELL:555-1234
REV:2025-01-20T10:00:00Z
END:
VCARD")
        (temp-file (make-temp-file "test-vcard" nil ".vcf")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert vcard))
          
          (with-temp-buffer
            (org-mode)
            (insert "* John Doe\n")
            (insert ":PROPERTIES:\n")
            (insert ":UID: uid-123\n")
            (insert ":EMAIL: john@example.com\n")
            (insert ":REV: 2025-01-20T10:00:00Z\n")
            (insert ":END:\n")
            (insert ":VCARD:\n")
            (insert "VERSION:3.0\n")
            (insert "TEL;TYPE=CELL:555-9999\n")
            (insert ":END:\n")
            
            (goto-char (point-min))
            (let ((org-contact (bergheim/contactor--org-to-contact))
                  (vcard-contact (bergheim/contactor--vcard-to-contact vcard)))
              (should (bergheim/contactor--changed-p org-contact vcard-contact)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest bergheim/test-export-unchanged-skips-write ()
  "Test that exporting unchanged contact doesn't modify vCard file."
  (let ((vcard "BEGIN:VCARD
VERSION:3.0
UID:uid-123
FN:John Doe
EMAIL:john@example.com
BDAY:19801225
REV:2025-01-20T10:00:00Z
END:VCARD")
        (temp-file (make-temp-file "test-vcard" nil ".vcf")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert vcard))
          
          (let ((org-contact '(("BEGIN" . "VCARD")
                               ("VERSION" . "3.0")
                               ("UID" . "uid-123")
                               ("FN" . "John Doe")
                               ("EMAIL" . "john@example.com")
                               ("BDAY" . "19801225")
                               ("REV" . "2025-01-20T10:00:00Z")
                               ("END" . "VCARD")))

                (vcard-contact (with-temp-buffer
                                 (insert-file-contents temp-file)
                                 (bergheim/contactor--vcard-to-contact (buffer-string)))))
            (should-not (bergheim/contactor--changed-p org-contact vcard-contact))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest bergheim/test-export-real-world-unchanged ()
  "Test real-world case - should detect no changes and not update REV."
  (let ((vcard "BEGIN:VCARD
VERSION:3.0
PRODID:-//Example//EN
UID:test-uid-12345
CATEGORIES:myContacts,imported
FN:Test Person
N:Person;Test;;;
BDAY:19700101
TEL;TYPE=CELL:555-1234
REV:2025-07-25T20:02:27Z
END:VCARD")
        (temp-file (make-temp-file "test-vcard" nil ".vcf")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert vcard))
          
          (with-temp-buffer
            (org-mode)
            (insert "* Test Person\n")
            (insert ":PROPERTIES:\n")
            (insert ":UID: test-uid-12345\n")
            (insert ":BIRTHDAY: <1970-01-01 +1y>\n")
            (insert ":REV: 2025-07-25T20:02:27Z\n")
            (insert ":END:\n")
            (insert ":VCARD:\n")
            (insert "VERSION:3.0\n")
            (insert "PRODID:-//Example//EN\n")
            (insert "UID:test-uid-12345\n")
            (insert "CATEGORIES:myContacts,imported\n")
            (insert "N:Person;Test;;;\n")
            (insert "TEL;TYPE=CELL:555-1234\n")
            (insert "REV:2025-07-25T20:02:27Z\n")
            (insert ":END:\n")
            
            (goto-char (point-min))
            (let ((org-contact (bergheim/contactor--org-to-contact))
                  (vcard-contact (with-temp-buffer
                                   (insert-file-contents temp-file)
                                   (bergheim/contactor--vcard-to-contact (buffer-string)))))
              (should-not (bergheim/contactor--changed-p org-contact vcard-contact)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest bergheim/test-export-detects-external-vcard-change ()
  "Test that exporting detects when vCard REV is newer than org REV."
  (let ((vcard "BEGIN:VCARD
VERSION:3.0
PRODID:-//Example//EN
UID:test-uid-12345
CATEGORIES:myContacts
FN:Test Person
N:Person;Test;;;
BDAY:19700101
TEL;TYPE=CELL:555-1234
REV:2026-07-25T20:02:27Z
END:VCARD")
        (temp-file (make-temp-file "test-vcard" nil ".vcf")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert vcard))
          
          (with-temp-buffer
            (org-mode)
            (insert "* Test Person\n")
            (insert ":PROPERTIES:\n")
            (insert ":UID: test-uid-12345\n")
            (insert ":BIRTHDAY: <1970-01-01 +1y>\n")
            (insert ":REV: 2025-07-25T20:02:27Z\n")
            (insert ":END:\n")
            (insert ":VCARD:\n")
            (insert "VERSION:3.0\n")
            (insert "PRODID:-//Example//EN\n")
            (insert "UID:test-uid-12345\n")
            (insert "CATEGORIES:myContacts\n")
            (insert "N:Person;Test;;;\n")
            (insert "TEL;TYPE=CELL:555-1234\n")
            (insert "REV:2025-07-25T20:02:27Z\n")
            (insert ":END:\n")
            
            (goto-char (point-min))
            (let* ((org-contact (bergheim/contactor--org-to-contact))
                   (vcard-contact (with-temp-buffer
                                    (insert-file-contents temp-file)
                                    (bergheim/contactor--vcard-to-contact (buffer-string)))))
              ;; Contact unchanged check should pass
              (should-not (bergheim/contactor--changed-p org-contact vcard-contact))
              ;; But REV mismatch should be detected
              (should-not (equal (assoc-default "REV" org-contact)
                                 (assoc-default "REV" vcard-contact))))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest bergheim/test-neo-vcard-string-to-contact ()
  "Test parsing vCard to canonical alist."
  (let ((vcard "BEGIN:VCARD
VERSION:3.0
UID:uid-123
FN:John Doe
EMAIL:john@example.com
END:VCARD"))
    (let ((contact (bergheim/contactor--vcard-to-contact vcard)))
      (should (equal (assoc-default "BEGIN" contact) "VCARD"))
      (should (equal (assoc-default "VERSION" contact) "3.0"))
      (should (equal (assoc-default "UID" contact) "uid-123"))
      (should (equal (assoc-default "FN" contact) "John Doe"))
      (should (equal (assoc-default "EMAIL" contact) "john@example.com"))
      (should (equal (assoc-default "END" contact) "VCARD"))
      ;; Verify order preserved
      (should (equal (car (nth 0 contact)) "BEGIN"))
      (should (equal (car (nth 1 contact)) "VERSION"))
      (should (equal (car (nth 5 contact)) "END")))))

(ert-deftest bergheim/test-neo-contact-to-vcard-string ()
  "Test converting contact alist to vCard string."
  (let ((contact '(("BEGIN" . "VCARD")
                   ("VERSION" . "3.0")
                   ("UID" . "uid-123")
                   ("FN" . "John Doe")
                   ("EMAIL" . "john@example.com")
                   ("END" . "VCARD"))))
    (let ((vcard (bergheim/contactor--contact-to-vcard contact)))
      (should (string-match-p "BEGIN:VCARD" vcard))
      (should (string-match-p "UID:uid-123" vcard))
      (should (string-match-p "FN:John Doe" vcard))
      (should (string-match-p "EMAIL:john@example.com" vcard))
      (should (string-match-p "END:VCARD" vcard)))))

(ert-deftest bergheim/test-neo-vcard-roundtrip ()
  "Test parse → serialize preserves data."
  (let ((original "BEGIN:VCARD
VERSION:3.0
UID:uid-123
FN:John Doe
N:Doe;John;;;
EMAIL;TYPE=WORK:john@example.com
TEL;TYPE=CELL:555-1234
CATEGORIES:Someones’s iBlob,Imported on 7/25,myContacts
BDAY:19801225
END:VCARD"))
    (let* ((contact (bergheim/contactor--vcard-to-contact original))
           (reconstructed (bergheim/contactor--contact-to-vcard contact))
           (reparsed (bergheim/contactor--vcard-to-contact reconstructed)))
      ;; Contact should equal itself after roundtrip
      (should (equal contact reparsed)))))

(ert-deftest bergheim/test-neo-vcard-roundtrip-no-escaping ()
  "Test that vCard → org → vCard doesn't add extra escaping."
  (let ((original "BEGIN:VCARD
VERSION:3.0
UID:test
-uid
FN:Carl Sagan
N:Sagan;Carl;;;
CATEGORIES:iPhone,Imported on 7/25,myContacts
TEL;TYPE=CELL:555-1234
BDAY:19700101
REV:2025-07-25T20:02:27Z
END:VCARD"))
    (with-temp-buffer
      (org-mode)
      ;; Import: vCard → org
      (let ((contact (bergheim/contactor--vcard-to-contact original)))
        (insert (bergheim/contactor--contact-to-org contact)))
      
      ;; Modify heading (simulate user edit)
      (goto-char (point-min))
      (re-search-forward "^\\* Carl Sagan")
      (replace-match "* Carl Sagan!")
      
      ;; Export: org → vCard
      (goto-char (point-min))
      (let* ((contact (bergheim/contactor--org-to-contact))
             (exported (bergheim/contactor--contact-to-vcard contact)))
        
        ;; Check that escaping didn't change (except FN and REV)
        (should (string-match-p "CATEGORIES:iPhone,Imported on 7/25,myContacts" exported))
        (should (string-match-p "N:Sagan;Carl;;;" exported))
        (should-not (string-match-p "\\\\," exported))  ; No escaped commas
        (should-not (string-match-p "\\\\;" exported))  ; No escaped semicolons
        (should (string-match-p "FN:Carl Sagan!" exported))))))

(ert-deftest bergheim/test-neo-vcard-with-params ()
  "Test fields with parameters like EMAIL;TYPE=WORK."
  (let ((vcard "BEGIN:VCARD
VERSION:3.0
UID:uid-123
EMAIL;TYPE=WORK:work@example.com
EMAIL;TYPE=HOME:home@example.com
TEL;TYPE=CELL:555-1234
END:VCARD"))
    (let ((contact (bergheim/contactor--vcard-to-contact vcard)))
      (should (equal (assoc-default "EMAIL;TYPE=WORK" contact) "work@example.com"))
      (should (equal (assoc-default "EMAIL;TYPE=HOME" contact) "home@example.com"))
      (should (equal (assoc-default "TEL;TYPE=CELL" contact) "555-1234")))))

(ert-deftest bergheim/test-neo-vcard-multiline-note ()
  "Test continuation lines in NOTE field."
  (let ((vcard "BEGIN:VCARD
VERSION:3.0
UID:uid-123
FN:John Doe
NOTE:This is a long note
 that continues on
 multiple lines
END:VCARD"))
    (let ((contact (bergheim/contactor--vcard-to-contact vcard)))
      (should (equal (assoc-default "NOTE" contact) 
                     "This is a long notethat continues onmultiple lines")))))

(ert-deftest bergheim/test-neo-vcard-escaped-values ()
  "Test vCard escape sequences are preserved as-is."
  (let ((vcard "BEGIN:VCARD
VERSION:3.0
UID:uid-123
FN:Doe\\, John
NOTE:Line1\\nLine2\\, with comma
END:VCARD"))
    (let ((contact (bergheim/contactor--vcard-to-contact vcard)))
      ;; Values stored RAW - escapes preserved
      (should (equal (assoc-default "FN" contact) "Doe\\, John"))
      (should (equal (assoc-default "NOTE" contact) "Line1\\nLine2\\, with comma")))))

(ert-deftest bergheim/test-neo-org-timestamp-to-bday ()
  "Test timestamp conversion."
  (should (equal (bergheim/contactor--org-bday-to-vcard "<1980-12-25 +1y>") "19801225"))
  (should (equal (bergheim/contactor--org-bday-to-vcard "<12-25 +1y>") "--1225")))

(ert-deftest bergheim/test-neo-org-heading-to-contact-basic ()
  "Test extracting contact from org heading."
  (with-temp-buffer
    (org-mode)
    (insert "* John Doe
:PROPERTIES:
:UID: uid-123
:EMAIL: john@example.com
:BIRTHDAY: <1980-12-25 +1y>
:NOTE: Test note
:END:
:VCARD:
VERSION:3.0
N:Doe;John;;;
TEL;TYPE=CELL:555-1234
REV:2025-01-20T10:00:00Z
:END:
")
    (goto-char (point-min))
    (let ((contact (bergheim/contactor--org-to-contact)))
      (message "Full contact: %S" contact)
      (should (equal (assoc-default "BEGIN" contact) "VCARD"))
      (should (equal (assoc-default "VERSION" contact) "3.0"))
      (should (equal (assoc-default "UID" contact) "uid-123"))
      (should (equal (assoc-default "FN" contact) "John Doe"))
      (should (equal (assoc-default "EMAIL" contact) "john@example.com"))
      (should (equal (assoc-default "BDAY" contact) "19801225"))
      (should (equal (assoc-default "NOTE" contact) "Test note"))
      (should (equal (assoc-default "REV" contact) "2025-01-20T10:00:00Z"))
      (should (equal (assoc-default "N" contact) "Doe;John;;;"))
      (should (equal (assoc-default "TEL;TYPE=CELL" contact) "555-1234"))
      (should (equal (assoc-default "END" contact) "VCARD")))))

(ert-deftest bergheim/test-neo-read-vcard-drawer ()
  "Test reading VCARD drawer."
  (with-temp-buffer
    (org-mode)
    (insert "* John Doe
:PROPERTIES:
:UID: uid-123
:END:
:VCARD:
VERSION:3.0
N:Doe;John;;;
TEL;TYPE=CELL:555-1234
:END:
")
    (goto-char (point-min))
    (let ((drawer (bergheim/contactor--vcard-drawer)))
      (message "Drawer content: %S" drawer)
      (should (string-match-p "VERSION:3.0" drawer))
      (should (string-match-p "N:Doe;John;;;" drawer))
      (should (string-match-p "TEL;TYPE=CELL:555-1234" drawer)))))

(ert-deftest bergheim/test-neo-vcard-bday-to-org-timestamp ()
  "Test BDAY conversion."
  (should (equal (bergheim/contactor--vcard-bday-to-org "19801225") "1980-12-25"))
  (should (equal (bergheim/contactor--vcard-bday-to-org "--1225") "12-25")))

(ert-deftest bergheim/test-neo-contact-to-org-heading ()
  "Test converting contact to org heading."
  (let ((contact '(("BEGIN" . "VCARD")
                   ("VERSION" . "3.0")
                   ("UID" . "uid-123")
                   ("FN" . "John Doe")
                   ("N" . "Doe;John;;;")
                   ("EMAIL" . "john@example.com")
                   ("BDAY" . "19801225")
                   ("TEL;TYPE=CELL" . "555-1234")
                   ("REV" . "2025-01-20T10:00:00Z")
                   ("END" . "VCARD"))))

    (let ((org-string (bergheim/contactor--contact-to-org contact)))
      (should (string-match-p "^\\* John Doe" org-string))
      (should (string-match-p ":UID: uid-123" org-string))
      (should (string-match-p ":EMAIL: john@example.com" org-string))
      (should (string-match-p ":BIRTHDAY: <1980-12-25 \\+1y>" org-string))
      (should (string-match-p ":VCARD:" org-string))
      (should (string-match-p "VERSION:3.0" org-string))
      (should (string-match-p "N:Doe;John;;;" org-string))
      (should (string-match-p "TEL;TYPE=CELL:555-1234" org-string)))))

(ert-deftest bergheim/test-neo-org-roundtrip ()
  "Test org → contact → org preserves data."
  (with-temp-buffer
    (org-mode)
    (insert "* John Doe
:PROPERTIES:
:UID: uid-123
:EMAIL: john@example.com
:BIRTHDAY: <1980-12-25 +1y>
:NOTE: Test note
:REV: 2025-01-20T10:00:00Z
:END:
:VCARD:
VERSION:3.0
N:Doe;John;;;
TEL;TYPE=CELL:555-1234
:END:

** Personal notes
Important stuff here
")
    (goto-char (point-min))
    (let* ((contact1 (bergheim/contactor--org-to-contact))
           (org-string (bergheim/contactor--contact-to-org contact1)))
      ;; Insert the generated org string in a new buffer
      (with-temp-buffer
        (org-mode)
        (insert org-string)
        (goto-char (point-min))
        (let ((contact2 (bergheim/contactor--org-to-contact)))
          ;; Should be identical (minus body content which we don't preserve yet)
          (should (equal contact1 contact2)))))))

(ert-deftest bergheim/test-neo-contact-update-preserves-body ()
  "Test that updating contact preserves body content."
  (with-temp-buffer
    (org-mode)
    (insert "* Old Name
:PROPERTIES:
:UID: uid-123
:EMAIL: old@example.com
:END:
:VCARD:
VERSION:3.0
TEL;TYPE=CELL:555-0000
:END:

** Important notes
Don't delete this!
")
    (goto-char (point-min))
    (let ((contact '(("BEGIN" . "VCARD")
                     ("VERSION" . "3.0")
                     ("UID" . "uid-123")
                     ("FN" . "New Name")
                     ("EMAIL" . "new@example.com")
                     ("TEL;TYPE=CELL" . "555-1111")
                     ("END" . "VCARD"))))
      (bergheim/contactor--update-org-heading contact))
    
    ;; Check properties updated
    (goto-char (point-min))
    (should (re-search-forward "^\\* New Name" nil t))
    (should (string= "new@example.com" (org-entry-get nil "EMAIL")))
    
    ;; Check VCARD updated
    (goto-char (point-min))
    (should (re-search-forward "TEL;TYPE=CELL:555-1111" nil t))
    (should-not (re-search-forward "555-0000" nil t))
    
    ;; Check body preserved
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\* Important notes" nil t))
    (should (re-search-forward "Don't delete this!" nil t))))

(ert-deftest bergheim/test-neo-export-updates-org-rev ()
  "Test that exporting contact updates REV in both vCard and org."
  (let ((temp-file (make-temp-file "test-vcard" nil ".vcf"))
        (old-rev "2025-01-20T10:00:00Z"))
    (unwind-protect
        (progn
          ;; Create initial vCard file
          (with-temp-file temp-file
            (insert "BEGIN:VCARD\n")
            (insert "VERSION:3.0\n")
            (insert "UID:test-uid-123\n")
            (insert "FN:John Doe\n")
            (insert "EMAIL:john@example.com\n")
            (insert (format "REV:%s\n" old-rev))
            (insert "END:VCARD\n"))
          
          ;; Create org contact
          (with-temp-buffer
            (org-mode)
            (insert "* John Doe\n")
            (insert ":PROPERTIES:\n")
            (insert ":UID: test-uid-123\n")
            (insert ":EMAIL: john@example.com\n")
            (insert ":END:\n")
            (insert ":VCARD:\n")
            (insert "VERSION:3.0\n")
            (insert "UID:test-uid-123\n")
            (insert "FN:John Doe\n")
            (insert "EMAIL:john@example.com\n")
            (insert (format "REV:%s\n" old-rev))
            (insert ":END:\n")
            
            ;; Modify contact (change email in VCARD)
            (goto-char (point-min))
            (re-search-forward "EMAIL:john@example.com" nil t 2) ; Find second occurrence (in VCARD)
            (replace-match "EMAIL:john.new@example.com")
            
            ;; Also update property
            (goto-char (point-min))
            (org-set-property "EMAIL" "john.new@example.com")
            
            ;; Mock bergheim/contactor--find-vcard-by-uid to return our temp file
            (cl-letf (((symbol-function 'bergheim/contactor--find-vcard-by-uid)
                       (lambda (dir uid) temp-file)))
              
              ;; Export contact
              (goto-char (point-min))
              (bergheim/contactor-export-at-point))
            
            ;; Check that VCARD has new REV (not property)
            (goto-char (point-min))
            (let* ((contact (bergheim/contactor--org-to-contact))
                   (new-org-rev (assoc-default "REV" contact)))
              (should new-org-rev)
              (should-not (equal new-org-rev old-rev))
              
              ;; Check that vCard file REV was updated
              (let* ((vcard-string (with-temp-buffer
                                     (insert-file-contents temp-file)
                                     (buffer-string)))
                     (vcard-contact (bergheim/contactor--vcard-to-contact vcard-string))
                     (new-vcard-rev (assoc-default "REV" vcard-contact)))
                (should new-vcard-rev)
                (should-not (equal new-vcard-rev old-rev))
                ;; Org and vCard REV should match
                (should (equal new-org-rev new-vcard-rev))))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest bergheim/test-neo-vcard-sync-preserves-extra-fields ()
  "Test that syncing preserves fields added manually to VCARD drawer."
  (let ((temp-file (make-temp-file "contacts-test" nil ".org"))
        (vcard-dir (make-temp-file "vcard-test" t)))
    (unwind-protect
        (progn
          ;; Create vCard file
          (let ((vcard-file (expand-file-name "test.vcf" vcard-dir)))
            (with-temp-file vcard-file
              (insert "BEGIN:VCARD\n")
              (insert "VERSION:3.0\n")
              (insert "UID:test-uid-123\n")
              (insert "FN:John Doe\n")
              (insert "EMAIL:john@example.com\n")
              (insert "REV:2025-01-20T10:00:00Z\n")
              (insert "END:VCARD\n"))
            
            ;; Create org file with same contact BUT with extra FOO:BAR field
            (with-temp-file temp-file
              (insert "#+TITLE: Contacts\n\n")
              (insert "* John Doe\n")
              (insert ":PROPERTIES:\n")
              (insert ":UID: test-uid-123\n")
              (insert ":EMAIL: john@example.com\n")
              (insert ":END:\n")
              (insert ":VCARD:\n")
              (insert "VERSION:3.0\n")
              (insert "UID:test-uid-123\n")
              (insert "FN:John Doe\n")
              (insert "EMAIL:john@example.com\n")
              (insert "FOO:BAR\n")  ; Extra field added manually
              (insert "REV:2025-01-20T10:00:00Z\n")
              (insert ":END:\n"))
            

            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (prompt) t)))  ; Always answer yes
              ;; Run sync (should import vCard, but preserve FOO:BAR)
              (let ((bergheim/contactor-sync-directory vcard-dir))
                (bergheim/contactor-import vcard-dir temp-file))
              
              ;; Check that FOO:BAR is still there
              (with-current-buffer (find-file-noselect temp-file)
                (goto-char (point-min))
                (re-search-forward "^:VCARD:")
                (let ((vcard-start (point))
                      (vcard-end (progn (re-search-forward "^:END:") (line-beginning-position))))
                  (goto-char vcard-start)
                  (should (re-search-forward "^FOO:BAR$" vcard-end t)))))))
      
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file))
      (when (file-directory-p vcard-dir)
        (delete-directory vcard-dir t)))))

(ert-deftest bergheim/test-import-conflict-detection ()
  "Test that import detects and handles conflicts when org differs from vCard."
  (let ((temp-file (make-temp-file "contacts-test" nil ".org"))
        (vcard-dir (make-temp-file "vcard-test" t)))
    (unwind-protect
        (progn
          ;; Create vCard file
          (let ((vcard-file (expand-file-name "test.vcf" vcard-dir)))
            (with-temp-file vcard-file
              (insert "BEGIN:VCARD\n")
              (insert "VERSION:3.0\n")
              (insert "UID:test-uid-123\n")
              (insert "FN:John Doe\n")
              (insert "CATEGORIES:Work,Imported on 7/25,myContacts\n")
              (insert "BDAY:19700101\n")
              (insert "REV:2025-07-25T20:02:27Z\n")
              (insert "END:VCARD\n"))
            
            ;; Create org file with DIFFERENT categories
            (with-temp-file temp-file
              (insert "#+TITLE: Contacts\n\n")
              (insert "* John Doe\n")
              (insert ":PROPERTIES:\n")
              (insert ":UID: test-uid-123\n")
              (insert ":BIRTHDAY: <1970-01-01 +1y>\n")
              (insert ":END:\n")
              (insert ":VCARD:\n")
              (insert "VERSION:3.0\n")
              (insert "UID:test-uid-123\n")
              (insert "FN:John Doe\n")
              (insert "CATEGORIES:Personal,Imported on 7/25,myContacts\n")
              (insert "BDAY:19700101\n")
              (insert "REV:2025-07-25T20:02:27Z\n")
              (insert ":END:\n"))
            
            ;; Run import with mocked prompt
            (let ((yes-or-no-p-called nil)
                  (prompt-text nil))
              (cl-letf (((symbol-function 'yes-or-no-p)
                         (lambda (prompt) 
                           (setq yes-or-no-p-called t)
                           (setq prompt-text prompt)
                           (message "Prompt called with: %s" prompt)
                           t)))  ; Always answer yes
                
                ;; Run import
                (condition-case err
                    (bergheim/contactor-import vcard-dir temp-file)
                  (error (message "Import error: %S" err)
                         (signal (car err) (cdr err))))
                
                ;; Verify prompt was called
                (message "yes-or-no-p called: %s" yes-or-no-p-called)
                (should yes-or-no-p-called)))))
      
      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file))
      (when (file-directory-p vcard-dir)
        (delete-directory vcard-dir t)))))

(ert-deftest bergheim/test-neo-contact-changed-categories ()
  "Test that CATEGORIES change in VCARD is detected."
  (let ((org-contact '(("BEGIN" . "VCARD")
                       ("VERSION" . "3.0")
                       ("PRODID" . "-//Example//EN")
                       ("UID" . "test-uid-123")
                       ("CATEGORIES" . "Personal,Imported on 7/25,myContacts")
                       ("FN" . "Carl Sagan")
                       ("N" . "Sagan;Carl;;;")
                       ("BDAY" . "19700101")
                       ("TEL;TYPE=CELL" . "555-1234")
                       ("REV" . "2025-07-25T20:02:27Z")
                       ("END" . "VCARD")))
        (vcard-contact '(("BEGIN" . "VCARD")
                         ("VERSION" . "3.0")
                         ("PRODID" . "-//Example//EN")
                         ("UID" . "test-uid-123")
                         ("CATEGORIES" . "Work,Imported on 7/25,myContacts")
                         ("FN" . "Carl Sagan")
                         ("N" . "Sagan;Carl;;;")
                         ("BDAY" . "19700101")
                         ("TEL;TYPE=CELL" . "555-1234")
                         ("REV" . "2025-07-25T20:02:27Z")
                         ("END" . "VCARD"))))
    ;; REV is same, but CATEGORIES differs - should detect change
    (should (bergheim/contactor--changed-p org-contact vcard-contact))))

(ert-deftest bergheim/test-export-sets-last-exported ()
  "Test that exporting contact sets LAST_EXPORTED property."
  (let ((temp-file (make-temp-file "test-vcard" nil ".vcf")))
    (unwind-protect
        (progn
          ;; Create vCard file
          (with-temp-file temp-file
            (insert "BEGIN:VCARD\n")
            (insert "VERSION:3.0\n")
            (insert "UID:test-uid-123\n")
            (insert "FN:John Doe\n")
            (insert "EMAIL:john@example.com\n")
            (insert "REV:2025-01-20T10:00:00Z\n")
            (insert "END:VCARD\n"))
          
          ;; Create org contact
          (with-temp-buffer
            (org-mode)
            (insert "* John Doe\n")
            (insert ":PROPERTIES:\n")
            (insert ":UID: test-uid-123\n")
            (insert ":EMAIL: john@example.com\n")
            (insert ":END:\n")
            (insert ":VCARD:\n")
            (insert "VERSION:3.0\n")
            (insert "UID:test-uid-123\n")
            (insert "FN:John Doe\n")
            (insert "EMAIL:john@example.com\n")
            (insert "REV:2025-01-20T10:00:00Z\n")
            (insert ":END:\n")
            
            ;; Verify no LAST_EXPORTED initially
            (goto-char (point-min))
            (should-not (org-entry-get nil "LAST_EXPORTED"))
            
            ;; mocks
            (cl-letf (((symbol-function 'bergheim/contactor--find-vcard-by-uid)
                       (lambda (dir uid) temp-file))
                      ((symbol-function 'yes-or-no-p)
                       (lambda (prompt) t)))
              
              ;; Export contact
              (goto-char (point-min))
              (bergheim/contactor-export-at-point)
              
              ;; Verify LAST_EXPORTED is now set
              (goto-char (point-min))
              (should (org-entry-get nil "LAST_EXPORTED")))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest bergheim/test-export-sets-only-last-exported ()
  "Test that exporting contact sets only LAST_EXPORTED."
  (let ((temp-file (make-temp-file "test-vcard" nil ".vcf")))
    (unwind-protect
        (progn
          ;; Create vCard file
          (with-temp-file temp-file
            (insert "BEGIN:VCARD\n")
            (insert "VERSION:3.0\n")
            (insert "UID:test-uid-123\n")
            (insert "FN:John Doe\n")
            (insert "EMAIL:john@example.com\n")
            (insert "REV:2025-01-20T10:00:00Z\n")
            (insert "END:VCARD\n"))
          
          ;; Create org contact
          (with-temp-buffer
            (org-mode)
            (insert "* John Doe\n")
            (insert ":PROPERTIES:\n")
            (insert ":UID: test-uid-123\n")
            (insert ":EMAIL: john@example.com\n")
            (insert ":LAST_EXPORTED: [2025-11-15 Sat 21:13:48]")
            (insert ":LAST_MODIFIED: [2025-11-15 Sat 21:13:48]")
            (insert ":END:\n")
            (insert ":VCARD:\n")
            (insert "VERSION:3.0\n")
            (insert "UID:test-uid-123\n")
            (insert "FN:John Doe\n")
            (insert "EMAIL:john@example.com\n")
            (insert "REV:2025-01-20T10:00:00Z\n")
            (insert ":END:\n")
            
            ;; Mock find-vcard-file
            (cl-letf (((symbol-function 'bergheim/contactor--find-vcard-by-uid)
                       (lambda (dir uid) temp-file)))
              
              ;; Export contact
              (goto-char (point-min))
              (bergheim/contactor-export-at-point)
              
              ;; Verify both timestamps are now set
              (goto-char (point-min))
              (should (not (equal
                            (org-entry-get nil "LAST_MODIFIED")
                            (org-entry-get nil "LAST_EXPORTED"))))))
          (when (file-exists-p temp-file)
            (delete-file temp-file))))))

(provide 'contactor-test)
;;; contactor-test.el ends here
