;;; bergheim-agent-helpers-test.el --- Tests for agent helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)
(load-file (expand-file-name "../modules/bergheim-agent-helpers.el"
                             (file-name-directory load-file-name)))

(defmacro bergheim/agent-test--with-dir (&rest body)
  `(let ((dir (make-temp-file "agent-helpers-test-" t))
         (bergheim/agent-worklog-dir nil))
     (unwind-protect
         (progn ,@body)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (string-prefix-p dir file)
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory dir t))))

(ert-deftest bergheim/agent-denote-update-preserves-id-and-links ()
  (bergheim/agent-test--with-dir
   (let* ((source (plist-get
                   (bergheim/agent-denote-create
                    dir "Old title" '("memory") "Old body")
                   :path))
          (target (plist-get
                   (bergheim/agent-denote-create
                    dir "Target" '("reference") "Target body")
                   :path))
          (id (plist-get (bergheim/agent-denote--parse-filename source) :id))
          (target-id
           (plist-get (bergheim/agent-denote--parse-filename target) :id)))
     (should-not (equal id target-id))
     (bergheim/agent-denote-link source (list target))
     (let* ((result (bergheim/agent-denote-update
                     source
                     :title "New title"
                     :keywords '("research" "agent api")
                     :body "New body"))
            (path (plist-get result :path))
            (content (bergheim/agent-denote-get path)))
       (should-not (file-exists-p source))
       (should (file-exists-p path))
       (should (equal id (plist-get result :id)))
       (should (string-match-p "^#\\+title:[[:space:]]+New title$" content))
       (should (string-match-p "^#\\+identifier:[[:space:]]+.*$" content))
       (should (string-match-p "New body" content))
       (should-not (string-match-p "Old body" content))
       (should (string-match-p "^\\* Related notes$" content))
       (should (string-match-p "denote:" content))
       (should-not (plist-get
                    (bergheim/agent-denote-update path :body "New body")
                    :wrote))))))

(ert-deftest bergheim/agent-org-task-add-log-rejects-plain-heading ()
  (bergheim/agent-test--with-dir
   (let ((file (expand-file-name "TODO.org" dir)))
     (write-region "#+TODO: TODO | DONE\n\n* Plain heading\n* TODO Task\n"
                   nil file nil 'silent)
     (should-error
      (bergheim/agent-org-task-add-log file "Plain heading" "Nope")
      :type 'error)
     (should (plist-get
              (bergheim/agent-org-task-add-log file "Task" "Progress")
              :wrote)))))

(ert-deftest bergheim/agent-org-task-planning-set-and-clear ()
  (bergheim/agent-test--with-dir
   (let ((file (expand-file-name "TODO.org" dir)))
     (write-region
      "#+TODO: TODO | DONE\n\n* TODO Alpha\n:PROPERTIES:\n:ID: alpha-1\n:END:\n* Plain\n"
      nil file nil 'silent)
     (let ((scheduled
            (bergheim/agent-org-task-schedule
             file "Alpha" "2026-08-25")))
       (should (string-match-p "2026-08-25"
                               (plist-get scheduled :scheduled))))
     (let ((deadline
            (bergheim/agent-org-task-deadline
             file "alpha-1" "2026-08-30" t)))
       (should (string-match-p "2026-08-30"
                               (plist-get deadline :deadline))))
     (let* ((json (bergheim/agent-org-task-list file '("TODO")))
            (item (car (json-parse-string json :array-type 'list
                                          :object-type 'alist))))
       (should (string-match-p "2026-08-25" (alist-get 'scheduled item)))
       (should (string-match-p "2026-08-30" (alist-get 'deadline item))))
     (should-error
      (bergheim/agent-org-task-schedule file "Plain" "2026-09-01")
      :type 'error)
     (should-not
      (plist-get (bergheim/agent-org-task-schedule file "Alpha" nil)
                 :scheduled))
     (should-not
      (plist-get (bergheim/agent-org-task-deadline file "alpha-1" nil t)
                 :deadline)))))

(ert-deftest bergheim/agent-org-task-list-returns-json ()
  (bergheim/agent-test--with-dir
   (let ((file (expand-file-name "TODO.org" dir)))
     (write-region "#+TODO: TODO | DONE\n\n* TODO Alpha\n* DONE Beta\n"
                   nil file nil 'silent)
     (let* ((json (bergheim/agent-org-task-list file '("TODO")))
            (items (json-parse-string json :array-type 'list
                                      :object-type 'alist)))
       (should (stringp json))
       (should (= 1 (length items)))
       (should (equal "Alpha" (alist-get 'heading (car items))))))))

(ert-deftest bergheim/agent-autonomous-stale-position-does-not-search-backward ()
  (bergheim/agent-test--with-dir
   (let ((file (expand-file-name "TODO.org" dir)))
     (write-region "#+TODO: TODO | DONE\n\n* TODO Alpha  :autonomous:\n"
                   nil file nil 'silent)
     (let* ((items (json-parse-string
                    (bergheim/agent-org-task-autonomous-select file)
                    :array-type 'list :object-type 'alist))
            (position (alist-get 'position (car items))))
       (with-temp-buffer
         (insert-file-contents file)
         (goto-char (point-min))
         (insert "shift\n")
         (write-region (point-min) (point-max) file nil 'silent))
       (should-not
        (bergheim/agent-org-task-autonomous-mark-dispatched
         file position "[2026-08-22 Sat 10:00]"))
       (should-not
        (string-match-p ":DISPATCHED:"
                        (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))))))

(provide 'bergheim-agent-helpers-test)
;;; bergheim-agent-helpers-test.el ends here
