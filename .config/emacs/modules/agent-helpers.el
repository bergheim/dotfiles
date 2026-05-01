;;; bergheim-agent-helpers.el --- Agent helpers for org-mode and denote -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-clock)
(require 'org-id)

;;; Notes auto-commit
;;
;; Public-notes mode: when a helper edits a file under a `docs/' directory
;; that contains a `.git' subdir, auto-commit and best-effort push so the
;; project's notes repo accumulates real history. Private projects have no
;; nested `.git' and pay zero ceremony.

(defun bergheim/agent-notes--repo-root (file)
  "Return the `docs/' root for FILE when that directory contains `.git/',
else nil. Walks ancestors looking for a directory named `docs' with a
nested `.git' subdir."
  (let ((dir (file-name-directory (expand-file-name file)))
        (found nil))
    (while (and dir (not found) (not (string= dir "/")))
      (let* ((stripped (directory-file-name dir))
             (basename (file-name-nondirectory stripped)))
        (when (and (string= basename "docs")
                   (file-directory-p (expand-file-name ".git" dir)))
          (setq found stripped)))
      (let ((parent (file-name-directory (directory-file-name dir))))
        (setq dir (unless (string= parent dir) parent))))
    found))

(defun bergheim/agent-notes--async-push (root)
  "Fire `git -C ROOT push' in the background; log failures via `message'.
Async so a stalled remote never blocks a helper call."
  (let ((proc (start-process "agent-notes-push"
                             (get-buffer-create " *agent-notes-push*")
                             "git" "-C" root "push")))
    (set-process-sentinel
     proc
     (lambda (p event)
       (unless (zerop (process-exit-status p))
         (message "agent-notes: git push failed: %s" (string-trim event)))))))

(defun bergheim/agent-notes--maybe-commit (file message)
  "If FILE lives under a public-notes `docs/' repo, stage and commit with
MESSAGE, then fire `git push' in the background. Failures from `git add'
or `git commit' are surfaced via `message' without signaling an error,
so the calling helper's already-completed org edit is not rolled back.
`git diff --cached --quiet' exit codes: 0 = nothing staged (no-op),
1 = staged changes (commit), >1 = error (logged, no commit)."
  (when-let* ((root (bergheim/agent-notes--repo-root file)))
    (let* ((default-directory (file-name-as-directory root))
           (add-rc (call-process "git" nil nil nil "add" "-A")))
      (cond
       ((not (zerop add-rc))
        (message "agent-notes: git add -A failed (exit %d) in %s" add-rc root))
       (t
        (let ((diff-rc (call-process "git" nil nil nil "diff" "--cached" "--quiet")))
          (cond
           ((zerop diff-rc) nil) ;; Nothing staged.
           ((= diff-rc 1)
            (let ((commit-rc (call-process "git" nil nil nil "commit" "-m" message)))
              (if (zerop commit-rc)
                  (bergheim/agent-notes--async-push root)
                (message "agent-notes: git commit failed (exit %d) in %s"
                         commit-rc root))))
           (t
            (message "agent-notes: git diff --cached errored (exit %d) in %s"
                     diff-rc root)))))))))

;;; Internal file/buffer lifecycle
;;
;; `--with-file' owns the agent-edit lifecycle: visit FILE, revert to disk
;; state, execute BODY, and save atomically. Concurrent modification during
;; BODY is detected via `verify-visited-file-modtime' and raised as an error
;; instead of clobbering the other process's writes.

(defmacro bergheim/agent-org--with-file (file &rest body)
  "Visit FILE safely for an agent edit. If FILE is already open in an Emacs
buffer with unsaved changes, error rather than clobbering them. Otherwise,
revert to disk state, execute BODY at point-min, and save only if BODY
modified the buffer. Errors on concurrent external modification before save."
  (declare (indent 1) (debug t))
  (let ((path (make-symbol "path"))
        (existing (make-symbol "existing")))
    `(let* ((,path ,file)
            (,existing (get-file-buffer ,path)))
       (when (and ,existing (buffer-modified-p ,existing))
         (error "Refusing to edit %s: buffer has unsaved changes" ,path))
       (with-current-buffer (find-file-noselect ,path t)
         (let ((auto-revert-mode nil)
               (super-save-mode nil))
           (revert-buffer t t)
           (goto-char (point-min))
           (prog1
               (progn ,@body)
             (when (buffer-modified-p)
               (unless (verify-visited-file-modtime (current-buffer))
                 (error "File modified externally while editing: %s" ,path))
               (save-buffer))))))))

;;; Cross-project worklog
;;
;; Every successful state transition or note-add appends a single org
;; entry to a stash-side worklog. Per-project `docs/TODO.org' remains
;; the source of truth (full body text, LOGBOOK, etc.); the worklog is
;; a denormalized chronological tape for cross-project scanning from
;; either the host shell (`rg DONE ~/stash/worklog.org') or `org-agenda'.

(defvar bergheim/agent-worklog-dir
  (cl-find-if #'file-directory-p
              '("/workspaces/stash/" "~/stash/"))
  "Stash dir resolved for host or devcontainer. nil disables logging.")

(defun bergheim/agent-worklog--project-name (file)
  "Best-guess project name: nearest git root's basename."
  (let ((abs (expand-file-name file)))
    (file-name-nondirectory
     (directory-file-name
      (or (locate-dominating-file abs ".git")
          (file-name-directory abs))))))

(defun bergheim/agent-worklog-append (file heading state-from state-to
                                           &optional note)
  "Append one worklog entry for the action on FILE/HEADING.
STATE-FROM and STATE-TO bracket a transition; both nil means a
plain note add. No-op when `bergheim/agent-worklog-dir' is unset.
Returns the absolute worklog path on append, nil on no-op — callers
add this to their `:wrote' list so agents re-Read it before any
subsequent Edit."
  (when bergheim/agent-worklog-dir
    (let* ((project (bergheim/agent-worklog--project-name file))
           (now     (format-time-string "[%Y-%m-%d %a %H:%M]"))
           (state   (or state-to "NOTE"))
           (transition (if state-from
                           (format "%s → %s" state-from state-to)
                         state))
           (link    (format "[[file:%s::*%s][%s]]"
                            (expand-file-name file) heading
                            (file-name-nondirectory file)))
           (entry   (concat
                     (format "* %s [%s] %s  %s\n"
                             now project state heading)
                     ":PROPERTIES:\n"
                     (format ":PROJECT:    %s\n" project)
                     (format ":TRANSITION: %s\n" transition)
                     (format ":SOURCE:     %s\n" link)
                     ":END:\n"
                     (when (and note (not (string-empty-p note)))
                       (concat note "\n"))))
           (path (expand-file-name "worklog.org"
                                   bergheim/agent-worklog-dir)))
      (write-region entry nil path 'append 'silent)
      path)))

;;; Heading selectors

(defun bergheim/agent-org--strip (s)
  "Strip text properties from S. Returns nil if S is nil."
  (and s (substring-no-properties s)))

(defun bergheim/agent-org--strip-list (lst)
  "Strip text properties from each string in LST."
  (mapcar #'bergheim/agent-org--strip lst))

(defun bergheim/agent-org--find-unique-heading (heading-re)
  "Move point to the unique heading whose heading line matches HEADING-RE.
Matches against heading lines only, not body text. Error if no match; on
multiple matches, error with the line numbers of each matching heading."
  (goto-char (point-min))
  (let (matches)
    (while (re-search-forward (concat "^\\*+ +.*" heading-re) nil t)
      (save-excursion
        (org-back-to-heading t)
        (let* ((bol (line-beginning-position))
               (line-num (line-number-at-pos bol)))
          ;; De-dup if the HEADING-RE also happened to match earlier on the
          ;; same heading line via a looser pattern.
          (unless (assoc bol matches)
            (push (cons bol line-num) matches)))
        (end-of-line)))
    (setq matches (nreverse matches))
    (cond
     ((null matches)
      (error "Heading not found: %s" heading-re))
     ((cdr matches)
      (error "Heading regex %S is ambiguous (%d matches at lines %s)"
             heading-re
             (length matches)
             (mapconcat (lambda (m) (number-to-string (cdr m))) matches ", ")))
     (t
      (goto-char (caar matches))
      (point)))))

(defun bergheim/agent-org--find-by-id (id)
  "Move point to the heading carrying :ID: equal to ID in the current buffer.
Error if not found. Returns point."
  (goto-char (point-min))
  (if (re-search-forward
       (concat "^[[:space:]]*:ID:[[:space:]]+"
               (regexp-quote id)
               "[[:space:]]*$")
       nil t)
      (progn
        (org-back-to-heading t)
        (point))
    (error "ID not found: %s" id)))

;;; State transition

(defun bergheim/agent-org--ensure-session-id ()
  "Ensure the heading at point has a :SESSION_ID: property. Returns the ID."
  (or (org-entry-get nil "SESSION_ID")
      (let ((id (format "%s-%06x"
                        (format-time-string "%Y%m%dT%H%M%SZ" (current-time) t)
                        (random #xFFFFFF))))
        (org-entry-put nil "SESSION_ID" id)
        id)))

(defun bergheim/agent-org--clock-in ()
  "Clock in on the heading at point, non-interactively."
  (let ((org-clock-in-resume nil)
        (org-clock-persist nil))
    (when (org-clocking-p)
      (ignore-errors (org-clock-out nil t)))
    (ignore-errors (org-clock-in))))

(defun bergheim/agent-org--clocking-current-heading-p ()
  "Non-nil when the active org clock is on the heading at point."
  (and (org-clocking-p)
       (markerp org-clock-hd-marker)
       (marker-buffer org-clock-hd-marker)
       (eq (current-buffer) (marker-buffer org-clock-hd-marker))
       (save-excursion
         (org-back-to-heading t)
         (= (point) (marker-position org-clock-hd-marker)))))

(defun bergheim/agent-org--clock-out ()
  "Clock out only if the active clock is on the heading at point.
Leaves clocks running on other headings alone."
  (when (bergheim/agent-org--clocking-current-heading-p)
    (ignore-errors (org-clock-out nil t))))

(defun bergheim/agent-org--apply-state (new-state note ensure-session-id clock)
  "At point-on-heading, apply NEW-STATE. Optionally attach NOTE, assign
SESSION_ID (on INPROGRESS), and clock in/out on the state transition.
Notes always land in the :LOGBOOK: drawer regardless of user config.
Returns the prior state (string or nil), so callers can log transitions."
  (let ((old-state (bergheim/agent-org--strip (org-get-todo-state)))
        (org-log-into-drawer "LOGBOOK"))
    (org-todo new-state)
    (let ((actual-state (org-get-todo-state)))
      (unless (equal actual-state new-state)
        (error "State change blocked: %s -> %s (got %s)"
               old-state new-state actual-state))
      (when (and ensure-session-id (equal new-state "INPROGRESS"))
        (bergheim/agent-org--ensure-session-id))
      (when clock
        (cond
         ((equal new-state "INPROGRESS")
          (bergheim/agent-org--clock-in))
         ((member new-state '("DONE" "BLOCKED" "CANCELLED"))
          (bergheim/agent-org--clock-out))))
      ;; If a NOTE was requested but org-todo's state-change config did not
      ;; trigger the log-setup machinery, force one so the note is persisted.
      (when (and note
                 (not (memq 'org-add-log-note (default-value 'post-command-hook)))
                 (not (get-buffer "*Org Note*")))
        (org-add-log-setup 'note nil nil 'findpos))
      (when (memq 'org-add-log-note (default-value 'post-command-hook))
        (remove-hook 'post-command-hook 'org-add-log-note)
        (org-add-log-note))
      (when (get-buffer "*Org Note*")
        (with-current-buffer "*Org Note*"
          (goto-char (point-max))
          (when note (insert note))
          (org-store-log-note))))
    old-state))

;;; Public API

(defun bergheim/agent-org-set-state (file heading-re new-state
                                          &optional note ensure-session-id clock)
  "Transition the UNIQUE TODO matching HEADING-RE in FILE to NEW-STATE.
Errors if HEADING-RE matches zero or multiple headings.

Optional args:
- NOTE: attach a state-transition log note
- ENSURE-SESSION-ID: when non-nil and NEW-STATE is INPROGRESS, add
  :SESSION_ID: property if absent
- CLOCK: when non-nil, `org-clock-in' on INPROGRESS and `org-clock-out'
  on DONE/BLOCKED/CANCELLED

Safe from `emacsclient --eval' — never prompts interactively.

Returns a plist:
  :wrote      list of absolute paths the helper modified (may be empty
              if the change was a no-op)
  :state      the new state string
  :state-from the prior state string (may be nil)
  :heading    the matched heading text (TODO/tags/priority stripped)

Agents should re-Read every path in `:wrote' before any subsequent
Edit so the harness's mtime check does not fire."
  (let ((inhibit-message t)
        old-state heading dirty worklog-path)
    (bergheim/agent-org--with-file file
      (bergheim/agent-org--find-unique-heading heading-re)
      (setq heading (bergheim/agent-org--strip (org-get-heading t t t t)))
      (setq old-state
            (bergheim/agent-org--apply-state
             new-state note ensure-session-id clock))
      (setq dirty (buffer-modified-p)))
    (bergheim/agent-notes--maybe-commit
     file (format "state: → %s (%s)" new-state heading-re))
    (setq worklog-path
          (bergheim/agent-worklog-append file heading old-state new-state note))
    (list :wrote (delq nil (list (when dirty (expand-file-name file))
                                 worklog-path))
          :state new-state
          :state-from old-state
          :heading heading)))

(defun bergheim/agent-org-set-state-by-id (file id new-state
                                                &optional note ensure-session-id clock)
  "Like `bergheim/agent-org-set-state' but selects the heading by its
:ID: property. IDs are globally unique so ambiguity is not possible.

Returns the same plist shape as `bergheim/agent-org-set-state', plus
`:id' echoing the input ID for caller convenience."
  (let ((inhibit-message t)
        old-state heading dirty worklog-path)
    (bergheim/agent-org--with-file file
      (bergheim/agent-org--find-by-id id)
      (setq heading (bergheim/agent-org--strip (org-get-heading t t t t)))
      (setq old-state
            (bergheim/agent-org--apply-state
             new-state note ensure-session-id clock))
      (setq dirty (buffer-modified-p)))
    (bergheim/agent-notes--maybe-commit
     file (format "state: → %s (id %s)" new-state id))
    (setq worklog-path
          (bergheim/agent-worklog-append file heading old-state new-state note))
    (list :wrote (delq nil (list (when dirty (expand-file-name file))
                                 worklog-path))
          :state new-state
          :state-from old-state
          :heading heading
          :id id)))

(defun bergheim/agent-org-ensure-id (file heading-re)
  "Ensure the unique heading matching HEADING-RE in FILE carries an :ID:.
Uses `org-id-get-create'. Idempotent — when the heading already has an
ID, the buffer is not modified.

Returns a plist:
  :wrote   list of absolute paths the helper modified — empty when the
           ID was already present
  :id      the existing or newly-created ID string
  :heading the matched heading text"
  (let ((inhibit-message t)
        id heading dirty)
    (bergheim/agent-org--with-file file
      (bergheim/agent-org--find-unique-heading heading-re)
      (setq heading (bergheim/agent-org--strip (org-get-heading t t t t)))
      (setq id (bergheim/agent-org--strip (org-id-get-create)))
      (setq dirty (buffer-modified-p)))
    (bergheim/agent-notes--maybe-commit
     file (format "id: ensure %s" heading-re))
    (list :wrote (when dirty (list (expand-file-name file)))
          :id id
          :heading heading)))

(defun bergheim/agent-org-add-note (file heading-re note)
  "Append NOTE to the :LOGBOOK: of the unique heading matching HEADING-RE
in FILE, without changing the TODO state.

Returns a plist with `:wrote' (list of modified paths) and `:heading'."
  (let ((inhibit-message t)
        heading dirty worklog-path)
    (bergheim/agent-org--with-file file
      (bergheim/agent-org--find-unique-heading heading-re)
      (setq heading (bergheim/agent-org--strip (org-get-heading t t t t)))
      (let ((org-log-into-drawer "LOGBOOK"))
        (org-add-log-setup 'note nil nil 'findpos)
        (when (memq 'org-add-log-note (default-value 'post-command-hook))
          (remove-hook 'post-command-hook 'org-add-log-note)
          (org-add-log-note))
        (when (get-buffer "*Org Note*")
          (with-current-buffer "*Org Note*"
            (goto-char (point-max))
            (insert note)
            (org-store-log-note))))
      (setq dirty (buffer-modified-p)))
    (bergheim/agent-notes--maybe-commit
     file (format "note: %s" heading-re))
    (setq worklog-path
          (bergheim/agent-worklog-append file heading nil nil note))
    (list :wrote (delq nil (list (when dirty (expand-file-name file))
                                 worklog-path))
          :heading heading)))

(defun bergheim/agent-org-add-tag (file heading-re tag)
  "Add TAG (string or list of strings) to the unique heading matching
HEADING-RE in FILE. Idempotent: when the tag is already present, the
buffer is not modified.

Returns a plist:
  :wrote   list of modified paths — empty on idempotent call
  :tags    final tag list on the heading
  :heading the matched heading text"
  (let ((inhibit-message t)
        heading tags dirty)
    (bergheim/agent-org--with-file file
      (bergheim/agent-org--find-unique-heading heading-re)
      (setq heading (bergheim/agent-org--strip (org-get-heading t t t t)))
      (let* ((new-tags (if (listp tag) tag (list tag)))
             (current (org-get-tags nil t))
             (merged (cl-remove-duplicates
                      (append current new-tags)
                      :test #'string=)))
        (unless (equal (sort (copy-sequence current) #'string<)
                       (sort (copy-sequence merged) #'string<))
          (org-set-tags merged))
        (setq tags (bergheim/agent-org--strip-list (org-get-tags nil t))))
      (setq dirty (buffer-modified-p)))
    (bergheim/agent-notes--maybe-commit
     file (format "tag: +%s (%s)"
                  (if (listp tag) (mapconcat #'identity tag ",") tag)
                  heading-re))
    (list :wrote (when dirty (list (expand-file-name file)))
          :tags tags
          :heading heading)))

(defun bergheim/agent-org-remove-tag (file heading-re tag)
  "Remove TAG (string or list of strings) from the unique heading matching
HEADING-RE in FILE. Idempotent: when the tag is absent, the buffer is
not modified.

Returns the same plist shape as `bergheim/agent-org-add-tag'."
  (let ((inhibit-message t)
        heading tags dirty)
    (bergheim/agent-org--with-file file
      (bergheim/agent-org--find-unique-heading heading-re)
      (setq heading (bergheim/agent-org--strip (org-get-heading t t t t)))
      (let* ((drop-tags (if (listp tag) tag (list tag)))
             (current (org-get-tags nil t))
             (kept (cl-set-difference current drop-tags :test #'string=)))
        (unless (equal (length current) (length kept))
          (org-set-tags kept))
        (setq tags (bergheim/agent-org--strip-list (org-get-tags nil t))))
      (setq dirty (buffer-modified-p)))
    (bergheim/agent-notes--maybe-commit
     file (format "tag: -%s (%s)"
                  (if (listp tag) (mapconcat #'identity tag ",") tag)
                  heading-re))
    (list :wrote (when dirty (list (expand-file-name file)))
          :tags tags
          :heading heading)))

;;; Denote-compatible agent helpers
;; Create/find/list/read follow denote's filename convention without requiring
;; denote.el. Linking requires denote.el for proper [[denote:ID]] links.

(defun bergheim/agent-denote--slugify (title)
  "Convert TITLE to a denote-compatible filename slug."
  (let* ((s (downcase title))
         (s (replace-regexp-in-string "[^a-z0-9 -]" "" s))
         (s (string-trim s))
         (s (replace-regexp-in-string " +" "-" s))
         (s (replace-regexp-in-string "-\\{2,\\}" "-" s)))
    s))

(defun bergheim/agent-denote--sanitize-keyword (kw)
  "Sanitize KW for use in denote filenames and filetags.
Replaces underscores and spaces with hyphens, strips non-alphanumeric chars."
  (let* ((s (downcase kw))
         (s (replace-regexp-in-string "[_ ]" "-" s))
         (s (replace-regexp-in-string "[^a-z0-9-]" "" s))
         (s (replace-regexp-in-string "-\\{2,\\}" "-" s))
         (s (replace-regexp-in-string "^-\\|-$" "" s)))
    s))

(defun bergheim/agent-denote--parse-filename (filepath)
  "Parse a denote-format FILEPATH into plist with :id :title :keywords :path.
Returns nil if the filename doesn't match denote format."
  (let ((name (file-name-sans-extension (file-name-nondirectory filepath))))
    (when (string-match "\\`\\([0-9]\\{8\\}T[0-9]\\{6\\}\\(?:-[0-9]+\\)?\\)--\\([^_]+\\)\\(?:__\\(.+\\)\\)?\\'" name)
      (list :id (match-string 1 name)
            :title (replace-regexp-in-string "-" " " (match-string 2 name))
            :keywords (when (match-string 3 name)
                        (split-string (match-string 3 name) "_"))
            :path filepath))))

(defun bergheim/agent-denote-create (dir title keywords &optional body)
  "Create a denote-format note in DIR with TITLE, KEYWORDS list, and BODY.
KEYWORDS are sanitized (underscores/spaces become hyphens).
On same-second collision, appends a counter suffix to the ID.

Returns a plist:
  :wrote list with the absolute file path
  :path  the absolute file path (same value, for caller convenience)
  :id    the denote identifier
  :title the input title

Safe for emacsclient --eval."
  (let* ((inhibit-message t)
         (id (format-time-string "%Y%m%dT%H%M%S"))
         (slug (bergheim/agent-denote--slugify title))
         (clean-kw (seq-filter (lambda (s) (not (string-empty-p s)))
                               (mapcar #'bergheim/agent-denote--sanitize-keyword keywords)))
         (kw-part (if clean-kw (concat "__" (mapconcat #'identity clean-kw "_")) ""))
         (dir (expand-file-name dir))
         (date-str (format-time-string "[%Y-%m-%d %a %H:%M]"))
         (tags-str (if clean-kw
                       (concat ":" (mapconcat #'identity clean-kw ":") ":")
                     ""))
         filepath filename final-id)
    (when (string-empty-p slug)
      (setq slug "untitled"))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (setq final-id id
          filename (concat final-id "--" slug kw-part ".org")
          filepath (expand-file-name filename dir))
    (let ((counter 0)
          (content (concat (format "#+title:      %s\n" title)
                           (format "#+date:       %s\n" date-str)
                           (format "#+filetags:   %s\n" tags-str)
                           (format "#+identifier: %s\n" id)
                           "\n"
                           (if body (concat body "\n") "")))
          (written nil))
      (while (not written)
        (condition-case nil
            (progn
              (write-region content nil filepath nil nil nil 'excl)
              (setq written t))
          (file-already-exists
           (setq counter (1+ counter)
                 final-id (format "%s-%d" id counter)
                 filename (concat final-id "--" slug kw-part ".org")
                 filepath (expand-file-name filename dir)
                 content (replace-regexp-in-string
                          "^#\\+identifier:.*$"
                          (format "#+identifier: %s" final-id)
                          content)))))
      (bergheim/agent-notes--maybe-commit filepath (format "note: %s" title))
      (list :wrote (list filepath)
            :path filepath
            :id final-id
            :title title))))

(defun bergheim/agent-denote-find (dir &optional keywords title-re)
  "Find denote notes in DIR, optionally filtered by KEYWORDS and TITLE-RE.
KEYWORDS is a list of strings; a note matches if it has ALL of them.
TITLE-RE is a regexp matched against the title (spaces, not hyphens).
Returns list of plists (:id :title :keywords :path) sorted newest first."
  (let* ((dir (expand-file-name dir))
         (files (and (file-directory-p dir)
                     (directory-files dir t "\\`[0-9]\\{8\\}T[0-9]\\{6\\}\\(-[0-9]+\\)?--.*\\.org\\'" t)))
         (parsed (delq nil (mapcar #'bergheim/agent-denote--parse-filename files)))
         (filtered
          (seq-filter
           (lambda (note)
             (and (or (null keywords)
                      (let ((nk (plist-get note :keywords)))
                        (seq-every-p (lambda (k) (member k nk)) keywords)))
                  (or (null title-re)
                      (string-match-p title-re (plist-get note :title)))))
           parsed)))
    (sort filtered (lambda (a b)
                     (string> (plist-get a :id) (plist-get b :id))))))

(defun bergheim/agent-denote-read (filepath)
  "Read denote note at FILEPATH. Returns content as string."
  (unless (file-exists-p filepath)
    (error "Note not found: %s" filepath))
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(defun bergheim/agent-denote-list (dir &optional limit)
  "List denote notes in DIR, newest first. Returns up to LIMIT entries (default 10).
Each entry is a plist with :id :title :keywords :path."
  (let* ((all (bergheim/agent-denote-find dir))
         (n (or limit 10)))
    (seq-take all n)))

(defun bergheim/agent-denote-link (source-path target-paths)
  "Add denote links from SOURCE-PATH to each file in TARGET-PATHS.
Appends a \"Related notes\" section if absent, then adds any links not
already present. Uses denote.el APIs for proper [[denote:ID]] links.
TARGET-PATHS is a list of absolute paths to denote notes.

Returns a plist:
  :wrote list of paths modified (just SOURCE-PATH if any link was
         added; empty list when every target was already linked)
  :added integer count of links added on this call

Safe for emacsclient --eval."
  (require 'denote)
  (let ((inhibit-message t)
        (denote-directory (file-name-directory source-path))
        (source-buf (find-file-noselect source-path t)))
    (with-current-buffer source-buf
      (let ((auto-revert-mode nil)
            (super-save-mode nil))
        (revert-buffer t t)
        (let ((links-to-add
               (delq nil
                     (mapcar
                      (lambda (target)
                        (let* ((id (denote-retrieve-filename-identifier target))
                               (title (denote-retrieve-front-matter-title-value target 'org))
                               (link (denote-format-link target title 'org nil)))
                          (save-excursion
                            (goto-char (point-min))
                            (unless (search-forward (concat "denote:" id) nil t)
                              link))))
                      target-paths))))
          (when links-to-add
            (goto-char (point-min))
            (if (re-search-forward "^\\* Related notes" nil t)
                (progn
                  (if (re-search-forward "^\\*" nil t)
                      (forward-line -1)
                    (goto-char (point-max)))
                  (unless (bolp) (insert "\n")))
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert "\n* Related notes\n"))
            (dolist (link links-to-add)
              (insert "- " link "\n"))
            (save-buffer))
          (when links-to-add
            (bergheim/agent-notes--maybe-commit
             source-path
             (format "note: link %d related" (length links-to-add))))
          (list :wrote (when links-to-add (list source-path))
                :added (length links-to-add)))))))

;;; Autonomous dispatch selector/marker
;; Companion to `jolo autonomous'. Safe for `emacsclient --eval' — never
;; prompts, refreshes stale buffers when possible, and marks by opaque
;; buffer position instead of heading text.
;;
;;   (bergheim/agent-org-autonomous-select ORG-FILE)
;;     Returns a JSON array string. Each element is an object with
;;       position — buffer (point) of the heading, used as stable identity
;;       heading  — title with TODO keyword / tags / priority stripped
;;       body     — entry body, all drawers removed
;;     Items are selected when all of the following hold:
;;       - todo state is TODO, NEXT, or INPROGRESS
;;       - tags include :autonomous:
;;       - no :DISPATCHED: property (idempotency guard)
;;
;;   (bergheim/agent-org-autonomous-mark-dispatched ORG-FILE POSITION TS)
;;     Sets :DISPATCHED: TS on the entry at POSITION. POSITION is the
;;     opaque buffer offset returned by -select above. Returns non-nil
;;     on success, nil if the entry is no longer eligible.

(defconst bergheim/agent-org--autonomous-dispatchable-states
  '("TODO" "NEXT" "INPROGRESS"))

(defmacro bergheim/agent-org--with-quiet-buffer (abs-file &rest body)
  "Visit ABS-FILE and run BODY without interactive prompts.

Suppresses the \"File is read-only on disk; make buffer read-only too?\"
prompt from `find-file-noselect-1', plus any other y/n or yes/no prompts
that would block an autonomous call.

Also reverts the buffer from disk when safe (unmodified + stale modtime):
if the host daemon already had this org file open from an earlier session,
selection must reflect edits made outside Emacs (git checkout, other
tooling) and marks must not save stale contents back over newer work."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'y-or-n-p) #'ignore)
             ((symbol-function 'yes-or-no-p) #'ignore))
     (let ((inhibit-message t)
           (find-file-suppress-same-file-warnings t))
       (with-current-buffer (find-file-noselect ,abs-file)
         (when (and (not (buffer-modified-p))
                    (not (verify-visited-file-modtime)))
           (revert-buffer t t t))
         (let ((inhibit-read-only t))
           ,@body)))))

(defconst bergheim/agent-org--drawer-regexp
  "^[[:space:]]*:[A-Za-z][A-Za-z_-]*:[[:space:]]*\n\\(?:.\\|\n\\)*?^[[:space:]]*:END:[[:space:]]*$"
  "Match any :NAME: ... :END: drawer (including LOGBOOK and custom drawers).")

(defun bergheim/agent-org--autonomous-body ()
  "Body of the entry at point with all drawers removed."
  (save-excursion
    (org-back-to-heading t)
    (let ((start (progn (org-end-of-meta-data t) (point)))
          (end (or (save-excursion (outline-next-heading) (point))
                   (point-max))))
      (string-trim
       (replace-regexp-in-string
        bergheim/agent-org--drawer-regexp ""
        (buffer-substring-no-properties start end))))))

(defun bergheim/agent-org--autonomous-eligible-p ()
  "Non-nil if the entry at point is eligible for autonomous dispatch."
  (and (not (org-entry-get nil "DISPATCHED"))
       (member (org-get-todo-state)
               bergheim/agent-org--autonomous-dispatchable-states)))

(defun bergheim/agent-org-autonomous-select (org-file)
  "Return JSON array of :autonomous: entries without :DISPATCHED: in ORG-FILE.

Each element has three fields: `position' (buffer character offset of the
heading, used as a stable identifier for marking), `heading' (stripped
heading text), and `body' (body with drawers removed)."
  (let ((abs (expand-file-name org-file))
        (items nil))
    (bergheim/agent-org--with-quiet-buffer abs
      (org-with-wide-buffer
       (org-map-entries
        (lambda ()
          (when (and (member "autonomous" (org-get-tags))
                     (bergheim/agent-org--autonomous-eligible-p))
            (push `((position . ,(point))
                    (heading . ,(substring-no-properties
                                 (org-get-heading t t t t)))
                    (body . ,(bergheim/agent-org--autonomous-body)))
                  items)))
        nil nil)))
    ;; `json-encode' on nil returns "null"; force array encoding so the empty
    ;; case round-trips as JSON "[]".
    (json-encode-array (nreverse items))))

(defun bergheim/agent-org-autonomous-mark-dispatched (org-file position timestamp)
  "Set :DISPATCHED: TIMESTAMP on the entry at POSITION in ORG-FILE.

POSITION is the `(point)' value returned by `-select'. Using the buffer
position instead of heading text avoids mis-marking duplicate-titled
entries. Returns non-nil if the mark was applied, nil if the entry at
POSITION is no longer `:autonomous:' or is no longer eligible."
  (let ((abs (expand-file-name org-file))
        (marked nil))
    (bergheim/agent-org--with-quiet-buffer abs
      (org-with-wide-buffer
       (goto-char position)
       (when (and (ignore-errors (org-back-to-heading t) t)
                  (member "autonomous" (org-get-tags))
                  (bergheim/agent-org--autonomous-eligible-p))
         (org-entry-put nil "DISPATCHED" timestamp)
         (setq marked t)))
      (when marked (save-buffer)))
    (when marked
      (bergheim/agent-notes--maybe-commit
       org-file (format "dispatch: mark %s" timestamp)))
    marked))

(provide 'bergheim-agent-helpers)
;;; bergheim-agent-helpers.el ends here
