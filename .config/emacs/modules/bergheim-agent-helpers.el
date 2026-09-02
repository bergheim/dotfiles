;;; bergheim-agent-helpers.el --- Agent helpers for org-mode and denote -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-id)

;; Declared special so the let-bindings below stay dynamic even when this
;; file is byte- or native-compiled before autorevert/super-save/denote load.
(defvar auto-revert-mode)
(defvar super-save-mode)
(defvar denote-directory)
(defvar denote-kill-buffers)
(defvar denote-rename-confirmations)
(defvar denote-save-buffers)

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
           (rel (file-relative-name (expand-file-name file) default-directory))
           (add-rc (call-process "git" nil nil nil "add" "--" rel)))
      (cond
       ((not (zerop add-rc))
        (message "agent-notes: git add failed (exit %d) in %s" add-rc root))
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
;;
;; `ask-user-about-supersession-threat' is NOT y-or-n-p. It uses `read-event'
;; for "changed on disk; really edit the buffer? (y, n, r)". An unanswered
;; prompt holds the emacsclient socket, so every later -e piles up behind it
;; — including evals from every other container sharing the daemon.

(defun bergheim/agent--ask-supersession (fn)
  "Non-interactive stand-in for `ask-user-about-supersession-threat'.
Revert a clean buffer from disk. Error if the buffer has unsaved edits.
Never prompt — emacsclient --eval cannot answer a minibuffer."
  (cond
   ((buffer-modified-p)
    (error "Refusing to edit %s: changed on disk and buffer has unsaved edits" fn))
   (t
    (revert-buffer t t t)
    nil)))

(defmacro bergheim/agent--noninteractive (&rest body)
  "Evaluate BODY with every prompt an agent could hit disabled.

Prompts we can answer correctly get a non-interactive answer: supersession
reverts a clean buffer or errors, locks are stolen, symlinks are followed.
Every other minibuffer read signals `inhibited-interaction' instead of
waiting — a hung emacsclient blocks the whole daemon, so a loud error is
always the better failure and tells us what to fix here.

Bound only for the dynamic extent of an agent call: interactive Emacs
keeps its prompts."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'ask-user-about-supersession-threat)
              #'bergheim/agent--ask-supersession)
             ((symbol-function 'ask-user-about-lock)
              (lambda (&rest _) t)))
     (let ((inhibit-interaction t)
           (vc-follow-symlinks t)
           (create-lockfiles nil)
           (large-file-warning-threshold nil)
           (find-file-suppress-same-file-warnings t))
       ,@body)))

(defmacro bergheim/agent-org--with-file (file &rest body)
  "Visit FILE safely for an agent edit. If FILE is already open in an Emacs
buffer with unsaved changes, error rather than clobbering them. Otherwise,
revert to disk state, execute BODY at point-min, and save only if BODY
modified the buffer. Errors on concurrent external modification before save."
  (declare (indent 1) (debug t))
  (let ((path (make-symbol "path"))
        (existing (make-symbol "existing")))
    `(bergheim/agent--noninteractive
       (let* ((,path ,file)
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
                 (save-buffer)))))))))

;;; Prose unfilling
;;
;; Agents hard-wrap prose at ~80 columns out of habit. Notes are read with
;; `visual-line-mode', so those breaks buy nothing and cost real edits: a
;; reworded sentence reflows every following line into the diff. Bodies are
;; unfilled on the way in, so no agent can persist a wrapped paragraph.
;;
;; Conservative by construction: when a line is not plainly prose, its newline
;; is kept. Only a continuation of a paragraph or a list item folds upward.

(defconst bergheim/agent-notes--list-item-re
  "\\`[ \t]*\\(?:[-+][ \t]\\|[ \t]\\*[ \t]\\|[0-9]+[.)][ \t]\\)"
  "List items. Keep the newline, but fold their wrapped continuations in.
A `*' bullet must be indented; at column zero it is a heading.")

(defconst bergheim/agent-notes--structural-re
  "\\`\\(?:\\*+[ \t]\\|[ \t]*[:|#]\\)"
  "Headings, drawers, property and fixed-width lines, tables, keywords.
These keep their newline and nothing folds into them.")

(defun bergheim/agent-notes--block-name (line prefix)
  "Return the block name when LINE opens or closes with PREFIX, else nil."
  (let ((s (downcase (string-trim-left line))))
    (when (string-prefix-p prefix s)
      (or (car (split-string (substring s (length prefix)))) ""))))

(defun bergheim/agent-notes--open-link-p (s)
  "Non-nil when S ends inside an unclosed Org link, where a space would corrupt."
  (let ((opens 0) (closes 0) (i 0))
    (while (setq i (string-search "[[" s i)) (setq opens (1+ opens) i (+ i 2)))
    (setq i 0)
    (while (setq i (string-search "]]" s i)) (setq closes (1+ closes) i (+ i 2)))
    (> opens closes)))

(defun bergheim/agent-notes-unfill (text)
  "Join hard-wrapped prose lines in TEXT, leaving Org structure intact.
Paragraphs and list items collapse to one line each. Blank lines, headings,
tables, drawers, fixed-width lines, `#+' keywords, explicit `\\\\' breaks and
everything inside a `#+begin_'/`#+end_' block are passed through untouched."
  (if (not (stringp text))
      text
    (let ((blocks nil) (out nil) (joinable nil))
      (dolist (line (split-string (string-replace "\r" "" text) "\n"))
        (let ((trimmed (string-trim-left line))
              (name nil))
          (cond
           (blocks
            (push line out)
            (cond
             ((setq name (bergheim/agent-notes--block-name line "#+begin_"))
              (push name blocks))
             ((equal (car blocks) (bergheim/agent-notes--block-name line "#+end_"))
              (pop blocks))))
           ((setq name (bergheim/agent-notes--block-name line "#+begin_"))
            (push name blocks) (push line out) (setq joinable nil))
           ((string-empty-p trimmed)
            (push line out) (setq joinable nil))
           ((string-match-p bergheim/agent-notes--list-item-re line)
            (push line out) (setq joinable t))
           ((string-match-p bergheim/agent-notes--structural-re line)
            (push line out) (setq joinable nil))
           (joinable
            (let ((prev (string-trim-right (car out))))
              (setcar out (concat prev
                                  (if (bergheim/agent-notes--open-link-p prev) "" " ")
                                  trimmed))))
           (t (push line out) (setq joinable t)))
          ;; An explicit Org line break ends the paragraph as far as we care.
          (when (string-suffix-p "\\\\" (string-trim-right line))
            (setq joinable nil))))
      (string-join (nreverse out) "\n"))))

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

(defun bergheim/agent-org--log-session-line (agent session-id)
  "Append a session line to the LOGBOOK of the heading at point.
AGENT identifies the tool and model (e.g. \"claude/claude-fable-5 (high)\");
SESSION-ID is the vendor session id, omitted when nil. Also sets the
last-writer-wins `:LAST_AGENT:' property; full history stays in LOGBOOK."
  (save-excursion
    (org-back-to-heading t)
    (org-entry-put nil "LAST_AGENT" agent)
    (let ((org-log-into-drawer "LOGBOOK"))
      (goto-char (org-log-beginning t))
      (insert "- Session " agent
              (if session-id (concat " " session-id) "")
              " " (format-time-string "[%Y-%m-%d %a %H:%M]") "\n"))))

;;; Durations
;;
;; Agents do not clock. `org-clock' keeps one marker per Emacs process, and
;; every agent in a container shares one daemon, so clocking in on B first
;; clocks out of A — writing A's closing timestamp into A's buffer, outside
;; the `--with-file' that owns it. That buffer is then modified-but-unsaved
;; and every later helper call on A fails with "buffer has unsaved changes".
;; Not a race: emacsclient is single-threaded, so it reproduces every time.
;; It is a side effect on a file the call never opened.
;;
;; The intervals survive anyway. `org-todo' logs a timestamped state line for
;; every transition (INPROGRESS carries `!' in the host keyword set), so a
;; heading's spans are last INPROGRESS -> next other state, and the same pairs
;; land in the stash worklog for cross-project sums. Compute when asked; a
;; heading abandoned at INPROGRESS then reads as open since T rather than
;; being billed the whole gap, which is what writing a closing CLOCK line at
;; exit time would do.

(defun bergheim/agent-org--apply-state (new-state note agent session-id)
  "At point-on-heading, apply NEW-STATE. Optionally attach NOTE and, when
AGENT is non-nil, a LOGBOOK session line plus `:LAST_AGENT:'.
Notes always land in the :LOGBOOK: drawer regardless of user config.
Returns the prior state (string or nil), so callers can log transitions."
  (when (and agent (not (stringp agent)))
    (error "AGENT must be a string, got %S — the old ENSURE-SESSION-ID/CLOCK args are gone" agent))
  (when (and session-id (not (stringp session-id)))
    (error "SESSION-ID must be a string, got %S" session-id))
  (let ((old-state (bergheim/agent-org--strip (org-get-todo-state)))
        (org-log-into-drawer "LOGBOOK"))
    (org-todo new-state)
    (let ((actual-state (org-get-todo-state)))
      (unless (equal actual-state new-state)
        (error "State change blocked: %s -> %s (got %s)"
               old-state new-state actual-state))
      (when agent
        (bergheim/agent-org--log-session-line agent session-id))
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

(defun bergheim/agent-org--log-tag-line (signed-tags)
  "Append a LOGBOOK line recording a tag change on the heading at point.
SIGNED-TAGS is a list like (\"+autonomous\" \"-blocked\")."
  (save-excursion
    (org-back-to-heading t)
    (let ((org-log-into-drawer "LOGBOOK"))
      (goto-char (org-log-beginning t))
      (insert "- Tag \"" (mapconcat #'identity signed-tags " ") "\" "
              (format-time-string "[%Y-%m-%d %a %H:%M]") "\n"))))

;;; Public API

(defun bergheim/agent-org-task-set-state (file heading-re new-state
                                          &optional note agent session-id)
  "Transition the UNIQUE TODO matching HEADING-RE in FILE to NEW-STATE.
Errors if HEADING-RE matches zero or multiple headings.

Optional args:
- NOTE: attach a state-transition log note
- AGENT: tool/model doing the work, e.g. \"claude/claude-fable-5 (high)\" —
  logged as a LOGBOOK session line and mirrored to `:LAST_AGENT:'.
  Callers obtain it from the `agent-meta' script; never hand-type it.
- SESSION-ID: the vendor session id accompanying AGENT

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
             new-state note agent session-id))
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

(defun bergheim/agent-org-task-set-state-by-id (file id new-state
                                                &optional note agent session-id)
  "Like `bergheim/agent-org-task-set-state' but selects the heading by its
:ID: property. IDs are globally unique so ambiguity is not possible.

Returns the same plist shape as `bergheim/agent-org-task-set-state', plus
`:id' echoing the input ID for caller convenience."
  (let ((inhibit-message t)
        old-state heading dirty worklog-path)
    (bergheim/agent-org--with-file file
      (bergheim/agent-org--find-by-id id)
      (setq heading (bergheim/agent-org--strip (org-get-heading t t t t)))
      (setq old-state
            (bergheim/agent-org--apply-state
             new-state note agent session-id))
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

(defun bergheim/agent-org-task--set-planning
    (file locator date by-id kind)
  "Set or clear planning KIND on one task and return its current planning."
  (unless (or (null date)
              (and (stringp date)
                   (not (string-empty-p (string-trim date)))))
    (error "DATE must be a non-empty Org date string or nil"))
  (let ((inhibit-message t)
        heading scheduled deadline dirty)
    (bergheim/agent-org--with-file file
      (if by-id
          (bergheim/agent-org--find-by-id locator)
        (bergheim/agent-org--find-unique-heading locator))
      (unless (org-get-todo-state)
        (error "Not a task (heading has no TODO keyword): %s" locator))
      (setq heading (bergheim/agent-org--strip (org-get-heading t t t t)))
      (funcall (if (eq kind 'scheduled) #'org-schedule #'org-deadline)
               (when (null date) '(4)) date)
      (setq scheduled (org-entry-get nil "SCHEDULED")
            deadline (org-entry-get nil "DEADLINE")
            dirty (buffer-modified-p)))
    (when dirty
      (bergheim/agent-notes--maybe-commit
       file (format "%s: %s (%s)"
                    kind (or date "clear") locator)))
    (list :wrote (when dirty (list (expand-file-name file)))
          :heading heading
          :scheduled scheduled
          :deadline deadline)))

(defun bergheim/agent-org-task-schedule (file locator date &optional by-id)
  "Set task LOCATOR's schedule to DATE, or clear it when DATE is nil.
LOCATOR is a unique heading regexp, or an :ID: when BY-ID is non-nil.
Returns a plist with `:wrote', `:heading', `:scheduled', and `:deadline'."
  (bergheim/agent-org-task--set-planning
   file locator date by-id 'scheduled))

(defun bergheim/agent-org-task-deadline (file locator date &optional by-id)
  "Set task LOCATOR's deadline to DATE, or clear it when DATE is nil.
LOCATOR is a unique heading regexp, or an :ID: when BY-ID is non-nil.
Returns a plist with `:wrote', `:heading', `:scheduled', and `:deadline'."
  (bergheim/agent-org-task--set-planning
   file locator date by-id 'deadline))

(defun bergheim/agent-org-entry-ensure-id (file heading-re)
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

(defun bergheim/agent-org-task-add-log (file heading-re note)
  "Append NOTE to the :LOGBOOK: of the unique task matching HEADING-RE.
The heading must carry a TODO keyword. Its state is not changed.

Returns a plist with `:wrote' (list of modified paths) and `:heading'."
  (let ((inhibit-message t)
        heading dirty worklog-path)
    (bergheim/agent-org--with-file file
      (bergheim/agent-org--find-unique-heading heading-re)
      (unless (org-get-todo-state)
        (error "Not a task (heading has no TODO keyword): %s" heading-re))
      (setq heading (bergheim/agent-org--strip (org-get-heading t t t t)))
      (let ((org-log-into-drawer "LOGBOOK"))
        (org-add-log-setup 'note nil nil 'findpos)
        (when (memq 'org-add-log-note (default-value 'post-command-hook))
          (remove-hook 'post-command-hook 'org-add-log-note)
          (org-add-log-note))
        (when (get-buffer "*Org Note*")
          (with-current-buffer "*Org Note*"
            (goto-char (point-max))
            (insert (bergheim/agent-notes-unfill note))
            (org-store-log-note))))
      (setq dirty (buffer-modified-p)))
    (bergheim/agent-notes--maybe-commit
     file (format "note: %s" heading-re))
    (setq worklog-path
          (bergheim/agent-worklog-append file heading nil nil note))
    (list :wrote (delq nil (list (when dirty (expand-file-name file))
                                 worklog-path))
          :heading heading)))

(defun bergheim/agent-org-entry-link-note (org-file locator note-path &optional by-id)
  "Insert a denote link to NOTE-PATH into the ORG-FILE entry at LOCATOR.
LOCATOR is a heading regexp, or an `:ID:' when BY-ID is non-nil.
The link is one body line after the heading's metadata:
  - [[denote:ID][title]]
Existing `denote:ID' is searched in that subtree only, so two TODOs
may cite the same note. Does not modify the note. Does not create a
Related notes section. Idempotent.

`denote-directory' for the call is the note's directory — ORG-FILE is
not treated as a denote file.

Returns a plist:
  :wrote   list of modified paths — empty on idempotent call
  :added   1 or 0
  :id      the note's denote identifier
  :heading the matched heading text"
  (require 'denote)
  (let* ((inhibit-message t)
         (note-abs (expand-file-name note-path))
         (denote-directory (file-name-directory note-abs))
         added id heading dirty)
    (unless (file-regular-p note-abs)
      (error "Note not found: %s" note-abs))
    (setq id (denote-retrieve-filename-identifier note-abs))
    (unless id
      (error "Not a denote note: %s" note-abs))
    (let* ((title (or (denote-retrieve-front-matter-title-value note-abs 'org)
                      id))
           (link (denote-format-link note-abs title 'org nil)))
      (bergheim/agent-org--with-file org-file
        (if by-id
            (bergheim/agent-org--find-by-id locator)
          (bergheim/agent-org--find-unique-heading locator))
        (setq heading (bergheim/agent-org--strip (org-get-heading t t t t)))
        (let ((end (save-excursion (org-end-of-subtree t t) (point))))
          (if (save-excursion
                (re-search-forward (concat "denote:" (regexp-quote id))
                                   end t))
              (setq added 0)
            (org-end-of-meta-data t)
            (unless (bolp) (insert "\n"))
            (insert "- " link "\n")
            (setq added 1)))
        (setq dirty (buffer-modified-p))))
    (when (> added 0)
      (bergheim/agent-notes--maybe-commit
       org-file (format "link: note %s (%s)" id heading)))
    (list :wrote (when dirty (list (expand-file-name org-file)))
          :added added
          :id id
          :heading heading)))

(defun bergheim/agent-org-entry-add-tag (file heading-re tag)
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
          (org-set-tags merged)
          (bergheim/agent-org--log-tag-line
           (mapcar (lambda (tg) (concat "+" tg))
                   (cl-set-difference merged current :test #'string=))))
        (setq tags (bergheim/agent-org--strip-list (org-get-tags nil t))))
      (setq dirty (buffer-modified-p)))
    (bergheim/agent-notes--maybe-commit
     file (format "tag: +%s (%s)"
                  (if (listp tag) (mapconcat #'identity tag ",") tag)
                  heading-re))
    (list :wrote (when dirty (list (expand-file-name file)))
          :tags tags
          :heading heading)))

(defun bergheim/agent-org-entry-remove-tag (file heading-re tag)
  "Remove TAG (string or list of strings) from the unique heading matching
HEADING-RE in FILE. Idempotent: when the tag is absent, the buffer is
not modified.

Returns the same plist shape as `bergheim/agent-org-entry-add-tag'."
  (let ((inhibit-message t)
        heading tags dirty)
    (bergheim/agent-org--with-file file
      (bergheim/agent-org--find-unique-heading heading-re)
      (setq heading (bergheim/agent-org--strip (org-get-heading t t t t)))
      (let* ((drop-tags (if (listp tag) tag (list tag)))
             (current (org-get-tags nil t))
             (kept (cl-set-difference current drop-tags :test #'string=)))
        (unless (equal (length current) (length kept))
          (org-set-tags kept)
          (bergheim/agent-org--log-tag-line
           (mapcar (lambda (tg) (concat "-" tg))
                   (cl-set-difference current kept :test #'string=))))
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
;; Create/find/get/list follow denote's filename convention without requiring
;; denote.el. Update and linking require denote.el for metadata and links.

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
    (let ((counter 0)
          (written nil))
      (while (not written)
        (setq final-id (if (zerop counter) id (format "%s-%d" id counter))
              filename (concat final-id "--" slug kw-part ".org")
              filepath (expand-file-name filename dir))
        (if (directory-files
             dir nil (concat "\\`" (regexp-quote final-id) "--") t)
            (setq counter (1+ counter))
          (let ((content
                 (concat (format "#+title:      %s\n" title)
                         (format "#+date:       %s\n" date-str)
                         (format "#+filetags:   %s\n" tags-str)
                         (format "#+identifier: %s\n" final-id)
                         "\n"
                         (if body (concat (bergheim/agent-notes-unfill body) "\n") ""))))
            (condition-case nil
                (progn
                  (write-region content nil filepath nil nil nil 'excl)
                  (setq written t))
              (file-already-exists
               (setq counter (1+ counter)))))))
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

(defun bergheim/agent-denote-get (filepath)
  "Return the complete content of the denote note at FILEPATH."
  (unless (file-exists-p filepath)
    (error "Note not found: %s" filepath))
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(defun bergheim/agent-denote-update (filepath &rest changes)
  "Update a denote note's :title, :keywords, and/or :body.

CHANGES is a plist. Omitted keys stay unchanged; :keywords nil clears tags
and :body \"\" clears the body. The identifier, date, unknown front matter,
and top-level `Related notes' section are preserved. Returns a plist with
`:wrote', `:path', `:old-path', `:id', `:title', and `:keywords'."
  (require 'denote)
  (unless (zerop (% (length changes) 2))
    (error "CHANGES must contain keyword/value pairs: %S" changes))
  (let ((tail changes)
        seen)
    (while tail
      (let ((key (pop tail)))
        (pop tail)
        (unless (memq key '(:title :keywords :body))
          (error "Unknown denote update key: %S" key))
        (when (memq key seen)
          (error "Duplicate denote update key: %S" key))
        (push key seen))))
  (let* ((abs (expand-file-name filepath))
         (title-p (plist-member changes :title))
         (keywords-p (plist-member changes :keywords))
         (body-p (plist-member changes :body))
         (requested-title (plist-get changes :title))
         (requested-keywords (plist-get changes :keywords))
         (requested-body (plist-get changes :body)))
    (unless (file-regular-p abs)
      (error "Note not found: %s" abs))
    (unless (string-equal (file-name-extension abs) "org")
      (error "Only Org denote notes can be updated: %s" abs))
    (when (and title-p
               (not (and (stringp requested-title)
                         (not (string-empty-p requested-title)))))
      (error ":title must be a non-empty string"))
    (when (and keywords-p
               (not (and (listp requested-keywords)
                         (seq-every-p #'stringp requested-keywords))))
      (error ":keywords must be a list of strings or nil"))
    (when (and body-p (not (stringp requested-body)))
      (error ":body must be a string"))
    ;; Validate/revert the visiting buffer even for metadata-only updates.
    (bergheim/agent-org--with-file abs nil)
    (let* ((id (denote-retrieve-filename-identifier abs))
           (old-title (or (denote-retrieve-front-matter-title-value abs 'org)
                          (denote-retrieve-filename-title abs)
                          ""))
           (old-keywords (denote-retrieve-filename-keywords-as-list abs))
           (title (if title-p requested-title old-title))
           (keywords
            (if keywords-p
                (cl-remove-duplicates
                 (seq-filter
                  (lambda (s) (not (string-empty-p s)))
                  (mapcar #'bergheim/agent-denote--sanitize-keyword
                          requested-keywords))
                 :test #'string=)
              old-keywords))
           (metadata-dirty (or (not (equal title old-title))
                               (not (equal keywords old-keywords))))
           (body-dirty nil)
           (final-path abs))
      (unless id
        (error "Not a denote note: %s" abs))
      (when body-p
        (bergheim/agent-org--with-file abs
          (goto-char (point-min))
          (while (looking-at "^#\\+[[:alnum:]_-]+:.*$")
            (forward-line 1))
          (while (and (< (point) (point-max))
                      (looking-at "^[[:space:]]*$"))
            (forward-line 1))
          (let* ((start (point))
                 (end (or (save-excursion
                            (when (re-search-forward
                                   "^\\* Related notes[[:space:]]*$" nil t)
                              (line-beginning-position)))
                          (point-max)))
                 (current (string-trim-right
                           (buffer-substring-no-properties start end)))
                 (body (string-trim-right
                        (bergheim/agent-notes-unfill requested-body))))
            (unless (equal current body)
              (delete-region start end)
              (unless (string-empty-p body)
                (insert body "\n"
                        (if (< (point) (point-max)) "\n" "")))
              (setq body-dirty t)))))
      (when metadata-dirty
        (let ((denote-directory (file-name-directory abs))
              (denote-rename-confirmations nil)
              (denote-save-buffers t)
              (denote-kill-buffers nil))
          (setq final-path
                (bergheim/agent--noninteractive
                  (denote-rename-file abs title keywords
                                      'keep-current 'keep-current id)))
          (setq title (or (denote-retrieve-front-matter-title-value
                           final-path 'org)
                          title)
                keywords (denote-retrieve-filename-keywords-as-list
                          final-path))))
      (when (or body-dirty metadata-dirty)
        (bergheim/agent-notes--maybe-commit
         final-path (format "note: update %s" title)))
      (list :wrote (when (or body-dirty metadata-dirty) (list final-path))
            :path final-path
            :old-path (unless (equal final-path abs) abs)
            :id id
            :title title
            :keywords keywords))))

(defun bergheim/agent-denote-list (dir &optional limit)
  "List denote notes in DIR, newest first. Returns up to LIMIT entries (default 10).
  Each entry is a plist with :id :title :keywords. To read or link, pass
  :id to the relevant helper, or call `bergheim/agent-denote-find' when
  you need :path."
  (let* ((all (bergheim/agent-denote-find dir))
         (n (or limit 10)))
    (mapcar (lambda (note)
              (list :id (plist-get note :id)
                    :title (plist-get note :title)
                    :keywords (plist-get note :keywords)))
            (seq-take all n))))

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
        (added 0))
    (bergheim/agent-org--with-file source-path
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
          (setq added (length links-to-add)))))
    (when (> added 0)
      (bergheim/agent-notes--maybe-commit
       source-path
       (format "note: link %d related" added)))
    (list :wrote (when (> added 0) (list source-path))
          :added added)))

(defun bergheim/agent-denote-backlinks (filepath)
  "Return the notes that link TO the denote note at FILEPATH.
FILEPATH must be an absolute path to a denote note, not an identifier:
denote's own `denote-get-backlinks' silently returns nil for a bare id
\(see the stash gotcha note on this).

Returns a list of plists (:id :title :keywords :path), one per linking
note, newest first — the same shape as `bergheim/agent-denote-find'.
Returns nil when nothing links to FILEPATH.

Safe for emacsclient --eval."
  (require 'denote)
  (let ((abs (expand-file-name filepath)))
    (unless (file-exists-p abs)
      (error "Note not found: %s" abs))
    (let* ((inhibit-message t)
           (denote-directory (file-name-directory abs))
           (files (denote-get-backlinks abs))
           (parsed (delq nil (mapcar #'bergheim/agent-denote--parse-filename files))))
      (sort parsed (lambda (a b)
                     (string> (plist-get a :id) (plist-get b :id)))))))

;;; Autonomous dispatch selector/marker
;; Companion to `jolo autonomous'. Safe for `emacsclient --eval' — never
;; prompts, refreshes stale buffers when possible, and marks by opaque
;; buffer position instead of heading text.
;;
;;   (bergheim/agent-org-task-autonomous-select ORG-FILE)
;;     Returns a JSON array string. Each element is an object with
;;       position — buffer (point) of the heading, used as stable identity
;;       heading  — title with TODO keyword / tags / priority stripped
;;       body     — entry body, all drawers removed
;;     Items are selected when all of the following hold:
;;       - todo state is TODO, NEXT, or INPROGRESS
;;       - tags include :autonomous:
;;       - no :DISPATCHED: property (idempotency guard)
;;
;;   (bergheim/agent-org-task-autonomous-mark-dispatched ORG-FILE POSITION TS)
;;     Sets :DISPATCHED: TS on the entry at POSITION. POSITION is the
;;     opaque buffer offset returned by -select above. Returns non-nil
;;     on success, nil if the entry is no longer eligible.

(defconst bergheim/agent-org--autonomous-dispatchable-states
  '("TODO" "NEXT" "INPROGRESS"))

(defmacro bergheim/agent-org--with-quiet-buffer (abs-file &rest body)
  "Visit ABS-FILE and run BODY without interactive prompts.

NOWARN drops the read-only, large-file and reread questions
`find-file-noselect' would otherwise ask; `--noninteractive' turns
anything else into an error rather than a stalled socket.

Also reverts the buffer from disk when safe (unmodified + stale modtime):
if the host daemon already had this org file open from an earlier session,
selection must reflect edits made outside Emacs (git checkout, other
tooling) and marks must not save stale contents back over newer work.
A stale buffer carrying unsaved edits errors instead."
  (declare (indent 1))
  `(bergheim/agent--noninteractive
     (let ((inhibit-message t))
       (with-current-buffer (find-file-noselect ,abs-file t)
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

(defun bergheim/agent-org-task-autonomous-select (org-file)
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

(defun bergheim/agent-org-task-autonomous-mark-dispatched (org-file position timestamp)
  "Set :DISPATCHED: TIMESTAMP on the task at POSITION in ORG-FILE.

POSITION is the exact heading position returned by `-select'. Returns nil
rather than searching backward when intervening edits made it stale."
  (let ((abs (expand-file-name org-file))
        (marked nil))
    (bergheim/agent-org--with-quiet-buffer abs
      (org-with-wide-buffer
       (when (and (integerp position)
                  (<= (point-min) position)
                  (<= position (point-max)))
         (goto-char position)
         (when (and (org-at-heading-p)
                    (member "autonomous" (org-get-tags))
                    (bergheim/agent-org--autonomous-eligible-p))
           (org-entry-put nil "DISPATCHED" timestamp)
           (setq marked t))))
      (when marked (save-buffer)))
    (when marked
      (bergheim/agent-notes--maybe-commit
       org-file (format "dispatch: mark %s" timestamp)))
    marked))

(defun bergheim/agent-org-task-create (file heading &optional body tags state)
  "Append a new entry HEADING to FILE as a top-level heading.

STATE defaults to \"TODO\" and must be a keyword declared in FILE's `#+TODO:'.
TAGS is a list of tag strings. BODY, when non-empty, is inserted under the
heading. A stable `:ID:' is generated so the entry can later be addressed with
`bergheim/agent-org-task-set-state-by-id'. Returns a plist (:wrote :id :heading
:state)."
  (let ((inhibit-message t)
        (st (or state "TODO"))
        (id nil))
    ;; Validate inputs against Org syntax before touching the file: a heading is
    ;; a single line, and tags accept only Org's tag character class.
    (when (string-match-p "[[:cntrl:]]" heading)
      (error "Heading must be a single line without control characters: %S" heading))
    (dolist (tag tags)
      (unless (string-match-p "\\`[[:alnum:]_@#%]+\\'" tag)
        (error "Invalid Org tag %S (allowed: letters, digits, _ @ # %%)" tag)))
    (bergheim/agent-org--with-file file
      (unless (member st org-todo-keywords-1)
        (error "Unknown TODO keyword %S (known: %s)" st
               (mapconcat #'identity org-todo-keywords-1 " ")))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (unless (or (bobp)
                  (looking-back "\n\n" (max (point-min) (- (point) 2))))
        (insert "\n"))
      (insert (format "* %s %s" st heading))
      (when tags
        (insert (format "  :%s:" (mapconcat #'identity tags ":"))))
      (insert "\n")
      (org-back-to-heading t)
      (setq id (format "%s-%06x"
                       (format-time-string "%Y%m%dT%H%M%SZ" (current-time) t)
                       (random #xFFFFFF)))
      (org-entry-put nil "ID" id)
      (org-entry-put nil "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))
      (when (and body (not (string-empty-p body)))
        (org-end-of-meta-data t)
        (insert (string-trim-right (bergheim/agent-notes-unfill body)) "\n")))
    (bergheim/agent-notes--maybe-commit file (format "todo: add %s" heading))
    (list :wrote (list (expand-file-name file)) :id id :heading heading :state st)))

(defun bergheim/agent-org--denote-ids-in-entry ()
  "Return denote ids linked from the heading at point, in file order.
Heading-search suffixes (`denote:ID::#custom') are stripped. Duplicates
are dropped."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t) (point)))
          ids)
      (while (re-search-forward "\\[\\[denote:\\([^]:]+\\)" end t)
        (let ((id (match-string-no-properties 1)))
          (unless (member id ids)
            (push id ids))))
      (nreverse ids))))

(defun bergheim/agent-org-task-list (org-file &optional states)
  "Return a JSON array of every task in ORG-FILE.

Each element has `line' (1-based heading line), `state', `heading', `tags',
`notes' (linked denote ids), and `autonomous'. STATES, when non-nil, filters
by TODO keyword. Emacs 31.1 transports large emacsclient replies reliably."
  (let ((abs (expand-file-name org-file)) (items nil))
    (bergheim/agent-org--with-quiet-buffer abs
      (org-with-wide-buffer
       (org-map-entries
        (lambda ()
          (let ((state (org-get-todo-state)))
            (when (and state (or (null states) (member state states)))
              (push `((line . ,(line-number-at-pos (point)))
                      (state . ,(substring-no-properties state))
                      (heading . ,(substring-no-properties (org-get-heading t t t t)))
                      (scheduled . ,(org-entry-get nil "SCHEDULED"))
                      (deadline . ,(org-entry-get nil "DEADLINE"))
                      (tags . ,(vconcat
                                (bergheim/agent-org--strip-list
                                 (org-get-tags))))
                      (notes . ,(vconcat (bergheim/agent-org--denote-ids-in-entry)))
                      (autonomous . ,(and (member "autonomous" (org-get-tags))
                                          (bergheim/agent-org--autonomous-eligible-p) t)))
                    items))))
        nil nil)))
    (json-encode-array (nreverse items))))

(defun bergheim/agent-org-entry-get (file locator &optional by-id)
  "Return the entry matching LOCATOR in FILE as a JSON object.

LOCATOR is a heading regexp, or an `:ID:' value when BY-ID is non-nil. Fields:
`state', `heading', `tags' (inherited, always an array), `priority',
`scheduled', `deadline', `properties' (drawer properties), and `body' (drawers
removed). Errors if the locator does not match a unique entry."
  (let ((abs (expand-file-name file))
        (obj nil))
    (bergheim/agent-org--with-quiet-buffer abs
      (org-with-wide-buffer
       (if by-id
           (bergheim/agent-org--find-by-id locator)
         (bergheim/agent-org--find-unique-heading locator))
       (let (props)
         (dolist (kv (org-entry-properties nil 'standard))
           (push (cons (intern (car kv)) (cdr kv)) props))
         ;; Absent scalars are left nil; `json-encode' renders nil as JSON null.
         ;; `tags' stays a vector so it always encodes as an array.
         (setq obj
               `((state . ,(org-get-todo-state))
                 (heading . ,(substring-no-properties (org-get-heading t t t t)))
                 (tags . ,(vconcat (bergheim/agent-org--strip-list (org-get-tags))))
                 (priority . ,(org-entry-get nil "PRIORITY"))
                 (scheduled . ,(org-entry-get nil "SCHEDULED"))
                 (deadline . ,(org-entry-get nil "DEADLINE"))
                 (properties . ,(nreverse props))
                 (body . ,(bergheim/agent-org--autonomous-body)))))))
    (json-encode obj)))

(defun bergheim/agent-worklog-recent (&optional n)
  "Return the last N worklog entries (default 10) as a JSON array, oldest first.

Each element has `time' (the inactive timestamp), `project', `transition', and
`summary' (the full worklog heading). Returns \"[]\" when no worklog directory
is configured or no worklog file exists yet."
  (let* ((dir bergheim/agent-worklog-dir)
         (path (and dir (expand-file-name "worklog.org" dir)))
         (items nil))
    (if (not (and path (file-exists-p path)))
        "[]"
      (bergheim/agent-org--with-quiet-buffer path
        (org-with-wide-buffer
         (org-map-entries
          (lambda ()
            (let ((h (substring-no-properties (org-get-heading t t t t))))
              (push `((time . ,(when (string-match "\\[\\([^]]+\\)\\]" h)
                                 (match-string 1 h)))
                      (project . ,(org-entry-get nil "PROJECT"))
                      (transition . ,(org-entry-get nil "TRANSITION"))
                      (summary . ,h))
                    items)))
          nil nil)))
      (json-encode-array (last (nreverse items) (or n 10))))))

(provide 'bergheim-agent-helpers)
;;; bergheim-agent-helpers.el ends here
