;;; ai.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Thomas Bergheim
;;
;; Author: Thomas Bergheim
;; Maintainer: Thomas Bergheim

(setq bergheim/llama-swap-endpoint "berghome:11434")

(defun bergheim/get-api-key (pass-key env-var)
  "Get API key from password-store first, fallback to environment variable."
  (or (and (fboundp 'password-store-get)
           (ignore-errors (password-store-get pass-key)))
      (getenv env-var)))

(use-package mcp
  :after gptel
  :config
  (load "gptel/tools/mcp"))

(use-package gptel
  :after evil
  :commands (gptel gptel-send gptel-menu gptel-system-prompt)
  :general
  (general-define-key
   :keymaps 'gptel-mode-map
   :states 'normal
   "q" 'kill-current-buffer)
  (general-define-key
   :keymaps 'gptel-mode-map
   :states '(normal visual insert)
   "C-c RET" 'bergheim/gptel-submit
   "C-c C-k" 'bergheim/gptel-close-no-save
   "C-c C-<return>" '(lambda ()
                       (interactive)
                       (when (org-at-heading-p)
                         (forward-line))
                       (delete-region (point) (point-max))
                       (bergheim/gptel-submit)))

  ;; (bergheim/localleader-keys
  ;;   :keymaps 'gptel-mode-map
  ;;   :states '(normal motion)
  ;;   "A" 'org-agenda-goto)

  (bergheim/global-menu-keys
    "ja" '(gptel-add :which-key "add")
    "jc" '((lambda () (interactive) (gptel-context-remove-all nil)) :which-key "remove all")
    "jx" '(gptel-abort :which-key "abort")
    "jj" '(bergheim/gptel :which-key "gptel")
    "jJ" '(lambda () (interactive)
            ;; jump straight to model selection
            (call-interactively 'bergheim/gptel)
            (bergheim/gptel-select-model)
            :which-key "gptel model")
    "jt" '(gptel :which-key "gptel basic")
    "js" '(gptel-send :which-key "Send bufffer"))

  :hook
  (gptel-mode . (lambda ()
                  (add-hook 'kill-buffer-hook #'bergheim/gptel--maybe-save-buffer nil t)
                  (add-hook 'kill-emacs-hook
                            (lambda ()
                              (dolist (buffer (buffer-list))
                                (with-current-buffer buffer
                                  (bergheim/gptel--maybe-save-buffer)))))))

  ;; ;; for some reason I had to hook the minor mode keys like this
  ;; FIXME: this will overwrite other keymaps - try for instance org localleader afterwards
  (gptel-mode . (lambda ()
                  ;; (evil-define-key 'motion gptel-mode-map (kbd "SPC") nil)
                  (bergheim/localleader-keys
                    :keymaps 'local
                    :predicate 'gptel-mode
                    ;; :keymaps 'gptel-mode-map
                    ;; :states '(normal motion)
                    "a" '(gptel-abort :which-key "Abort")
                    "l" '(bergheim/gptel-select-model :which-key "llm")
                    "m" '(gptel-menu :which-key "Menu")
                    "s" '(bergheim/gptel--maybe-save-buffer :which-key "Save")
                    "p" '(bergheim/gptel-select-system-prompt :which-key "Prompt")
                    "o" '(bergheim/gptel-select-model :which-key "mOdel")
                    "d" '(gptel-system-prompt :which-key "Directive"))))
  :init
  (defun bergheim/read-directives (dir-path)
    "Read all Markdown files in DIR-PATH and return an alist with filenames as keys and file contents as values."
    (mapcar
     (lambda (file)
       (with-temp-buffer
         (insert-file-contents file)
         (let* ((filename (intern (file-name-sans-extension (file-name-nondirectory file))))
                (body (s-trim (buffer-string))))
           (cons filename body))))
     (directory-files dir-path t "\\.md\\'")))
  :config
  (require 'gptel-integrations)
  (load "gptel/tools/init")

  (add-hook 'find-file-hook  #'bergheim/gptel-ready-archived-files)

  (defun bergheim/gptel-select-model ()
    "Select a gptel model using completing-read."
    (interactive)
    (let* ((backends (mapcar 'cdr gptel--known-backends))
           (models-with-backends
            (seq-mapcat
             (lambda (backend)
               (let ((provider (gptel-backend-name backend))
                     (model-list (gptel-backend-models backend)))
                 (mapcar (lambda (model)
                           (list (format "%s: %s" provider model) backend model))
                         model-list)))
             backends))
           (choice (completing-read "Select model: "
                                    (mapcar #'car models-with-backends)))
           (selected (assoc choice models-with-backends)))

      (when selected
        (setq-local gptel-backend (nth 1 selected)
                    gptel-model (nth 2 selected))
        (message "Model set to %s (%s)"
                 gptel-model
                 (gptel-backend-name gptel-backend)))))

  (defun bergheim/gptel-ready-archived-files ()
    (when (and (buffer-file-name)
               ;; TODO: extract this dir out to a defvar
               (string-match-p "/llm/" (buffer-file-name)))
      (gptel-mode 1)))

  (defun bergheim/llama-swap-get-models ()
    "Fetch available models from llama-swap's OpenAI-compatible API."
    (condition-case err
        (let* ((url (concat "http://" bergheim/llama-swap-endpoint "/v1/models"))
               (response-buf (url-retrieve-synchronously url t))
               (response (when response-buf
                           (with-current-buffer response-buf
                             (goto-char (point-min))
                             (if (re-search-forward "^$" nil t)
                                 (buffer-substring-no-properties (point) (point-max))
                               "")))))
          (when response-buf (kill-buffer response-buf))
          (if (and response (not (string-empty-p (s-trim response))))
              (let* ((json-object-type 'hash-table)
                     (json-array-type 'list)
                     (json-key-type 'string)
                     (data (json-read-from-string response)))
                (mapcar (lambda (model)
                          (intern (gethash "id" model)))
                        (gethash "data" data)))
            (message "llama-swap returned empty response")
            nil))
      (error
       (message "Failed to fetch llama-swap models: %s" (error-message-string err))
       nil)))

  (gptel-make-openai "llama-swap"
    :stream t
    :protocol "http"
    :host bergheim/llama-swap-endpoint
    :models (bergheim/llama-swap-get-models))

  (gptel-make-gemini "Gemini"
    :stream t
    :key (bergheim/get-api-key "api/llm/google" "GOOGLE_API_KEY"))

  (gptel-make-openai "OpenAI"
    :stream t
    :key (bergheim/get-api-key "api/llm/openai" "OPENAI_API_KEY"))

  (setq gptel-cache '(message system tool))
  (setq gptel-api-key (bergheim/get-api-key "api/llm/openai" "OPENAI_API_KEY"))

  (gptel-make-anthropic "Claude"
    :stream t
    :key (bergheim/get-api-key "api/llm/anthropic" "ANTHROPIC_API_KEY"))

  (setq gptel-model 'qwen3-coder
        gptel-backend (gptel-get-backend "llama-swap"))
  ;; :models '((claude-sonnet-4-20250514
  ;;            :capabilities (media    json                 tool-use)
  ;;            ;;                ▲     ▲ supports           ▲
  ;;            ;; supports media╶╯     ╰─structured outputs ╰─can use tools
  ;;            :mimetypes ("application/pdf" "image/png" "image/jpeg")))))

  (defun bergheim/gptel-submit ()
    (interactive)
    (save-excursion
      (goto-char (point-max))
      (gptel-send)
      (evil-force-normal-state)))

  (defun bergheim/gptel-close-no-save ()
    "Kill the current buffer without saving it."
    (interactive)
    (when (and (buffer-live-p (current-buffer)) (bound-and-true-p gptel-mode))
      (cl-letf (((symbol-function 'bergheim/gptel--maybe-save-buffer) (lambda (&rest args))))
        (kill-buffer (current-buffer)))))

  (defun safe-buffer-name (buffer-name)
    (let ((safe-string (replace-regexp-in-string " " "_" buffer-name)))
      (replace-regexp-in-string "[^[:alnum:]_-]" "" safe-string)))

  (defun bergheim/gptel--save-buffer ()
    (if (buffer-file-name)
        (save-buffer)
      (let* ((gptel-buffer-name-regex "\\*gptel \\(.*\\)\\*")
             (gptel-default-name "default")
             (gptel-folder (expand-file-name "~/llm/"))
             (buffer-name-str (buffer-name))
             (name (or (when (string-match gptel-buffer-name-regex buffer-name-str)
                         (safe-buffer-name (match-string 1 buffer-name-str)))
                       gptel-default-name))
             (file-name (concat name ".org"))
             (timestamp (format-time-string "%Y%m%d%H%M%S"))
             (backup-file-name (expand-file-name (concat gptel-folder timestamp "_" file-name))))
        (write-file backup-file-name))))

  (defun bergheim/gptel--maybe-save-buffer ()
    "Save the gptel buffer when gptel-mode is enabled, the buffer is killed, and the buffer has unsaved changes."
    (interactive)
    (when (and (buffer-live-p (current-buffer))
               (bound-and-true-p gptel-mode)
               (buffer-modified-p)
               ;; TODO Avoid saving empty or nearly empty buffers
               ;; (not (string-match-p "^\\s-*\\*?\\s-*$" (buffer-string))))
               (not (string= (buffer-string) "* ")))
      (bergheim/gptel--save-buffer)))

  ;; (defvar bergheim/gptel-buffer-name "*gptel*")

  (defun bergheim/gptel--get-chat-buffers ()
    (mapcar #'buffer-name
            (seq-filter (lambda (buf)
                          (with-current-buffer buf
                            (bound-and-true-p gptel-mode)))
                        (buffer-list))))

  (defun bergheim/gptel ()
    "Manage GPTeL chat buffers.
Prompts for session name if none provided. Inserts selected region text into chat buffer. Uses custom prompts from `bergheim/load-project-prompt`."
    (interactive)
    (let* ((region-text (when (use-region-p)
                          (string-trim (buffer-substring-no-properties
                                        (region-beginning) (region-end)))))
           (gptel-buffers (bergheim/gptel--get-chat-buffers))
           (chat-buffer-name
            (if (and (featurep 'consult) gptel-buffers)
                (consult--read
                 (cons "*gptel*" gptel-buffers)
                 :prompt "GPTeL Session Name: "
                 :state (consult--buffer-preview)
                 :default "*gptel*")
              (completing-read "GPTeL Session Name (leave empty for *gptel*): "
                               (cons "*gptel*" gptel-buffers)
                               nil nil nil nil "*gptel*")))
           (existing-buffer (get-buffer chat-buffer-name))
           (chat-buffer (or existing-buffer
                            (gptel (if (string= chat-buffer-name "*gptel*")
                                       chat-buffer-name
                                     (concat "*gptel " chat-buffer-name "*")))))
           (quote-style (if (derived-mode-p 'prog-mode)
                            ;; BEGIN_SRC does not use the -mode suffix
                            (let ((lang (string-remove-suffix "-mode"
                                                              (symbol-name major-mode))))
                              (format "#+begin_src %s\n%s\n#+end_src"
                                      lang  region-text))
                          (format "#+begin_quote\n%s\n#+end_quote" region-text)))
           (custom-prompts (bergheim/load-project-prompt)))
      (when (buffer-live-p chat-buffer)
        (with-current-buffer chat-buffer
          (setq-local gptel--system-message (or (cdr custom-prompts)
                                                (assoc-default 'default gptel-directives))
                      gptel--system-message-name (or (car custom-prompts)
                                                     'default))
          (when region-text
            (save-excursion
              ;; (insert "\n\n~~~\n" (string-trim region-text) "\n~~~"))))
              (insert "\n\n" quote-style))))
        (pop-to-buffer chat-buffer))))

  (defun bergheim/load-project-prompt ()
    "Load the first 'prompt.md' found upwards in directory hierarchy or project root."
    (let* ((prompt-file "prompt.md")
           (prompt-dir (or (locate-dominating-file default-directory prompt-file)
                           (when-let ((project (project-current)))
                             (project-root project))))
           (prompt-path (and prompt-dir (expand-file-name prompt-file prompt-dir))))
      (when (and prompt-path (file-exists-p prompt-path))
        (with-temp-buffer
          (insert-file-contents prompt-path)
          (cons (file-name-sans-extension (file-name-nondirectory prompt-path))
                (buffer-string))))))

  (defun bergheim/gptel--annotate-directives (s &optional metadata)
    "Annotate a given directive S with a description, using optional METADATA."
    (when-let ((item (assoc (intern s) gptel-directives)))
      (let ((desc (s-truncate 200 (s-replace "\n" " " (cdr item)))))
        (concat (string-pad "" (- 20 (string-width s))) desc))))

  (defvar-local gptel--system-message-name 'default)

  (defun bergheim/gptel-select-system-prompt (&optional directive-key)
    "Set system message in local gptel buffer to directive/prompt indicated by DIRECTIVE-KEY."
    (interactive)
    (setq gptel-directives (bergheim/read-directives (expand-file-name "prompts" bergheim/config-dir)))
    (let* ((completion-extra-properties '(:annotation-function bergheim/gptel--annotate-directives))
           (current-name (or gptel--system-message-name "default"))
           (directive-key (or directive-key
                              (intern
                               (completing-read
                                (format "Current prompt \"%s\": " current-name)
                                gptel-directives
                                nil ;; predicate/filter
                                ;; TODO make custom prompt work
                                nil ;; do not require a match - allow custom prompt
                                nil ;; no initial input
                                nil ;; no history specified
                                "default")))))

      (if-let ((directive (assoc directive-key gptel-directives)))
          (progn
            (setq-local gptel--system-message-name (car directive))
            (setq-local gptel--system-message (cdr directive))
            (message "Set system prompt to \"%s\"" (car directive)))
        (message "No directive found for key %s" directive-key))))

  (defun bergheim//copy-text-from-other-window ()
    "Copy the diff from the other window."
    (let* ((other-window (next-window))
           (diff-contents
            (with-selected-window other-window
              (buffer-substring-no-properties (point-min) (point-max)))))
      diff-contents))

  (defun bergheim/gptel-git-commit ()
    (interactive)
    (let ((diff (bergheim//copy-text-from-other-window)))
      (gptel-mode)
      ;; (setq-local gptel--system-message (cdr (assoc 'git gptel-directives)))))
      (gptel-request
       diff
       :system "The user provides the result of running `git diff --cached`. You suggest a conventional commit message. Start with the 80 char summary and then provide relevant details, if any. Keep it short and use bullet-points where it makes sense. Don't add anything else to the response. THE ONLY THING THAT MATTERS IS WHAT THE CHANGE ENABLES, NOT HOW - THAT IS IN THE CODE!"
       :stream t)))

  (defun bergheim/gptel-email-response ()
    (interactive)
    (let ((diff (buffer-substring-no-properties (point) (point-max)))
          (curr-mode major-mode))
      (unwind-protect
          (progn
            (org-mode)
            (gptel-mode)
            ;; (setq-local gptel--system-message (cdr (assoc 'git gptel-directives)))))
            (gptel-request
             diff
             :system "The user provides an email that he wants to respond to. You suggest a brief and professional email response. Do not add anything else to the response."
             :stream t))
        (funcall curr-mode))))

  :custom
  ;; (gptel-post-stream-hook . gptel-auto-scroll)
  ;; (gptel-post-response-hook . gptel-end-of-response)

  ;; in theory I should like to have this t..
  (gptel-org-branching-context nil)
  (gptel-default-mode 'org-mode)
  (gptel-directives (bergheim/read-directives (expand-file-name "prompts" bergheim/config-dir)))
  ;; (gptel-temperature 1.0)
  ;; (gptel-max-tokens 400)
  (gptel-response-prefix-alist
   '((markdown-mode . "# HAL")
     (org-mode . "* HAL:\n")
     (text-mode . "@HAL:")))
  (gptel-prompt-prefix-alist
   '((markdown-mode . "# ")
     (org-mode . "* ")
     (text-mode . "@user "))))

(use-package gptel-quick
  :ensure (gptel-quick :host github :repo "karthink/gptel-quick")
  :after gptel
  :config
  (setq gptel-quick-backend (gptel-get-backend "llama-swap")
        gptel-quick-model 'gemma4)
  :general
  (bergheim/global-menu-keys
    "sq" 'gptel-quick)
  :bind ("C-c q" . gptel-quick))

(use-package ob-gptel
  :ensure (:host github :repo "jwiegley/ob-gptel")
  :hook ((org-mode . ob-gptel-install-completions))
  :defines ob-gptel-install-completions
  :config
  (add-to-list 'org-babel-load-languages '(gptel . t))
  ;; Optional, for better completion-at-point
  (defun ob-gptel-install-completions ()
    (add-hook 'completion-at-point-functions
              'ob-gptel-capf nil t)))

(use-package copilot
  :disabled
  :demand t
  :general
  (bergheim/global-menu-keys
    "lc" '(:ignore t :wk "copilot")
    "lcm" 'copilot-mode
    "lcD" 'copilot-diagnose)
  (copilot-completion-map
   "TAB" 'copilot-accept-completion
   "C-TAB" 'copilot-accept-completion-by-word
   "C-j" 'copilot-next-completion
   "C-k" 'copilot-previous-completion)
  ;; :hook
  ;; (prog-mode-hook . copilot-mode)
  ;; (git-commit-mode-hook . copilot-mode)
  :init
  (setq copilot-indent-offset-warning-disable t)
  (setq copilot-max-char 1000000)
  (setq copilot-max-char-warning-disable t))

(use-package aider
  :disabled
  :bind (("C-c b" . aider-transient-menu))
  :config
  (setenv "ANTHROPIC_API_KEY" (auth-source-pick-first-password :host "anthropic"))
  (setenv "GEMINI_API_KEY" (auth-source-pick-first-password :host "google" :user "gemini"))
  ;; llama-swap is OpenAI-compatible; aider talks to it via the OpenAI provider
  (setenv "OPENAI_API_BASE" (concat "http://" bergheim/llama-swap-endpoint "/v1"))
  (setenv "OPENAI_API_KEY" "sk-no-key-required")
  (setq aider-args '("--model" "openai/qwen3-coder")))

(use-package monet
  :disabled
  :ensure (:host github :repo "stevemolitor/monet"))

(use-package claude-code
  :disabled
  :ensure (:host github :repo "stevemolitor/claude-code.el")
  :bind-keymap ("C-c c" . claude-code-command-map)
  :bind (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode))
  :config
  (setq claude-code-program-switches '("--dangerously-skip-permissions"))
  ;; Monet integration
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)
  (claude-code-mode))

(use-package claude-code-ide
  :disabled
  :ensure (:host github :repo "manzaltu/claude-code-ide.el")
  :after eat
  :commands (claude-code-ide-menu claude-code-ide-open-project-terminal)
  :init
  (setq claude-code-ide-terminal-backend 'eat)  ; or 'vterm if you prefer
  :config
  ;; MCP tools: xref, imenu, tree-sitter, project.el
  (claude-code-ide-emacs-tools-setup))

(use-package agent-shell
  :general
  (general-define-key
   :keymaps 'agent-shell-mode-map
   :states 'insert
   "RET" 'newline)
  (general-define-key
   :keymaps 'agent-shell-mode-map
   :states 'normal
   "RET" 'comint-send-input)
  (general-define-key
   :keymaps 'agent-shell-mode-map
   :states '(normal insert visual motion emacs)
   "M-RET" 'comint-send-input)
  (bergheim/global-menu-keys
    "k"  '(:ignore t :wk "agent-shell")
    ;; shells
    "kk" '(bergheim/agent-shell-switch-buffer :wk "switch/new shell")
    "kn" '(agent-shell-new-shell :wk "new shell")
    "kw" '(agent-shell-new-worktree-shell :wk "new worktree")
    "kt" '(agent-shell-toggle :wk "toggle")
    "kb" '(agent-shell-other-buffer :wk "other buffer")
    "kF" '(agent-shell-fork :wk "fork session")
    ;; send
    "kf" '(agent-shell-send-file :wk "send file")
    "kr" '(agent-shell-send-region :wk "send region")
    "ka" '(agent-shell-send-dwim :wk "send dwim")
    "ks" '(agent-shell-send-screenshot :wk "send screenshot")
    "k!" '(agent-shell-insert-shell-command-output :wk "shell output")
    "kc" '(agent-shell-prompt-compose :wk "compose")
    ;; inspect
    "kh" '(agent-shell-search-history :wk "history")
    "kT" '(agent-shell-open-transcript :wk "transcript")
    "kl" '(agent-shell-view-acp-logs :wk "view logs"))
  :config
  (defun bergheim/agent-shell-switch-buffer ()
    "Switch between agent-shell buffers, or create a new one.
Type an existing name to switch, or a new suffix to start a fresh shell."
    (interactive)
    (let* ((buffers (agent-shell-buffers))
           (names (mapcar #'buffer-name buffers))
           (choice (completing-read "Agent shell: " names)))
      (if (member choice names)
          (switch-to-buffer choice)
        (let* ((base (when-let ((cur (car buffers)))
                       (string-trim-right
                        (string-trim (buffer-name cur) "\\*" "\\*"))))
               (new-name (if base
                             (format "%s %s" base choice)
                           (format "%s" choice))))
          (agent-shell-new-shell)
          (rename-buffer new-name t)))))

  (add-hook 'agent-shell-mode-hook #'agent-shell-toggle-logging)
  (setq agent-shell-permission-responder-function #'agent-shell-permission-allow-always)
  (setq agent-shell-anthropic-claude-acp-command '("claude-agent-acp")))
;;; ai.el ends here
