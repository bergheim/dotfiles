;;; ~/.config/doom/autoload/bergheim.el -*- lexical-binding: t; -*-

;;;###autoload
(defun find-in-dotfiles ()
  "Open a file somewhere in ~/.config via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/.config")))

;;;###autoload
(defun browse-dotfiles ()
  "Browse the files in ~/.config."
  (interactive)
  (doom-project-browse (expand-file-name "~/.config")))


;;;###autoload
(defun bergheim-toggle-yadm ()
  "Toggle the GIT_DIR between nil and yadm. Opens magit-status when it is enabled."
  (interactive)
  ;; use a property “state”. Value is t or nil
  (if (get 'tsb-toggle-yadm 'state)
      (progn
        (message "Disabling YADM")
        (setenv "GIT_DIR" nil)
        (put 'tsb-toggle-yadm 'state nil))
    (progn
      (message (concat "Enabling YADM " (getenv "XDG_CONFIG_HOME") "/yadm/repo.git"))
      (setenv "GIT_DIR" (concat (getenv "XDG_CONFIG_HOME") "/yadm/repo.git"))
      (put 'tsb-toggle-yadm 'state t)
      (magit-status))
    ))
