;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(nginx
     systemd
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (helm :variables
           helm-completion-style 'emacs
           completion-styles '(helm-flex))
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t)
     docker
     emacs-lisp
     git
     github
     markdown
     multiple-cursors
     prettier
     (mu4e :variables
           ;; enabling async removes feedback if a sent mail failed for instance
           ;; mu4e-enable-async-operations t
           mu4e-enable-notifications t
           mu4e-use-maildirs-extension t ;; see number of unread emails
           mu4e-enable-mode-line nil
           )
     ;; notmuch
     deft
     (org :variables
          org-want-todo-bindings t ;; one key support on headings
          org-enable-reveal-js-support t
          org-enable-sticky-header t
          org-enable-hugo-support t
          org-journal-file-format "%Y-%m"
          org-journal-date-format "%A, %d %B %Y"
          org-enable-org-journal-support t)
     (shell :variables
          shell-default-height 30
          shell-default-position 'bottom)
     (spell-checking
          :variables spell-checking-enable-by-default nil)
     syntax-checking
     evil-snipe
     version-control
     ;; multiple-cursors
     ranger
     restclient
     (elfeed :variables
             rmh-elfeed-org-files (list "~/org/elfeed.org"))
     colors
     rust
     elm
     react
     ;; dap
     html
     (javascript :variables
                 ;; javascript-backend 'lsp
                 node-add-modules-path nil)
     (typescript :variables
                 ;; node-add-modules-path t
                 typescript-fmt-tool nil
                 ;; typescript-fmt-tool 'prettier
                 typescript-fmt-on-save nil)
     sql
     lsp
     java
     scala
     semantic
     yaml
     spotify
     python
     tmux
     (themes-megapack :packages
                      badwolf-theme
                      doom-city-lights
                      doom-dracula
                      doom-nord
                      doom-nord-light
                      gruvbox
                      material-theme
                      molokai-theme
                      monokai-theme
                      seti-theme
                      zenburn-theme)
     xkcd
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(editorconfig
                                      nodejs-repl
                                      doom-themes
                                      git-auto-commit-mode
                                      org-mru-clock
                                      org-plus-contrib
                                      heaven-and-hell
                                      ;; all-the-icons
                                      ;; magit-todos
                                      magit-gh-pulls
                                      evil-collection
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)

   ;; dotspacemacs-editing-style 'vim
   dotspacemacs-editing-style '(vim :variables
                                    vim-style-remap-Y-to-y$ t)

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '(
                                ;; (recents . 5)
                                (todos . 5)
                                (agenda . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(gruvbox
                         doom-molokai
                         doom-nord
                         material
                         monokai
                         spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   ;; dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)
   dotspacemacs-mode-line-theme '(doom :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Iosevka"
                               :size 28
                               :weight normal
                               :width expanded
                               :powerline-scale 1.1)
   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; this is a hack to make .spacemacs.env work when run as a daemon. this has
  ;; been a bug for years....
  ;; (spacemacs|do-after-display-system-init
  ;;  (spacemacs/load-spacemacs-env))

  ;; Set transparency of emacs window (active . inactive) where 0 is completely transparent.
  ;; (set-frame-parameter (selected-frame) 'alpha '(90 . 70))
  ;; (add-to-list 'default-frame-alist '(alpha . (90 . 70)))

  (load-file (expand-file-name "~/.emacs.d/custom/private.el"))

  (defun tsb-toggle-yadm ()
    "Toggle the GIT_DIR between nil and yadm. Opens magit-status when it is enabled."
    (interactive)
    ;; use a property “state”. Value is t or nil
    (if (get 'tsb-toggle-yadm 'state)
        (progn
          (message "Disabling YADM")
          (setenv "GIT_DIR" nil)
          (put 'tsb-toggle-yadm 'state nil))
      (progn
        (message (concat "Enabling YADM" (getenv "HOME") "/.config/yadm/repo.git"))
        (setenv "GIT_DIR" (concat (getenv "HOME") "/.config/yadm/repo.git"))
        (put 'tsb-toggle-yadm 'state t)
        (magit-status))
      ))

  ;; (setq projectile-enable-caching t)
  ;; (setq projectile-indexing-method 'alien)
  (setq projectile-indexing-method 'alien)
  ;; (setq projectile-git-command)
  (setq projectile-git-command "fd -H --ignore-file .ignore -t f -0")
  (setq projectile-generic-command "fd -H --ignore-file .ignore -t f -0")

  (spacemacs/set-leader-keys "ogy" 'tsb-toggle-yadm)
  (spacemacs/set-leader-keys "ogo" 'vc-revision-other-window)
  (spacemacs/set-leader-keys "ogD" 'magit-diff-buffer-file)
  (spacemacs/set-leader-keys "ogd" 'magit-diff-buffer-file-popup)
  (spacemacs/set-leader-keys "ogl" 'magit-log-buffer-file)
  (spacemacs/set-leader-keys "ogf" 'magit-file-popup)

  (spacemacs/set-leader-keys "oa" 'org-agenda)
  (spacemacs/set-leader-keys "oi" (lambda () (interactive) (find-file "~/org/inbox.org")))
  (spacemacs/set-leader-keys "ocg" 'org-clock-goto)
  (spacemacs/set-leader-keys "oci" 'org-clock-in)
  (spacemacs/set-leader-keys "ocI" (lambda () (interactive) (org-clock-in '(4))))

  (spacemacs/set-leader-keys "ocl" 'org-clock-in-last)
  (spacemacs/set-leader-keys "oco" 'org-clock-out)
  (spacemacs/set-leader-keys "ocr" #'org-mru-clock-in)
  (spacemacs/set-leader-keys "ocR" #'org-mru-clock-select-recent-task)

  (spacemacs/set-leader-keys "op" 'spotify-playpause)
  (spacemacs/set-leader-keys "on" 'spotify-next)
  (spacemacs/set-leader-keys "os" 'helm-spotify-plus)
  (spacemacs/set-leader-keys "oj" 'dumb-jump-go)

  ;; (spaceline-toggle-buffer-size-off)
  ;; (spaceline-toggle-buffer-encoding-abbrev-off)
  ;; (spaceline-toggle-purpose-off)
  ;; (spaceline-toggle-org-clock-on)
  (setq size-indication-mode nil)

  (setq mu4e-maildir "~/.mail/neptune"
        mu4e-trash-folder "/Deleted Items"
        mu4e-refile-folder "/Archive"
        mu4e-sent-folder "/Sent Items"
        mu4e-drafts-folder "/Drafts"
        mu4e-get-mail-command "mbsync -a"
        mu4e-attachment-dir "~/Downloads/email"
        mu4e-confirm-quit nil
        ; mu4e-update-interval nil
        ; mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        ;; this fixes some sync issues with mbsync
        mu4e-change-filenames-when-moving t
        mu4e-compose-dont-reply-to-self t
        mu4e-confirm-quit nil
        ;; display is nicer with these
        mu4e-use-fancy-chars nil
        mail-user-agent 'mu4e-user-agent
        ;; don't keep message buffers around
        message-kill-buffer-on-exit t
        mu4e-update-interval 60
        mu4e-sent-messages-behavior 'delete

        mu4e-hide-index-messages t

        ;; show overview to left, email to the right
        mu4e-split-view 'vertical
        mu4e-headers-visible-columns 110

        ;; oooor
        ;; mu4e-split-view 'horizontal
        ;; mu4e-headers-visible-lines 12

        alert-fade-time 20)

  (setq message-send-mail-function 'message-send-mail-with-sendmail
        send-mail-function 'sendmail-send-it)

  ;; substitute sendmail with msmtp
  (setq sendmail-program "msmtp")

  ;; allow setting account through email header
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq message-sendmail-f-is-evil t)

  (setq user-mail-address bergheim/neptune/email
        user-full-name  bergheim/neptune/name
        mu4e-compose-signature bergheim/neptune/signature)

  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Inbox"
                :query "maildir:/Inbox"
                :key ?i))

  (with-eval-after-load 'mu4e-alert
    ;; Enable Desktop notifications
    (mu4e-alert-set-default-style 'libnotify)
    ;; (mu4e-alert-enable-mode-line-display)
    ;; (mu4e-alert-enable-notifications)
    ;; (mu4e-alert-enable-mode-line-display)
    ;; (mu4e-alert-set-default-style 'libnotify)
    ;; (alert-add-rule :category "mu4e-alert" :style 'fringe :predicate (lambda (_) (string-match-p "^mu4e-" (symbol-name major-mode))) :continue t)
    ;; (mu4e-alert-enable-notifications)
    )

  ;; (with-eval-after-load 'magit (evil-collection-magit-todos-setup))
  ;; (magit-todos-mode 1)

  ;; (spaceline-toggle-buffer-size-off)
  ;; (spaceline-toggle-buffer-encoding-abbrev-off)
  ;; (spaceline-toggle-purpose-off)
  ;; (spaceline-toggle-org-clock-on)

  ;; Lock files are annoying when using sync and backup software
  (setq create-lockfiles nil)
  ;; (setq ensime-startup-notification nil)
  ;; Backups. Make a bunch
  (setq make-backup-files t
        version-control t     ;; Use version numbers for backups.
        kept-new-versions 10  ;; Number of newest versions to keep.
        kept-old-versions 2   ;; Number of oldest versions to keep.
        delete-old-versions t ;; Don't ask to delete excess backup versions.
        backup-by-copying t  ;; Copy all files, don't rename them.
        vc-make-backup-files t
        backup-directory-alist '((".*" . "~/.emacs.d/backup"))) ;; Default and per-save backups go here

  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.2)
  (setq-default evil-escape-excluded-states '(visual))
  (setq-default evil-escape-excluded-major-modes '(dired-mode
                                                   neotree-mode
                                                   evil-visual-mode
                                                   magit-mode
                                                   magit-log-mode
                                                   magit-diff-mode
                                                   magit-revision-mode
                                                   magit-stash-mode
                                                   magit-status-mode))

  ;; (define-key global-map (kbd "C-h") #'evil-window-left)
  ;; (define-key global-map (kbd "C-j") #'evil-window-down)
  ;; (define-key global-map (kbd "C-k") #'evil-window-up)
  ;; (define-key global-map (kbd "C-l") #'evil-window-right)
  ;; (define-key magit-log-mode-map (kbd "TAB") 'magit-cycle-margin-style)

  ;; (define-key evil-normal-state-map (kbd "M-h") #'evil-window-left)
  ;; (define-key evil-normal-state-map (kbd "M-j") #'evil-window-down)
  ;; (define-key evil-normal-state-map (kbd "M-k") #'evil-window-up)
  ;; (define-key evil-normal-state-map (kbd "M-l") #'evil-window-right)


  (setq evil-want-Y-yank-to-eol t)

  (add-hook 'focus-out-hook
            (lambda () (save-some-buffers t)))

  ;; dont try to line up tabs
  (setq-default evil-shift-round nil)


 (dumb-jump-mode)
  (nconc dumb-jump-language-file-exts
         '((:language "typescript" :ext "ts" :agtype "ts" :rgtype nil)
           (:language "typescript" :ext "tsx" :agtype "ts" :rgtype "ts")))
  (nconc dumb-jump-language-comments
         '((:comment "//" :language "typescript")))
  (nconc dumb-jump-find-rules
         ;; Rules translated from link below, except where noted
         ;; https://github.com/jacktasia/dumb-jump/issues/97#issuecomment-346441412
         ;;
         ;; --regex-typescript=/^[ \t]*(export[ \t]+(abstract[ \t]+)?)?class[ \t]+([a-zA-Z0-9_$]+)/\3/c,classes/
         '((:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                  :regex "(export\\s+(abstract\\s+)?)?class\\s+JJJ\\b"
                  :tests ("class test" "export class test" "abstract class test"
                          "export abstract class test")
                  :not ("class testnot"))
           ;; --regex-typescript=/^[ \t]*(declare[ \t]+)?namespace[ \t]+([a-zA-Z0-9_$]+)/\2/c,modules/
           (:type "module" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                  :regex "(declare\\s+)?namespace\\s+JJJ\\b"
                  :tests ("declare namespace test" "namespace test")
                  :not ("declare testnot"))
           ;; --regex-typescript=/^[ \t]*(export[ \t]+)?module[ \t]+([a-zA-Z0-9_$]+)/\2/n,modules/
           (:type "module" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                  :regex "(export\\s+)?module\\s+JJJ\\b"
                  :tests ("export module test" "module test")
                  :not ("module testnot"))
           ;; --regex-typescript=/^[ \t]*(export[ \t]+)?(async[ \t]+)?function[ \t]+([a-zA-Z0-9_$]+)/\3/f,functions/
           (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                  :regex "(export\\s+)?(async\\s+)?function\\s+JJJ\\b"
                  :tests ("function test" "export function test" "export async function test"
                          "async function test")
                  :not ("function testnot"))
           ;;--regex-typescript=/^[ \t]*export[ \t]+(var|let|const)[ \t]+([a-zA-Z0-9_$]+)/\2/v,variables/
           (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                  :regex "export\\s+(var|let|const)\\s+JJJ\\b"
                  :tests ("export var test" "let test" "const test")
                  :not ("var testnot"))
           ;; --regex-typescript=/^[ \t]*(var|let|const)[ \t]+([a-zA-Z0-9_$]+)[ \t]*=[ \t]*function[ \t]*[*]?[ \t]*\(\)/\2/v,varlambdas/
           (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                  :regex "(var|let|const)\\s+JJJ\\s*=\\s*function\\s*\\*?\\s*\\\(\\\)"
                  :tests ("var test = function ()" "let test = function()" "const test=function*()")
                  :not ("var testnot = function ()"))
           ;; --regex-typescript=/^[ \t]*(export[ \t]+)?(public|protected|private)[ \t]+(static[ \t]+)?(abstract[ \t]+)?(((get|set)[ \t]+)|(async[ \t]+[*]*[ \t]*))?([a-zA-Z1-9_$]+)/\9/m,members/
           (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                  :regex "(export\\s+)?(public|protected|private)\\s+(static\\s+)?(abstract\\s+)?(((get|set)\\s+)|(async\\s+))?JJJ\\b"
                  :tests ("public test" "protected static test" "private abstract get test"
                          "export public static set test" "export protected abstract async test")
                  :not ("public testnot"))
           ;; --regex-typescript=/^[ \t]*(export[ \t]+)?interface[ \t]+([a-zA-Z0-9_$]+)/\2/i,interfaces/
           (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                  :regex "(export\\s+)?interface\\s+JJJ\\b"
                  :tests ("interface test" "export interface test")
                  :not ("interface testnot"))
           ;; --regex-typescript=/^[ \t]*(export[ \t]+)?type[ \t]+([a-zA-Z0-9_$]+)/\2/t,types/
           (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                  :regex "(export\\s+)?type\\s+JJJ\\b"
                  :tests ("type test" "export type test")
                  :not ("type testnot"))
           ;; --regex-typescript=/^[ \t]*(export[ \t]+)?enum[ \t]+([a-zA-Z0-9_$]+)/\2/e,enums/
           (:type "enum" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                  :regex "(export\\s+)?enum\\s+JJJ\\b"
                  :tests ("enum test" "export enum test")
                  :not ("enum testnot"))
           ;; --regex-typescript=/^[ \t]*import[ \t]+([a-zA-Z0-9_$]+)/\1/I,imports/
           (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                  :regex "import\\s+JJJ\\b"
                  :tests ("import test")
                  :not ("import testnot"))
           ;; Custom definition for public methods without "public" keyword.
           ;; Fragile! Requires brace on same line.
           (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
                  :regex "\\bJJJ\\s*\\(.*\\{"
                  :tests ("test() {" "test(foo: bar) {")
                  :not ("testnot() {"))))



  ;; force js2-mode to use flycheck-next-error (fixes spc e n/p)
  (add-hook 'js2-init-hook '(lambda ()
                              (setq next-error-function 'flycheck-next-error)
                              ))
  ;; ignore jshint. TODO is this used?
  (setq-default javascript-jshint nil)
  (setq-default js2-basic-offset 2)
  ;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)

  ;; if we are in text mode, i prefer to have linebreaks added automatically
  (add-hook 'text-mode-hook 'auto-fill-mode)

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  ;; (defun my/use-eslint-from-node-modules ()
  ;;   (let* ((root (locate-dominating-file
  ;;                 (or (buffer-file-name) default-directory)
  ;;                 "node_modules"))
  ;;          (eslint (and root
  ;;                       (expand-file-name "node_modules/eslint/bin/eslint.js"
  ;;                                         root))))
  ;;     (when (and eslint (file-executable-p eslint))
  ;;       (setq-local flycheck-javascript-eslint-executable eslint))))
  ;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

  ;; (defun my/use-tslint-from-node-modules ()
  ;;   (let* ((root (locate-dominating-file
  ;;                 (or (buffer-file-name) default-directory)
  ;;                 "node_modules"))
  ;;          (tslint (and root
  ;;                       (expand-file-name "node_modules/tslint/bin/tslint"
  ;;                                         root))))
  ;;     (when (and tslint (file-executable-p tslint))
  ;;       (setq-local flycheck-typescript-tslint-executable tslint))))
  ;; (add-hook 'flycheck-mode-hook #'my/use-tslint-from-node-modules)

  (defun my/use-prettier-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (prettier (and root
                        (expand-file-name "node_modules/prettier/bin-prettier.js"
                                          root))))
      (when (and prettier (file-executable-p prettier))
        (setq-local prettier-js-command prettier))))
  (add-hook 'prettier-js-mode-hook #'my/use-prettier-from-node-modules)

                                        ;typescript
  ;; (eval-after-load 'typescript-mode
  ;;   '(progn
  ;;      (add-hook 'typescript-mode-hook #'node-add-modules-path)
  ;;      (add-hook 'typescript-mode-hook #'prettier-js-mode)))

  ;; (add-hook 'typescript-mode-hook #'add-node-modules-path)
  ;; (add-hook 'typescript-mode-hook #'prettier-js-mode)

  ;; (add-hook 'js2-mode-hook 'prettier-js-mode)

  ;; (add-hook 'web-mode-hook 'prettier-js-mode)

  ;; (eval-after-load 'typescript-mode
  ;;   '(add-hook 'typescript-mode-hook #'add-node-modules-path))

  ;; (eval-after-load 'js-mode
  ;;   '(add-hook 'js-mode-hook #'node-add-modules-path))

  (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))

  ;; (require 'dap-node)


  (defun bergheim/find-npm-file (npm)
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (file (and root
                      (expand-file-name (concat "node_modules/.bin/" npm)
                                        root))))
      (cond ((and file (file-executable-p file)) file)
            (t nil))))

  ;; (let ((eslint (bergheim/find-npm-file "eslint")))
  ;;   (if eslint
  ;;       (setq-local flycheck-javascript-eslint-executable eslint)))

  ;; (defun bergheim/js-hook ()
  ;;   (let ((eslint (bergheim/find-npm-file "eslint")))
  ;;     (if eslint
  ;;         (setq-local flycheck-javascript-eslint-executable eslint))))
  ;; (add-hook 'flycheck-mode-hook 'bergheim/js-hook)

  (when (spacemacs/system-is-mac)
    (setq mac-command-modifier 'meta
          mac-option-modifier  'none))

  (defun bergheim/set-color-scheme (color)
    "Sets the color scheme based on the color input (light or dark)"
    (setq heaven-and-hell-theme-type color)
    (heaven-and-hell-clean-load-themes (heaven-and-hell-themes-switch-to)))

  (editorconfig-mode 1) ;; always respect editorconfig files

  ;; nodejs-repl
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode "ne" 'nodejs-repl-send-last-expression)
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode "nj" 'nodejs-repl-send-line)
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode "nr" 'nodejs-repl-send-region)
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode "nl" 'nodejs-repl-load-file)
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode "n'" 'nodejs-repl-switch-to-repl)
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode "ns" 'nodejs-repl-switch-to-repl)

  (setq smerge-command-prefix "\C-cv") ;; because the default is impractical

  (setq magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))


  (setq-default tab-width 2) ;; tabs vs spaces, i give up..
  ;; TODO: am I using this..?
  (setq-default sh-basic-offset tab-width
                sh-indentation tab-width)

  (setq mu4e-maildir "~/.mail/neptune"
        mu4e-trash-folder "/Deleted Items"
        mu4e-refile-folder "/Archive"
        mu4e-sent-folder "/Sent Items"
        mu4e-drafts-folder "/Drafts"
        mu4e-get-mail-command "mbsync -a"
        mu4e-attachment-dir "~/Downloads/email"
        mu4e-confirm-quit nil
        ; mu4e-update-interval nil
        ; mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        ;; this fixes some sync issues with mbsync
        mu4e-change-filenames-when-moving t
        mu4e-compose-dont-reply-to-self t
        ;; display is nicer with these. Note: causes mis-alignment
        mu4e-use-fancy-chars nil
        ;; mail-user-agent 'mu4e-user-agent
        ;; don't keep message buffers around
        ;; mu4e-enable-notifications t
        ;; mu4e-enable-mode-line t
        message-kill-buffer-on-exit t
        mu4e-update-interval 60 ;; i think this fucks up emacsclient - are you SURE you want to kill mu4e-update?
        alert-fade-time 20
        ;; set up a more ISO timestamp
        mu4e-headers-date-format "%Y-%m-%d %H:%M"
        ;; stop spamming the minibuffer
        mu4e-hide-index-messages t
        )


  ;; (setq message-send-mail-function 'message-send-mail-with-sendmail
  ;;       send-mail-function 'sendmail-send-it)

  ;; ;; substitute sendmail with msmtp
  ;; (setq sendmail-program "msmtp")

  ;; ;; allow setting account through email header
  ;; (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  ;; (setq message-sendmail-f-is-evil t)


  ;; the headers to show in the headers list -- a pair of a field
  ;; and its width, with `nil' meaning 'unlimited'
  ;; (better only use that for the last field.
  ;; These are the defaults:
  (setq mu4e-headers-fields
        '( (:date          .  20)    ;; alternatively, use :human-date
           (:flags         .   6)
           (:from          .  22)
           (:subject       .  nil))) ;; alternatively, use :thread-subject

  ;; (mu4e-alert-enable-notifications)
  ;; (setq mu4e-alert-interesting-mail-query
  ;;       (concat "(maildir:<fu> AND date:today..now"
  ;;               " OR maildir:<bar> AND date:today..now"
  ;;               " AND flag:unread"))


  ;; (alert-add-rule :severity 'high :continue t)
  ;; (setq send-mail-function 'async-smtpmail-send-it)

  (setq user-mail-address "this@needs.com"
        user-full-name "to be changed"
        mu4e-compose-signature
        (concat
         "Kind regards,\n"
         "me\n"))

  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Inbox"
                :query "maildir:/Inbox"
                :key ?i))

  (with-eval-after-load 'mu4e-alert
    ;; Enable Desktop notifications
    (mu4e-alert-set-default-style 'libnotify)
    ;; (mu4e-alert-enable-mode-line-display)
    ;; (mu4e-alert-enable-notifications)
    ;; (mu4e-alert-enable-mode-line-display)
    ;; (mu4e-alert-set-default-style 'libnotify)
    ;; (alert-add-rule :category "mu4e-alert" :style 'fringe :predicate (lambda (_) (string-match-p "^mu4e-" (symbol-name major-mode))) :continue t)
    ;; (mu4e-alert-enable-notifications)
    )

  ;; this ensures we don't load the org-mode shipped with regular emacs
  (with-eval-after-load 'org
    (load-file (expand-file-name "~/.emacs.d/custom/bergheim-org.el")))


  ;; Default is 'light
  (setq heaven-and-hell-theme-type 'dark)

  ;; Set preferred light and dark themes
  ;; default light is emacs default theme, default dark is wombat
  ;; Themes can be the list: (dark . (tsdh-dark tango-dark))
  (setq heaven-and-hell-themes
        '((dark . doom-nord)
          (light . material-light)))
  ;; Optionall, load themes without asking for confirmation.
  (setq heaven-and-hell-load-theme-no-confirm t)

  ;; Add init-hook so heaven-and-hell can load your theme
  (add-hook 'after-init-hook 'heaven-and-hell-init-hook)

  (spacemacs/set-leader-keys "ot" 'heaven-and-hell-toggle-theme)
  (spacemacs/set-leader-keys "oT" 'heaven-and-hell-load-default-theme)

  ;; (add-to-list 'recentf-exclude "~/org/")

  (if (equal system-type 'gnu/linux)
      (spacemacs/disable-transparency))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol t)
 '(org-agenda-files
   '("~/dev/planet9/TODOs.org" "/home/tsb/org/projects/babeltest.org" "/home/tsb/org/projects/kp.org" "/home/tsb/org/projects/mediaserver.org" "/home/tsb/org/projects/recoil.org" "/home/tsb/org/projects/serverscript.org" "/home/tsb/org/projects/sg23borettslag.org" "/home/tsb/org/projects/vimdesktop.org" "/home/tsb/org/bergtech.org" "/home/tsb/org/elfeed.org" "/home/tsb/org/inbox.org" "/home/tsb/org/life.org" "/home/tsb/org/p9.org" "/home/tsb/org/test.org" "/home/tsb/org/work.org"))
 '(package-selected-packages
   '(ox-twbs ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))
 '(send-mail-function 'mailclient-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
