;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

(package! org-mru-clock)
(package! org-super-agenda)
(package! org-ql)
(package! org-sticky-header)
(package! doct)
;; https://github.com/IvanMalison/org-projectile
;; (package! org-projectile)
(package! org-fancy-priorities)
;; (package! helm-org-rifle)
(package! org-roam-ui)
(unpin! org-roam)

(package! org-caldav)
(package! calendar-norway)
;; (package! excorporate)

(package! mu4e-alert :disable t)
(package! mu4e-thread
  :recipe (:host github :repo "rougier/mu4e-thread"))
;; TODO: just here while 1.10 settles..
(unpin! evil-collection)
(package! evil-collection
  :recipe (:host github :repo "meliache/evil-collection"))
(package! evil-matchit)
(unpin! mu4e)

;; should improve auto-complete (in theory)
(package! company :disable t)
(unpin! corfu)
(package! corfu)
(package! orderless)
(package! cape)

(package! auto-dim-other-buffers)
(package! tao-theme) ;; uncolored back and white themes
(package! heaven-and-hell)
(package! rainbow-mode)
;; allows you to search in a browser from emacs
;; (package! engine-mode)

(package! restclient)
;; this is useful for things like org repos
(package! git-auto-commit-mode)

;; TODO: trials

(package! org-recent-headings)
(package! ox-report)
(package! affe)
(package! deadgrep)
