;;; bergheim-containers.el --- Docker and devcontainers -*- lexical-binding: t; -*-

(use-package docker
  :config
  (setq docker-show-messages nil)
  (bergheim/global-menu-keys
    "ad" '(:ignore t :which-key "Docker")
    "add" '(docker :which-key "Docker")
    "adc" '(docker-containers :which-key "Containers")
    "adi" '(docker-images :which-key "Images")
    "adn" '(docker-networks :which-key "Networks")
    "adv" '(docker-volumes :which-key "Volumes")))

(use-package docker-compose-mode)

(use-package devcontainer
  :demand
  :ensure (:host github :repo "johannes-mueller/devcontainer.el")
  :general
  (bergheim/global-menu-keys
    "cc" '(:ignore t :which-key "devcontainers")
    "ccc" '(devcontainer-up :which-key "Start")
    "ccd" '(devcontainer-tramp-dired :which-key "dired")
    "ccr" '(devcontainer-restart :which-key "Restart")
    "ccR" '(devcontainer-rebuild-and-restart :which-key "rebuild and restart")
    "cct" '(devcontainer-term :which-key "terminal")
    "ccx" '(devcontainer-kill-container :which-key "kill"))
  :config
  (add-to-list 'devcontainer-execute-outside-container "podman")
  (setq devcontainer-engine 'podman
        devcontainer-term-shell "zsh"
        devcontainer-term-function #'eat)
  (devcontainer-mode 1))

;;; bergheim-containers.el ends here
