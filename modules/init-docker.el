;;; init-docker.el --- Configuration of Docker related features -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package docker
  :bind ("C-c D" . #'docker))

(use-package dockerfile-mode
  :init
  (use-package sh-script
    :ensure nil
    :autoload (sh-backslash-region))
  :mode "Dockerfile\\'"
  :bind
  (:map dockerfile-mode-map
   ("C-c C-\\" . #'sh-backslash-region))
  :custom
  (docker-use-sudo nil))

(use-package dockerfile-ts-mode
  :init
  (use-package sh-script
    :ensure nil
    :autoload (sh-backslash-region))
  :ensure nil
  :defer t
  :bind
  (:map dockerfile-ts-mode-map
   ("C-c C-\\" . #'sh-backslash-region)))

(when (version< emacs-version "29")
  (use-package docker-tramp
    :after docker
    :defer t))

(provide 'init-docker)

;;; init-docker.el ends here
