;;; init-desktop.el --- Configuration of desktop state and history -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'saveplace)

(use-package desktop
  :ensure nil
  :functions (exordium--restore-desktop)
  :init
  (defun exordium--restore-desktop ()
    "Restore desktop."
    (unless desktop-save-mode
      (setq desktop-path (list user-emacs-directory))
      (setq desktop-save t)
      (message (format "Loading desktop from %s" desktop-path))
      (desktop-read)
      (desktop-save-mode 1)))
  :custom
  (desktop-files-not-to-save
   (rx-to-string `(or
                   ;; original value of `desktop-files-not-to-save'
                   (seq string-start "/" (zero-or-more (not (any "/" ":"))) ":")
                   (seq "(ftp)" string-end)
                   ;; skip also Emacs and ELPA
                   (seq string-start
                        (or ,(expand-file-name
                              (file-name-parent-directory data-directory))
                            ,(expand-file-name
                              (file-name-as-directory package-user-dir)))))))
  (desktop-restore-eager 8)
  (desktop-load-locked-desktop (if (version< "29" emacs-version) 'check-pid 'ask))
  :config
  ;; Don't save some buffers in desktop
  (dolist (mode'(dired-mode
                 Info-mode
                 info-lookup-mode
                 fundamental-mode
                 helpful-mode
                 helm-major-mode
                 magit-mode
                 magit-log-mode
                 magit-status-mode
                 magit-process-mode
                 magit-diff-mode
                 forge-pullreq-mode
                 forge-notifications-mode
                 difftastic-mode))
    (add-to-list 'desktop-modes-not-to-save mode))

  (add-to-list 'desktop-minor-mode-table '(company-posframe-mode nil))

  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'exordium--restore-desktop)
    (add-hook 'after-init-hook #'desktop-save-mode)))

(use-package savehist
  :ensure nil
  :custom
  (savehist-additional-variables '(command-history
                                   kill-ring
                                   log-edit-comment-ring
                                   recentf-list
                                   regexp-search-ring
                                   register-alist
                                   search-ring))
  :config
  (savehist-mode))

(setq save-place-file
      (locate-user-emacs-file "saveplace"))   ;; location to save point
(save-place-mode)                             ;; activate it for all buffers

(provide 'init-desktop)

;;; init-desktop.el ends here
