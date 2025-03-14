;;; init-prog-mode.el --- Shared prog-mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

(require 'newcomment)
(require 'prog-mode)
(require 'whitespace)

(use-package cmake-mode
  :defer t)

(define-minor-mode exordium-show-trailing-whitespace-mode
  "Enable `show-trailing-whitespace'."
  :init-value nil
  :lighter nil
  (progn (setq show-trailing-whitespace exordium-show-trailing-whitespace-mode)))

(define-minor-mode exordium-require-final-newline-mode
  "Enables `require-final-newline'."
  :init-value nil
  :lighter nil
  (progn (setq require-final-newline exordium-require-final-newline-mode)))

(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'prog-mode-hook #'exordium-show-trailing-whitespace-mode)
(add-hook 'prog-mode-hook #'exordium-require-final-newline-mode)

(use-package flyspell
  :ensure nil
  :if (eq exordium-spell-check :prog)
  :diminish flyspell-mode
  :hook
  (prog-mode . flyspell-prog-mode)
  :bind
  ;; unbind as it colides with company, see init-company.el
  (:map flyspell-mode-map
   ("C-." . nil)))

;;; Electric pair: automatically close parenthesis, curly brace etc.
;;; `electric-pair-open-newline-between-pairs'.
(setq electric-pair-open-newline-between-pairs t)
(when exordium-enable-electric-pair-mode
  (add-hook 'prog-mode-hook #'electric-pair-mode))

;;; The return key
(cond (exordium-enable-newline-and-indent
       (bind-key "RET" #'newline-and-indent prog-mode-map)
       (bind-key "S-RET" #'newline prog-mode-map))
      (t
       (bind-key "S-RET" #'newline-and-indent prog-mode-map)))


;;; Fill comments, comment regions
(setq comment-auto-fill-only-comments 1)
(bind-key "C-c C-c" #'comment-dwim prog-mode-map)

;;; Step through compile errors
(bind-key "<f10>" #'next-error)
(bind-key "C-<f10>" #'previous-error)


;;; Font lock changes

(defun exordium--add-keywords-for-todos ()
  "Display TODO: and FIXME: and TBD: in warning face."
    (font-lock-add-keywords
     nil
     '(("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
       ("\\<\\(TBD\\):" 1 font-lock-warning-face prepend)
       ("\\<\\(TODO\\):" 1 font-lock-warning-face prepend))))

(when exordium-font-lock
  (add-hook 'prog-mode-hook #'exordium--add-keywords-for-todos))


(defvar diminished-mode-alist)

(define-minor-mode exordium-whitespace-tabs-mode
  "Toggle tabs visualisation.

Use `whitespace-newline-mode' only for NEWLINE visualization
exclusively.  For other visualizations, including NEWLINE
visualization together with (HARD) SPACEs and/or TABs, please,
use `whitespace-mode'."
  :lighter    " ts"
  :init-value nil
  :global     nil
  :group      'exordium
  (let ((whitespace-style '(face tabs tab-mark)))
    (whitespace-mode (if exordium-whitespace-tabs-mode 1 -1)))
  (setq exordium-whitespace-tabs-mode whitespace-mode)
  (if exordium-whitespace-tabs-mode
      (progn
        (make-local-variable 'diminished-mode-alist)
        (diminish 'whitespace-mode))
    (when (and (local-variable-p 'diminished-mode-alist)
               (equal (default-value 'diminished-mode-alist)
                      (cl-remove-if (lambda (diminished)
                                      (eq (car diminished) 'whitespace-mode))
                                    diminished-mode-alist)))
      (kill-local-variable 'diminished-mode-alist))))

(use-package make-mode
  :defer t
  :init
  :hook (makefile-mode . exordium-whitespace-tabs-mode))

(provide 'init-prog-mode)

;;; init-prog-mode.el ends here
