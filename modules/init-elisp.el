;;; init-elisp.el --- Configuration for Emacs Lisp   -*- lexical-binding: t -*-

;;; Commentary:
;;
;; ----------------- ---------------------------------------------------------
;; Key               Definition
;; ----------------- ---------------------------------------------------------
;; M-C-g             `helm-imenu' (lists functions and variables in buffer)
;; C-c M-e           `pp-eval-last-sexp'

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-helm)

(when exordium-help-extensions
  (exordium-require 'init-help))

(use-package elisp-mode
  :ensure nil
  :defer t
  :functions (exordium--pp-last-sexp-filter-quote)
  :mode ("/Cask\\'" . emacs-lisp-mode)
  :init
  (defun exordium--pp-output-setup ()
    "Enable `lexical-binding' and disable `flycheck-checkdoc' in PP output buffers."
    (when (string-match-p (rx string-start
                              "*Pp " (or "Eval" "Macroexpand") " Output*"
                              string-end)
                          (buffer-name))
      (when (boundp 'flycheck-disabled-checkers)
        (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc))
      (goto-char (point-min))
      (insert ";; -*- lexical-binding: t -*-\n\n")
      (goto-char (point-max))
      (setq-local lexical-binding t)))

  :bind
  (:map emacs-lisp-mode-map
   ("M-C-g" . #'helm-imenu)
   ("C-c M-e" . #'pp-eval-last-sexp)
   ("C-c C-M-e" . #'pp-macroexpand-last-sexp)
   ("M-." . #'xref-find-definitions)
   ("M-," . #'xref-go-back)
   ("M-r" . #'xref-find-references)
   :map lisp-interaction-mode-map
   ("M-C-g" . #'helm-imenu)
   ("C-c M-e" . #'pp-eval-last-sexp)
   ("C-c C-M-e" . #'pp-macroexpand-last-sexp)
   ("M-." . #'xref-find-definitions)
   ("M-," . #'xref-go-back)
   ("M-r" . #'xref-find-references))
  :hook
  (emacs-lisp-mode . exordium--pp-output-setup))

(use-package elisp-mode
  :ensure nil
  :defer t
  :if exordium-help-extensions
  :bind
  (:map emacs-lisp-mode-map
   ("M-?" . #'helpful-at-point)
   ("C-c C-d" . #'helpful-at-point)
   :map lisp-interaction-mode-map
   ("M-?" . #'helpful-at-point)
   ("C-c C-d" . #'helpful-at-point)))


;;; Display page breaks with an horizontal line instead of ^L.
;;; Note: To insert a page break: C-q C-l
;;;       To jump to the previous/next page break: C-x [ and C-x ]
(use-package page-break-lines
  :diminish
  :init
  (defun exordium-page-break-lines ()
    "Enable `page-break-lines' mode.
When in TUI enable line truncation as well to prevent a rendering
bug (page break lines wrap around)."
    (unless (display-graphic-p)
      (set (make-local-variable 'truncate-lines) t))
    (page-break-lines-mode))
  :hook
  ((emacs-lisp-mode compilation-mode help-mode emacs-news-mode)
   . exordium-page-break-lines))

;;; Animation when evaluating a defun or a region:
;; The `eval-sexp-fu-mode' is global so it makes no sense to add it to relevant hooks.
;; Package install advices as part of initialisation.
(use-package eval-sexp-fu
  :demand t)

;;; Scans Emacs Lisp files for mistakes in regexps
;; Use M-x `relint-current-buffer' to see report.
(use-package relint
  :defer t)


(provide 'init-elisp)

;;; init-elisp.el ends here
