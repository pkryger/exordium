;;; init-markdown.t.el --- Tests for init-markdown   -*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-markdown)
(exordium-require 'init-prefs)

(require 'ert)
(require 'cl-lib)

(ert-deftest test-exordium-markdown-fill-paragraph-scenarios ()
  (when (fboundp 'ert-test-erts-file) ;; Since Emacs-29
    (when-let* ((file (file-name-concat (locate-user-emacs-file "modules")
                                        "init-markdown.t.erts")))
      (should (file-exists-p file))
      (ert-test-erts-file
       file
       (lambda ()
         (let ((fill-column 25)
               (exordium-markdown-gh-render-links-predicate
                '(or (derived-mode . markdown-mode)
                     (derived-mode . markdown-ts-mode))))
           (markdown-mode)
           (fill-paragraph)))))))

(ert-deftest test-exordium-markdown-ts-fill-paragraph-scenarios ()
  (when (fboundp 'markdown-ts-mode) ;; Since Emacs-31
    (when-let* ((file (file-name-concat (locate-user-emacs-file "modules")
                                        "init-markdown.t.erts")))
      (should (file-exists-p file))
      (ert-test-erts-file
       file
       (lambda ()
         (cl-letf (((symbol-function 'treesit-ensure-installed) #'ignore)
                   (fill-column 25)
                   (exordium-markdown-gh-render-links-predicate
                    '(or (derived-mode . markdown-mode)
                         (derived-mode . markdown-ts-mode))))
           (markdown-ts-mode)
           (fill-paragraph)))))))

(provide 'init-markdown.t)

;;; init-markdown.t.el ends here
