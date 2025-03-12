;;; init-treesit.el --- Configuration for Treesitter -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)
(exordium-require 'init-git)

(defmacro exordium-eval-unless-compile-or-ci (&rest body)
  "Don't evaluate BODY when either file is byte compiled when in CI."
  (declare (debug (&rest def-form)) (indent 0))
  (if (or (bound-and-true-p byte-compile-current-file)
          (eval-when-compile (getenv "ci_tests")))
      `(message "Skipping during %s %S"
                ,(if (bound-and-true-p byte-compile-current-file)
                     "compilation"
                   "CI tests")
                ',body)
    `(progn ,@body)))

(if (and (version< "29" emacs-version) (treesit-available-p))
    (progn
      (message "Enabling built-in treesit and external treesit-auto")
      (use-package git-commit-ts-mode
        :functions (exordium--git-commit-ts-verify)
        :init
        (use-package git-commit
          :ensure magit
          :defer t
          :custom
          (git-commit-major-mode #'git-commit-ts-mode))

        (defun exordium--git-commit-ts-verify ()
          "Used for `flyspell-generic-check-word-predicate' in `git-commit-ts-mode'."
          ;; Like `flyspell-generic-progmode-verify', but use faces from
          ;; `git-commit-ts-mode' and include nil (no face) for regular message
          ;; body text.  Unfortunately, the `font-lock' in a `treesit' mode
          ;; kicks in too late to fontify heading prefixes, and trailers, so
          ;; these tend to be highlighted as well.
          (unless (eql (point) (point-min))
            (let ((f (get-text-property (1- (point)) 'face)))
              (memq f '(nil
                        git-commit-ts-comment-face
                        git-commit-ts-title-face
                        git-commit-ts-overflow-face
                        git-commit-ts-breaking-change-value-face)))))
        :defer t
        :config
        (put 'git-commit-ts-mode
             'flyspell-mode-predicate
             #'exordium--git-commit-ts-verify))

      (use-package treesit-auto
        :after treesit
        :demand t
        :init
        (defun exordium--add-forward-ts-hook (recipe)
          "Add hook to a `ts-mode' slot from RECIPE.
A hook is added for each mode that is found in `remap' slot in recipe."
          (when-let* ((modes (ensure-list (treesit-auto-recipe-remap recipe)))
                      (ts-mode (treesit-auto-recipe-ts-mode recipe)))
            (dolist (mode modes)
              (let* ((ts-hook (intern (concat (symbol-name ts-mode) "-hook")))
                     (hook (intern (concat (symbol-name mode) "-hook"))))
                (add-hook ts-hook
                          (lambda ()
                            (run-hooks hook)))))))

        :functions (exordium--add-forward-ts-hook)
        :autoload (make-treesit-auto-recipe)
        :commands (global-treesit-auto-mode)
        :defines (treesit-auto-recipe-alist
                  treesit-auto-mode-langs)
        :custom
        (treesit-auto-install (if (boundp 'treesit-auto-install)
                                  treesit-auto-install
                                (unless (getenv "ci_tests")
                                  'prompt))
                              "Disable automatic grammar downloading in CI")
        :config
        (unless (memq 'gitcommit treesit-auto-langs)
          (push
           (make-treesit-auto-recipe
            :lang 'gitcommit
            :ts-mode 'git-commit-ts-mode
            :url "https://github.com/gbprod/tree-sitter-gitcommit"
            :ext "\\.COMMIT_EDITMSG\\'")
           treesit-auto-recipe-list)
          (push 'gitcommit treesit-auto-langs))
        (global-treesit-auto-mode)
        (dolist (recipe treesit-auto-recipe-list)
          (exordium--add-forward-ts-hook recipe)))

      (use-package treesit
        :ensure nil
        :custom
        (treesit-font-lock-level 4)))

  (exordium-eval-unless-compile-or-ci
    (message "Enabling external tree-sitter and tree-sitter-langs")
    (use-package tree-sitter-langs)
    (use-package tree-sitter
      :diminish
      :commands (global-tree-sitter-mode)
      :defines (tree-sitter-major-mode-language-alist)
      :hook
      (tree-sitter-after-on . tree-sitter-hl-mode)
      :custom
      (font-lock-maximum-decoration t)
      :config
      (when-let* ((language-name (alist-get 'ruby-mode
                                            tree-sitter-major-mode-language-alist)))
        (add-to-list 'tree-sitter-major-mode-language-alist
                     (cons 'enh-ruby-mode language-name)))
      (add-to-list 'tree-sitter-major-mode-language-alist
                   (cons 'forge-post-mode 'markdown))
      (global-tree-sitter-mode))))

(provide 'init-treesit)

;;; init-treesit.el ends here
