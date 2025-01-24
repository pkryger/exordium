;;; init-company.el --- company-mode                 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; ----------------- -------------------------------------------------------
;; Key               Definition
;; ----------------- -------------------------------------------------------
;; C-.               Force trigger company-complete.
;; C-o or C-\        Switch to other backend (`company-other-backend').
;; C-M-h             Show doc buffer for current candidate (`company-show-doc-buffer').
;; C-M-v             Scroll doc buffer up (`scroll-other-window').
;; C-M-S-v           Scroll doc buffer down ('scroll-other-window-down').
;; C-h or <f1>       Show quick help for current candidate (`company-posframe-quickhelp-toggle').
;; C-S-v or <f2>     Scroll quick help up (`company-posframe-quickhelp-scroll-up').
;; M-V or <f3>       Scroll quick help down (`company-posframe-quickhelp-scroll-down').

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)
(exordium-require 'init-git)

(require 'cl-lib)

(when (version< "29" emacs-version)
  (exordium-require 'init-forge))

(use-package company
  :diminish "CA"
  :demand t
  :commands (company-other-backend
             company-abort)
  :custom
  (company-idle-delay nil)
  (company-files-exclusions '(".git/" ".gitignore" ".gitmodules" ".DS_Store"
                              ".vscode/" ".envrc" ".direnv/" ".clangd"
                              "venv/" ".venv/"))
  (company-transformers '(delete-consecutive-dups))

  :config
  (add-to-list 'company-backends
               '(company-capf company-yasnippet company-files
                 :with company-dabbrev-code))

  ;; Turn on company mode everywhere
  (global-company-mode)

  :bind
  (("C-." . #'company-complete)
   :map company-active-map
   ("C-\\" . #'company-other-backend)
   ("C-o" . #'company-other-backend)))

(use-package company
  :diminish "CA"
  :if (version< "29" emacs-version)
  :defer t
  :commands (company-begin-backend)
  :autoload (company-doc-buffer
             company--render-icons-margin)
  :functions (exordium--company-extra-icons
              exordium--company-conventional-commits-type-p)
  :init
  (use-package forge-core
    :ensure forge
    :defer t
    :autoload (forge-get-repository))

  (use-package git-commit
    :ensure magit)


  (defvar exordium--company-extra-icons-mapping
    '((issue . "issue-opened-16.svg")
      (issue-closed . "issue-closed-16.svg")
      (issue-draft . "issue-draft-16.svg")
      (pullreq . "git-pull-request-16.svg")
      (pullreq-merged . "git-merge-16.svg")
      (pullreq-rejected . "git-pull-request-closed-16.svg")
      (pullreq-draft . "git-pull-request-draft-16.svg")
      (person . "person-16.svg")
      (team . "people-16.svg")))

  (defun exordium-company-assignees (command &optional arg &rest _ignored)
    "A `company-mode' backend for assigneees in `forge-mode' repository."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'exordium-company-assignees))
      (prefix
       (save-match-data
         (when (and (or (bound-and-true-p git-commit-mode)
                        (derived-mode-p 'forge-post-mode
                                        'git-commit-elisp-text-mode))
                    (forge-get-repository :tracked?)
                    (looking-back
                     (rx "@"
                         (zero-or-one
                          (group alphanumeric
                                 (repeat 0 38
                                         (or alphanumeric
                                             (seq (any "-/") alphanumeric))))))
                     (max (- (point) 40)
                          (line-beginning-position))))
           ;; IDK how to match end of a 'symbol' that is equal to an "@" or is
           ;; equal to "@foo" in neither `git-commit-mode' nor
           ;; `forge-post-mode'. Hence it's handled manually.  The
           ;; `looking-back' above matches an "@" or an "@foo". When it was the
           ;; latter there was a match in group 1.  Now, check if this is at
           ;; the very end of the "@" or "@foo".  Note that "@<point>@" also
           ;; matches. Probably a few other characters, substituting the second
           ;; "@" in latter pattern, would also give a positive result. Yet, in
           ;; such a case the `match' is "", so that's all fine - all
           ;; candidates will be shown.
           (when (or (save-match-data (looking-at "\\W"))
                     (= (point) (point-max)))
             (cons (or (match-string 1) "") t)))))
      (candidates (when-let* ((repo (forge-get-repository :tracked?)))
                    (append
                     (cl-remove-if-not
                      (lambda (assignee)
                        (string-prefix-p arg assignee))
                      (mapcar (lambda (assignee)
                                (propertize (cadr assignee)
                                            'full-name (caddr assignee)))
                              (ignore-errors (oref repo assignees))))
                     (cl-remove-if-not
                      (lambda (team)
                        (or (string-prefix-p arg team)
                            (string-prefix-p arg
                                             (cadr (string-split team "/")))))
                      (ignore-errors (oref repo teams))))))
      (annotation (when-let* ((assignee (get-text-property 0 'full-name arg)))
                    (format " [%s]" assignee)))))

  (defun exordium-company-topics (command &optional arg &rest _ignored)
    "Backend for issues and pull requests in `forge-mode' repository."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'exordium-company-topics))
      (prefix
       (save-match-data
         (when (and (or (bound-and-true-p git-commit-mode)
                        (derived-mode-p 'forge-post-mode
                                        'git-commit-elisp-text-mode))
                    (forge-get-repository :tracked?)
                    (looking-back
                     (rx "#"
                          (group (repeat 0 10 digit)))
                     (max (- (point) 11)
                          (line-beginning-position))))
           ;; IDK how to match end of a 'symbol' that is equal to an "#" or is
           ;; equal to "#123" in neither `git-commit-mode' nor
           ;; `forge-post-mode'. Hence it's handled manually.  The
           ;; `looking-back' above matches an "#" or an "#123". When it was the
           ;; latter there was a match in group 1.  Now, check if this is at
           ;; the very end of the "#" or "#123".  Note that "#<point>#" also
           ;; matches. Probably a few other characters, substituting the second
           ;; "#" in latter pattern, would also give a positive result. Yet, in
           ;; such a case the `match' is "", so that's all fine - all
           ;; candidates will be shown.
           (when (or (save-match-data (looking-at "\\W"))
                     (= (point) (point-max)))
             (cons (or (match-string 1) "") t)))))
      (candidates (when-let* ((repo (forge-get-repository :tracked?)))
                    (mapcar
                     (lambda (topic)
                       (propertize
                        (number-to-string (oref topic number))
                        'title (oref topic title)
                        'id (oref topic id)
                        'exordium-kind (pcase (list
                                               (eieio-object-class topic)
                                               (oref topic state)
                                               (when (slot-exists-p topic 'draft-p)
                                                 (oref topic draft-p)))
                                         ('(forge-issue open nil) 'issue)
                                         ('(forge-issue open t) 'issue-draft)
                                         (`(forge-issue ,_ ,_) 'issue-closed)
                                         ('(forge-pullreq open nil) 'pullreq)
                                         ('(forge-pullreq open t) 'pullreq-draft)
                                         (`(forge-pullreq merged ,_) 'pullreq-merged)
                                         (`(forge-pullreq ,_ ,_) 'pullreq-rejected))))
                            (cl-remove-if-not
                             (lambda (topic)
                               (string-prefix-p arg (number-to-string
                                                     (oref topic number))))
                             (forge--list-topics
                              (forge--topics-spec :type 'topic
                                                  :active nil
                                                  :state nil
                                                  :order 'recently-updated)
                              repo)))))
      (kind (get-text-property 0 'exordium-kind arg))
      (annotation (when-let* ((title (get-text-property 0 'title arg)))
                    (format " [%s]"  title)))
      (doc-buffer (when-let* ((id (get-text-property 0 'id arg))
                              (topic (forge-get-topic id))
                              (repo (forge-get-repository topic))
                              (magit-display-buffer-noselect t)
                              (buffer (company-doc-buffer)))
                    ;; Do like `forge-topic-setup-buffer' does, except:
                    ;; - ensure buffer is not selected,
                    ;; - use `company-doc-buffer',
                    ;; - don't mark topics as read,
                    ;; - ensure `buffer-read-only' is nil.
                    (unwind-protect
                        (magit-setup-buffer-internal
                         (if (forge-issue-p topic)
                             #'forge-issue-mode
                           #'forge-pullreq-mode)
                         t
                         `((forge-buffer-topic ,topic))
                         buffer
                         (or (forge-get-worktree repo) "/"))
                      (with-current-buffer buffer
                        (setq buffer-read-only nil)))))
      (quickhelp-string (when-let* ((id (get-text-property 0 'id arg))
                                    (topic (forge-get-topic id)))
                          (concat
                           (exordium--company-extra-icons
                            (lambda (&rest _)
                              (if-let* ((kind (assq
                                               (get-text-property 0 'exordium-kind)
                                               company-text-icons-mapping)))
                                  (propertize (format "[%s] " kind)
                                              'face 'italic)
                                ""))
                            arg)
                           (propertize (oref topic title)
                                       'face 'bold)
                           "\n\n"
                           (oref topic body))))
      (sorted t)))

  (defun exordium--company-extra-icons (orig-fun &rest args)
    "If (car ARGS) has `exordium-kind' try to render Exordium icon for it."
    (if-let* (((display-graphic-p))
              ((image-type-available-p 'svg))
              (candidate (car args))
              ((get-text-property 0 'exordium-kind candidate))
              ;; Octicons delivered with Exordium are slightly larger than
              ;; icons delivered with `company'. Make them appear a bit smaller
              (icon (let ((company-icon-size
                           (pcase company-icon-size
                             ((and (pred numberp) value)
                              (truncate (fround (* .9 value))))
                             (`(auto-scale . ,value)
                              (cons 'auto-scale
                                    (truncate (fround (* .9 value))))))))
                      (company--render-icons-margin
                       exordium--company-extra-icons-mapping
                       (expand-file-name "company-icons" user-emacs-directory)
                       candidate
                       (cadr args)))))
        icon
      (apply orig-fun args)))

  (defconst exordium--company-conventional-commits-types
    (list
     ;; From Conventional Commits: https://www.conventionalcommits.org
     ;; and Angular: https://github.com/angular/angular/blob/22b96b9/CONTRIBUTING.md#type
     (propertize
      "build"
      'exordium-cc-annotation
      "Changes that affect the build system or external dependencies")
     (propertize
      "ci"
      'exordium-cc-annotation
      "Changes to CI configuration files and scripts")
     "chore"
     (propertize
      "docs"
      'exordium-cc-annotation
      "Documentation only changes")
     (propertize
      "feat"
      'exordium-cc-annotation
      "Documentation only changes")
     (propertize
      "fix"
      'exordium-cc-annotation
      "A bug fix")
     (propertize
      "perf"
      'exordium-cc-annotation
      "A change that improves performance")
     (propertize
      "refactor"
      'exordium-cc-annotation
      "A change that neither fixes a bug nor adds a feature")
     (propertize
      "revert"
      'exordium-cc-annotation
      "A change that reverts other commit(s)")
     (propertize
      "style"
      'exordium-cc-annotation
      "Changes that do not affect the meaning of the code ")
     (propertize
      "test"
      'exordium-cc-annotation
      "Adding missing tests or correcting existing tests")
     )
    "A list of Conventional Commit types that appear in completion candidates.")

  (defconst exordium--company-conventional-commits-longest-type
    (apply #'max (mapcar #'length exordium--company-conventional-commits-types)))

  (defconst exordium--company-conventional-commits-footers
    (append
     (list
     ;; From Conventional Commits: https://www.conventionalcommits.org
     ;; and Angular: https://github.com/angular/angular/blob/22b96b9/CONTRIBUTING.md#type
     (propertize
      "BREAKING CHANGE"
      'exordium-cc-annotation
      "Explanation of breaking changes")
     ;; From GitHub:
     ;; https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/using-keywords-in-issues-and-pull-requests#linking-a-pull-request-to-an-issue
     (propertize
      "close"
      'exordium-cc-annotation
      "Closes issue # on GitHub")
     (propertize
      "closes"
      'exordium-cc-annotation
      "Closes issue # on GitHub")
     (propertize
      "closed"
      'exordium-cc-annotation
      "Closes issue # on GitHub")
     (propertize
      "fix"
      'exordium-cc-annotation
      "Fixes and closes issue # on GitHub")
     (propertize
      "fixes"
      'exordium-cc-annotation
      "Fixes and closes issue # on GitHub")
     (propertize
      "fixed"
      'exordium-cc-annotation
      "Fixes and closes issue # on GitHub")
     (propertize
      "resolve"
      'exordium-cc-annotation
      "Resolves and closes issue # on GitHub")
     (propertize
      "resolves"
      'exordium-cc-annotation
      "Resolves and closes issue # on GitHub")
     (propertize
      "resolved"
      'exordium-cc-annotation
      "Resolves and closes issue # on GitHub")
     ;; Top 1% of trailers: https://lore.kernel.org/git/60ad75ac7ffca_2ae08208b@natae.notmuch/
     "acked-by"
     "reviewed-by"
     )
     git-commit-trailers)
    "A list of Conventional Commit footers that appear in completion candidates.")

  (defconst exordium--company-conventional-commits-longest-footer
    (apply #'max (mapcar #'length exordium--company-conventional-commits-footers)))

  (defun exordium--company-conventional-commits-type-p ()
    "Return non-nil when point is at possible type completion.
Type completion can possibly happen in first line or in any line
that is preceded by lines that are either empty or contain only
whitespaces."
    (or (equal 1 (line-number-at-pos (point)))
        (when-let* ((pos-last-empty
                     (save-excursion
                       (goto-char (point-min))
                       (re-search-forward
                        (rx-to-string
                         `(seq string-start (one-or-more (or "\n" whitespace))))
                        nil t))))
          (<= (compat-call pos-bol) pos-last-empty))))

  (defun exordium-company-conventional-commits (command &optional arg &rest _ignored)
    "A `company-mode' backend for Conventional Commits.
See https://www.conventionalcommits.org for details."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'company-simple-backend))
      (prefix
       (when (or (bound-and-true-p git-commit-mode)
                 (derived-mode-p 'forge-post-mode
                                 'git-commit-ts-mode
                                 'git-commit-elisp-text-mode))
         (let ((col (current-column))
               (type (exordium--company-conventional-commits-type-p)))
           (when (and
                  (< col (if type
                             exordium--company-conventional-commits-longest-type
                           exordium--company-conventional-commits-longest-footer))
                  (save-excursion
                    (goto-char (compat-call pos-bol)) ; since Emacs-29
                    (re-search-forward
                     (rx-to-string
                      `(seq line-start
                            (= ,col ,@(if type '(alnum) '((or alnum space))))))
                     nil t)))
             (match-string 0)))))
      (candidates
       (let ((candidates
              (if (exordium--company-conventional-commits-type-p)
                  exordium--company-conventional-commits-types
                exordium--company-conventional-commits-footers)))
         (cl-remove-if-not (lambda (candidate)
                             (string-prefix-p arg candidate))
                           candidates)))
      (annotation
       (when-let* ((annotation (get-text-property
                                0 'exordium-cc-annotation arg)))
         (format " [%s]" annotation)))))

  :config
  ;; This is block is deferred , so these backends will end up first
  (add-to-list 'company-backends 'exordium-company-assignees)
  (add-to-list 'company-backends 'exordium-company-topics)
  (add-to-list 'company-backends 'exordium-company-conventional-commits)
  (dolist (mapping '((issue "i")
                     (issue-closed "c")
                     (issue-draft "d")
                     (pullreq "p")
                     (pullreq-merged "m")
                     (pullreq-rejected "r")
                     (pullreq-draft  "d")
                     (person "p")
                     (team "t")))
    (add-to-list 'company-text-icons-mapping mapping))
  (advice-add 'company-detect-icons-margin
              :around #'exordium--company-extra-icons))

(use-package company-statistics
  :after (company)
  :config
  (company-statistics-mode)
  (add-to-list 'company-transformers
               'company-sort-by-backend-importance 'append))

(use-package company-posframe
  :demand t
  :diminish
  :init
  (use-package company
    :defer t
    :commands (company-show-doc-buffer))
  :commands (company-posframe-quickhelp-toggle
             company-posframe-quickhelp-scroll-down
             company-posframe-quickhelp-scroll-up)
  :bind
  (:map company-posframe-active-map
   ("C-h" . #'company-posframe-quickhelp-toggle)
   ("C-M-h" . #'company-show-doc-buffer)
   ("C-S-v" . #'company-posframe-quickhelp-scroll-up)
   ("M-V" . #'company-posframe-quickhelp-scroll-down))
  :custom
  (company-posframe-quickhelp-delay 0.2)
  (company-posframe-quickhelp-x-offset 5)
  :config
  (company-posframe-mode 1))

(provide 'init-company)

;;; init-company.el ends here
