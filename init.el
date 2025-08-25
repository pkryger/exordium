;;; init.el --- Exordium init.el                     -*- lexical-binding: t -*-
;;; Commentary:
;;;;  ___      __   __   __
;;;; |__  \_/ /  \ |__) |  \ | |  |  |\/|
;;;; |___ / \ \__/ |  \ |__/ | \__/  |  |
;;;;
;;;; Emacs Makes All Computing Simple.

;;; Code:

;; Reduce the frequency of garbage collection by making it happen on
;; each 100MB of allocated data (the default is on every 0.76MB). This reduces
;; the startup time.
(setq gc-cons-threshold 100000000)

(let ((min-version "28"))
  (when (version< emacs-version min-version)
    (error "This config requires at least Emacs-%s, but you're running %s"
           min-version emacs-version)))

(setq initial-scratch-message
      "EXORDIUM DID NOT LOAD CORRECTLY.
Check the warnings and messages buffers, or restart with --debug-init")

(defconst exordium-before-init "before-init.el"
  "Name of the before init file.")

(defconst exordium-prefs "prefs.el"
  "Name of the prefs file.")

(defconst exordium-after-init "after-init.el"
  "Name of the after init file.")

(defconst exordium-custom "emacs-custom.el"
  "Name of the customization file.")

;; Use this file for HTTP proxy settings if needed for packages.  Also add
;; additional packages to exordium-extra-packages for packages to be
;; automatically pulled from the elpa archives

(defconst exordium-before-init-file
  (expand-file-name (locate-user-emacs-file exordium-before-init))
  "Location of the master before init file.")

(defconst exordium-modules-dir
  (expand-file-name (locate-user-emacs-file "modules"))
  "Location of the modules directory.")

(defconst exordium-themes-dir
  (expand-file-name (locate-user-emacs-file "themes"))
  "Location of the themes directory.")

(defconst exordium-extensions-dir
  (expand-file-name (locate-user-emacs-file "extensions"))
  "Location of the extensions directory.")

(defconst exordium-local-dir
  (expand-file-name (locate-user-emacs-file "local"))
  "Location of the local directory.")

(defconst exordium-prefs-file
  (expand-file-name (locate-user-emacs-file exordium-prefs))
  "Location of the master prefs file.")

(defconst exordium-after-init-file
  (expand-file-name (locate-user-emacs-file exordium-after-init))
  "Location of the master after init file.")

(defconst exordium-custom-file
  (expand-file-name (locate-user-emacs-file exordium-custom))
  "Location of the customization file.")

;; Save any custom set variable in exordium-custom-file rather than at the end of init.el:
(setq custom-file exordium-custom-file)

(defcustom exordium-extra-packages ()
  "A list of additional packages to auto load from ELPA repositories."
  :group 'exordium
  :type  '(repeat (symbol :tag "Package")))

(defcustom exordium-extra-pinned ()
  "An alist of additional packages locations to pin to.

Each element of the list is in the same form as in `package-pinned-packages'."
  :group 'exordium
  :type  '(alist :key-type (symbol :tag "Package")
                 :value-type (string :tag "Archive name")))

(defconst exordium-melpa-package-repo "https://melpa.org/packages/"
  "URL for MELPA packages repository.
Default for Exordium.")

(defconst exordium-stable-melpa-package-repo "https://stable.melpa.org/packages/"
  "URL for stable MELPA packages repository.
Only active when there are `exordium-extra-pinned' packages from
melpa-stable.")

;; Taps definition of before and after files. These are loaded
;; after master 'before', 'after', and 'prefs' files

(defconst exordium-taps-root
  (expand-file-name (locate-user-emacs-file "taps"))
  "Location of the tapped directories.")

(defvar exordium-tapped-before-init-files ()
  "All tapped before init files, including master one.")

(defvar exordium-tapped-prefs-files ()
  "All tapped prefs files, including master one.")

(defvar exordium-tapped-after-init-files ()
  "All tapped after init files, including master one.")


(when (file-accessible-directory-p exordium-taps-root)
  (dolist (tap (nreverse (directory-files
                          exordium-taps-root t
                          directory-files-no-dot-files-regexp)))
    (when (file-accessible-directory-p tap)
      (when-let* ((tapped (file-name-concat tap exordium-before-init))
                  ((file-readable-p tapped)))
        (add-to-list 'exordium-tapped-before-init-files tapped))
      (when-let* ((tapped (file-name-concat tap exordium-prefs))
                  ((file-readable-p tapped)))
        (add-to-list 'exordium-tapped-prefs-files tapped))
      (when-let* ((tapped (file-name-concat tap exordium-after-init))
                  ((file-readable-p tapped)))
        (add-to-list 'exordium-tapped-after-init-files tapped)))))

(when (file-readable-p exordium-before-init-file)
  (add-to-list 'exordium-tapped-before-init-files exordium-before-init-file))

(when (file-readable-p exordium-prefs-file)
  (add-to-list 'exordium-tapped-prefs-files exordium-prefs-file))

(when (file-readable-p exordium-after-init-file)
  (add-to-list 'exordium-tapped-after-init-files exordium-after-init-file))

(when (file-readable-p exordium-custom-file)
  (add-to-list 'exordium-tapped-after-init-files exordium-custom-file))

;; Bind this early, and only if it has not been bound already,
;; so customisation from other places (i.e., before-init files)
;; are not overwritten.
(unless (boundp 'package-install-upgrade-built-in) ;; since Emacs-29
  (setq package-install-upgrade-built-in t))

;; Load before init files
(dolist (tapped-file exordium-tapped-before-init-files)
  (message "Loading tapped before-init file: %s" tapped-file)
  (load (file-name-sans-extension tapped-file)))

(require 'cl-lib)
(when (version< "29" emacs-version)
  ;; The `if-let*' macros are not available by default until Emacs-29.
  (unless (fboundp 'if-let*)
    (require 'subr-x)))

;;; Packages from Melpa
;; Use M-x `package-refresh-contents' to update the cache.
;; Use M-x `package-list-package' to load and display the list of packages,
;; then press I to mark for installation and X to execute (it's like `dired').

;; Initialize the package system
(require 'package)

(add-to-list 'package-archives
             (cons "melpa" exordium-melpa-package-repo) t)

(when (let (match)
        (mapc (lambda (pkg)
                (when (equal "melpa-stable" (cdr pkg))
                  (setq match t)))
              exordium-extra-pinned)
        match)
  (add-to-list 'package-archives
               (cons "melpa-stable" exordium-stable-melpa-package-repo) t))

(setq package-user-dir
      (locate-user-emacs-file (concat "elpa-" emacs-version)))

(when (fboundp 'native-comp-available-p)
  (setq package-native-compile (native-comp-available-p)))

(package-initialize)

;; Limit calls to `package-refresh-contents' to when it may be necessary
;; respecting users forcible refreshes and inhibits.
(let ((last-exordium-commit (string-to-number
                             (let ((default-directory user-emacs-directory))
                               (shell-command-to-string
                                "git log -1 --format=%ct | tr -d '\n'"))))
      (last-archive-contents
       (apply #'max
              (or
               (mapcar (lambda (archive)
                         (let ((contents-file (file-name-concat
                                               package-user-dir
                                               "archives"
                                               (car archive)
                                               "archive-contents")))
                           (if (file-exists-p contents-file)
                               (time-convert (file-attribute-modification-time
                                              (file-attributes contents-file))
                                             'integer)
                             0)))
                       package-archives)
               (list 0)))))
  (when (and
         (not (member "--exordium-inhibit-package-refresh-contents"
                      command-line-args))
         (or
          (member "--exordium-force-package-refresh-contents"
                  command-line-args)
          ;; No archive contents, likely a first Exordium start refresh
          ;; package contents to allow for packages installation
          (null package-archive-contents)
          ;; The newest archive contents is older than last Exordium commit:
          ;; refresh package contents to allow installation of possible new
          ;; packages
          (< last-archive-contents last-exordium-commit)))
    (package-refresh-contents)))

;; If there's a new use-package to upgrade it needs to happen before any other
;; work.  Otherwise the new version is not visible when something tries to pull
;; it in.
(when-let* ((package-pinned-packages '(use-package . "gnu"))
            (package-install-upgrade-built-in t)
            (desc (cl-find-if
                   (lambda (desc) (equal "gnu" (package-desc-archive desc)))
                   (alist-get 'use-package package-archive-contents)))
            (archive-version (package-desc-version desc))
            ((not (package-installed-p 'use-package archive-version))))
  (package-install 'use-package))

;; This is only needed once, near the top of the file
(require 'use-package)

(eval-and-compile
  (load (file-name-concat (locate-user-emacs-file "modules") "init-require")))
(exordium-require 'init-force-elpa)
(exordium-require 'init-vc-checkout)

;; Make sure summaries are generated for `package-vc-install' (and co.)
;; functions, see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=76065
(defun exordium--package-vc-erase-summary (args)
  "Set summary slot in (car ARGS) to nil."
  (when-let* ((pkg-desc (car args))
              ((package-desc-p pkg-desc))
              ((equal package--default-summary (oref pkg-desc summary))))
    (oset pkg-desc summary nil))
  args)

(when (and (require 'package-vc nil t) ;; Since Emacs-29
           (version< emacs-version "31")) ;; Until Emacs-31
  (advice-add 'package-vc--generate-description-file
              :filter-args #'exordium--package-vc-erase-summary))

;; Pin user extra packages early, in case they are dependencies of some other
;; packages that are installed early.
(dolist (pkg exordium-extra-pinned)
  (use-package-pin-package (car pkg) (cdr pkg)))

(use-package use-package
  :exordium-force-elpa gnu
  :custom
  (use-package-always-ensure t)
  (use-package-compute-statistics t))

;; Some packages (i.e., magit, forge) require relatively new package `seq'.
;; Unfortunately, `package' is unable to bump the built-in `seq'.  Ensure it is
;; installed in the newest available version.
(use-package seq
  :defer t
  :exordium-force-elpa gnu)

;; `org' may be upgraded from ELPA (for example, as a part of a first start)
;; and some packages depend on it.  To prevent loading a built in version by
;; such packages make sure `org' has been upgraded early.
(use-package org
  :defer t
  :exordium-force-elpa gnu)

(use-package diminish
  :exordium-force-elpa gnu)

(use-package bind-key
  :exordium-force-elpa gnu)

(dolist (pkg (append
              exordium-extra-packages
              (mapcar #'car exordium-extra-pinned)))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Byte recompile modules, if necessary

(require 'bytecomp)
(defun exordium-recompile-modules ()
  "Recompile modules.
For which the .elc is older than the .el, if the .elc exists.
Also discard .elc without corresponding .el."
  (interactive)
  (dolist (dir (list exordium-modules-dir
                     exordium-themes-dir
                     exordium-extensions-dir
                     exordium-local-dir))
    (when (file-directory-p dir)
      ;; Recompile
      (dolist (el (directory-files dir t "\\.el\\'"))
        (let ((elc (byte-compile-dest-file el)))
          (when (and (file-exists-p elc)
                     (file-newer-than-file-p el elc))
            (byte-compile-file el))))
      ;; Discard .elc singletons
      (dolist (elc (directory-files dir t "\\.elc\\'"))
        (let ((el (concat (concat (file-name-sans-extension elc) ".el"))))
          (unless (file-exists-p el)
            (warn "Removing singleton .elc file: %s" elc)
            (delete-file elc)))))))
(exordium-recompile-modules)


;; Turn asynchronous byte compilation on

(use-package async
  :functions (exordium--async-package-reload-previously-loaded)
  :init
  (defun exordium--async-package-reload-previously-loaded (&rest args)
    "Reload a previously loaded package when descriptor exists in car ARGS.
This is to mimic what `package-unpack' does: it reloads package
after it's been byte compiled."
    (when-let* ((desc (package-load-descriptor (car args))))
      (cond
       ((fboundp 'package--reload-previously-loaded) ;; Since Emacs-29
        (package--reload-previously-loaded desc))
       ((fboundp 'package--load-files-for-activation) ;; Until Emacs-28
        (package--load-files-for-activation desc :reload)))))
  :config
  (advice-add 'async-bytecomp--file-to-comp-buffer
              :after #'exordium--async-package-reload-previously-loaded)
  (async-bytecomp-package-mode))


;;; Load Modules

(exordium-require 'init-prefs)            ; Defines variables that prefs.el can override
(exordium-require 'init-lib)              ; Utility functions - load this first
(exordium-require 'init-environment)      ; environment variables


(dolist (tapped-file exordium-tapped-prefs-files)
  (message "Loading tapped prefs file: %s" tapped-file)
  (load (file-name-sans-extension tapped-file)))

;; Themes
;; Note: use "export TERM=xterm-256color" for emacs -nw
(setq custom-theme-directory exordium-themes-dir)
(exordium-require 'init-progress-bar)

(when exordium-nw
  (set-face-background 'highlight nil))
(when exordium-theme
  (exordium-require 'init-themes))

;; Look and feel
(exordium-require 'init-look-and-feel)     ; fonts, UI, keybindings, saving files etc.
(exordium-require 'init-font-lock)         ; enables/disables font-lock globally.
(exordium-require 'init-linum)             ; line numbers
(when exordium-smooth-scroll
  (exordium-require 'init-smooth-scroll)
  (smooth-scroll-mode 1))                  ; smooth scroll

(update-progress-bar)

;; Usability
(exordium-require 'init-window-manager)   ; navigate between windows
(exordium-require 'init-util)             ; utilities like match paren, bookmarks...
(unless exordium-helm-everywhere
  (exordium-require 'init-ido))           ; supercharged completion engine
(exordium-require 'init-highlight)        ; highlighting current line, symbol under point

(pcase exordium-complete-mode
  (:auto-complete
   (exordium-require 'init-autocomplete))
  (:company
   (exordium-require 'init-company)))     ; completion

(exordium-require 'init-helm)             ; setup helm
(when exordium-projectile
  (exordium-require 'init-projectile))
(when (and exordium-projectile exordium-helm-projectile)
  (exordium-require 'init-helm-projectile))

(when exordium-help-extensions
  (exordium-require 'init-help))           ; extra help

(update-progress-bar)

(exordium-require 'init-dired)            ; enable dired+ and wdired permission editing
(exordium-require 'init-git)              ; Magit and git gutter
(exordium-require 'init-git-visit-diffs)  ; visit diffs in successive narrowed buffers
(when (version< "29" emacs-version)
  (exordium-require 'init-forge))         ; Forge

(exordium-require 'init-flb-mode)         ; frame-local buffers

(update-progress-bar)

;; Prog mode
(exordium-require 'init-prog-mode)

;; Shell mode
(exordium-require 'init-shell)

;; Major modes
(exordium-require 'init-markdown)
(exordium-require 'init-org)
(exordium-require 'init-xml)

;; OS-specific things
(when exordium-osx
  (exordium-require 'init-osx))

;; C++
(exordium-require 'init-cpp)
(exordium-require 'init-bde-style)
(when exordium-yasnippet
  (exordium-require 'init-yasnippet))
(exordium-require 'init-gdb)

(update-progress-bar)

;; JS
(exordium-require 'init-javascript)

;; Python
(exordium-require 'init-python)

;; Ruby
(exordium-require 'init-ruby)

;; Lisp
(exordium-require 'init-elisp)

;; Clojure
(when exordium-clojure
  (exordium-require 'init-clojure))

;; Groovy
(exordium-require 'init-groovy)

;; include-what-you-use
(exordium-require 'init-iwyu)

(update-progress-bar)

(when (and exordium-theme exordium-enable-powerline)
  (exordium-require 'init-powerline))

;; Docker
(exordium-require 'init-docker)

;; Flycheck
(exordium-require 'init-flycheck)

;; Treesit
(when exordium-treesit-modes-enable
  (exordium-require 'init-treesit))

;; LSP
(when exordium-lsp-mode-enable
  (exordium-require 'init-lsp))

;; Desktop - close to the end so customisations had a chance to kick in
(when exordium-desktop
  (exordium-require 'init-desktop))

;; Configure `gcmh' since it is only loaded via `after-init-hook' and some
;; buffers restored in init-desktop may require `gcmh-high-cons-threshold' to
;; be bound.
(exordium-require 'init-gcmh)

;; Local extensions
(dolist (tapped-file exordium-tapped-after-init-files)
  (message "Loading tapped after-init file: %s" tapped-file)
  (load (file-name-sans-extension tapped-file)))

(update-progress-bar)

;; Greetings
(setq initial-scratch-message
      (let ((current-user (split-string (user-full-name) " ")))
        (format ";; -*- lexical-binding: t -*-

;; Happy hacking %s!

" (if current-user (car current-user) exordium-current-user))))


(provide 'init)

;;; init.el ends here
