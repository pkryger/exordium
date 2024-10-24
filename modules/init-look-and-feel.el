;;; init-look-and-feel.el --- Look and feel -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Keyboard preferences: remaps existing functions to new keys
;;
;; ----------------- ---------------------------------------------------------
;; Key               Definition
;; ----------------- ---------------------------------------------------------
;; ESC               Quit (= Ctrl-G)
;; M-g               Goto line
;; C-z               Undo
;; C-`               Kill current buffer (= C-x k)
;;
;; RETURN            Return or Return + indent, depending on init-prefs
;; S-RETURN          The opposite
;;
;; M-C-l             Switch to last buffer
;; C-x C-b           Buffer menu with `ibuffer', replacing `list-buffers'
;; C- +/-            Zoom

;; C-=               Expand region by semantic units
;; M-C-=             Contract region by semantic units
;;
;; M-<up>            Move selected region up
;; M-<down>          Move selected region down
;;
;; F10               Speedbar
;; ----------------- ---------------------------------------------------------

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

(require 'cl-lib)

;;; Font

(defun exordium-available-preferred-fonts ()
  "Trim the unavailable fonts from the preferred font list."
  (cl-remove-if-not (lambda (font-and-size)
                      (member (car font-and-size) (font-family-list)))
                    exordium-preferred-fonts))

(defun exordium-font-size ()
  "Find the available preferred font size."
  (when (exordium-available-preferred-fonts)
    (cdar (exordium-available-preferred-fonts))))

(defun exordium-font-name ()
  "Find the avaliable preferred font name."
  (when (exordium-available-preferred-fonts)
    (caar (exordium-available-preferred-fonts))))

(defun exordium-set-font (&optional font size)
  "Find the preferred fonts that are available and choose the first one.
Set FONT and SIZE if they are passed as arguments."
  (interactive
   (list (completing-read (format "Font (default %s): " (exordium-font-name))
                          (exordium-available-preferred-fonts) nil nil nil nil
                          (exordium-font-name))
         (read-number "Size: " (exordium-font-size))))
  (let ((font (or font (exordium-font-name)))
        (size (or size (exordium-font-size))))
    (when (and font size)
      (message "Setting font family: %s, height: %s" font size)
      (set-face-attribute 'default nil
                          :family font
                          :height size
                          :weight 'normal)
      t))) ;; indicate that the font has been set

(when exordium-preferred-fonts
  (exordium-set-font))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'exordium-set-font))

;;; User interface

;;; Default frame size
(when (and exordium-preferred-frame-width
           exordium-preferred-frame-height)
  (setq default-frame-alist `((width  . ,exordium-preferred-frame-width)
                              (height . ,exordium-preferred-frame-height))))

;;; Remove the toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;;; Only show the menu bar in a graphical window
;;; (we don't want to loose that top line in a tty)
(menu-bar-mode (if (null (window-system)) -1 1))

;;; Remove welcome message
(setq inhibit-startup-message t)

;;; Disable blinking cursor
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;;; Display column number in the modebar
(column-number-mode 1)

;;; Smooth scrolling
(setq scroll-step 1)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t)

;;; Scrollbar
(when (fboundp 'set-scroll-bar-mode)
  (if exordium-scroll-bar
      (set-scroll-bar-mode `right)
    (set-scroll-bar-mode nil)))

;;; Better frame title with buffer name
(setq frame-title-format (concat "%b - emacs@" (system-name)))

;;; Disable beep
;;(setq visual-bell t)

;;; Colorize selection
(transient-mark-mode 'on)

;;; Show matching parentheses
(show-paren-mode t)

;;; Mouse selection
(use-package select
  :ensure nil
  :custom
  (select-enable-clipboard t))

;;; http://www.reddit.com/r/emacs/comments/30g5wo/the_kill_ring_and_the_clipboard/
(setq save-interprogram-paste-before-kill t)

;;; Electric pair: automatically close parenthesis, curly brace etc.
;;; `electric-pair-open-newline-between-pairs'.
(when exordium-enable-electric-pair-mode
  (setq electric-pair-open-newline-between-pairs t)
  (electric-pair-mode))

;;; Indent with spaces, not tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;; Autofill at 79 characters
(setq-default fill-column 79)

;;; Wordwrap at word boundadies
;;;(global-visual-line-mode 1)

;; Show only 1 window on startup (useful if you open multiple files)
(add-hook 'emacs-startup-hook #'delete-other-windows t)


;;; Keyboard preferences

;; Use ESC as Control-G
(when exordium-keyboard-escape
  (bind-key "ESC" #'keyboard-quit))

;;; Use "y or n" answers instead of full words "yes or no"
(when exordium-enable-y-or-n
  (fset 'yes-or-no-p 'y-or-n-p))

;;; Delete selection when typing
(delete-selection-mode t)

;;; Let me scroll the buffer while searching, without exiting the search.
;;; This allows for using C-l during isearch.
(when (boundp 'isearch-allow-scroll)
  (setq isearch-allow-scroll t))

;;; Evil-mode
(if exordium-enable-evil-mode
    (use-package evil
      :commands (evil-mode)
      :config
      (evil-mode))
  ;; Evil mode depends in undo-tree, which thinks it should work by default
  (when (fboundp 'global-undo-tree-mode)
    (global-undo-tree-mode -1)))

(defun insert-gui-primary-selection ()
  "If no region is selected, insert current gui selection at point."
  (interactive)
  (when (not (use-region-p))
    (let ((text (gui-get-selection)))
      (when text
        (push-mark (point))
        (insert-for-yank text)))))

(when exordium-enable-insert-gui-primary-selection
  (bind-key "M-<insert>" #'insert-gui-primary-selection))


;;; Shortcut keys

(bind-key "M-g" #'goto-line)
(when exordium-keyboard-ctrl-z-undo
  (bind-key "C-z" #'undo))
(bind-key "C-`" #'kill-this-buffer)

;;; Meta-Control-L = switch to last buffer
(defun switch-to-other-buffer ()
  "Alternates between the two most recent buffers."
  (interactive)
  (switch-to-buffer (other-buffer)))

(bind-key "M-C-l" #'switch-to-other-buffer)

;;; C-x C-b = ibuffer (better than list-buffers)
(bind-key "C-x C-b" #'ibuffer)

;;; Zoom
(use-package default-text-scale
  :bind
  ("C-+" . #'default-text-scale-increase)
  ("C--" . #'default-text-scale-decrease)
  ("C-<mouse-4>" . #'default-text-scale-increase)
  ("C-<mouse-5>" . #'default-text-scale-decrease))

;;; CUA.
;;; CUA makes C-x, C-c and C-v cut/copy/paste when a region is selected.
;;; Adding shift or doubling the Ctrl-* makes it switch back to Emacs keys.
;;; It also has a nice feature: C-RET for selecting rectangular regions.
;;; If exordium-enable-cua-mode is nil, only the rectangular regions are enabled.
(cond ((eq exordium-enable-cua-mode :region)
       (cua-selection-mode t))
      (exordium-enable-cua-mode
       (cua-mode t)))


;;; Cool extensions

;;; Expand region
(use-package expand-region
  :bind
  (("C-=" . #'er/expand-region)
   ("C-M-=" . #'er/contract-region)))

;;; Move regions up and down (from https://www.emacswiki.org/emacs/MoveRegion)
(defun move-region (start end n)
  "Move the current region from START to END up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current region from START to END up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current region from START to END down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(bind-key "M-<up>" #'move-region-up)
(bind-key "M-<down>" #'move-region-down)


;;; File saving and opening

;; Warn when opening files bigger than 100MB (use nil to disable it entirely)
(setq large-file-warning-threshold 100000000)

;; Propose vlf (Very Large File) as a choice when opening large files
;; (otherwise one can open a file using M-x vlf):
(use-package vlf-setup
  :ensure vlf
  :defer t)

(require 'rx)

(defun exordium--alist-get-derived-mode (alist)
  "Return value from ALIST where key matches `derived-mode-p' of current buffer."
  (alist-get nil alist nil nil
             (lambda (elt _)
               (apply #'derived-mode-p
                      (if (listp elt) elt (list elt))))))

(defun exordium--delete-trailing-whitespace-data-start-and-cleanup ()
  "Find beginning of data section start.
Also cleanup trailing lines when `delete-trailing-lines' is
non-nil and insert new line when `require-final-newline' is
non-nil."
  (save-excursion
    (when-let*
        ((exordium-delete-trailing-whitespace-skip-data)
         (data-keywords (exordium--alist-get-derived-mode
                         exordium-delete-trailing-whitespace-data-keywords)))
      (when (re-search-forward
             (rx-to-string
              `(seq line-start
                    (or ,@data-keywords)
                    (zero-or-more whitespace)
                    line-end)
              t)
             nil t)
        (let ((point (point)))
          ;; When this is not a perlpod, clean it too - return nil
          (unless (when-let*
                      ((perlpod-keywords
                        (exordium--alist-get-derived-mode
                         exordium-delete-trailing-whitespace-data-perlpod-keywords)))
                    (re-search-forward
                     (rx-to-string
                      `(seq "\n" (zero-or-more whitespace)
                            "\n=" (or ,@perlpod-keywords))
                      t)
                     nil t))
            ;; Otherwise do a minimal cleanup:
            ;; Delete trailing lines, as `delete-trailing-whitespace' won't
            ;; handle it
            (when-let*
                ((delete-trailing-lines)
                 ((re-search-forward
                   (rx line-start
                       (group (one-or-more (or whitespace "\n")))
                       string-end)
                   nil t))
                 (b (match-beginning 1))
                 (e (match-end 1))
                 ((region-modifiable-p b e)))
              (delete-region b e))
            ;; Ensure trailing new line is present, as
            ;; `delete-trailing-whitespace' won't handle it
            (when (and require-final-newline
                       (not (re-search-forward (rx "\n" string-end) nil t)))
              (goto-char (point-max))
              (insert "\n"))
            ;; trim whitespace only up to data section
            point))))))

(defun exordium-delete-trailing-whitespace-in-buffer (&optional start end)
  "Delete trailing whitespace in current buffer.
Like `delete-trailing-whitespace', but any restrictions are
ignored.

Additionally, when called with a nil END and
`exordium-delete-trailing-whitespace-skip-data' is non-nil it
will not affect data sections in Ruby and Perl modes (with an
exception of a perlpod).  When both END and
`exordium-delete-trailing-whitespace-skip-data' are nil it will
delete whitespace in the whole buffer.  When called with a
non-nil END it will delete trailing whitespace up to END.

If called interactively, START and END are the start/end of the
region if the mark is active, or the whole buffer, up to data
secion (when `exordium-delete-trailing-whitespace-skip-data' is
non-nil) if the mark is inactive.

The function doesn't delete trailing whitespaces when buffer is
in any of `exordium-delete-trailing-whitespace-inhibit-modes',
and the function has been called without a prefix argument.

Depending on mode a data section starts after a line with a one
of keywords __END__ or __DATA__ or a one of control
characters (^D, ^Z) and ends at the end of file.  Such a
preservation of whitespaces may be important for some types of
data (i.e., patches) but should be to safe to clear it from
others other (i.e., perlpod).  See
https://perldoc.perl.org/perldata,
https://perldoc.perl.org/perlpodspec, and
https://docs.ruby-lang.org/en/3.3/Object.html for more details."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list nil nil))))
  (if (and (not current-prefix-arg)
           (apply #'derived-mode-p
                  exordium-delete-trailing-whitespace-inhibit-modes))
      (when (called-interactively-p 'any)
        (user-error "Not deleting trailing whitespaces in '%s', call with prefix to override"
                    major-mode))
    (without-restriction
      (save-mark-and-excursion
        (save-match-data
          (goto-char (point-min))
          (when-let*
              (((looking-at
                 (rx string-start
                     (group (zero-or-more (or whitespace "\n"))
                            (or "\n" line-end)))))
               (b (match-beginning 1))
               (e (match-end 1))
               ((region-modifiable-p b e)))
            (delete-region b e))

          (let ((end
                 (or end
                     (exordium--delete-trailing-whitespace-data-start-and-cleanup))))
            (delete-trailing-whitespace start end)))))))

;; Remove trailing blanks on save
(define-minor-mode delete-trailing-whitespace-mode
  "Remove trailing whitespace upon saving a buffer.
See `exordium-delete-trailing-whitespace-in-buffer' for more details."
  :lighter nil
  (if delete-trailing-whitespace-mode
      (add-hook 'before-save-hook
                #'exordium-delete-trailing-whitespace-in-buffer nil t)
    (remove-hook 'before-save-hook
                 #'exordium-delete-trailing-whitespace-in-buffer t)))

(define-globalized-minor-mode global-delete-trailing-whitespace-mode
  delete-trailing-whitespace-mode
  (lambda ()
    (delete-trailing-whitespace-mode t))
  :group 'exordium)

(when exordium-delete-trailing-whitespace
  (global-delete-trailing-whitespace-mode t))

;;; Disable backup files (e.g. file~)
(defun no-backup-files ()
  "Disable creation of backup files."
  (interactive)
  (setq make-backup-files nil))

(unless exordium-backup-files
  (no-backup-files))


(provide 'init-look-and-feel)

;;; init-look-and-feel.el ends here
