;;; init-markdown.el ---  Markdown http://jblevins.org/projects/markdown-mode/ -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module provide a minor mode "impatient markdown mode", which similar to
;; impatient-mode but for markdown buffers as opposed to HTML buffers.  It is
;; actually implemented with the impatient-mode itself.
;;
;; When you type command `impatient-markdown-mode' in a markdown buffer, Emacs
;; starts an embedded HTTP server listening to port 8080 (by default), and it
;; will direct your favorite web browser to URL
;; "http://localhost:8080/imp/live/<buffer-name.md>"
;;
;; Any change you make in the buffer from that point is automatically rendered
;; in real-time in the web browser.  To stop the HTTP server, run
;; `impatient-markdown-mode' again.  Note that Emacs will complain if you quit
;; before stopping the server.
;;
;; Before you can use it, you need to set the variable `markdown-command' to
;; the command to execute to render a markdown file into HTML.  To use the
;; GitHub command, clone https://github.com/github/markup and set
;; `markdown-command' to the path of bin/github-markup in your after-init.el.
;; Other options include Pandoc or RedCarpet.
;;
;; Note: you can change the variable `httpd-port' if 8080 does not work in your
;; environment.  Also the current implementation uses a temporary file whose
;; path is defined in `exordium-markdown-file' which can also be changed.

;;; Code:
(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)
(exordium-require 'init-lib)
(exordium-require 'init-window-manager)


(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :autoload (markdown-inline-code-at-pos)
  :functions (exordium--markdown-nobreak-inline-code-at-point-p
              exordium--markdown-adjust-for-gh-render)
  :init
  (defun exordium--markdown-nobreak-inline-code-at-point-p ()
    "Retrun non-nil when point is in inline code.
The non-nil value is only possible when buffer matches
`exordium-markdown-nobreak-inline-code-predicate'."
    (when-let* ((point (point))
                ((buffer-match-p exordium-markdown-nobreak-inline-code-predicate
                                 (current-buffer)
                                 point)))
      (save-match-data
        (when-let* (((markdown-inline-code-at-pos point (pos-bol))))
          (< (max (point-min)
                  (1- (match-beginning 0)))
             point
             (match-end 0))))))

  (defun exordium--markdown-add-nobreak-inline-code-hook ()
    "Add no break inline code local hook to `fill-nobreak-predicate'."
    (add-hook 'fill-nobreak-predicate
              #'exordium--markdown-nobreak-inline-code-at-point-p nil t))

  (defun exordium--markdown-adjust-for-gh-render (orig-fun linebeg)
    "Adjust position of `current-column' by invisible parts of links in the line.
Use LINEBEG to determine where to look for links.  When the adjusted
position is in current paragraph call ORIG-FUN.  This only happens when
buffer matchse `exordium-markdown-gh-render-links-predicate'."
    (if (buffer-match-p exordium-markdown-gh-render-links-predicate
                        (current-buffer)
                        linebeg)
        (let* ((current-column-pos (point))
               (regex-link (rx-to-string
                            `(or (regexp ,markdown-regex-link-inline)
                                 (regexp ,markdown-regex-link-reference))))
               (eop (save-excursion
                      (forward-paragraph 1)
                      (point)))
               ;; When the line doesn't begin in a middle of a link text and
               ;; that the link text is not overflowing to a next line, then
               ;; add the invisible characters (ignoring leading exclamation
               ;; point and opening square bracket that are in the previous
               ;; line).
               (invisible-length (save-excursion
                                   (goto-char linebeg)
                                   (if (and (re-search-backward
                                             (rx "[")
                                             (save-excursion
                                               (forward-paragraph -1)
                                               (point))
                                             t)
                                            (looking-at regex-link)
                                            (< linebeg
                                               (match-beginning 4) ;; closing square bracket for the link text
                                               current-column-pos))
                                       (- (or (match-end 8) ;; closing parenthesis
                                              (match-end 7)) ;; closing square bracket for the reference label
                                          (match-beginning 4)) ;; closing square bracket for the link text
                                     0))))
          (goto-char linebeg)
          ;; Search for all links in the paragraph if the point is before
          ;; adjusted fill column.
          (while (and (< (point) (+ current-column-pos invisible-length))
                      (re-search-forward regex-link eop t)
                      ;; Abort the search when closing square bracket for link
                      ;; text is behind the adjusted fill column.  In such a
                      ;; case only include leading exclamation point and
                      ;; opening square bracket, but only when they are in the
                      ;; same line.
                      (let ((too-far (< (match-beginning 4) ;; closing square bracket for the link text
                                        (+ current-column-pos invisible-length))))
                        (when (and (not too-far)
                                   (< linebeg (match-end 2))) ;; opening square bracket for the link text
                          (cl-incf invisible-length
                                   (- (match-end 2) ;; opening square bracket for the link text
                                      (or (match-beginning 1) ;; leading exclamation point (optional)
                                          (match-beginning 2))))) ;; opening square bracket for the link text
                        too-far))
            ;; Increase the number of invisible characters by lenght of all
            ;; link parts except for the link text.
            (cl-incf invisible-length
                     (+ (- (match-end 2) ;; opening square bracket for the link text
                           (or (match-beginning 1) ;; leading exclamation point (optional)
                               (match-beginning 2))) ;; opening square bracket for the link text
                        (- (or (match-end 8) ;; closing parenthesis
                               (match-end 7)) ;; closing square bracket for the reference label
                           (match-beginning 4))))) ;; closing square bracket for the link text
          ;; Go to new column only when it is before the end of the
          ;; paragraph. Otherwise go to the end of the paragraph and don't
          ;; call `orig-fun' [sic!] as there's no need to split the line
          ;; anymore.
          (let ((new-column (+ current-column-pos
                               invisible-length
                               (if (zerop invisible-length) 0 1))))
            (if (< eop new-column)
                (goto-char eop)
              (goto-char new-column)
              (funcall orig-fun linebeg))))
      (funcall orig-fun linebeg)))

  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  :hook
  (markdown-mode . exordium-electric-mode-add-back-tick)
  (markdown-mode . exordium--markdown-add-nobreak-inline-code-hook)
  :config
  ;; called from `fill-region-as-paragraph'
  (advice-add #'fill-move-to-break-point
              :around #'exordium--markdown-adjust-for-gh-render)

  ;; TODO: he following feature seems to has changed and a new solution needs
  ;; to be developed.
  ;; Loud face for TODOs in markdown documents
  ;; (when exordium-font-lock
  ;;   (setq markdown-mode-font-lock-keywords-core
  ;;         (list
  ;;          (cons markdown-regex-italic '(2 markdown-italic-face))
  ;;          (cons "\\<\\(TODO\\|FIXME\\|TBD\\):" '(1 font-lock-warning-face)))))
  )

;;; FIXME: quick workaround for a bug in markdown-mode 2.1 (font lock is broken)
(when (and (boundp 'markdown-mode-version)
           (equal markdown-mode-version "2.1"))
  (add-hook 'markdown-mode-hook #'font-lock-mode))



;;; Impatient markdown mode
(use-package impatient-mode
  :defer t)
(use-package simple-httpd
  :defer t
  :autoload (httpd-send-header))

(define-minor-mode impatient-markdown-mode
  "Markdown rendering for people who lack patience."
  :group 'exordium
  :lighter "" ;; impatient-mode already has a modeline marker "imp"
  :keymap nil
  :global nil
  ;; Body
  (cond (impatient-markdown-mode
         (start-imp-markdown))
        (t
         (stop-imp-markdown))))

(defcustom exordium-markdown-file "/tmp/imp-markdown-temp.md"
  "Temporary file for markdown rendering."
  :group 'exordium
  :type  'string)

(defun markdown-to-html (buffer)
  "Render markdown in BUFFER as HTML."
  (let ((md-file exordium-markdown-file))
    (unwind-protect
        (progn
          (with-temp-file md-file
            (kill-region (point-min) (point-max))
            (insert (with-current-buffer buffer (buffer-string))))
          (shell-command-to-string
           (concat markdown-command " " md-file)))
      (delete-file md-file))))

(defun imp-markdown-visit-buffer ()
  "Visit the buffer in a browser."
  (browse-url
   (format "http://localhost:%d/imp/live/%s/"
           httpd-port (url-hexify-string (buffer-name)))))

(defun start-imp-markdown ()
  "Start the impatient mode for markdown and open web browser.
Note that if the web browser wasn't running, Emacs starts it -
you may want to close the browser before Emacs in this
case (Emacs will complain at quit time otherwise)"
  (httpd-start)
  (impatient-mode 1)
  ;; Save the old function
  (unless (fboundp 'imp--send-state-old)
    (defalias 'imp--send-state-old (symbol-function 'imp--send-state)))
  ;; Define a new function
  (defun imp--send-state (proc)
    (let ((id (number-to-string imp-last-state))
          (buffer (current-buffer)))
      (with-temp-buffer
        (insert (markdown-to-html buffer))
        (httpd-send-header proc "text/html" 200
                           :Cache-Control "no-cache"
                           :X-Imp-Count id))))
  (imp-markdown-visit-buffer))

(defun stop-imp-markdown ()
  "Stop the impatient mode for markdown."
  (impatient-mode 0)
  (httpd-stop)
  ;; Restore the old function
  (defalias 'imp--send-state 'imp--send-state-old))

(provide 'init-markdown)

;;; init-markdown.el ends here
