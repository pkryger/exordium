;;; init-gcmh.el ---  the Garbage Collector Magic Hack -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Garbage collect magic hack from https://gitlab.com/koral/gcmh tuned like in
;; DOOM:
;; https://github.com/hlissner/doom-emacs/blob/db16e5c/core/core.el#L350-L351
;; https://github.com/hlissner/doom-emacs/blob/ed1996e/modules/lang/org/config.el#L548
;; N.B., This file should be loaded after init-desktop.el.  This is because
;; some of buffers restored may require `gcmh-high-cons-threshold' to be bound
;; and that will happen only when `gcmh-mode' is loaded in `after-init-hook'
;; (hooks are executed in reverse order of addition).

;;; Code:

(use-package gcmh
  :diminish
  :init
  ;; From DOOM FAQ:
  ;; https://github.com/hlissner/doom-emacs/blob/64922dd/docs/faq.org#L215-L225
  (defun exodrium--defer-garbage-collection ()
    "Use max value for gc, when in minibuffer.
It's so it won't slow expensive commands and completion frameworks."
    (setf gc-cons-threshold most-positive-fixnum))

  (defun exordium--restore-garbage-collection ()
    "Get back to the original gc threshold.
Defer it so that commands launched immediately after will enjoy the benefits."
    (run-at-time
     1 nil (lambda () (setf gc-cons-threshold
                            (or (bound-and-true-p gcmh-high-cons-threshold)
                                (* 16 1024 1024))))))

  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-high-cons-threshold (* 16 1024 1024))
  :hook
  ;; TODO: this also needs to be after a buffer switch, including current value of `gc-cons-threshold'
  ;; likely in `buffer-list-update-hook'
  (org-mode . (lambda ()
                (setq-local gcmh-high-cons-threshold
                            (* 2 gcmh-high-cons-threshold))))
  (after-init . gcmh-mode)
  (minibuffer-setup . exodrium--defer-garbage-collection)
  (helm-before-initialize . exodrium--defer-garbage-collection)
  (minibuffer-exit . exordium--restore-garbage-collection)
  (helm-cleanup . exordium--restore-garbage-collection))

(provide 'init-gcmh)

;;; init-gcmh.el ends here
