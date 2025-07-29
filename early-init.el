;;; early-init.el --- Exordium early-init.el -*- lexical-binding: t -*-


;;; Commentary:
;;

;;; Code:


(setq
 ;; Will restore `gc-cons-threshold' via package gcmh.
 gc-cons-threshold (* 256 1024 1024)
 ;; see https://github.com/jdtsmith/emacs-mac/issues/66
 frame-resize-pixelwise t)

;; TODO: `(window-system)' is nil in early-init when running on GUI

(provide 'early-init)

;;; early-init.el ends here
