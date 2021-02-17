;;; early-init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;;; Code:

(push '(menu-bar-lines     . nil) default-frame-alist)
(push '(tool-bar-lines     . nil) default-frame-alist)
(push '(scroll-bar-mode    . nil) default-frame-alist)
(push '(blink-cursor-mode  . nil) default-frame-alist)
(push '(column-number-mode . nil) default-frame-alist)
(setq frame-inhibit-implied-resize t)
(setq site-run-file nil)
(setq package-enable-at-startup nil)

(provide 'early-init)

;;; early-init.el ends here
