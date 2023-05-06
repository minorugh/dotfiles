;;; early-init.el --- Early Initialization. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs 27+ introduces early-init.el, which is run before init.el,
;; before package and UI initialization.
;;
;;; Code:
;; (setq debug-on-error t)

;; Move garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; To prevent double initialisation
(setq package-enable-at-startup nil)

;; Debugging
;;(setq debug-on-error t)
(setq warning-minimum-level :error)

;; Suppress cl warning
(setq byte-compile-warnings '(cl-functions))

;; Always load newest byte code
(setq load-prefer-newer t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(push '(fullscreen . maximized) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Suppress flashing at startup
(when (file-directory-p "~/.emacs.d/elpa/")
  (setq inhibit-message t)
  (add-hook 'window-setup-hook
			(lambda ()
			  (setq inhibit-redisplay nil)
			  (setq inhibit-message nil)
			  (redisplay)))
  (custom-set-faces '(default ((t (:background "#282a36"))))))


(provide 'early-init)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; early-init.el ends here
