;;; early-init.el --- Early Initialization.
;;; Commentary:
;;
;; Emacs 27+ introduces early-init.el, which is run before init.el,
;; before package and UI initialization.
;;
;;; Code:
;; (setq debug-on-error t)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent emacs from JIT compiling packages
(setq native-comp-jit-compilation nil)

;; For slightly faster startup
(setq package-enable-at-startup nil)

;; Always load newest byte code
(setq load-prefer-newer t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Disable warnings at initialization
(setq warning-minimum-level :emergency)

;; Language & encode
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; Do not use file selection window
(setq use-file-dialog nil)

;; Suppress use of buffer menu
(setq inhibit-startup-buffer-menu t)

;; Faster to disable these here (before they've been initialized)
(push '(fullscreen . maximized) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; Prevent flashing of unstyled modeline and headerline at startup
(setq-default mode-line-format nil)
(setq-default header-line-format nil)
(set-fringe-mode '(0 . 1))

;; No startup screen appears
(setq inhibit-splash-screen t)

;; Emacs really shouldn't be displaying anything
;; until it has fully started up. This saves a bit of time.
(when (file-directory-p "~/.emacs.d/elpa/")
  (add-hook 'window-setup-hook
	    (lambda ()
	      (setq inhibit-redisplay nil)
	      (setq inhibit-message nil)
	      (redisplay)))
  (setq inhibit-message t)
  (custom-set-faces '(default ((t (:background "#282a36"))))))


(provide 'early-init)

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; early-init.el ends here
