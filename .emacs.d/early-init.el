;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;;
;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Package initialization is automatic,
;; but prevent Emacs from doing that early.
(setq package-enable-at-startup nil)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(push '(fullscreen . maximized) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq byte-compile-warnings '(cl-functions))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
