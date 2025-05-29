;;; early-init.el --- Emacs early init configurations.  -*- no-byte-compile: t; -*-
;;; Commentary:

;; Emacs 27 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens.

;;; Code:
;; (setq debug-on-error t)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(setq native-comp-jit-compilation nil)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Set language & System encoding
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; Font
(add-to-list 'default-frame-alist '(font . "Cica-18"))

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)
(setq inhibit-startup-message t)

;; Suppress flashing at startup
(when (file-directory-p "~/.emacs.d/elpa/")
  (add-hook 'window-setup-hook
   	    (lambda ()
   	      (setq inhibit-redisplay nil)
   	      (setq inhibit-message nil)
   	      (redisplay)))
  (setq inhibit-redisplay t)
  (setq inhibit-message t)
  (setq inhibit-splash-screen t)
  (custom-set-faces '(default ((t (:background "#282a36"))))))


(provide 'early-init)
;;; early-init.el ends here
