;;; early-init.el --- Early initialization.  -*- lexical-binding: t -*-
;;; Commentary:

;; Emacs 27 introduced early-init.el, which is run before init.el,
;; befor package and UI initialization happens.

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

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
;; Always load newest byte code
(setq load-prefer-newer noninteractive)

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Set language & font
(set-language-environment "Japanese")
(add-to-list 'default-frame-alist '(font . "Cica-18"))

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)
(setq inhibit-startup-message t)

;; If package is already installed, suppress message
;; and set the background color to dark
;; If package is clean install, do not suppress message
(if (file-directory-p "~/.emacs.d/elpa/")
    (progn
      (setq inhibit-message t)
      (custom-set-faces '(default ((t (:background "#282a36"))))))
  (add-hook 'window-setup-hook
	    (lambda ()
	      "Restart Emacs after a clean install of packages."
	      (delete-file "~/.emacs.d/projects")
	      (restart-emacs))))


(provide 'early-init)
;;; early-init.el ends here
