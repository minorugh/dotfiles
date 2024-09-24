;;; early-init.el --- Emacs early init -*- lexical-binding: t; -*-
;;; Commentary:

;; Emacs 27 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens.

;;; Code:
;; (setq debug-on-error t)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent unwanted runtime compilation for native-comp users
(setq native-comp-jit-compilation nil)

;; For slightly faster startup
(setq package-enable-at-startup nil)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Set language & System encoding
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; Font
(if (string-match "P1" (shell-command-to-string "uname -n"))
    (add-to-list 'default-frame-alist '(font . "Cica-20"))
  (add-to-list 'default-frame-alist '(font . "Cica-18")))

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)

;; Disable warnings at initialization
(eval-and-compile
  (setq warning-minimum-level :emergency))

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Default frame settings.
(push '(fullscreen . maximized) default-frame-alist)

;; Prevent flashing of unstyled modeline and headerline at startup
(setq-default mode-line-format nil)
(setq-default header-line-format nil)

;; Do not display anything until Emacs is fully started
;; If there is no elpa directory nothing suppresses
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
