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


;; Package initialize occurs automatically, before `user-init-file' is loaded,
;; but after `early-init-file'. We handle package initialization,
;; so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)


;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)


;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)


;; Faster to disable these here (before they've been initialized)
(push '(fullscreen . maximized) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(set-fringe-mode '(0 . 1))

;; Prevent flashing of unstyled modeline at startup
(setq-default mode-line-format nil)


;; Suppress flashing at startup
(when (file-directory-p "~/.emacs.d/elpa/")
  (setq inhibit-message t)
  (add-hook 'window-setup-hook
			(lambda ()
			  (setq inhibit-redisplay nil)
			  (setq inhibit-message nil)
			  (redisplay)))
  (custom-set-faces
   '(default ((t (:background "#282a36"))))))


;; Encoding
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)


;; Fonts
(if (string-match "e590" (shell-command-to-string "uname -n"))
	(add-to-list 'default-frame-alist '(font . "Cica-21"))
  (add-to-list 'default-frame-alist '(font . "Cica-15")))


(provide 'early-init)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; early-init.el ends here
