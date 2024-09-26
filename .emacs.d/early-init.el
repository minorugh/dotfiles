;;; early-init.el --- Emacs early init configurations.
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

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq-default mode-line-format nil)

;; Default frame settings.
(push '(fullscreen . maximized) default-frame-alist)

;; If there is no elpa directory nothing suppresses
(when (file-directory-p "~/.emacs.d/elpa/")
  ;; Emacs really shouldn't be displaying anything until it has fully started.
  ;; This saves a bit of time.
  (setq inhibit-redisplay t)
  (setq inhibit-message t)
  (setq inhibit-splash-screen t)
  (custom-set-faces '(default ((t (:background "#282a36")))))
  (add-hook 'window-setup-hook
	    (lambda ()
	      (setq inhibit-redisplay nil)
	      (setq inhibit-message nil)
	      (redisplay))))

;; Write any customizations to a temp file so they are discarded.
(setq custom-file "~/.emacs.d/tmp/custom.el")


(provide 'early-init)
;;; early-init.el ends here
