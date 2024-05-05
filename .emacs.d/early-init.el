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

;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(setq native-comp-deferred-compilation nil ;; obsolete since 29.1
      native-comp-jit-compilation nil)

;; For slightly faster startup
(setq package-enable-at-startup nil)

;; Always load newest byte code
(setq load-prefer-newer t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Disable warnings at initialization
(setq warning-minimum-level :emergency)

;; Faster to disable these here (before they've been initialized)
(push '(fullscreen . maximized) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(set-fringe-mode '(0 . 1))

;; Prevent flashing of unstyled modeline and headerline at startup
(setq-default mode-line-format nil)
(setq-default header-line-format nil)

;; No startup screen appears
(setq inhibit-splash-screen t)

;; Language & encode
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; Suppress flashing at startup
(when (file-directory-p "~/.emacs.d/elpa/")
  (setq inhibit-message t)
  (add-hook 'window-setup-hook
	    (lambda ()
	      (setq inhibit-redisplay nil)
	      (setq inhibit-message nil)
	      (redisplay)))
  (custom-set-faces '(default ((t (:background "#282a36"))))))


(defun emacs-init-time ()
  "Overwrite `emacs-init-time'."
  (interactive)
  (let ((str
	 (format "%.1f seconds"
		 (float-time
		  (time-subtract after-init-time before-init-time)))))
    (if (called-interactively-p 'interactive)
	(message "%s" str)
      str)))


(provide 'early-init)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; early-init.el ends here
