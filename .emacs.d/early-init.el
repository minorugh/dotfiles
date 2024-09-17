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

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)

;; Prevent flashing of unstyled modeline and headerline at startup
(setq-default mode-line-format nil)
(setq-default header-line-format nil)
(set-fringe-mode '(0 . 1))

;; No startup screen appears
(setq inhibit-splash-screen t)

;; Do not display anything until Emacs is fully started
(when (file-directory-p "~/.emacs.d/elpa/")
  (add-hook 'window-setup-hook
	    (lambda ()
	      (setq inhibit-redisplay nil)
	      (setq inhibit-message nil)
	      (redisplay)))
  (setq inhibit-message t)
  (custom-set-faces '(default ((t (:background "#282a36"))))))

;; Font
(if (string-match "P1" (shell-command-to-string "uname -n"))
    (add-to-list 'default-frame-alist '(font . "Cica-20"))
  (add-to-list 'default-frame-alist '(font . "Cica-18")))

;; Overwrite `emacs-init-time'
(defun emacs-init-time ()
  "Overwrite `emacs-init-time'."
  (interactive)
  (let ((str
	 (format "%.3f seconds"
		 (float-time
		  (time-subtract after-init-time before-init-time)))))
    (if (called-interactively-p 'interactive)
	(message "%s" str)
      str)))

;; Custom files
(setq custom-file "~/.emacs.d/tmp/custom.el")


(provide 'early-init)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; early-init.el ends here
