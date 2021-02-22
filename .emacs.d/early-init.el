;;; early-init.el --- Early Initialization. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;;; Code:

;; Speed up
(defvar default-file-name-handler-alist file-name-handler-alist)
(defvar default-gc-cons-threshold gc-cons-threshold)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum)
(run-with-idle-timer 60.0 t #'garbage-collect)
(add-hook 'emacs-startup-hook
		  (lambda ()
			"Restore defalut values after startup."
			(setq file-name-handler-alist default-file-name-handler-alist)
			(setq gc-cons-threshold default-gc-cons-threshold)))

;; GUI
(push '(fullscreen . maximized) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(setq inhibit-splash-screen t)
(setq inhibit-splash-screen t)
(setq frame-inhibit-implied-resize t)

;; Compile
(setq load-prefer-newer t)
(setq byte-compile-warnings '(cl-functions))

;; Package
(customize-set-variable
 'package-archives '(("org"   . "https://orgmode.org/elpa/")
					 ("melpa" . "https://melpa.org/packages/")
 					 ("gnu"   . "https://elpa.gnu.org/packages/")))
(setq package-enable-at-startup nil)

;; Load user elisp
(add-to-list 'load-path "~/.emacs.d/template")
(require 'my:dired)
(require 'my:template)

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
