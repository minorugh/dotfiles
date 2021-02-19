;;; early-init.el --- Early initialization. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;;; Code:

(setq gc-cons-threshold most-positive-fixnum)


(push '(fullscreen . maximized) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)


(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq frame-inhibit-implied-resize t)


(customize-set-variable
 'package-archives '(("org"   . "https://orgmode.org/elpa/")
					 ("melpa" . "https://melpa.org/packages/")
 					 ("gnu"   . "https://elpa.gnu.org/packages/")))
(setq package-enable-at-startup nil)


(setq load-prefer-newer t)
(setq custom-file (locate-user-emacs-file "~/.emacs.d/tmp/custom.el"))
(setq byte-compile-warnings '(cl-functions))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
