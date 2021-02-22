;;; init.el --- Emacs first Configuration. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)

;; Settings for Emacs 26.3
(when (version< emacs-version "27")
  (set-frame-parameter nil 'fullscreen 'maximized)
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (load (concat user-emacs-directory "early-init.el")))

;; Speed up startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(defvar default-gc-cons-threshold gc-cons-threshold)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold (* 1024 1024 100))
(add-hook 'emacs-startup-hook
		  (lambda ()
			"Restore defalut values after startup."
			(setq file-name-handler-alist default-file-name-handler-alist)
			(setq gc-cons-threshold default-gc-cons-threshold)))

;; Package
(customize-set-variable
 'package-archives '(("org"   . "https://orgmode.org/elpa/")
					 ("melpa" . "https://melpa.org/packages/")
 					 ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))

(leaf leaf-keywords
  :ensure t
  :init
  (leaf bind-key :ensure t)
  (leaf hydra :ensure t)
  :config
  (leaf-keywords-init))

;; init loader
(leaf init-loader
  :ensure t
  :config
  (setq custom-file "~/.emacs.d/tmp/custom.el")
  (custom-set-variables '(init-loader-show-log-after-init 'error-only))
  (add-hook 'after-init-hook
			(lambda ()
			  (init-loader-load))))

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
