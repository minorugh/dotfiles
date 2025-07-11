;;; init.el --- User initialization.  -*- lexical-binding: t -*-
;;; Commentary:
;; URL: https://github.com/minorugh/dotfiles
;;; Code:
;; Package-Requires: ((emacs "29.1"))
;; (setq debug-on-error t)

(when (version< emacs-version "29.1")
  (error "This requires Emacs 29.1 and above!"))

;; Speed up startup
(setq gc-cons-threshold most-positive-fixnum)

(defconst default-hadlers file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
	      (lambda ()
	        "Recover file name handlers and GC values after startup."
	        (setq file-name-handler-alist default-hadlers)
	        (setq gc-cons-threshold 800000)
	        (setq inhibit-message nil)))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
		       ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  (use-package leaf :ensure t)

  (leaf leaf-keywords :ensure t
    :init
    (leaf hydra :ensure t)
    :config
    (leaf-keywords-init)))

(leaf server
  :commands server-running-p
  :hook (emacs-startup-hook
	 . (lambda ()
	     (unless (server-running-p)
	       (server-start)))))

(leaf exec-path-from-shell :ensure t
  :when (memq window-system '(mac ns x))
  :hook (emacs-startup-hook . exec-path-from-shell-initialize))

(leaf init-loader :ensure t
  :doc "Loading inits configuration"
  :load-path "~/.emacs.d/elisp"
  :require my:dired my:template my:compile my:evil-hugo
  :config
  (custom-set-variables
   '(init-loader-show-log-after-init 'error-only))
  (init-loader-load)
  :init
  (setq custom-file (locate-user-emacs-file "~/.emacs.d/tmp/custom.el")))

(leaf *auto-byte-compile
  :doc "Byte compilation is performed when Emacs exits."
  :hook (kill-emacs-hook . auto-compile-inits)
  :init
  (defun auto-compile-inits ()
    "Byte-compilation of all initial configuration files."
    (interactive)
    (byte-compile-file "~/.emacs.d/early-init.el")
    (byte-compile-file "~/.emacs.d/init.el")
    (byte-recompile-directory (expand-file-name "~/.emacs.d/elisp") 0)
    (byte-recompile-directory (expand-file-name "~/.emacs.d/inits") 0)))

(provide 'init)
;;; init.el ends here
