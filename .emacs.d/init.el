;;; init.el --- Emacs first Configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:
;; (setq debug-on-error t)

(when (version< emacs-version "27.1")
  (error "This requires Emacs 27.1 and above!"))

;; Speed up startup
(defconst my:file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
			"Recover file name handlers and GC values after startup."
			(setq file-name-handler-alist my:file-name-handler-alist)
			(setq gc-cons-threshold 800000)))

;; Package
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))

  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf el-get
      :ensure t
      :custom (el-get-git-shallow-clone . t))
    :config
    (leaf-keywords-init)))

;;  Auto compile
(defun auto-compile-inits ()
  "Byte compile Lisp files modified in the directory."
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d/elisp") 0)
  (byte-recompile-directory (expand-file-name "~/.emacs.d/inits") 0))
(add-hook 'kill-emacs-hook 'auto-compile-inits)


;; Load user configurations
(setq load-path (cons "~/.emacs.d/elisp/" load-path))
(defun load-user-conf ()
  "Load user configurations."
  (interactive)
  (require 'my:dired)
  (require 'my:template))
(add-hook 'emacs-startup-hook 'load-user-conf)


;; Init loader
(leaf init-loader
  :ensure t
  :config
  (custom-set-variables
   '(init-loader-show-log-after-init 'error-only))
  (init-loader-load)
  (setq custom-file (locate-user-emacs-file "~/.emacs.d/tmp/custom.el")))


(provide 'init)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init.el ends here
