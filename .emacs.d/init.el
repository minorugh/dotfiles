;;; init.el --- Emacs first Configuration. -*- lexical-binding: t -*-
;;
;; Compatible with Emacs 27 and later
;; Aiming for a fancy and fast Emacs configuration
;;
;;; Code:
;; (setq debug-on-error t)

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
                       ("org"   . "https://orgmode.org/Elpa/")))

  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :doc "Use leaf as a package manager"
    :url "https://github.com/conao3/leaf.e"
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf el-get
      :ensure t
      :custom (el-get-git-shallow-clone . t))
    :config
    (setq byte-compile-warnings '(not cl-functions obsolete))
    (leaf-keywords-init)))


;;  Auto byte compile
(defun auto-compile-inits ()
  "Byte compile Lisp files modified in the directory."
  (interactive)
  (byte-compile-file "~/.emacs.d/init.el")
  (byte-recompile-directory (expand-file-name "~/.emacs.d/elisp") 0)
  (byte-recompile-directory (expand-file-name "~/.emacs.d/inits") 0))
(add-hook 'kill-emacs-hook 'auto-compile-inits)


;; Load configuration files
(leaf *load-usr-define-function
  :init
  (setq load-path (cons "~/.emacs.d/elisp/" load-path))
  :config
  (require 'my:dired)
  (require 'my:template))

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
