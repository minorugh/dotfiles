;;; init.el --- Emacs first Configuration. -*- lexical-binding: t -*-
;;; Commentary:
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
   'package-archives '(("org" . "https://orgmode.org/elpa/")
					   ("melpa" . "https://melpa.org/packages/")
					   ("gnu" . "https://elpa.gnu.org/packages/")))

  (package-initialize)
  (unless (package-installed-p 'leaf)
	(package-refresh-contents)
	(package-install 'leaf))

  (leaf leaf-keywords
	:ensure t
	:init
	(leaf hydra :ensure t)
	(leaf el-get :ensure t)
	:config
	(leaf-keywords-init)
	(setq custom-file (locate-user-emacs-file "~/.emacs.d/tmp/custom.el"))))

;; Load init files
(leaf init-loader
  :ensure t
  :config
  (custom-set-variables
   '(init-loader-show-log-after-init 'error-only))
  (init-loader-load))


(provide 'init)
;; Local Variables:
;; End:
;;; init.el ends here
