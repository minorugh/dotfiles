;;; init.el --- Emacs first Configuration.
;;; Commentary:
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
                       ("org"   . "https://orgmode.org/elpa/")))

  (if (and (fboundp 'native-comp-available-p)
		   (native-comp-available-p))
	  (setq package-native-compile t))
  (package-initialize)
  (unless (package-installed-p 'leaf)
	(package-refresh-contents)
	(package-install 'leaf))

  (leaf leaf-keywords :ensure t
	:init
	(leaf hydra :ensure t)
	(leaf el-get :ensure t
      :custom (el-get-git-shallow-clone . t))
	:config
	(leaf-keywords-init)))

;; Encode
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; Fonts
(if (string-match "P1" (shell-command-to-string "uname -n"))
	(add-to-list 'default-frame-alist '(font . "Cica-21.5"))
  (add-to-list 'default-frame-alist '(font . "Cica-18")))

;; Server
(eval-and-compile (require 'server))
(unless (server-running-p)
  (add-hook 'after-init-hook 'server-start))

(leaf exec-path-from-shell :ensure t
  :doc "Share PATH from shell environment variables"
  :when (memq window-system '(mac ns x))
  :custom (exec-path-from-shell-check-startup-files . nil)
  :hook (after-init-hook . exec-path-from-shell-initialize))

(push (expand-file-name "elisp/" user-emacs-directory) load-path)
(leaf load-user-conf
  :doc "Load user configurations"
  :require (my:dired my:template))

(leaf init-loader
  :doc "Loader of configuration files"
  :ensure t
  :config
  (custom-set-variables
   '(init-loader-show-log-after-init 'error-only))
  (init-loader-load)
  (setq custom-file (locate-user-emacs-file "~/.emacs.d/tmp/custom.el")))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init.el ends here
