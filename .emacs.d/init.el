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

;; Load user configurations
(push (expand-file-name "elisp/" user-emacs-directory) load-path)
(leaf load-user-conf
  :doc "Load user configurations"
  :require (my:dired my:template))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 0.1))
  :global-minor-mode global-auto-revert-mode)

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :custom ((flycheck-emacs-lisp-initialize-packages . t))
  :hook prog-mode-hook)

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
