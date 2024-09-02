;;; init.el --- Emacs first Configuration.
;;; Commentary:
;;
;;; Code:
;; (setq debug-on-error t)

;; Speed up startup
(setq gc-cons-threshold most-positive-fixnum)
(defconst my:file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
	    "Recover file name handlers and GC values after startup."
	    (setq file-name-handler-alist my:file-name-handler-alist)
	    (setq gc-cons-threshold 800000)))

;; Package
(customize-set-variable
 'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                     ("melpa" . "https://melpa.org/packages/")
                     ("org"   . "https://orgmode.org/elpa/")))

;; Do not check signatures
(package-initialize)
(setq package-check-signature nil)
(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))

(leaf leaf-keywords :ensure t
  :config
  (leaf-keywords-init)
  :init
  (leaf hydra :ensure t)
  (leaf el-get :ensure t
    :config
    (setq el-get-git-shallow-clone t)))

;;Auto revert
(setq auto-revert-interval 0.1)
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; Font settings on the main machine or sub machine
(if (string-match "P1" (shell-command-to-string "uname -n"))
    (add-to-list 'default-frame-alist '(font . "Cica-21.5"))
  (add-to-list 'default-frame-alist '(font . "Cica-18")))

;; Start server unless running
(require 'server)
(unless (server-running-p)
  (server-start))

(leaf exec-path-from-shell :ensure t
  :doc "Share PATH from shell environment variables"
  :when (memq window-system '(mac ns x))
  :hook (emacs-startup-hook . exec-path-from-shell-initialize))

(push (expand-file-name "elisp/" user-emacs-directory) load-path)
(leaf load-user-conf
  :doc "Load user configurations"
  :require my:browse my:dired my:template my:make)

(leaf init-loader :ensure t
  :doc "Loader of configuration files"
  :config
  (custom-set-variables
   '(init-loader-show-log-after-init 'error-only))
  (init-loader-load)
  (setq custom-file (locate-user-emacs-file "~/.emacs.d/tmp/custom.el")))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; init.el ends here
