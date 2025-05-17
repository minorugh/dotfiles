;;; init.el --- Emacs first Configuration.  -*- no-byte-compile: t; -*-
;;; Commentary:
;;
;;; Code:
;; (setq debug-on-error t)

(when (version< emacs-version "29.1")
  (error "This requires Emacs 29.1 and above!"))

;; Speed up startup
(setq gc-cons-threshold most-positive-fixnum)
(defconst my:file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
	    "Recover file name handlers and GC values after startup."
	    (setq file-name-handler-alist my:file-name-handler-alist)
	    (setq gc-cons-threshold 800000)))

;; Always load newest byte code
(setq load-prefer-newer t)

;; Disable warnings at initialization
(eval-and-compile
  (setq warning-minimum-level :emergency))

;; Package
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
		       ("gnu"   . "https://elpa.gnu.org/packages/")
		       ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  (use-package leaf :ensure t)

  (leaf leaf-keywords :ensure t
    :init
    (leaf hydra :ensure t)
    :config
    (leaf-keywords-init)))

(leaf server
  :doc "Check if the Emacs server is running and start it"
  :commands (server-running-p)
  :hook
  (emacs-startup-hook . (lambda ()
			  (unless (server-running-p)
			    (server-start)))))

(leaf exec-path-from-shell :ensure t
  :doc "Get environment variables such as `$PATH' from the shell"
  :when (memq window-system '(mac ns x))
  :hook (emacs-startup-hook . exec-path-from-shell-initialize))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "~/.emacs.d/tmp/custom.el"))))

(leaf init-loader :ensure t
  :doc "Init loader."
  :config
  (custom-set-variables
   '(init-loader-show-log-after-init 'error-only))
  (init-loader-load))

;;;###autoload
(leaf *load-user-conf
  :doc "Load user configurations"
  :load-path "~/.emacs.d/elisp/"
  :require my:dired my:template my:make-command)


(provide 'init)
;;; init.el ends here
