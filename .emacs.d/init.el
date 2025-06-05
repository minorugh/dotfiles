;;; init.el --- Emacs first Configuration.
;;; Commentary:
;;
;;; Code:
;; (setq debug-on-error t)

;; Speed up startup
(setq gc-cons-threshold most-positive-fixnum)

(defconst default-hadlers file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    "Recover file name handlers and GC values after startup."
	    (setq file-name-handler-alist default-hadlers)
	    (setq gc-cons-threshold 800000)
	    (setq inhibit-redisplay nil)
   	    (setq inhibit-message nil)
   	    (redisplay)))

(customize-set-variable
 'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
		     ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(use-package leaf :ensure t)

(leaf leaf-keywords :ensure t
  :init
  (leaf hydra :ensure t)
  :config
  (leaf-keywords-init))

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
  :custom `((custom-file . ,(locate-user-emacs-file "~/.emacs.d/tmp/custom.el")))
  :config
  (custom-set-variables
   '(init-loader-show-log-after-init 'error-only))
  (init-loader-load))

(leaf *load-user-elisp
  :load-path "~/.emacs.d/elisp/"
  :hook (emacs-startup-hook
	 . (lambda ()
	     "Load user configuration elisp."
	     (require 'my:dired)
	     (require 'my:template)
	     (require 'my:make-command)
	     (require 'evil-easy-hugo))))

(provide 'init)

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; init.el ends here
