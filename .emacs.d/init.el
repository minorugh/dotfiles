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
                     ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(setq package-check-signature nil)

(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))

(leaf leaf-keywords :ensure t
  :config
  (leaf-keywords-init)
  :init
  (leaf hydra :ensure t))

;; Set language & System encoding
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; Font
(if (string-match "P1" (shell-command-to-string "uname -n"))
    (add-to-list 'default-frame-alist '(font . "Cica-20"))
  (add-to-list 'default-frame-alist '(font . "Cica-18")))

;; Server start
(leaf server
  :commands (server-running-p)
  :hook
  (emacs-startup-hook . (lambda ()
			  (unless (server-running-p)
			    (server-start)))))

;; PATH from shell
(leaf exec-path-from-shell :ensure t
  :when (memq window-system '(mac ns x))
  :hook (emacs-startup-hook . exec-path-from-shell-initialize))

;; Load user configuration
(leaf *load-user-conf
  :load-path "~/.emacs.d/elisp/"
  :require my:browse my:dired my:template)

;; Init loader
(leaf init-loader :ensure t
  :config
  (custom-set-variables
   '(init-loader-show-log-after-init 'error-only))
  (init-loader-load))


(provide 'init)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; init.el ends here
