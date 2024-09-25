;;; init.el --- Emacs first Configuration.
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

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code.
(setq load-prefer-newer noninteractive)

;; Package
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  (use-package leaf :ensure t)

  (leaf leaf-keywords :ensure t
    :config
    (leaf-keywords-init)
    :init
    (leaf hydra :ensure t)))

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

;; User configuration
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
