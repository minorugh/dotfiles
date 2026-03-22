;;; init.el --- User initialization.  -*- lexical-binding: t -*-
;;; Commentary:
;; URL: https://github.com/minorugh/dotfiles
;;; Code:
;; Package-Requires: ((emacs "29.1"))
;; (setq debug-on-error t)

(when (version< emacs-version "29.1")
  (error "This requires Emacs 29.1 and above!"))

;;; Temporarily suppress file-handlers processing to speed up startup
(defconst default-handlers file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    "Recover file name handlers and GC values after startup."
	    (setq file-name-handler-alist default-handlers)
	    (setq gc-cons-threshold (* 16 1024 1024))
	    (setq inhibit-message nil)
	    (message "Emacs ready in %s with %d GCs."
		     (emacs-init-time) gcs-done)))

;;; package system
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
		       ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  (use-package leaf :ensure t)

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    :config
    (leaf-keywords-init))

  (leaf init-loader
    :ensure t
    :doc "Load inits configuration."
    :config
    (setq init-loader-show-log-after-init 'error-only)
    (setq init-loader-byte-compile t)
    (init-loader-load)
    :init
    ;; Add ~/.emacs.d/elisp/ and all its subdirectories to load-path.
    ;; This eliminates the need to specify :load-path individually in each leaf block
    ;; for packages installed under this directory.
    ;; Note: normal-top-level-add-subdirs-to-load-path only adds subdirectories,
    ;; so we explicitly add elisp/ itself with add-to-list first.
    (let ((default-directory "~/.emacs.d/elisp/"))
      (add-to-list 'load-path default-directory)
      (normal-top-level-add-subdirs-to-load-path))
    (setq custom-file (locate-user-emacs-file "tmp/custom.el"))))

;;; server
(leaf server
  :commands server-running-p
  :hook (emacs-startup-hook
	 . (lambda ()
	     (unless (server-running-p)
	       (server-start)))))

;;; shell env
(leaf exec-path-from-shell
  :ensure t
  :when (memq window-system '(mac ns x))
  :hook (emacs-startup-hook . exec-path-from-shell-initialize)
  :config
  ;; Pass SSH_AUTH_SOCK from shell to Emacs so that ssh-agent (keychain)
  ;; is available for git, FileZilla (shell), and other SSH operations.
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))


(provide 'init)
;; Local Variables:
;; byte-compile-warnings: t(not free-vars)
;; End:
;;; init.el ends here
