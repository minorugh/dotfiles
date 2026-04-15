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
    (leaf-keywords-init)))

;; Personal settings file managed outside of version control.
(setq custom-file (locate-user-emacs-file "tmp/custom.el"))

(leaf init-loader
  :ensure t
  :load-path "~/.emacs.d/elisp" ;; local elisp packages
  :config
  (setq init-loader-show-log-after-init 'error-only)
  (setq init-loader-byte-compile t)
  (init-loader-load)
  ;; Suppress *scratch* flickering on startup
  (switch-to-buffer (get-buffer-create "*dashboard*")))

;; Start Emacs server if not already running
(leaf server
  :commands server-running-p
  :hook (emacs-startup-hook
	 . (lambda ()
	     (unless (server-running-p)
	       (server-start)))))

;; Inherit shell environment variables including SSH_AUTH_Sock
(leaf exec-path-from-shell
  :ensure t
  :when (memq window-system '(mac ns x))
  :hook (emacs-startup-hook . exec-path-from-shell-initialize)
  :config
  ;; Pass SSH_AUTH_SOCK from shell to Emacs so that ssh-agent (keychain)
  ;; is available for git, FileZilla (shell), and other SSH operations.
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(defun my-reload-keychain ()
  "Reload keychain environment variables in Emacs session for SSH."
  (interactive)
  ;; keychain が書いた SSH_AUTH_SOCK と SSH_AGENT_PID を Emacs 内に設定
  (let ((keychain-file (expand-file-name (concat "~/.keychain/" (system-name) "-sh"))))
    (when (file-exists-p keychain-file)
      (with-temp-buffer
        (insert-file-contents keychain-file)
        ;; export 文を eval して Emacs 内に反映
        (goto-char (point-min))
        (while (re-search-forward "^export \\([^=]+\\)=\\(.*\\)$" nil t)
          (setenv (match-string 1) (replace-regexp-in-string "^\"\\|\"$" "" (match-string 2))))))
    (message "Keychain reloaded in Emacs!")))

(add-hook 'after-init-hook #'my-reload-keychain)


(provide 'init)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init.el ends here
