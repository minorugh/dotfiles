;;; init.el --- User initialization.  -*- lexical-binding: t -*-
;;; Commentary:
;; Author: minorugh <https://github.com/minorugh/dotfiles>
;; Package-Requires: ((Emacs "29.1"))
;;; Code:
;; (setq debug-on-error t)
;; (with-current-buffer " *Flymake log*" (buffer-string))

;;; ============================================================
;;;  Version Guard
;;; ============================================================

(when (version< emacs-version "29.1")
  (error "This requires Emacs 29.1 and above!"))


;;; ============================================================
;;;  Startup Speed — Suppress File Handlers
;;; ============================================================

(defconst default-handlers file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover file-name handlers and reset GC after startup."
            (setq file-name-handler-alist default-handlers)
            (setq gc-cons-threshold (* 16 1024 1024))
            (setq inhibit-message nil)
            (message "Emacs ready in %s with %d GCs."
                     (emacs-init-time) gcs-done)
            (mapc #'delete-file (file-expand-wildcards "~/.emacs.d/session.*"))))


;;; ============================================================
;;;  Package System (leaf + hydra)
;;; ============================================================

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


;;; ============================================================
;;;  Config Loader
;;; ============================================================

(leaf init-loader
  :ensure t
  :load-path "~/.emacs.d/elisp"   ;; Path to the local packages
  :config
  (setq init-loader-show-log-after-init 'error-only)
  (setq init-loader-byte-compile t)
  (init-loader-load))


;;; ============================================================
;;;  Language & Misc
;;; ============================================================

(set-language-environment "Japanese")
(setq-default indent-line-function 'indent-for-current-mode)
(setq custom-file (locate-user-emacs-file "tmp/custom.el"))


;;; ============================================================
;;;  Server
;;; ============================================================

(leaf server
  :commands server-running-p
  :hook (emacs-startup-hook
         . (lambda ()
             (unless (server-running-p)
               (server-start)))))


;;; ============================================================
;;;  Shell Environment (exec-path-from-shell)
;;; ============================================================

;; Inherit shell env vars including SSH_AUTH_SOCK (for ssh-agent/keychain)
(leaf exec-path-from-shell
  :ensure t
  :when (memq window-system '(mac ns x))
  :hook (emacs-startup-hook . exec-path-from-shell-initialize)
  :config
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))


(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages '((tempbuf :url "https://github.com/minorugh/tempbuf"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init.el ends here
