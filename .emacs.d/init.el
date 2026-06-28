;;; init.el --- User initialization.  -*- lexical-binding: t -*-
;;; Commentary:
;; Author: minorugh <https://github.com/minorugh/dotfiles>
;; Package-Requires: ((Emacs "29.1"))
;;; Code:
;; (setq debug-on-error t)
;; (with-current-buffer " *Flymake log*" (buffer-string))

;; ============================================================
;;  Version Guard
;; ============================================================

(when (version< emacs-version "29.1")
  (error "This requires Emacs 29.1 and above!"))


;; ============================================================
;;  Startup Speed — Suppress File Handlers
;; ============================================================

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


;; ============================================================
;;  Package System (leaf + hydra)
;; ============================================================

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  (use-package leaf :ensure t)

  (leaf hydra :ensure t)

  (leaf leaf-keywords
    :ensure t
    :config
    (leaf-keywords-init)))


;; ============================================================
;;  Config Loader
;; ============================================================

(leaf init-loader
  :ensure t
  :load-path "~/.emacs.d/elisp"   ;; Path to the local packages
  :config
  (setq init-loader-show-log-after-init 'error-only)
  (setq init-loader-byte-compile t)
  (init-loader-load))


;; ============================================================
;;  Language & Misc
;; ============================================================

(set-language-environment "Japanese")
(setq-default indent-line-function 'indent-for-current-mode)
(setq custom-file (locate-user-emacs-file "tmp/custom.el"))


;; ============================================================
;;  Server
;; ============================================================

(leaf server
  :commands server-running-p
  :hook (emacs-startup-hook
         . (lambda ()
             (unless (server-running-p)
               (server-start)))))


(provide 'init)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init.el ends here
