;;; init.el --- User initialization.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  Version Guard
;; ============================================================

(when (version< emacs-version "29.1")
  (error "This requires Emacs 29.1 and above!"))


;; ============================================================
;;  Startup Speed — Suppress File Handlers
;; ============================================================

(defvar default-handlers file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover file-name handlers and reset GC after startup."
            (setq file-name-handler-alist default-handlers)
            (setq gc-cons-threshold (* 16 1024 1024))
            (setq inhibit-message nil)
            (message "Emacs ready in %s with %d GCs."
                     (emacs-init-time) gcs-done)))


;; ============================================================
;;  Package System (leaf + hydra + key-chord)
;; ============================================================

(eval-and-compile
  (setq package-archives
        '(("gnu"   . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")))

  (package-initialize)

  (use-package leaf :ensure t)
  (leaf hydra :ensure t)
  (leaf leaf-keywords
    :ensure t
    :config
    (leaf-keywords-init)))

(leaf key-chord
  :ensure t
  :defun my-key-chord-ensure
  :hook (after-init-hook . key-chord-mode)
  :config
  (defun my-key-chord-ensure ()
    "Key-chord stall recovery."
    (when key-chord-mode
      (key-chord-mode -1)
      (key-chord-mode 1)))
  (add-hook 'input-method-activate-hook   #'my-key-chord-ensure)
  (add-hook 'input-method-deactivate-hook #'my-key-chord-ensure))


;; ============================================================
;;  Config Loader
;; ============================================================

(leaf *byte-compile-elisp
  :load-path "~/.emacs.d/elisp"
  :hook (kill-emacs-hook . my-clean-and-compile-elisp)
  :preface
  (defun my-clean-and-compile-elisp ()
    "Clean orphan .elc files in elisp/ and inits/, then recompile."
    (dolist (dir '("elisp" "inits"))
      (dolist (elc (directory-files
                    (concat "~/.emacs.d/" dir) t "\\.elc\\'"))
        (unless (file-exists-p (concat (file-name-sans-extension elc) ".el"))
          (delete-file elc))))
    (byte-recompile-directory "~/.emacs.d/elisp" 0)
    (byte-recompile-directory "~/.emacs.d/inits" 0)))

(leaf init-loader
  :ensure t
  :doc "Load Emacs configuration files from the inits/ directory."
  :config
  (setq init-loader-show-log-after-init 'error-only)
  (key-chord-define-global "l;" 'init-loader-show-log)
  (init-loader-load))


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
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
