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
                     (emacs-init-time) gcs-done)
            (mapc #'delete-file (file-expand-wildcards "~/.emacs.d/session.*"))))


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

(leaf init-loader
  :ensure t
  :load-path "~/.emacs.d/elisp"   ; Path to the local packages
  :config
  (setq init-loader-show-log-after-init 'error-only)
  (setq init-loader-byte-compile t)
  (key-chord-define-global "l;" 'init-loader-show-log)
  (init-loader-load))

;; elisp/ 配下の自作パッケージを終了時に自動byte-compile
(defun my-byte-recompile-elisp-dir ()
  "Byte-compile outdated or missing .elc files under ~/.emacs.d/elisp/."
  (let ((elisp-dir (expand-file-name "elisp" user-emacs-directory)))
    (when (file-directory-p elisp-dir)
      (byte-recompile-directory elisp-dir 0))))

(add-hook 'kill-emacs-hook #'my-byte-recompile-elisp-dir)


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
