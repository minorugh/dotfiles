;;; init.el --- User initialization.  -*- lexical-binding: t -*-
;;; Commentary:
;; Requires Emacs 29.1 or later.
;;; Code:
;; (setq debug-on-error t)

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

  ;; Already done at startup by default; kept so that `leaf' is found
  ;; when this file is checked or byte-compiled on its own.
  (package-activate-all)

  (use-package leaf :ensure t)
  (leaf hydra :ensure t)
  (leaf leaf-keywords
    :ensure t
    :config
    (leaf-keywords-init)))

;; Use the fixed fork: the MELPA version stops responding to chords after an
;; input method is toggled. No my-key-chord-ensure workaround is needed.
(leaf key-chord
  :vc (:url "https://github.com/minorugh/key-chord")
  :hook (after-init-hook . key-chord-mode))


;; ============================================================
;;  Config Loader
;; ============================================================

(leaf *byte-compile-elisp
  :load-path "~/.emacs.d/elisp"
  :hook (kill-emacs-hook . my-clean-and-compile-elisp)
  :preface
  (defun my-clean-and-compile-elisp ()
    "Delete the orphaned .elc file, then byte-recompile."
    (dolist (dir '("~/.emacs.d/elisp" "~/.emacs.d/inits"))
      (dolist (elc (directory-files dir t "\\.elc\\'"))
        (unless (file-exists-p (concat (file-name-sans-extension elc) ".el"))
          (delete-file elc)))
      (byte-recompile-directory dir 0))))

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
