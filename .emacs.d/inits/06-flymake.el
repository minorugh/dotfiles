;;; 06-flymake.el --- Syntax checking configurations.      -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf flymake
  :tag "builtin"
  :doc "On-the-fly syntax checking."
  :hook ((prog-mode-hook . flymake-mode)
         (markdown-mode-hook . flymake-mode)
         (lisp-interaction-mode-hook . (lambda () (flymake-mode 0)))
         ;; Disable elisp-flymake-byte-compile to avoid "untrusted content" warnings
         ;; on Emacs 29+. This backend triggers security prompts when checking
         ;; Elisp files outside standard locations.
         (emacs-lisp-mode-hook . (lambda ()
                                   (setq-local flymake-diagnostic-functions
                                               (remove 'elisp-flymake-byte-compile
                                                       flymake-diagnostic-functions)))))
  :bind (("C-c f" . flymake-show-buffer-diagnostics))
  :config
  (setq flymake-suppress-zero-messages t))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 06-flymake.el ends here
