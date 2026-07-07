;;; 09-flymake.el --- Flymake configurations.      -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf flymake
  :tag "builtin"
  :doc "On-the-fly syntax checking."
  :hook ((prog-mode-hook     . flymake-mode)
         (markdown-mode-hook . flymake-mode)
         (lisp-interaction-mode-hook . (lambda () (flymake-mode 0))))
  :config
  ;; Hide Trust notifications from elisp-flymake-byte-compile.
  (defun my-flymake--filter-message (orig fmt &rest args)
    (unless (and (stringp fmt)
                 (string-prefix-p
                  "Disabling elisp-flymake-byte-compile"
                  fmt))
      (apply orig fmt args)))
  (advice-add 'message :around #'my-flymake--filter-message)

  (with-eval-after-load 'elisp-mode
    (advice-add 'elisp-flymake-byte-compile :around
                (lambda (orig-fun report-fn &rest args)
                  (condition-case nil
                      (apply orig-fun report-fn args)
                    (user-error nil))))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 09-flymake.el ends here
