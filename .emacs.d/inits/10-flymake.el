;;; 10-flymake.el --- Flymake configurations.      -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(add-hook 'prog-mode-hook     'flymake-mode)
(add-hook 'markdown-mode-hook 'flymake-mode)
(add-hook 'lisp-interaction-mode-hook #'(lambda () (flymake-mode 0)))

;; Hide Trust notifications from elisp-flymake-byte-compile.
(defun my-flymake--filter-message (orig fmt &rest args)
  "Suppress elisp-flymake-byte-compile's `untrusted-content' notice.
Otherwise call ORIG with FMT and ARGS as usual."
  (unless (and (stringp fmt)
               (string-prefix-p
                "Disabling elisp-flymake-byte-compile"
                fmt))
    (apply orig fmt args)))
(advice-add 'message :around #'my-flymake--filter-message)

;; Swallow the user-error elisp-flymake-byte-compile signals for
;; untrusted content, so Flymake doesn't stop in the buffer.
(with-eval-after-load 'elisp-mode
  (advice-add 'elisp-flymake-byte-compile :around
              (lambda (orig-fun report-fn &rest args)
                (condition-case nil
                    (apply orig-fun report-fn args)
                  (user-error nil)))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 10-flymake.el ends here
