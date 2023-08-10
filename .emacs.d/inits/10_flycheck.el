;;; 10_flycheck.el --- Define functions.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf flycheck
  :ensure t
  :hook (prog-mode-hook . flycheck-mode)
  :bind* (("M-l" . flycheck-list-errors)
		  ("M-n" . flycheck-next-error)
		  ("M-p" . flycheck-previous-error))
  :custom ((flycheck-emacs-lisp-initialize-packages . t))
  :config
  (eval-and-compile (require 'flycheck))
  (setq flycheck-emacs-lisp-package-initialize-form
		(flycheck-sexp-to-string
		 '(progn
			(with-demoted-errors "Error during package initialization: %S"
              (package-initialize))
			(leaf-keywords-init)))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 10_flycheck.el ends here
