;;; 10-flycheck.el --- flycheck configulation. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf flycheck :ensure t
  :doc "On-the-fly syntax checking"
  :url "http://www.flycheck.org"
  :hook ((prog-mode-hook . flycheck-mode)
	 (lisp-interaction-mode-hook
 	  . (lambda () (interactive)(flycheck-mode 0))))
  :bind (("M-n" . flycheck-next-error)
	 ("M-p" . flycheck-previous-error))
  :config
  (setq flycheck-emacs-lisp-initialize-packages t)
  ;; Fixing leaf-keywords "Unrecognized keyword" error in flycheck
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
;;; 10-flycheck.el ends here
