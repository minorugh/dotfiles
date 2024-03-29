;;; 05_company.el --- Company configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf company :ensure t
  :doc "Modular in-buffer completion framework"
  :defun (company-mode/backend-with-yas)
  :hook (after-init-hook . global-company-mode)
  :bind (("C-1"          . company-complete)
		 ("C-<tab>"      . company-yasnippet)
		 (:company-active-map
		  ("<tab>"       . company-complete-common-or-cycle)
		  ("<backtab>"   . company-select-previous)
		  ("<muhenkan>"  . company-abort)))
  :custom
  `((company-transformers          . '(company-sort-by-backend-importance))
	(company-idle-delay            . 0)
	(company-require-match         . 'never)
	(company-minimum-prefix-length . 2)
	(company-selection-wrap-around . t)
	(completion-ignore-case        . t)
	(company-dabbrev-downcase      . nil))
  :config
  (defvar company-mode/enable-yas t
	"Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
	(if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
		backend
	  (append (if (consp backend) backend (list backend))
			  '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))


(leaf yasnippet :ensure t
  :doc "Template system"
  :hook (after-init-hook . yas-global-mode)
  :custom (yas-indent-line . 'fixed)
  :init
  (leaf yasnippet-snippets :ensure t
	:after yasnippet))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 05_company.el ends here
