;;; 05_company.el --- Company configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf company :ensure t
  :doc "Modular in-buffer completion framework"
  :hook (after-init-hook . global-company-mode)
  :bind (("C-1"          . company-complete)
	 ("C-<tab>"      . company-yasnippet)
	 (:company-active-map
	  ("<tab>"       . company-complete-common-or-cycle)
	  ("<backtab>"   . company-select-previous)
	  ("<muhenkan>"  . company-abort)))
  :config
  (setq company-transformers          '(company-sort-by-backend-importance))
  (setq company-idle-delay            0)
  (setq company-require-match         'never)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case        t)
  (setq company-dabbrev-downcase      nil)
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
  :config
  (setq yas-indent-line 'fixed)
  (leaf yasnippet-snippets :ensure t))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 05_company.el ends here
