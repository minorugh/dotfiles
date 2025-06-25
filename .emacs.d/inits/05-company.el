;;; 05-company.el --- Company configurations.    -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf company :ensure t
  :defun company-mode/backend-with-yas
  :doc "Modular in-buffer completion framework"
  :hook (after-init-hook . global-company-mode)
  :bind (("<backtab>"      . company-complete)
	 ("C-<tab>"      . company-yasnippet)
	 (:company-active-map
	  ("<tab>"       . company-complete-common-or-cycle)
	  ("<backtab>"   . company-select-previous)
	  ("<muhenkan>"  . company-abort)))
  :config
  (setq company-transformers          '(company-sort-by-backend-importance))
  (setq company-idle-delay            0.5)
  (setq company-require-match         'never)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case        t)
  (setq company-dabbrev-downcase      nil)
  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
	      '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(leaf prescient :ensure t
  :doc "Better sorting and filtering"
  :hook (after-init-hook . prescient-persist-mode)
  :config
  (setq prescient-aggressive-file-save t)
  (setq prescient-save-file "~/.emacs.d/tmp/prescient-save")
  (with-eval-after-load 'prescient
    (leaf ivy-prescient :ensure t)
    (leaf company-prescient :ensure t)))

(leaf yasnippet :ensure t
  :doc "Template system"
  :hook (after-init-hook . yas-global-mode)
  :config
  (setq yas-indent-line 'fixed)
  (leaf yasnippet-snippets :ensure t))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 05-company.el ends here
