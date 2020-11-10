;;; 03_company.el --- Modular text completion framework  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)

(leaf company
  :ensure t
  :commands global-company-mode
  :global-minor-mode global-company-mode
  :config
  (bind-key "C-<tab>" 'company-complete)
  (bind-key "<tab>" 'company-complete-common-or-cycle company-active-map)
  (bind-key "b" 'company-select-previous company-active-map)
  (bind-key "SPC" 'company-select-next company-active-map)
  (bind-key "C-d" 'company-show-doc-buffer company-active-map)
  (setq company-transformers '(company-sort-by-backend-importance))
  (setq company-minimum-prefix-length 3)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil))


(leaf company-quickhelp
  :ensure t
  :global-minor-mode t
  :config
  (setq company-quickhelp-color-foreground "#C7C9D1")
  (setq company-quickhelp-color-background "#161822")
  (setq company-quickhelp-max-lines 5))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 03_company.el ends here
