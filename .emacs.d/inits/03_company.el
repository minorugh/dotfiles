;;; 03_company.el --- Company configurations. -*- lexical-binding: t no-byte-compile: t -*-
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 03_company.el ends here
