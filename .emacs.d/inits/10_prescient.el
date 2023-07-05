;;; 10_prescient.el --- Prescent configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf prescient
  :doc "Better sorting and filtering"
  :url "https://github.com/raxod502/prescient.el"
  :ensure t
  :hook (after-init-hook . prescient-persist-mode)
  :custom
  `((prescient-aggressive-file-save . t)
	(prescient-save-file . "~/.emacs.d/tmp/prescient-save"))
  :config
  (leaf ivy-prescient :ensure t :global-minor-mode t)
  (leaf company-prescient :ensure t :global-minor-mode t))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 10_prescient.el ends here
