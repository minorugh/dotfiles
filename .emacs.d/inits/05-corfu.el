;;; 05-corfu.el --- Corfu configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf corfu :ensure t
  :doc "Completion in region function"
  :hook (after-init-hook . global-corfu-mode)
  :bind ((:corfu-map
	  ([tab]      . corfu-next)
          ([backtab]  . corfu-previous)
	  ("<return>" . corfu-insert)
	  ("C-q" . corfu-quit)))
  :config
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.5)
  (setq corfu-cycle t)
  (setq corfu-preselect 'prompt))

;;; 05-corfu.el ends here
