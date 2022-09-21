;;; 06_flymake.el --- Flymake Syntax cheking configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf flymake
  :hook (emacs-lisp-mode-hook . flymake-mode)
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (leaf flymake-posframe
	:el-get Ladicle/flymake-posframe
	:hook (flymake-mode-hook . flymake-posframe-mode)
	:custom
	(flymake-posframe-error-prefix . " ")))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 06_flymake.el ends here
