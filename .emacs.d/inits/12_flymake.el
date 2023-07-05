;;; 12_flymake.el --- Syntax Check configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)


(leaf flymake
  :hook (emacs-startup-hook . my:flymake-hook)
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
  (defun my:flymake-hook ()
	(interactive)
	(add-hook 'prog-mode-hook 'flymake-mode)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 12_flymake.el ends here
