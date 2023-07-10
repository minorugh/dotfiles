;;; 10_whitespace.el --- Witespace configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf whitespace
  :doc "Highligh trailing whitespace"
  :hook (prog-mode-hook . my:enable-trailing-mode)
  :bind ("C-c C-c" . my:cleanup-for-spaces)
  :custom
  (show-trailing-whitespace . nil)
  :init
  (defun my:enable-trailing-mode ()
	"Show tail whitespace."
	(setq show-trailing-whitespace t))

  (defun my:cleanup-for-spaces ()
	"Remove contiguous line breaks at end of line + end of file."
	(interactive)
	(delete-trailing-whitespace)
	(save-excursion
	  (save-restriction
		(widen)
		(goto-char (point-max))
		(delete-blank-lines)))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 10_whitespace.el ends here
