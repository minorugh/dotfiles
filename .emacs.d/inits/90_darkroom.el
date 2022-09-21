;;; 90_darkroom.el --- Writing mode configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf darkroom
  :ensure t
  :bind (("<f12>" . my:darkroom-in)
		 (:darkroom-mode-map
		  ("<f12>" . my:darkroom-out)))
  :init
  (defun my:darkroom-in ()
	"Enter to the `darkroom-mode'."
	(interactive)
	(view-mode 0)
	(diff-hl-mode 0)
	(display-line-numbers-mode 0)
	(darkroom-mode 1)
	(setq-local line-spacing .5))

  (defun my:darkroom-out ()
	"Returns from `darkroom-mode' to the previous state."
	(interactive)
	(my:linespacing)
	(darkroom-mode 0)
	(display-line-numbers-mode 1)
	(my:revert-buffer-no-confirm))

  (defun my:revert-buffer-no-confirm ()
	"Revert buffer without confirmation."
	(interactive)
	(revert-buffer t t)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 90_darkroom.el ends here
