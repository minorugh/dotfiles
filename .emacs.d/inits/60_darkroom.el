;;; 60_editing.el --- Writing mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf darkroom
  :doc "Remove visual distractions and focus on writing"
  :url "https://github.com/joaotavora/darkroom"
  :ensure t
  :bind (([f12] . my:darkroom-in)
		 (:darkroom-mode-map
		  ([f12] . my:darkroom-out)))
  :init
  (defun my:darkroom-in ()
	"Enter to the `darkroom-mode'."
	(interactive)
	(diff-hl-mode 0)
	(display-line-numbers-mode 0)
	(darkroom-mode 1)
	(setq-local line-spacing .5))

  (defun my:darkroom-out ()
	"Returns from `darkroom-mode' to the previous state."
	(interactive)
	(darkroom-mode 0)
	(display-line-numbers-mode 1)
	(setq-local line-spacing .1)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 60_darkroom.el ends here
