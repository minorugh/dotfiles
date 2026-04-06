;;; 80-darkroom.el --- Darkroom configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf darkroom :ensure t
  :doc "Remove visual distractions and focus on writing."
  :bind (([f8] . my-darkroom-in)
	 (:darkroom-mode-map
	  ([f8] . my-darkroom-out)))
  :config
  (defun my-darkroom-in ()
    "Enter to the `darkroom-mode'."
    (interactive)
    (display-line-numbers-mode 0)
    (darkroom-tentative-mode 1)
    (toggle-frame-fullscreen)
    (setq-local line-spacing .2)
    (evil-emacs-state))

  (defun my-darkroom-out ()
    "Returns from `darkroom-mode' to the previous state."
    (interactive)
    (darkroom-tentative-mode 0)
    (display-line-numbers-mode 1)
    (toggle-frame-fullscreen)
    (setq-local line-spacing 0)
    (evil-normal-state)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 80-darkroom.el ends here
