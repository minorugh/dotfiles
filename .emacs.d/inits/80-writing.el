;;; 80-writing.el --- Writing mode configulation. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf darkroom :ensure t
  :defun evil-emacs-state evil-normal-state
  :doc "Remove visual distractions and focus on writing"
  :bind (([f8] . my:darkroom-in)
	 (:darkroom-mode-map
	  ([f8] . my:darkroom-out)))
  :config
  (defun my:darkroom-in ()
    "Enter to the `darkroom-mode'."
    (interactive)
    (diff-hl-mode 0)
    (display-line-numbers-mode 0)
    (darkroom-tentative-mode 1)
    (toggle-frame-fullscreen)
    (setq-local line-spacing .2)
    (evil-emacs-state))
  (defun my:darkroom-out ()
    "Returns from `darkroom-mode' to the previous state."
    (interactive)
    (darkroom-tentative-mode 0)
    (display-line-numbers-mode 1)
    (toggle-frame-fullscreen)
    (setq-local line-spacing 0)
    (evil-normal-state)))

(leaf pangu-spacing
  :doc "Put a space between Japanese and English"
  :url "http://github.com/coldnew/pangu-spacing"
  :ensure t
  :hook ((markdown-mode-hook text-mode-hook) . pangu-spacing-mode)
  :config
  (setq pangu-spacing-real-insert-separtor t)
  (setq pangu-spacing-include-regexp ;; alphabet only
	(rx (or (and (or (group-n 3 (any "。，！？；：「」（）、"))
			 (group-n 1 (or (category japanese))))))
	    (group-n 2 (in "a-zA-Z")))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 80-writing.el ends here
