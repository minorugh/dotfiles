;;; 10_writing.el --- Editing support configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf undo-fu :ensure t
  :bind (("C-_" . undo-fu-only-undo)
		 ("C-/" . undo-fu-only-redo)))


(leaf undohist :ensure t
  :hook (after-init-hook . undohist-initialize)
  :custom `((undohist-directory     . "~/.emacs.d/tmp/undohist")
			(undohist-ignored-files . '("/tmp/" "COMMIT_EDITMSG"))))


(leaf smartparens :ensure t
  :hook (after-init-hook . smartparens-global-mode)
  :config (leaf smartparens-config :require t))


(leaf darkroom :ensure t
  :bind (([f7] . my:darkroom-in)
		 (:darkroom-mode-map
		  ([f7] . my:darkroom-out)))
  :config
  (eval-and-compile (require 'evil))
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


(leaf atomic-chrome :ensure t
  :hook (after-init-hook . atomic-chrome-start-server)
  :custom (atomic-chrome-buffer-open-style . 'full))


(leaf mail-mode
  :doc "Using mail-mode for eml files for Thunderbird plugin support"
  :mode ("\\.eml\\'" . mail-mode)
  :hook (mail-mode-hook . darkroom-mode))


(leaf pangu-spacing
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
;;; 10_writing.el ends here
