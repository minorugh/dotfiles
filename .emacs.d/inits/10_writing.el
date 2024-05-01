;;; 10_writing.el --- Editing support configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf undo-fu
  :doc "Undo helper with redo"
  :ensure t
  :bind (("C-_" . undo-fu-only-undo)
		 ("C-/" . undo-fu-only-redo)))

(leaf undohist
  :doc "Persistent undo history"
  :ensure t
  :hook (after-init-hook . undohist-initialize)
  :custom `((undohist-directory     . "~/.emacs.d/tmp/undohist")
			(undohist-ignored-files . '("/tmp/" "COMMIT_EDITMSG"))))

(leaf smartparens
  :doc "minor mode for dealing with pairs"
  :ensure t
  :hook (after-init-hook . smartparens-global-mode)
  :config (leaf smartparens-config :require t))

(leaf darkroom
  :doc "Remove visual distractions and focus on writing"
  :ensure t
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

(leaf atomic-chrome
  :doc "Edit Chrome text area with Emacs"
  :ensure t
  :hook (after-init-hook . atomic-chrome-start-server)
  :custom (atomic-chrome-buffer-open-style . 'full))

(leaf mail-mode
  :doc "Using mail-mode for eml files for Thunderbird plugin support"
  :mode ("\\.eml\\'" . mail-mode)
  :hook (mail-mode-hook . darkroom-mode))

;; (leaf pangu-spacing
;;   :doc "Add space between Chinese and English characters"
;;   :ensure t
;;   :hook ((markdown-mode-hook text-mode-hook) . pangu-spacing-mode)
;;   :config
;;   (setq pangu-spacing-real-insert-separtor t)
;;   (setq pangu-spacing-include-regexp ;; alphabet only
;; 		(rx (or (and (or (group-n 3 (any "。，！？；：「」（）、"))
;; 						 (group-n 1 (or (category japanese))))))
;; 			(group-n 2 (in "a-zA-Z")))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 10_writing.el ends here
