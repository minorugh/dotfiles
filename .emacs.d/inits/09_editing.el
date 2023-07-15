;;; 09_editing.el --- Editing support configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf undo-fu
  :doc "Redo and Undo operations"
  :url "https://codeberg.org/ideasman42/emacs-undo-fu"
  :ensure t
  :bind (("C-_" . undo-fu-only-undo)
		 ("C-/" . undo-fu-only-redo)))


(leaf undohist
  :doc "Persistent undo history"
  :url "https://github.com/emacsorphanage/undohist"
  :ensure t
  :hook (after-init-hook . undohist-initialize)
  :custom
  `((undohist-directory     . "~/.emacs.d/tmp/undohist")
	(undohist-ignored-files . '("/tmp/" "COMMIT_EDITMSG"))))


(leaf sudo-edit
  :doc "Open root parmission files as sudo user"
  :url "https://github.com/nflath/sudo-edit"
  :ensure t)


(leaf iedit
  :doc "Edit multiple regions in the same way simultaneously"
  :url "https://github.com/victorhge/iedit"
  :ensure t
  :bind ([insert] . iedit-mode))


(leaf expand-region
  :doc "Extension for region"
  :url "https://github.com/magnars/expand-region.el"
  :ensure t
  :bind ("C-@" . er/expand-region))


(leaf smartparens
  :doc "Minor mode for dealing with pairs"
  :url "https://github.com/Fuco1/smartparens"
  :ensure t
  :hook (after-init-hook . smartparens-global-mode)
  :config
  (leaf smartparens-config :require t))


(leaf atomic-chrome
  :doc "Edit Chrome text area with Emacs"
  :url "https://github.com/alpha22jp/atomic-chrome"
  :ensure t
  :hook (after-init-hook . atomic-chrome-start-server)
  :custom
  (atomic-chrome-buffer-open-style . 'full))


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
;;; 09_editing.el ends here
