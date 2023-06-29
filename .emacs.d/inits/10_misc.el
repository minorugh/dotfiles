;;; 10_misc.el --- Misc utility configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf prescient
  :doc "Better sorting and filtering"
  :url "https://github.com/raxod502/prescient.el"
  :ensure t
  :hook (after-init-hook . prescient-persist-mode)
  :custom
  `((prescient-aggressive-file-save . t)
	(prescient-save-file . "~/.emacs.d/tmp/prescient-save"))
  :config
  (leaf ivy-prescient :ensure t :global-minor-mode t)
  (leaf company-prescient :ensure t :global-minor-mode t))


(leaf projectile
  :doc "Project navigation and management library"
  :url "https://github.com/bbatsov/projectile"
  :ensure t
  :hook (after-init-hook . projectile-mode)
  :custom
  (projectile-known-projects-file . "~/.emacs.d/tmp/projectile-bookmarks.eld"))


(leaf quickrun
  :doc "Qick executes editing buffer"
  :url "https://github.com/emacsorphanage/quickrun"
  :ensure t
  :bind ([f6] . quickrun))


(leaf flycheck
  :doc "On-the-fly syntax checking"
  :url "http://www.flycheck.org"
  :ensure t
  :hook (prog-mode-hook . flycheck-mode)
  :bind (("M-n" . flycheck-next-error)
		 ("M-p" . flycheck-previous-error)))


(leaf which-key
  :doc "Displays available keybindings in popup"
  :url "https://github.com/justbur/emacs-which-key"
  :ensure t
  :hook (after-init-hook . which-key-mode)
  :custom ((which-key-max-description-length . 40)))


(leaf popwin
  :doc "Popup window manager"
  :url "https://github.com/emacsorphanage/popwin"
  :ensure t
  :hook (after-init-hook . popwin-mode))


(leaf iedit
  :doc "Edit multiple regions in the same way simultaneously"
  :url "https://github.com/victorhge/iedit"
  :ensure t
  :bind ([insert] . iedit-mode))


(leaf restart-emacs
  :doc "Restart emacs from within emacs"
  :url "https://github.com/iqbalansari/restart-emacs"
  :ensure t
  :bind ("C-x C-c" . restart-emacs))


(leaf sudo-edit
  :doc "Open root parmission files as sudo user"
  :url "https://github.com/nflath/sudo-edit"
  :ensure t)
										;

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


(leaf atomic-chrome
  :doc "Edit Chrome text area with Emacs"
  :url "https://github.com/alpha22jp/atomic-chrome"
  :ensure t
  :hook (after-init-hook . atomic-chrome-start-server)
  :custom ((atomic-chrome-buffer-open-style . 'full)))


(leaf expand-region
  :doc "Extension for region"
  :url "https://github.com/magnars/expand-region.el"
  :ensure t
  :bind ("C-@" . er/expand-region))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 10_misc.el ends here
