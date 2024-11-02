;;; 20_misc.el --- Misc utilities configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf quickrun :ensure t
  :bind ([f6] . quickrun))


(leaf key-chord :ensure t
  :hook (after-init-hook . key-chord-mode)
  :chord (("df" . counsel-descbinds)
	  ("l;" . init-loader-show-log))
  :config
  (setq key-chord-two-keys-delay 0.1)
  :hook after-init-hook)


(leaf expand-region :ensure t
  :bind ("C-@" . er/expand-region))


(leaf undo-fu :ensure t
  :doc "Undo helper with redo"
  :bind (("C-_" . undo-fu-only-undo)
	 ("C-/" . undo-fu-only-redo)))


(leaf undohist :ensure t
  :doc "Persistent undo history"
  :hook (after-init-hook . undohist-initialize)
  :config
  (setq undohist-directory     "~/.emacs.d/tmp/undohist")
  (setq undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG")))


(leaf which-key :ensure t
  :doc "Display available keybindings in popup"
  :config
  (setq which-key-max-description-length 40)
  :hook after-init-hook)


(leaf projectile :ensure t
  :doc "Manage and navigate projects in Emacs"
  :config
  (setq projectile-known-projects-file "~/.emacs.d/tmp/projectile.eld")
  :hook after-init-hook)


(leaf prescient :ensure t
  :doc "Better sorting and filtering"
  :hook (after-init-hook . prescient-persist-mode)
  :config
  (setq prescient-aggressive-file-save t)
  (setq prescient-save-file "~/.emacs.d/tmp/prescient-save")
  (leaf ivy-prescient :ensure t :global-minor-mode t)
  (leaf company-prescient :ensure t :global-minor-mode t))


(leaf popwin :ensure t
  :doc "popup window manager for Emacs"
  :hook after-init-hook)


(leaf aggressive-indent :ensure t
  :doc "Minor mode to aggressively keep your code always indented"
  :hook (emacs-lisp-mode-hook css-mode-hook))


(leaf iedit :ensure t
  :doc "Edit multiple occurrences in the same way simultaneously"
  :bind ("<insert>" . iedit-mode))


;;; 20_misc.el ends here
