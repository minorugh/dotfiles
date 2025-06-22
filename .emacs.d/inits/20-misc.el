;;; 20-misc.e,l --- Misc utilities configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf which-key :tag "builtin"
  :doc "Display available keybindings in popup"
  :config
  (setq which-key-max-description-length 40)
  (setq which-key-delay 0.0)
  :hook after-init-hook)

(leaf quickrun :ensure t
  :bind ([f5] . quickrun))

(leaf key-chord
  :vc (:url "https://github.com/minorugh/key-chord-20240910.1441")
  :config
  :chord (("df" . counsel-descbinds)
	  ("l;" . init-loader-show-log))
  :hook after-init-hook)

(leaf expand-region :ensure t
  :bind ("C-@" . er/expand-region))

(leaf undo-fu :ensure t
  :bind (("C-_" . undo-fu-only-undo)
	 ("C-/" . undo-fu-only-redo)))

(leaf undohist :ensure t
  :doc "Persistent undo history"
  :config
  (setq undohist-directory     "~/.emacs.d/tmp/undohist")
  (setq undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG"))
  :hook (after-init-hook . undohist-initialize))

(leaf projectile :ensure t
  :doc "Manage and navigate projects in Emacs"
  :config
  (setq projectile-known-projects-file "~/.emacs.d/tmp/projectile.eld")
  :hook after-init-hook)

(leaf popwin :ensure t
  :doc "popup window manager for Emacs"
  :hook after-init-hook)

(leaf atomic-chrome :ensure t
  :doc "Edit text areas of the browser in Emacs"
  :hook (after-init-hook . atomic-chrome-start-server)
  :config (setq atomic-chrome-buffer-open-style 'full))

(leaf iedit :ensure t
  :doc "Edit multiple occurrences in the same way simultaneously"
  :bind ("<insert>" . iedit-mode))

(leaf sudo-edit :ensure t
  :doc "Edit currently visited file as another user.")

(leaf ediff
  :doc "Edit while viewing the difference"
  :hook (ediff-mode-hook . dimmer-off)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
	ediff-split-window-function 'split-window-horizontally
	ediff-diff-options "-twB"))

(provide '20-misc)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 20-misc.el ends here
