;;; 08_misc.e,l --- Misc utilities configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf quickrun :ensure t
  :bind ([f5] . quickrun))

(leaf key-chord
  :vc (:url "https://github.com/minorugh/key-chord-10240910.1441")
  :hook (after-init-hook . key-chord-mode)
  :chord (("df" . counsel-descbinds)
	  ("l;" . init-loader-show-log)))

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

(leaf atomic-chrome :ensure t
  :doc "Edit text areas of the browser in Emacs"
  :hook (after-init-hook . atomic-chrome-start-server)
  :custom (atomic-chrome-buffer-open-style . 'full))

(leaf google-this :ensure t
  :doc "doc"
  :config
  (defun my:google-this ()
    "Run without confirmation"
    (interactive)
    (google-this (current-word) t)))


;;; 08_misc.el ends here

