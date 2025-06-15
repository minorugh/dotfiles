;;; 20-misc.e,l --- Misc utilities configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

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

(leaf web-mode :ensure t
  :doc "Web template editing mode for emacs"
  :mode ("\\.js?\\'" "\\.html?\\'" "\\.php?\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset    2)
  (setq web-mode-code-indent-offset   2))

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
  (with-eval-after-load 'prescient
    (leaf ivy-prescient :ensure t)
    (leaf company-prescient :ensure t)))

(leaf popwin :ensure t
  :doc "popup window manager for Emacs"
  :hook after-init-hook)

(leaf aggressive-indent :ensure t
  :doc "Minor mode to aggressively keep your code always indented"
  :hook emacs-lisp-mode-hook css-mode-hook)

(leaf atomic-chrome :ensure t
  :doc "Edit text areas of the browser in Emacs"
  :hook (after-init-hook . atomic-chrome-start-server)
  :config (setq atomic-chrome-buffer-open-style 'full))

(leaf iedit :ensure t
  :doc "Edit multiple occurrences in the same way simultaneously"
  :bind ("<insert>" . iedit-mode))

(leaf sudo-edit :ensure t
  :doc "Edit currently visited file as another user.")

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 20-misc.el ends here
