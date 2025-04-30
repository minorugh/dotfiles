;;; 20_misc.el --- Misc utilities configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf quickrun :ensure t
  :bind ([f6] . quickrun))

(leaf key-chord
  :vc (:url "https://github.com/minorugh/key-chord")
  :hook (after-init-hook . key-chord-mode)
  :chord (("df" . counsel-descbinds)
	  ("l;" . init-loader-show-log))
  :config
  (setq key-chord-two-keys-delay 0.1))

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

(leaf which-key
  :doc "Display available keybindings in popup"
  :hook (after-init-hook . which-key-mode)
  :config
  (setq which-key-max-description-length 40))

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :url "http://www.flycheck.org"
  :ensure t
  :hook ((after-init-hook . global-flycheck-mode)
	 (lisp-interaction-mode-hook . (lambda () (flycheck-mode 0))))
  :bind (:flycheck-mode-map
         ("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error)))

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

(leaf iedit :ensure t
  :doc "Edit multiple occurrences in the same way simultaneously"
  :bind ("<insert>" . iedit-mode))


;;; 20_misc.el ends here
