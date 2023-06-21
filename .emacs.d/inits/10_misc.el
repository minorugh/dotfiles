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


(leaf key-chord
  :doc "Mapping a pair of simultaneously pressed keys"
  :url "https://github.com/emacsorphanage/key-chord"
  :ensure t
  :hook (after-init-hook . key-chord-mode)
  :chord (("df" . counsel-descbinds)
		  ("l;" . init-loader-show-log))
  :custom (key-chord-two-keys-delay . 0.1))


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


(leaf quickrun
  :doc "Qick executes editing buffer"
  :url "https://github.com/emacsorphanage/quickrun"
  :ensure t
  :bind ([f6] . quickrun))


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
  :config
  (setq undohist-directory "~/.emacs.d/tmp/undohist")
  (setq undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG")))


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


(leaf web-mode
  :doc "Editing web templates"
  :url "http://github.com/fxbois/web-mode"
  :ensure t
  :mode ("\\.js?\\'" "\\.html?\\'" "\\.php?\\'")
  :bind ("s-w" . counsel-web-suggest)
  :custom
  `((web-mode-markup-indent-offset . 2)
	(web-mode-css-indent-offset . 2)
	(web-mode-code-indent-offset . 2)))


;; counsel-web-search with migemo
(leaf counsel-web
  :doc "Search the Web using Ivy"
  :url "https://github.com/mnewt/counsel-web"
  :ensure t
  :config
  (setq counsel-web-search-action #'browse-url)
  (setq counsel-web-engine 'google)
  (setq counsel-web-search-dynamic-update t))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 10_misc.el ends here
