;;; 10_misc.el --- Misc utility configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; Effective sorting
(leaf prescient
  :ensure t
  :hook (after-init-hook . prescient-persist-mode)
  :custom
  `((prescient-aggressive-file-save . t)
	(prescient-save-file . "~/.emacs.d/tmp/prescient-save"))
  :config
  (leaf ivy-prescient :ensure t :global-minor-mode t)
  (leaf company-prescient :ensure t :global-minor-mode t))


;; Key Chord
(leaf key-chord
  :ensure t
  :hook (after-init-hook . key-chord-mode)
  :chord (("df" . counsel-descbinds)
		  ("l;" . init-loader-show-log)
		  ("@@" . howm-list-all))
  :custom (key-chord-two-keys-delay . 0.1))


;; Popup window
(leaf popwin
  :ensure t
  :hook (after-init-hook . popwin-mode))


;; Edit multiple regions
(leaf iedit
  :ensure t
  :bind ([insert] . iedit-mode))


;; Run command quickly
(leaf quickrun
  :ensure t
  :bind ([f6] . quickrun))


;; Restart emacs
(leaf restart-emacs
  :ensure t
  :bind ("C-x C-c" . restart-emacs))


;; sudo-edit
(leaf sudo-edit
  :ensure t)
										;

;; undo redo
(leaf undo-fu
  :ensure t
  :bind (("C-_" . undo-fu-only-undo)
		 ("C-/" . undo-fu-only-redo)))


;; undohist
(leaf undohist
  :ensure t
  :hook (after-init-hook . undohist-initialize)
  :config
  (setq undohist-directory "~/.emacs.d/tmp/undohist")
  (setq undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG")))


;; atomic-chrome
(leaf atomic-chrome
  :ensure t
  :hook (after-init-hook . atomic-chrome-start-server)
  :custom ((atomic-chrome-buffer-open-style . 'full)))


;; Extension for region
(leaf expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))


;; Html editing
(leaf web-mode
  :ensure t
  :mode ("\\.js?\\'" "\\.html?\\'" "\\.php?\\'")
  :bind ("s-w" . counsel-web-suggest)
  :custom
  `((web-mode-markup-indent-offset . 2)
	(web-mode-css-indent-offset . 2)
	(web-mode-code-indent-offset . 2)))


;; counsel-web-search with migemo
(leaf counsel-web
  :ensure t
  :config
  (setq counsel-web-search-action #'browse-url)
  (setq counsel-web-engine 'google)
  (setq counsel-web-search-dynamic-update t))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 10_misc.el ends here
