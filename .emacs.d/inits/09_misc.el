;;; 09_misc.el --- Misc utility configurations. -*- lexical-binding: t -*-
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


;; Flymake
(leaf flymake
  :hook (emacs-lisp-mode-hook . flymake-mode)
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (leaf flymake-posframe
	:el-get Ladicle/flymake-posframe
	:hook (flymake-mode-hook . flymake-posframe-mode)
	:custom
	(flymake-posframe-error-prefix . " ")))


;; Popup menu-item bindings
(leaf which-key
  :ensure t
  :hook (after-init-hook . which-key-mode)
  :custom
  (which-key-max-description-length . 40))


;; imenu-list
(leaf imenu-list
  :ensure t
  :bind ([f2] . imenu-list-smart-toggle)
  :custom
  `((imenu-list-size . 30)
	(imenu-list-auto-resize . t)
	(imenu-list-position . 'left)
	(imenu-list-focus-after-activation . t))
  :config
  (leaf counsel-css
	:ensure t
	:hook (css-mode-hook . counsel-css-imenu-setup)))


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


;; Html editing
(leaf web-mode
  :ensure t
  :mode ("\\.js?\\'" "\\.html?\\'" "\\.php?\\'")
  :custom
  `((web-mode-markup-indent-offset . 2)
	(web-mode-css-indent-offset . 2)
	(web-mode-code-indent-offset . 2)))


;; undo redo
(leaf undo-fu
  :ensure t
  :bind (("C-_" . undo-fu-only-undo)
		 ("C-/" . undo-fu-only-redo)))


(leaf undohist
  :ensure t
  :hook (after-init-hook . undohist-initialize)
  :config
  (setq undohist-directory "~/.emacs.d/tmp/undohist")
  (setq undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG")))


;; Aggressive indent
(leaf aggressive-indent
  :ensure t
  :hook ((emacs-lisp-mode-hook css-mode-hook) . aggressive-indent-mode))


;; Font-awesom
;; (leaf fontawesome
;;   :ensure t
;;   :bind ("s-f" . counsel-fontawesome))


;; atomic-chrome
(leaf atomic-chrome
  :ensure t
  :hook (after-init-hook . atomic-chrome-start-server)
  :custom ((atomic-chrome-buffer-open-style . 'full)))


;; Extension for region
(leaf expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 09_misc.el ends here
