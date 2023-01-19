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
  :init
  (with-eval-after-load 'prescient
	(leaf ivy-prescient :ensure t :global-minor-mode t)
	(leaf company-prescient :ensure t :global-minor-mode t)))


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
  :bind ("<f2>" . imenu-list-smart-toggle)
  :custom
  `((imenu-list-size . 30)
	(imenu-list-auto-resize . t)
	(imenu-list-position . 'left)
	(imenu-list-focus-after-activation . t)))


;; Popup window
(leaf popwin
  :ensure t
  :hook (after-init-hook . popwin-mode))


;; Edit multiple regions
(leaf iedit
  :ensure t
  :bind ("<insert>" . iedit-mode))


;; Run command quickly
(leaf quickrun
  :ensure t
  :bind ("<f6>" . quickrun))


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


;; sudo-edit
(leaf sudo-edit
  :ensure t
  :bind ("C-x s" . sudo-edit))


;; Aggressive indent
(leaf aggressive-indent
  :ensure t
  :hook ((emacs-lisp-mode-hook css-mode-hook) . aggressive-indent-mode))


;; Font-awesom
(leaf fontawesome
  :ensure t
  :bind ("s-f" . counsel-fontawesome))


;; atomic-chrome
(leaf atomic-chrome
  :ensure t
  :hook (after-init-hook . atomic-chrome-start-server)
  :custom ((atomic-chrome-buffer-open-style . 'full)))


;; Sequential-command
(leaf sequential-command
  :el-get HKey/sequential-command
  :config
  (leaf sequential-command-config
	:hook (emacs-startup-hook . sequential-command-setup-keys)))


;; Extension for region
(leaf expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))


;; Key Chord
(leaf key-chord
  :ensure t
  :hook (after-init-hook . key-chord-mode)
  :chord (("df" . counsel-descbinds)
		  ("l;" . init-loader-show-log)
		  ("@@" . howm-list-all))
  :custom
  `((key-chord-two-keys-delay . 0.15)
	(key-chord-safety-interval-backward . 0.1)
	(key-chord-safety-interval-forward  . 0.25)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 09_misc.el ends here
