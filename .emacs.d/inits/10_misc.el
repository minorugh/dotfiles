;;; 10_misc.el --- Misc utility configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; Popup menu-item bindings
(leaf which-key
  :ensure t
  :hook (after-init-hook . which-key-mode)
  :custom (which-key-max-description-length . 40))

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

;; Aggressive indent
(leaf aggressive-indent
  :ensure t
  :hook ((emacs-lisp-mode-hook css-mode-hook) . aggressive-indent-mode))

;; Rainbow-delimiters
(leaf rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

;; atomic-chrome
(leaf atomic-chrome
  :ensure t
  :hook (after-init-hook . atomic-chrome-start-server)
  :custom ((atomic-chrome-buffer-open-style . 'full)))

;; Extension for region
(leaf expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))

;; Sequential-command
(leaf sequential-command
  :el-get HKey/sequential-command
  :config
  (leaf sequential-command-config
	:hook (emacs-startup-hook . sequential-command-setup-keys)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 10_misc.el ends here
