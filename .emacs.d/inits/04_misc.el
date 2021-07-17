;;; 04_misc.el --- Misc tools configuration. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf popwin
  :ensure t
  :global-minor-mode t)


(leaf iedit
  :ensure t
  :bind ("C-;" . iedit-mode))


(leaf expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))


(leaf flymake
  :hook (prog-mode-hook . flymake-mode)
  :config
  (leaf flymake-diagnostic-at-point	:ensure t
  	:hook (flymake-mode-hook . flymake-diagnostic-at-point-mode)))


(leaf key-chord
  :el-get zk-phi/key-chord
  :global-minor-mode t
  :config
  (key-chord-define-global "df" 'counsel-descbinds)
  (key-chord-define-global "l;" 'init-loader-show-log))


(leaf prescient
  :ensure t
  :config
  (prescient-persist-mode 1)
  (setq prescient-save-file	"~/.emacs.d/tmp/prescient-save")
  :init
  (leaf ivy-prescient :ensure t
	:global-minor-mode ivy-prescient-mode)
  (leaf company-prescient :ensure t
	:global-minor-mode t))


(leaf quickrun
  :ensure t
  :bind ("<f5>" . quickrun))


(leaf which-key
  :ensure t
  :global-minor-mode t
  :config
  (setq which-key-max-description-length 40)
  (setq which-key-use-C-h-commands t))


(add-hook 'emacs-startup-hook
		  (lambda ()
			(leaf projectile
			  :ensure t
			  :global-minor-mode t
			  :init
			  (setq projectile-known-projects-file "~/.emacs.d/tmp/projectile-bookmarks.eld")
			  (leaf counsel-projectile :ensure t
				:global-minor-mode t))))


(leaf aggressive-indent
  :ensure t
  :hook ((emacs-lisp-mode-hook css-mode-hook) . aggressive-indent-mode))


(leaf yasnippet
  :ensure t
  :commands yas-global-mode
  :config
  (yas-global-mode)
  (setq yas-snippet-dirs '("~/Dropbox/emacs.d/snippets"))
  :init
  (leaf yasnippet-snippets :ensure t)
  (leaf ivy-yasnippet :ensure t))


(leaf restart-emacs
  :ensure t
  :bind ("C-x C-c" . restart-emacs))


(leaf web-mode
  :ensure t
  :mode "\\.js?\\'" "\\.html?\\'" "\\.php?\\'")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 04_misc.el ends here
