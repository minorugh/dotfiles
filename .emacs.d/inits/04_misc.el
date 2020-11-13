;;; 04_misc.el --- misc tools  -*- lexical-binding: t -*-
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


(leaf flycheck
  :ensure t
  :hook (emacs-startup-hook . global-flycheck-mode)
  :config
  (leaf flycheck-title :ensure t
  	:global-minor-mode t)
  (setq flycheck-global-modes
  		'(not text-mode markdown-mode fundamental-mode lisp-interaction-mode
  			  org-mode diff-mode toml-mode web-mode eshell-mode makefile-mode css-mode)))


(leaf key-chord
  :ensure t
  :global-minor-mode t
  :config
  (key-chord-define-global "df" 'counsel-descbinds)
  (key-chord-define-global "l;" 'init-loader-show-log))


(leaf prescient
  :ensure t
  :global-minor-mode prescient-persist-mode
  :init
  (leaf company-prescient :ensure t
    :after prescient company
    :global-minor-mode t)
  (leaf ivy-prescient :ensure t
    :after prescient ivy
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


(leaf projectile
  :ensure t
  :global-minor-mode t
  :init
  (setq projectile-known-projects-file "~/Dropbox/emacs/projectile-bookmarks.eld")
  (leaf counsel-projectile
    :ensure t
	:global-minor-mode t))


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


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 04_misc.el ends here
