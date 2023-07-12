;;; 20_ui.el --- Graphical interface configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf doom-themes
  :doc "Megapack of themes"
  :url "https://github.com/doomemacs/themes"
  :ensure t
  :hook (emacs-startup-hook . (lambda () (load-theme 'doom-dracula t)))
  :custom
  (doom-themes-enable-italic . nil)
  (doom-themes-enable-bold   . nil)
  :config
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  :custom-face
  (region  . '((t (:background "#6272a4" :extend t))))
  (hl-line . '((t (:background "#3B4252" :extend t)))))


(leaf doom-modeline
  :doc "Fast mode-line inspired by minimalism design."
  :url "https://github.com/seagle0128/doom-modeline"
  :ensure t
  :hook (after-init-hook . doom-modeline-mode)
  :custom
  (doom-modeline-icon            . t)
  (doom-modeline-major-mode-icon . nil)
  (doom-modeline-minor-modes     . nil)
  :config
  (line-number-mode 0)
  (column-number-mode 0)
  (leaf nerd-icons :ensure t)
  (leaf hide-mode-line
	:ensure t
	:after doom-modeline
	:hook ((imenu-list-major-mode-hook neotree-mode-hook) . hide-mode-line-mode)))


(leaf imenu-list
  :doc "Show imenu entries in a separate buffer"
  :url "https://github.com/bmag/imenu-list"
  :ensure t
  :bind ([f2]  . imenu-list-smart-toggle)
  :custom
  (imenu-list-auto-resize . t)
  (imenu-list-position    . 'left)
  :preface
  (leaf counsel-css
	:ensure t
	:hook (css-mode-hook . counsel-css-imenu-setup)))


(leaf *follow-mode
  :doc "Scroll two windows displaying as one virtual window"
  :bind ([f8] . follow-mode))


(leaf *display-line-numbers
  :doc "Show line numbers"
  :hook ((after-init-hook . global-display-line-numbers-mode)
		 ((dired-mode-hook
		   neotree-mode-hook
		   lisp-interaction-mode-hook
		   eshell-mode-hook) . (lambda () (display-line-numbers-mode -1))))
  :bind ([f9] . display-line-numbers-mode)
  :custom (display-line-numbers-width-start . t))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 20_ui.el ends here
