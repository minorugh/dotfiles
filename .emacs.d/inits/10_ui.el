;;; 10_ui.el --- Graphical interface configurations. -*- lexical-binding: t -*-
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
	:hook ((imenu-list-major-mode-hook neotree-mode-hook) . hide-mode-line-mode))
  (leaf nyan-mode
	:ensure t
	:if (display-graphic-p)
	:after doom-modeline
	:config
	(nyan-mode 1)
	(nyan-start-animation)))


(leaf display-line-numbers
  :doc "Show line numbers"
  :hook ((after-init-hook . global-display-line-numbers-mode)
		 ((dired-mode-hook
		   neotree-mode-hook
		   lisp-interaction-mode-hook
		   eshell-mode-hook) . (lambda () (display-line-numbers-mode 0))))
  :bind ([f9] . display-line-numbers-mode)
  :custom (display-line-numbers-width-start . t))


(leaf darkroom
  :doc "Remove visual distractions and focus on writing"
  :url "https://github.com/joaotavora/darkroom"
  :ensure t
  :bind (([f12] . my:darkroom-in)
		 (:darkroom-mode-map
		  ([f12] . my:darkroom-out)))
  :init
  (defun my:darkroom-in ()
	"Enter to the `darkroom-mode'."
	(interactive)
	(diff-hl-mode 0)
	(display-line-numbers-mode 0)
	(darkroom-mode 1)
	(setq-local line-spacing .5))

  (defun my:darkroom-out ()
	"Returns from `darkroom-mode' to the previous state."
	(interactive)
	(darkroom-mode 0)
	(display-line-numbers-mode 1)
	(setq-local line-spacing .1)))


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


(leaf sequential-command
  :doc "Many commands into one command"
  :url "https://github.com/HKey/sequential-command/blob/master/sequential-command.el"
  :el-get "HKey/sequential-command"
  :config
  (leaf sequential-command-config
	:hook (after-init-hook . sequential-command-setup-keys)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 10_ui.el ends here
