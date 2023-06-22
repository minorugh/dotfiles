;;; 08_ui.el --- Graphical interface configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf doom-themes
  :doc "Megapack of themes"
  :url "https://github.com/doomemacs/themes"
  :ensure t
  :hook (after-init-hook . (lambda () (load-theme 'doom-dracula t)))
  :custom
  (doom-themes-enable-italic . nil)
  (doom-themes-enable-bold   . nil)
  :config
  (doom-themes-neotree-config)
  (doom-themes-org-config))


(leaf doom-nano-modeline
  :doc "Modeline for Doom Emacs based on N Λ N O"
  :url "https://github.com/ronisbr/doom-nano-modeline"
  :el-get "ronisbr/doom-nano-modeline"
  :hook (after-init-hook . doom-nano-modeline-mode)
  :config
  (load-file "~/.emacs.d/template/my:doom-nano-modeline.el") ;; Load custom function
  (leaf hide-mode-line
	:ensure t
	:config (global-hide-mode-line-mode 1))
  :custom-face
  (region                                    . '((t (:background "#6272a4"))))
  (hl-line                                   . '((t (:background "#3B4252" :extend t ))))
  (doom-nano-modeline-active-face            . '((t (:inherit mode-line :background "#44475a" :weight bold))))
  (doom-nano-modeline-cursor-position-face   . '((t (:foreground "#b0b8d1" :background "#44475a"))))
  (doom-nano-modeline-evil-emacs-state-face  . '((t (:foreground "#ffa07a" :background "#6272a4" :weight bold))))
  (doom-nano-modeline-evil-normal-state-face . '((t (:foreground "#adff2f" :background "#4682b4" :weight bold))))
  (doom-nano-modeline-evil-visual-state-face . '((t (:foreground "#e0ffff" :background "#6495ed" :weight bold))))
  (doom-nano-modeline-major-mode-face        . '((t (:foreground "#b0b8d1" :background "#44475a"))))
  (doom-nano-modeline-vc-branch-name-face    . '((t (:foreground "#b0b8d1" :background "#44475a")))))


(leaf display-line-numbers
  :doc "Show line numbers"
  :hook ((after-init-hook . global-display-line-numbers-mode)
		 ((imenu-list-minor-mode-hook dired-mode-hook neotree-mode-hook lisp-interaction-mode-hook)
		  . (lambda () (display-line-numbers-mode 0))))
  :bind ([f9] . display-line-numbers-mode)
  :custom (display-line-numbers-width-start . t))


(leaf sequential-command
  :doc "Many commands into one command"
  :url "https://github.com/HKey/sequential-command/blob/master/sequential-command.el"
  :el-get HKey/sequential-command
  :config
  (leaf sequential-command-config
	:hook (after-init-hook . sequential-command-setup-keys)))


(leaf cus-line-spacing
  :doc "Set line spacing"
  :hook (buffer-list-update-hook . my:linespacing)
  :init
  (defun my:linespacing ()
	"Set default linespace."
	(unless (minibufferp)
	  (setq-local line-spacing 0.1))))


(leaf cus-cursor
  :doc "Controls cursor blinking"
  :hook (emacs-startup-hook . blink-cursor-mode)
  :custom
  `((blink-cursor-blinks . 0)
	(blink-cursor-interval . 0.3)
	(blink-cursor-delay . 10))
  :init
  ;; Hide cursor in inactive window
  (setq-default cursor-in-non-selected-windows nil))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 08_ui.el ends here
