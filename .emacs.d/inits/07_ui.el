;;; 07_ui.el --- Graphical interface configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; Theme configuration
(leaf doom-themes
  :ensure t
  :hook (after-init-hook . (lambda () (load-theme 'doom-dracula t)))
  :custom
  `((doom-themes-enable-italic . nil)
	(doom-themes-enable-bold . nil))
  :config
  (doom-themes-neotree-config)
  (doom-themes-org-config))


;; Mode-line
(leaf doom-modeline
  :ensure t
  :hook (after-init-hook . doom-modeline-mode)
  :custom
  `((doom-modeline-icon . t)
	(doom-modeline-major-mode-icon . nil)
	(doom-modeline-minor-modes . nil))
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


;; Set line spacing
(leaf cus-line-spacing
  :hook (buffer-list-update-hook . my:linespacing)
  :init
  (defun my:linespacing ()
	"Set default linespace."
	(unless (minibufferp)
	  (setq-local line-spacing 0.1))))


;; Show line numbers
(leaf display-line-numbers
  :hook ((after-init-hook . global-display-line-numbers-mode)
		 ((imenu-list-minor-mode-hook dired-mode-hook neotree-mode-hook)
		  . (lambda () (display-line-numbers-mode 0))))
  :bind ([f9] . display-line-numbers-mode)
  :custom (display-line-numbers-width-start . t))


;; Controls cursor blinking
(leaf cus-cursor
  :hook (emacs-startup-hook . blink-cursor-mode)
  :custom
  ;; Controls cursor blinking
  `((blink-cursor-blinks . 0)
	(blink-cursor-interval . 0.3)
	(blink-cursor-delay . 10))
  :config
  ;; Hide cursor in inactive window
  (setq-default cursor-in-non-selected-windows nil))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 07_ui.el ends here
