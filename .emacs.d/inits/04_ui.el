;;; 04_ui.el --- Graphical interface configurations. -*- lexical-binding: t -*-
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
  (column-number-mode 0))

;; nyan-mode
(leaf nyan-mode
  :ensure t
  :when window-system
  :after doom-modeline
  :config
  (nyan-mode 1)
  (nyan-start-animation))

;; Icon
(when (window-system)
  (leaf all-the-icons
	:ensure t
	:after doom-modeline
	:custom
	(all-the-icons-scale-factor . 0.1)
	:config
	(unless (member "all-the-icons" (font-family-list))
	  (all-the-icons-install-fonts t)))

  (leaf all-the-icons-dired
	:el-get jtbm37/all-the-icons-dired
	:after doom-modeline
	:hook (dired-mode-hook . all-the-icons-dired-mode))

  (leaf all-the-icons-ivy-rich
	:ensure t
	:hook (after-init-hook . all-the-icons-ivy-rich-mode))

  (leaf all-the-icons-ibuffer
	:ensure t
	:hook (ibuffer-mode-hook . all-the-icons-ibuffer-mode))
  )


;; Set linespacing
(defun my:linespacing ()
  "Set default linespace."
  (unless (minibufferp)
    (setq-local line-spacing 0.1)))
(add-hook 'buffer-list-update-hook 'my:linespacing)


;; Show line numbers
(leaf display-line-numbers
  :hook (after-init-hook . global-display-line-numbers-mode)
  :bind ("<f9>" . display-line-numbers-mode)
  :custom
  (display-line-numbers-width-start . t))


;; Control cursor blinking
(leaf cus-blink-cursor
  :hook (emacs-startup-hook . blink-cursor-mode)
  :custom
  `((blink-cursor-blinks . 0)
	(blink-cursor-interval . 0.3)
	(blink-cursor-delay . 10)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 04_ui.el ends here
