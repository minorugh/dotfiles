;;; 20_ui.el --- Better lookings and appearances. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf doom-themes :ensure t
  :doc "Pack of modern color-themes"
  :hook (after-init-hook . (lambda () (load-theme 'doom-dracula t)))
  :config
  (setq doom-themes-enable-italic nil)
  (doom-themes-org-config)
  :custom-face
  (region  . '((t (:background "#6272a4" :extend t))))
  (hl-line . '((t (:background "#3B4252" :extend t)))))

(leaf doom-modeline :ensure t
  :doc "A minimal and modern mode-line"
  :hook after-init-hook
  :config
  (setq doom-modeline-icon            t)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-minor-modes     nil)
  (line-number-mode 0)
  (column-number-mode 0)
  (leaf hide-mode-line :ensure t
    :doc "Hides the mode-line in current buffer"
    :hook (imenu-list-major-mode-hook neotree-mode-hook)))

(leaf display-line-numbers
  :doc "interface for display-line-numbers"
  :tag "builtin"
  :bind  ([f9] . display-line-numbers-mode)
  :config
  (setq display-line-numbers-width-start t)
  :hook (prog-mode-hook text-mode-hook))

(leaf blink-cursor
  :doc "Blinking cursor mode for GNU Emacs"
  :tag "Builtin"
  :config
  (setq blink-cursor-blinks   0)
  (setq blink-cursor-interval 0.3)
  (setq blink-cursor-delay    10))

(leaf nerd-icons :ensure t
  :config
  (leaf nerd-icons-dired :ensure t
    :if (display-graphic-p)
    :config
    (setq nerd-icons-scale-factor 0.8)
    :hook dired-mode-hook))

(leaf darkroom :ensure t
  :doc "Remove visual distractions and focus on writing"
  :bind (([f8] . my:darkroom-in)
	 (:darkroom-mode-map
	  ([f8] . my:darkroom-out)))
  :config
  (defun my:darkroom-in ()
    "Enter to the `darkroom-mode'."
    (interactive)
    (diff-hl-mode 0)
    (display-line-numbers-mode 0)
    (darkroom-tentative-mode 1)
    (toggle-frame-fullscreen)
    (setq-local line-spacing .2)
    (evil-emacs-state))
  (defun my:darkroom-out ()
    "Returns from `darkroom-mode' to the previous state."
    (interactive)
    (darkroom-tentative-mode 0)
    (display-line-numbers-mode 1)
    (toggle-frame-fullscreen)
    (setq-local line-spacing 0)
    (evil-normal-state)))

;;; 20_ui.el ends here
