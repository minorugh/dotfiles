;;; 20_ui.el --- Graphical interface configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf doom-themes
  :doc "Pack of modern color-themes"
  :ensure t
  :hook (emacs-startup-hook . (lambda () (load-theme 'doom-dracula t)))
  :custom ((doom-themes-enable-italic . nil))
  :config
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  :custom-face
  (region  . '((t (:background "#6272a4" :extend t))))
  (hl-line . '((t (:background "#3B4252" :extend t)))))


(leaf doom-modeline
  :doc "A minimal and modern mode-line"
  :ensure t
  :custom
  `((doom-modeline-icon            . t)
	(doom-modeline-major-mode-icon . nil)
	(doom-modeline-minor-modes     . nil))
  :hook after-init-hook
  :config
  (line-number-mode 0)
  (column-number-mode 0)
  (leaf nerd-icons :ensure t)
  (leaf hide-mode-line
	:doc "Hides the mode-line in current buffer"
	:ensure t
	:hook (imenu-list-major-mode-hook neotree-mode-hook)))


(leaf display-line-numbers
  :doc "interface for display-line-numbers"
  :tag "builtin"
  :bind  ([f9] . display-line-numbers-mode)
  :custom (display-line-numbers-width-start . t)
  :hook (prog-mode-hook text-mode-hook))


(leaf page-break-lines
  :doc "Display ^L page breaks as tidy horizontal lines"
  :ensure t
  :after dashboard
  :global-minor-mode t
  :hook after-init-hook)


(leaf blink-cursor
  :doc "Blinking cursor mode for GNU Emacs"
  :tag "Builtin"
  :hook (emacs-startup-hook . blink-cursor-mode)
  :custom
  `((blink-cursor-blinks   . 0)
	(blink-cursor-interval . 0.3)
	(blink-cursor-delay    . 10))
  :init
  ;;Hide cursor in inactive window
  (setq-default cursor-in-non-selected-windows nil))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_ui.el ends here
