;;; 20_ui.el --- Graphical interface configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf doom-themes
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
  :ensure t
  :hook (after-init-hook . doom-modeline-mode)
  :custom
  `((doom-modeline-icon            . t)
	(doom-modeline-major-mode-icon . nil)
	(doom-modeline-minor-modes     . nil))
  :config
  (line-number-mode 0)
  (column-number-mode 0)
  (leaf nerd-icons :ensure t)
  (leaf hide-mode-line
	:ensure t
	:hook ((imenu-list-major-mode-hook neotree-mode-hook) . hide-mode-line-mode)))


(leaf *display-line-numbers
  :hook ((prog-mode-hook text-mode-hook) . display-line-numbers-mode)
  :bind  ([f9] . display-line-numbers-mode)
  :custom (display-line-numbers-width-start . t))


(leaf page-break-lines
  :ensure t
  :after dashboard
  :hook (after-init-hook . global-page-break-lines-mode)
  :global-minor-mode t)


(leaf *cus-cursor
  :doc "Controls cursor blinking"
  :hook (emacs-startup-hook . blink-cursor-mode)
  :custom
  `((blink-cursor-blinks   . 0)
	(blink-cursor-interval . 0.3)
	(blink-cursor-delay    . 10))
  :init
  (leaf *hide-cursor
	:doc "Hide cursor in inactive window"
	:config
	(setq-default cursor-in-non-selected-windows nil)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 20_ui.el ends here
