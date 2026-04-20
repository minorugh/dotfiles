;;; 01-ui.el --- Better lookings and appearances. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf doom-themes
  :ensure t
  :doc "Pack of modern color-themes."
  :hook (after-init-hook . (lambda () (load-theme 'doom-dracula t)))
  :config
  (setq doom-themes-enable-italic nil)
  (doom-themes-org-config))

(leaf hl-line
  :ensure nil
  :tag "builtin"
  :doc "Highlight the current line."
  :hook ((after-init-hook . global-hl-line-mode)
         (dashboard-mode-hook
          . (lambda () (setq-local global-hl-line-mode nil))))
  :config
  (custom-set-faces
   '(region  ((t (:background "#6272a4" :extend t))))
   '(hl-line ((t (:background "#3B4252" :extend t))))))

(leaf blink-cursor
  :ensure nil
  :tag "builtin"
  :doc "Blinking cursor mode for GNU Emacs."
  :config
  (setq blink-cursor-blinks   0)
  (setq blink-cursor-interval 0.3)
  (setq blink-cursor-delay    10))

(leaf doom-modeline
  :ensure t
  :doc "A minimal and modern mode-line."
  :hook (after-init-hook . doom-modeline-mode)
  :config
  (setq doom-modeline-icon            t)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-minor-modes     nil)
  (line-number-mode   0)
  (column-number-mode 0))

(leaf hide-mode-line
  :ensure t
  :doc "Hides the mode-line in current buffer."
  :hook ((imenu-list-major-mode-hook . hide-mode-line-mode)
         (neotree-mode-hook          . hide-mode-line-mode)))

(leaf imenu-list
  :ensure t
  :doc "Show imenu entries in a separate buffer."
  :bind (([f2] . imenu-list-smart-toggle)
         (:imenu-list-major-mode-map
          ("j" . next-line)
          ("k" . previous-line)))
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t)
  (setq imenu-list-position 'left))

(leaf nerd-icons
  :ensure t
  :if (display-graphic-p))

(leaf nerd-icons-dired
  :ensure t
  :hook (dired-mode-hook . nerd-icons-dired-mode)
  :config (setq nerd-icons-scale-factor 0.8))

(leaf display-line-numbers
  :ensure nil
  :tag "builtin"
  :doc "Interface for display-line-numbers."
  :bind ([f9] . display-line-numbers-mode)
  :hook ((prog-mode-hook . display-line-numbers-mode)
         (prog-mode-hook . goto-address-prog-mode)
         (text-mode-hook . display-line-numbers-mode)
         (lisp-interaction-mode-hook
          . (lambda () (display-line-numbers-mode 0))))
  :config (setq display-line-numbers-width-start t))

(leaf display-fill-column-indicator
  :ensure nil
  :tag "builtin"
  :doc "Indicate maximum column."
  :hook ((gfm-mode-hook  . display-fill-column-indicator-mode)
         (text-mode-hook . display-fill-column-indicator-mode))
  :config
  (setopt display-fill-column-indicator-column 79)
  (setq-default display-fill-column-indicator-character ?│))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 01-ui.el ends here
