;;; 30-ui.el --- Better lookings and appearances. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf doom-themes :ensure t
  :doc "Pack of modern color-themes."
  :hook (after-init-hook . (lambda () (load-theme 'doom-dracula t)))
  :config
  (setq doom-themes-enable-italic nil)
  (doom-themes-org-config))

(leaf hl-line :ensure nil
  :tag "Builtin"
  :doc "Highlight the current line."
  :hook ((after-init-hook . global-hl-line-mode)
	 (dashboard-mode-hook
	  . (lambda () (setq-local global-hl-line-mode nil))))
  :custom-face
  (region  . '((t (:background "#6272a4" :extend t))))
  (hl-line . '((t (:background "#3B4252" :extend t)))))

(leaf doom-modeline :ensure t
  :doc "A minimal and modern mode-line."
  :hook after-init-hook
  :config
  (setq doom-modeline-icon              t)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-minor-modes     nil)
  (line-number-mode   0)
  (column-number-mode 0))

(leaf hide-mode-line :ensure t
  :doc "Hides the mode-line in current buffer."
  :hook imenu-list-major-mode-hook neotree-mode-hook)

(leaf nerd-icons :ensure t
    :if (display-graphic-p))

(leaf nerd-icons-dired :ensure t
    :config (setq nerd-icons-scale-factor 0.8)
    :hook dired-mode-hook)

(leaf display-line-numbers :tag "builtin"
  :doc "interface for display-line-numbers."
  :hook ((prog-mode-hook text-mode-hook)
	 (lisp-interaction-mode-hook
	  . (lambda () (interactive) (display-line-numbers-mode 0))))
  :bind  ([f9] . display-line-numbers-mode)
  :config (setq display-line-numbers-width-start t))

(leaf whitespace :tag "Builtin"
  :doc "minor mode to visualize TAB, (HARD) SPACE, NEWLINE."
  :hook (prog-mode-hook . (lambda () (setq show-trailing-whitespace t)))
  :bind ("C-c C-c" . my:cleanup-for-spaces)
  :config
  (setq show-trailing-whitespace nil)
  :init
  (defun my:cleanup-for-spaces ()
    "Remove contiguous line breaks at end of line + end of file."
    (interactive)
    (delete-trailing-whitespace)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-max))
	(delete-blank-lines)))))

(leaf display-fill-column-indicator-mode :tag "Builtin"
  :doc "Indicate maximum colum."
  :hook gfm-mode-hook text-mode-hook
  :config
  (setopt display-fill-column-indicator-column 79)
  (setq-default display-fill-column-indicator-character ?│))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 30-ui.el ends here
