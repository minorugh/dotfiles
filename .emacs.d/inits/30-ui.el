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
  :tag "builtin"
  :doc "Highlight the current line."
  :hook ((after-init-hook . global-hl-line-mode)
	 (dashboard-mode-hook
	  . (lambda () (setq-local global-hl-line-mode nil))))
  :config
  (custom-set-faces
   '(region  ((t (:background "#6272a4" :extend t))))
   '(hl-line ((t (:background "#3B4252" :extend t))))))

(leaf doom-modeline :ensure t
  :doc "A minimal and modern mode-line."
  :hook (after-init-hook . doom-modeline-mode)
  :config
  (setq doom-modeline-icon            t)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-minor-modes     nil)
  (line-number-mode   0)
  (column-number-mode 0))

(leaf hide-mode-line :ensure t
  :doc "Hides the mode-line in current buffer."
  :hook ((imenu-list-major-mode-hook . hide-mode-line-mode)
	 (neotree-mode-hook           . hide-mode-line-mode)))

(leaf nerd-icons :ensure t
  :if (display-graphic-p))

(leaf nerd-icons-dired :ensure t
  :hook (dired-mode-hook . nerd-icons-dired-mode)
  :config (setq nerd-icons-scale-factor 0.8))

(leaf display-line-numbers :ensure nil
  :tag "builtin"
  :doc "Interface for display-line-numbers."
  :hook ((prog-mode-hook . display-line-numbers-mode)
	 (text-mode-hook . display-line-numbers-mode)
	 (lisp-interaction-mode-hook
	  . (lambda () (display-line-numbers-mode 0))))
  :bind ([f9] . display-line-numbers-mode)
  :config (setq display-line-numbers-width-start t))

(leaf whitespace :ensure nil
  :tag "builtin"
  :doc "Minor mode to visualize TAB, (HARD) SPACE, NEWLINE."
  :bind ("C-c C-c" . my:cleanup-for-spaces-safe)
  :config
  ;; 行末スペース／タブ／NBSP／ゼロ幅スペース＋末尾空行をまとめて削除
  (defun my:cleanup-for-spaces-safe ()
    "Remove trailing spaces, tabs, NBSP, zero-width spaces.
Also remove blank lines at the end of buffer."
    (interactive)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward "[ \t\u00A0\u200B]+$" nil t)
	  (replace-match ""))
	(goto-char (point-max))
	(while (and (not (bobp))
		    (looking-back "^[ \t]*\n" (line-beginning-position 2)))
	  (delete-region (line-beginning-position) (point))))))
  (setq show-trailing-whitespace nil)
  (add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t))))

(leaf display-fill-column-indicator :ensure nil
  :tag "builtin"
  :doc "Indicate maximum column."
  :hook ((gfm-mode-hook  . display-fill-column-indicator-mode)
	 (text-mode-hook . display-fill-column-indicator-mode))
  :config
  (setopt display-fill-column-indicator-column 79)
  (setq-default display-fill-column-indicator-character ?│))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 30-ui.el ends here
