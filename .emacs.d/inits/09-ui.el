;;; 09-ui.el --- Better lookings and appearances. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf doom-themes :ensure t
  :doc "Pack of modern color-themes"
  :hook (after-init-hook . (lambda () (load-theme 'doom-dracula t)))
  :config
  (setq doom-themes-enable-italic nil)
  (doom-themes-org-config)
  :preface
  (leaf hl-line
    :doc "Highlight the current line"
    :tag "Builtin"
    :hook ((after-init-hook . global-hl-line-mode)
	   ((dashboard-mode-hook eshell-mode-hook)
	    . (lambda () (setq-local global-hl-line-mode nil))))
    :custom-face
    (region  . '((t (:background "#6272a4" :extend t))))
    (hl-line . '((t (:background "#3B4252" :extend t))))))

(leaf doom-modeline :ensure t
  :doc "A minimal and modern mode-line"
  :hook after-init-hook
  :config
  (setq doom-modeline-icon            t)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-minor-modes     nil)
  (line-number-mode 0)
  (column-number-mode 0)
  :preface
  (leaf nyan-mode :ensure t
    :doc "Shows position in current buffer in mode-line"
    :global-minor-mode t
    :config
    (setq nyan-animate-nyancat t)
    (setq nyan-bar-length 24))
  (leaf hide-mode-line :ensure t
    :doc "Hides the mode-line in current buffer"
    :hook (imenu-list-major-mode-hook neotree-mode-hook))
  (leaf nerd-icons :ensure t
    :if (display-graphic-p))
  (leaf nerd-icons-dired :ensure t
    :config (setq nerd-icons-scale-factor 0.8)
    :hook dired-mode-hook))

(leaf whitespace
  :doc "minor mode to visualize TAB, (HARD) SPACE, NEWLINE"
  :tag "Builtin"
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

(leaf display-line-numbers
  :doc "interface for display-line-numbers"
  :tag "builtin"
  :hook ((prog-mode-hook text-mode-hook)
	 (lisp-interaction-mode-hook . (lambda () (interactive) (display-line-numbers-mode 0))))
  :bind  ([f9] . display-line-numbers-mode)
  :config
  (setq display-line-numbers-width-start t))

(leaf blink-cursor
  :doc "Blinking cursor mode for GNU Emacs"
  :tag "Builtin"
  :config
  (setq blink-cursor-blinks   0)
  (setq blink-cursor-interval 0.3)
  (setq blink-cursor-delay    10))

(leaf beacon :ensure t
  :doc "Indicate the cursor's position."
  :hook after-init-hook)

(leaf volatile-highlights :ensure t
  :doc "Hilight the pasted region"
  :hook after-init-hook
  :custom-face
  (vhl/default-face . '((t (:foreground "#FF3333" :background "#FFCDCD"))))
  :config
  (vhl/define-extension 'my:evil-highlights 'evil-yank 'evil-move 'evil-paste-after)
  (vhl/install-extension 'my:evil-highlights)
  (when (fboundp 'pulse-momentary-highlight-region)
    (defun my:vhl-pulse (beg end &optional _buf face)
      "Pulse the changes."
      (pulse-momentary-highlight-region beg end face))
    (advice-add #'vhl/.make-hl :override #'my:vhl-pulse)))

(leaf paren :tag "builtin"
  :doc "Highlight matching parens"
  :hook (after-init-hook . show-paren-mode)
  :custom-face
  (show-paren-match . '((t (:background "#6272a4" :foreground "#f1fa8c" :weight bold))))
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

(leaf elec-pair :tag "builtin"
  :doc "Automatic parenthesis pairing"
  :hook (after-init-hook . electric-pair-mode))

(leaf rainbow-delimiters :ensure t
  :doc "Display brackets in rainbow"
  :url "https://www.emacswiki.org/emacs/RainbowDelimiters"
  :hook prog-mode-hook)

(leaf rainbow-mode :ensure t
  :doc "Color letter that indicate the color"
  :url "https://elpa.gnu.org/packages/rainbow-mode.html"
  :hook after-init-hook
  :bind ("C-c r" . rainbow-mode))

;; Writing environment
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

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 09-ui.el ends here
