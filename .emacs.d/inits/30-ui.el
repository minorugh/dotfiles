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
  :doc "A minimal and modern mode-line for active/inactive window highlight."
  :hook (after-init-hook . doom-modeline-mode)
  :config
  (setq doom-modeline-icon            t)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-minor-modes     nil)
  (line-number-mode   0)
  (column-number-mode 0)
  ;; Face initialization
  (add-hook
   'doom-modeline-mode-hook
   (lambda ()
     (set-face-attribute 'mode-line          nil :background "unspecified" :foreground "gray")
     (set-face-attribute 'mode-line-inactive nil :background "#852941"     :foreground "white")))
  ;; Reset function when window returns to one
  (defun my-reset-modeline-faces (&rest _)
    (when (= 1 (length (window-list)))
      (set-face-attribute 'mode-line-inactive nil
                          :background "unspecified"
                          :foreground "gray")))
  ;; Triggers reset in two ways
  (add-hook 'window-configuration-change-hook #'my-reset-modeline-faces)
  (advice-add 'delete-window        :after #'my-reset-modeline-faces)
  (advice-add 'delete-other-windows :after #'my-reset-modeline-faces))

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
	 (prog-mode-hook . goto-address-prog-mode)
	 (text-mode-hook . display-line-numbers-mode)
	 (lisp-interaction-mode-hook
	  . (lambda () (display-line-numbers-mode 0))))
  :bind ([f9] . display-line-numbers-mode)
  :config (setq display-line-numbers-width-start t))

(leaf whitespace :ensure nil
  :tag "builtin"
  :doc "Minor mode to visualize whitespace characters."
  :bind ("C-x c" . my-cleanup-for-spaces-safe)
  :config
  (setq show-trailing-whitespace nil)
  (defun my-cleanup-for-spaces-safe ()
    "Clean up trailing whitespace and blank lines at end of buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[ \t\u00A0\u200B]+$" nil t)
	(replace-match ""))
      (delete-trailing-whitespace)))
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
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 30-ui.el ends here
