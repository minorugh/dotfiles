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


(leaf *whitespace
  :hook (prog-mode-hook . (lambda () (setq show-trailing-whitespace t)))
  :bind ("C-c C-c" . my:cleanup-for-spaces)
  :custom ((show-trailing-whitespace . nil))
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


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 20_ui.el ends here
