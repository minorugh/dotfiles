;;; 20_ui.el --- Graphical interface configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf doom-themes
  :ensure t
  :hook (emacs-startup-hook . (lambda () (load-theme 'doom-dracula t)))
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
	:after doom-modeline
	:hook ((imenu-list-major-mode-hook neotree-mode-hook) . hide-mode-line-mode)))


(leaf whitespace
  :hook (prog-mode-hook . my:enable-trailing-mode)
  :bind ("C-c C-c" . my:cleanup-for-spaces)
  :custom
  (show-trailing-whitespace . nil)
  :init
  (defun my:enable-trailing-mode ()
    "Show tail whitespace."
    (setq show-trailing-whitespace t))

  (defun my:cleanup-for-spaces ()
    "Remove contiguous line breaks at end of line + end of file."
    (interactive)
    (delete-trailing-whitespace)
    (save-excursion
      (save-restriction
		(widen)
		(goto-char (point-max))
		(delete-blank-lines)))))


(leaf flycheck
  :ensure t
  :hook (prog-mode-hook . flycheck-mode)
  :bind* (("M-l" . flycheck-list-errors)
		  ("M-n" . flycheck-next-error)
		  ("M-p" . flycheck-previous-error))
  :custom ((flycheck-emacs-lisp-initialize-packages . t))
  :config
  (eval-and-compile (require 'flycheck))
  (setq flycheck-emacs-lisp-package-initialize-form
		(flycheck-sexp-to-string
		 '(progn
			(with-demoted-errors "Error during package initialization: %S"
              (package-initialize))
			(leaf-keywords-init)))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 20_ui.el ends here
