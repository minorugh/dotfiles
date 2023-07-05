;;; 20_misc.el --- Misc utility configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf flymake
  :hook (emacs-startup-hook . my:flymake-hook)
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
  (defun my:flymake-hook ()
	(interactive)
	(add-hook 'prog-mode-hook 'flymake-mode)))


(leaf projectile
  :doc "Project navigation and management library"
  :url "https://github.com/bbatsov/projectile"
  :ensure t
  :hook (after-init-hook . projectile-mode)
  :custom
  (projectile-known-projects-file . "~/.emacs.d/tmp/projectile.eld"))


(leaf prescient
  :doc "Better sorting and filtering"
  :url "https://github.com/raxod502/prescient.el"
  :ensure t
  :hook (after-init-hook . prescient-persist-mode)
  :custom `((prescient-aggressive-file-save . t)
			(prescient-save-file . "~/.emacs.d/tmp/prescient-save"))
  :config
  (leaf ivy-prescient :ensure t :global-minor-mode t)
  (leaf company-prescient :ensure t :global-minor-mode t))


(leaf popwin
  :doc "Popup window manager"
  :url "https://github.com/emacsorphanage/popwin"
  :ensure t
  :hook (after-init-hook . popwin-mode))


(leaf posframe
  :doc "Pop a posframe (just a frame) at point"
  :url "https://github.com/tumashu/posframe"
  :ensure t
  :when window-system)


(leaf adaptive-wrap
  :doc "Wrap long lines for easier viewing"
  :url "https://taipapamotohus.com/post/adaptive-wrap/"
  :ensure t
  :config
  (setq-default adaptive-wrap-extra-indent 1)
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
  (global-visual-line-mode +1)
  (add-hook 'org-mode-hook 'visual-line-mode))


(leaf aggressive-indent
  :doc "Keeps your code always indented"
  :url "https://github.com/Malabarba/aggressive-indent-mode"
  :ensure t
  :hook ((emacs-lisp-mode-hook css-mode-hook) . aggressive-indent-mode))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_misc.el ends here
