;;; 20_misc.el --- Misc utility configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf projectile
  :ensure t
  :custom (projectile-known-projects-file . "~/.emacs.d/tmp/projectile.eld")
  :hook after-init-hook)


(leaf prescient
  :ensure t
  :custom `((prescient-aggressive-file-save . t)
			(prescient-save-file . "~/.emacs.d/tmp/prescient-save"))
  :hook (after-init-hook . prescient-persist-mode)
  :config
  (leaf ivy-prescient :ensure t :global-minor-mode t)
  (leaf company-prescient :ensure t :global-minor-mode t))


(leaf popwin
  :ensure t
  :hook after-init-hook)


(leaf posframe
  :ensure t
  :when window-system)


(leaf aggressive-indent
  :ensure t
  :hook ((emacs-lisp-mode-hook css-mode-hook) . aggressive-indent-mode))


(leaf imenu-list
  :ensure t
  :bind ([f2]  . imenu-list-smart-toggle)
  :custom `((imenu-list-auto-resize . t)
			(imenu-list-position    . 'left))
  :preface
  (leaf counsel-css
    :ensure t
    :hook (css-mode-hook . counsel-css-imenu-setup)))


(leaf web-mode
  :ensure t
  :mode ("\\.js?\\'" "\\.html?\\'" "\\.php?\\'")
  :custom `((web-mode-markup-indent-offset . 2)
			(web-mode-css-indent-offset . 2)
			(web-mode-code-indent-offset . 2)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_misc.el ends here
