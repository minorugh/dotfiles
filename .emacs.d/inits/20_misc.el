;;; 20_misc.el --- Misc utility configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf projectile
  :doc "Manage and navigate projects in Emacs"
  :ensure t
  :config
  (setq projectile-known-projects-file "~/.emacs.d/tmp/projectile.eld")
  :hook after-init-hook)


(leaf prescient
  :doc "Better sorting and filtering"
  :ensure t
  :hook (after-init-hook . prescient-persist-mode)
  :config
  (setq prescient-aggressive-file-save t)
  (setq prescient-save-file "~/.emacs.d/tmp/prescient-save")
  (leaf ivy-prescient :ensure t :global-minor-mode t)
  (leaf company-prescient :ensure t :global-minor-mode t))


(leaf popwin
  :doc "popup window manager for Emacs"
  :ensure t
  :hook after-init-hook)


(leaf posframe
  :doc "Pop a posframe (just a frame) at point"
  :ensure t
  :when window-system)


(leaf aggressive-indent
  :doc "Minor mode to aggressively keep your code always indented"
  :ensure t
  :hook (emacs-lisp-mode-hook css-mode-hook))


(leaf flycheck
  :doc "On-the-fly syntax checking"
  :ensure t
  :config
  (setq flycheck-emacs-lisp-initialize-packages t)
  :hook  prog-mode-hook)


(leaf imenu-list
  :doc "Show imenu entries in a separate buffer"
  :ensure t
  :bind ([f2]  . imenu-list-smart-toggle)
  :config
  (setq imenu-list-auto-resize t)
  (setq imenu-list-position    'left)
  :preface
  (leaf counsel-css
    :doc "stylesheet-selector-aware swiper"
    :ensure t
    :hook (css-mode-hook . counsel-css-imenu-setup)))


(leaf web-mode
  :doc "Web template editing mode for emacs"
  :ensure t
  :mode ("\\.js?\\'" "\\.html?\\'" "\\.php?\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset    2)
  (setq web-mode-code-indent-offset   2))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_misc.el ends here
