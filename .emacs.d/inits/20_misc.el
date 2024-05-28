;;; 20_misc.el --- Misc utility configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf projectile :ensure t
  :doc "Manage and navigate projects in Emacs"
  :config
  (setq projectile-known-projects-file "~/.emacs.d/tmp/projectile.eld")
  :hook after-init-hook)


(leaf prescient :ensure t
  :doc "Better sorting and filtering"
  :hook (after-init-hook . prescient-persist-mode)
  :config
  (setq prescient-aggressive-file-save t)
  (setq prescient-save-file "~/.emacs.d/tmp/prescient-save")
  (leaf ivy-prescient :ensure t :global-minor-mode t)
  (leaf company-prescient :ensure t :global-minor-mode t))


(leaf popwin :ensure t
  :doc "popup window manager for Emacs"
  :hook after-init-hook)


(leaf posframe :ensure t
  :doc "Pop a posframe (just a frame) at point"
  :when window-system)


(leaf aggressive-indent :ensure t
  :doc "Minor mode to aggressively keep your code always indented"
  :hook (emacs-lisp-mode-hook css-mode-hook))


(leaf flycheck :ensure t
  :doc "On-the-fly syntax checking"
  :config
  (setq flycheck-emacs-lisp-initialize-packages t)
  :hook  prog-mode-hook)


(leaf imenu-list :ensure t
  :doc "Show imenu entries in a separate buffer"
  :bind ([f2]  . imenu-list-smart-toggle)
  :config
  (setq imenu-list-auto-resize t)
  (setq imenu-list-position    'left)
  :init
  (leaf counsel-css :ensure t
    :doc "stylesheet-selector-aware swiper"
    :hook (css-mode-hook . counsel-css-imenu-setup)))


(leaf web-mode :ensure t
  :doc "Web template editing mode for emacs"
  :mode ("\\.js?\\'" "\\.html?\\'" "\\.php?\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset    2)
  (setq web-mode-code-indent-offset   2))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_misc.el ends here
