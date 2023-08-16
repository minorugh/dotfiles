;;; 20_misc.el --- Misc utility configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf projectile
  :ensure t
  :hook (after-init-hook . projectile-mode)
  :custom
  (projectile-known-projects-file . "~/.emacs.d/tmp/projectile.eld"))


(leaf prescient
  :ensure t
  :hook (after-init-hook . prescient-persist-mode)
  :custom `((prescient-aggressive-file-save . t)
			(prescient-save-file . "~/.emacs.d/tmp/prescient-save"))
  :config
  (leaf ivy-prescient :ensure t :global-minor-mode t)
  (leaf company-prescient :ensure t :global-minor-mode t))


(leaf popwin
  :ensure t
  :hook (after-init-hook . popwin-mode))


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


(leaf flycheck
  :ensure t
  :hook (prog-mode-hook . flycheck-mode)
  :custom ((flycheck-emacs-lisp-initialize-packages . t))
  :config
  (eval-and-compile (require 'flycheck))
  ;; Fixing leaf-keywords "Unrecognized keyword" error in flycheck
  (setq flycheck-emacs-lisp-package-initialize-form
		(flycheck-sexp-to-string
		 '(progn
			(with-demoted-errors "Error during package initialization: %S"
              (package-initialize))
			(leaf-keywords-init)))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 20_misc.el ends here
