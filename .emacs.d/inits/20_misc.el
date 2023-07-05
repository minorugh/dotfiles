;;; 20_misc.el --- Misc utility configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf which-key
  :doc "Displays available keybindings in popup"
  :url "https://github.com/justbur/emacs-which-key"
  :ensure t
  :hook (after-init-hook . which-key-mode)
  :custom ((which-key-max-description-length . 40)))


(leaf popwin
  :doc "Popup window manager"
  :url "https://github.com/emacsorphanage/popwin"
  :ensure t
  :hook (after-init-hook . popwin-mode))


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


(leaf quickrun
  :doc "Qick executes editing buffer"
  :url "https://github.com/emacsorphanage/quickrun"
  :bind ([f6]  . quickrun)
  :ensure t)


(leaf restart-emacs
  :doc "Restart emacs from within emacs"
  :url "https://github.com/iqbalansari/restart-emacs"
  :bind ("C-x C-c"  . restart-emacs)
  :ensure t)


(leaf key-chord
  :doc "Mapping a pair of simultaneously pressed keys"
  :url "https://github.com/emacsorphanage/key-chord"
  :ensure t
  :hook (after-init-hook . key-chord-mode)
  :chord (("df" . counsel-descbinds)
		  ("l;" . init-loader-show-log))
  :custom (key-chord-two-keys-delay . 0.1))


(leaf sequential-command
  :doc "Many commands into one command"
  :url "https://github.com/HKey/sequential-command/blob/master/sequential-command.el"
  :el-get "HKey/sequential-command"
  :config
  (leaf sequential-command-config
	:hook (after-init-hook . sequential-command-setup-keys)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_misc.el ends here
