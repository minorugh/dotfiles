;;; 20_misc.el --- Misc utility configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf popwin
  :doc "Popup window manager"
  :url "https://github.com/emacsorphanage/popwin"
  :ensure t
  :hook (after-init-hook . popwin-mode))


(leaf posframe
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


(leaf sudo-edit
  :doc "Open root parmission files as sudo user"
  :url "https://github.com/nflath/sudo-edit"
  :ensure t)


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_misc.el ends here
