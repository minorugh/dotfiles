;;; init.el --- emacs initial setting  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)

(when (version< emacs-version "27")
  (set-frame-parameter nil 'fullscreen 'maximized)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (setq inhibit-splash-screen t)
  (setq inhibit-startup-message t)
  (load (concat user-emacs-directory "early-init.el")))

(package-initialize)
(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))

(leaf leaf-keywords
  :ensure t
  :init
  (leaf bind-key :ensure t)
  (leaf hydra :ensure t)
  :config
  (leaf-keywords-init)
  (setq load-prefer-newer t)
  (setq custom-file (locate-user-emacs-file "~/.emacs.d/tmp/custom.el")))

(leaf init-loader
  :ensure t
  :config
  (custom-set-variables '(init-loader-show-log-after-init 'error-only))
  (add-hook 'after-init-hook
			(lambda ()
			  (init-loader-load))))


(provide 'init)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; init.el ends here
