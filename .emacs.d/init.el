;;; init.el --- A Fancy and Fast Emacs Configuration.	-*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)

(when (version< emacs-version "27")
  ;; Settings for Emacs 26.3
  (set-frame-parameter nil 'fullscreen 'maximized)
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
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
  (leaf-keywords-init))


(leaf init-loader
  :ensure t
  :config
  (custom-set-variables '(init-loader-show-log-after-init 'error-only))
  (add-hook 'after-init-hook
			(lambda ()
			  (init-loader-load))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
