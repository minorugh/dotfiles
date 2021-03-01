;;; init.el --- Emacs first Configuration. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)

;; Settings for Emacs 26.3
(when (version< emacs-version "27")
  (set-frame-parameter nil 'fullscreen 'maximized)
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (load (concat user-emacs-directory "early-init.el")))

;; Package
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

;; Compile
(setq load-prefer-newer t)
(setq byte-compile-warnings '(cl-functions))
(setq custom-file "~/.emacs.d/tmp/custom.el")

;; Init-loader
(leaf init-loader
  :ensure t
  :hook (after-init-hook . init-loader-load)
  :config
  (setq init-loader-show-log-after-init 'error-only)
  :init
  ;; Load user elisp
  (add-to-list 'load-path "~/.emacs.d/elisp")
  (require 'my:dired)
  (require 'my:template))

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
