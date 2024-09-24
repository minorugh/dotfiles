;;; init.el --- Emacs first Configuration.
;;; Commentary:
;;
;;; Code:
;; (setq debug-on-error t)

(when (version< emacs-version "29.1")
  (error "This requires Emacs 29.1 and above!"))

;; Speed up startup
(setq gc-cons-threshold most-positive-fixnum)
(defconst my:file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
	    "Recover file name handlers and GC values after startup."
	    (setq file-name-handler-alist my:file-name-handler-alist)
	    (setq gc-cons-threshold 800000)))

;; Package
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  (use-package leaf :ensure t)

  (leaf leaf-keywords :ensure t
    :config
    (leaf-keywords-init)
    :init
    (leaf hydra :ensure t)))

;; Server start
(leaf server
  :commands (server-running-p)
  :hook
  (emacs-startup-hook . (lambda ()
			  (unless (server-running-p)
			    (server-start)))))

;; PATH from shell
(leaf exec-path-from-shell :ensure t
  :when (memq window-system '(mac ns x))
  :hook (emacs-startup-hook . exec-path-from-shell-initialize))

;; User configuration
(leaf *load-user-conf
  :load-path "~/.emacs.d/elisp/"
  :require my:browse my:dired my:template)

;; Init loader
(leaf init-loader :ensure t
  :config
  (custom-set-variables
   '(init-loader-show-log-after-init 'error-only))
  (init-loader-load))

;; Write any customizations to a temp file so they are discarded.
(setq custom-file "~/.emacs.d/tmp/custom.el")


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(init-loader-show-log-after-init 'error-only)
 '(package-vc-selected-packages
   '((deepl-translate :url "https://github.com/minorugh/deepl-translate")
     (easy-hugo :url "https://github.com/minorugh/emacs-easy-hugo")
     (swiper-migemo :url "https://github.com/tam17aki/swiper-migemo"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "#8adf80" :foreground "#333"))) nil "Customized with leaf in `diff-hl' block at `/home/minoru/.emacs.d/inits/02_git.el'")
 '(diff-hl-delete ((t (:background "#ff8f88" :foreground "#333"))) nil "Customized with leaf in `diff-hl' block at `/home/minoru/.emacs.d/inits/02_git.el'")
 '(diff-hl-insert ((t (:background "#bfc9ff" :foreground "#333"))) nil "Customized with leaf in `diff-hl' block at `/home/minoru/.emacs.d/inits/02_git.el'")
 '(hl-line ((t (:background "#3B4252" :extend t))) nil "Customized with leaf in `doom-themes' block at `/home/minoru/.emacs.d/inits/20_ui.el'")
 '(markdown-code-face ((t (:inherit nil :background "gray10"))) nil "Customized with leaf in `markdown-mode' block at `/home/minoru/.emacs.d/inits/50_markdown.el'")
 '(markdown-pre-face ((t (:inherit font-lock-constant-face))) nil "Customized with leaf in `markdown-mode' block at `/home/minoru/.emacs.d/inits/50_markdown.el'")
 '(region ((t (:background "#6272a4" :extend t))) nil "Customized with leaf in `doom-themes' block at `/home/minoru/.emacs.d/inits/20_ui.el'")
 '(show-paren-match ((t (:background "#6272a4" :foreground "#f1fa8c" :weight bold))) nil "Customized with leaf in `*paren' block at `/home/minoru/.emacs.d/inits/10_highlight.el'")
 '(vhl/default-face ((t (:foreground "#FF3333" :background "#FFCDCD"))) nil "Customized with leaf in `volatile-highlights' block at `/home/minoru/.emacs.d/inits/10_highlight.el'"))
