;;; 08_ui.el --- Graphical interface configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf doom-themes
  :doc "Megapack of themes"
  :url "https://github.com/doomemacs/themes"
  :ensure t
  ;; :hook (after-init-hook . (lambda () (load-theme 'doom-dracula t)))
  :hook (after-init-hook . (lambda () (load-theme 'doom-nord t)))
  :custom
  `((doom-themes-enable-italic . nil)
	(doom-themes-enable-bold . nil))
  :config
  (doom-themes-neotree-config)
  (doom-themes-org-config))


(leaf nano-modeline
  :doc "Nice and consistent look theme"
  :url "https://github.com/rougier/nano-emacs"
  :el-get "rougier/nano-emacs"
  :require nano-faces nano-modeline
  :custom
  (frame-background-mode . 'dark)
  (nano-color-foreground . "#f8f8f2")
  (nano-color-background . "#282a36")
  (nano-color-highlight  . "#373844")
  (nano-color-critical   . "#bd93f9")
  (nano-color-salient    . "#0189cc")
  (nano-color-strong     . "#e2e2dc")
  (nano-color-popout     . "#f8f8f2")
  (nano-color-subtle     . "#44475a")
  (nano-color-faded      . "#6272a4")
  :custom-face
  (region                    . '((t (:background "#6272a4"))))
  (hl-line                   . '((t (:background "#3B4252" :extend t ))))
  (vertical-border           . '((t (:background "#282a36" :foreground "#1E2029"))))
  (mode-line                 . '((t (:background "#282a36"))))
  (mode-line-inactive        . '((t (:background "#282a36"))))
  (nano-face-header-salient  . '((t (:foreground "#282a36" :background "#0189cc"))))
  (nano-face-header-popout   . '((t (:foreground "#282a36" :background "#f1fa8c"))))
  (nano-face-header-critical . '((t (:foreground "#282a36" :background "#bd93f9"))))
  (nano-face-header-faded    . '((t (:foreground "#282a36" :background "#6272a4"))))
  (nano-face-subtle          . '((t (:foreground "#282a36" :background "#44475a"))))
  (nano-face-header-default  . '((t (:foreground "#b0b8d1" :background "#44475a"))))
  (nano-face-header-strong   . '((t (:foreground "#f8f8f2" :background "#44475a" :weight bold)))))


(leaf cus-line-spacing
  :doc "Set line spacing"
  :hook (buffer-list-update-hook . my:linespacing)
  :init
  (defun my:linespacing ()
	"Set default linespace."
	(unless (minibufferp)
	  (setq-local line-spacing 0.1))))


(leaf display-line-numbers
  :doc "Show line numbers"
  :hook ((after-init-hook . global-display-line-numbers-mode)
		 ((imenu-list-minor-mode-hook dired-mode-hook neotree-mode-hook lisp-interaction-mode-hook)
		  . (lambda () (display-line-numbers-mode 0))))
  :bind ([f9] . display-line-numbers-mode)
  :custom (display-line-numbers-width-start . t))


(leaf cus-cursor
  :doc "Controls cursor blinking"
  :hook (emacs-startup-hook . blink-cursor-mode)
  :custom
  `((blink-cursor-blinks . 0)
	(blink-cursor-interval . 0.3)
	(blink-cursor-delay . 10))
  :init
  ;; Hide cursor in inactive window
  (setq-default cursor-in-non-selected-windows nil))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 08_ui.el ends here
