;;; 08_ui.el --- Graphical interface configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf doom-themes
  :doc "Megapack of themes"
  :url "https://github.com/doomemacs/themes"
  :ensure t
  :hook (emacs-startup-hook . (lambda () (load-theme 'doom-dracula t)))
  :custom
  (doom-themes-enable-italic . nil)
  (doom-themes-enable-bold   . nil)
  :config
  (doom-themes-neotree-config)
  (doom-themes-org-config))


(leaf doom-nano-modeline
  :doc "Nice look modeline based on N Λ N O"
  :url "https://github.com/ronisbr/doom-nano-modeline"
  :el-get "ronisbr/doom-nano-modeline"
  :hook (emacs-startup-hook . doom-nano-modeline-mode)
  :custom-face
  (region                                    . '((t (:background "#6272a4" :extend t))))
  (hl-line                                   . '((t (:background "#3B4252" :extend t))))
  (doom-nano-modeline-active-face            . '((t (:foreground "#f8f8f2" :background "#44475a" :weight bold))))
  (doom-nano-modeline-evil-emacs-state-face  . '((t (:foreground "#f4a460" :background "#6272a4" :weight bold))))
  (doom-nano-modeline-evil-normal-state-face . '((t (:foreground "#adff2f" :background "#3cb371" :weight bold))))
  (doom-nano-modeline-evil-visual-state-face . '((t (:foreground "#e0ffff" :background "#4682b4" :weight bold))))
  (doom-nano-modeline-cursor-position-face   . '((t (:foreground "#b0b8d1" :background "#44475a"))))
  (doom-nano-modeline-major-mode-face        . '((t (:foreground "#b0b8d1" :background "#44475a"))))
  (doom-nano-modeline-vc-branch-name-face    . '((t (:foreground "#b0b8d1" :background "#44475a"))))
  :preface
  (leaf hide-mode-line
	:doc "Hide modeline in current buffer"
	:ensure t
	:hook (after-init-hook . global-hide-mode-line-mode)))


(leaf display-line-numbers
  :doc "Show line numbers"
  :hook ((after-init-hook . global-display-line-numbers-mode)
		 ((imenu-list-minor-mode-hook
		   dired-mode-hook
		   neotree-mode-hook
		   lisp-interaction-mode-hook
		   eshell-mode-hook) . (lambda () (display-line-numbers-mode 0))))
  :bind ([f9] . display-line-numbers-mode)
  :custom (display-line-numbers-width-start . t))


(leaf *cus-cursor
  :doc "Controls cursor blinking"
  :hook (emacs-startup-hook . blink-cursor-mode)
  :custom
  `((blink-cursor-blinks . 0)
	(blink-cursor-interval . 0.3)
	(blink-cursor-delay . 10))
  :init
  ;; Hide cursor in inactive window
  (setq-default cursor-in-non-selected-windows nil))


(leaf darkroom
  :doc "Remove visual distractions and focus on writing"
  :url "https://github.com/joaotavora/darkroom"
  :ensure t
  :bind (([f12] . my:darkroom-in)
		 (:darkroom-mode-map
		  ([f12] . my:darkroom-out)))
  :init
  (defun my:darkroom-in ()
	"Enter to the `darkroom-mode'."
	(interactive)
	(diff-hl-mode 0)
	(display-line-numbers-mode 0)
	(darkroom-mode 1)
	(setq-local line-spacing .5))

  (defun my:darkroom-out ()
	"Returns from `darkroom-mode' to the previous state."
	(interactive)
	(darkroom-mode 0)
	(display-line-numbers-mode 1)
	(setq-local line-spacing .1)))


(leaf web-mode
  :doc "Editing web templates"
  :url "http://github.com/fxbois/web-mode"
  :ensure t
  :mode ("\\.js?\\'" "\\.html?\\'" "\\.php?\\'")
  :bind ("s-w" . counsel-web-suggest)
  :custom
  `((web-mode-markup-indent-offset . 2)
	(web-mode-css-indent-offset . 2)
	(web-mode-code-indent-offset . 2)))


(leaf counsel-web
  :doc "Search the Web using Ivy"
  :url "https://github.com/mnewt/counsel-web"
  :ensure t
  :config
  (setq counsel-web-search-action #'browse-url)
  (setq counsel-web-engine 'google)
  (setq counsel-web-search-dynamic-update t))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 08_ui.el ends here
