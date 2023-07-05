;;; 20_highlight.el --- Highlighting configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *hl-line
  :doc "Highlight the current line"
  :hook ((after-init-hook . global-hl-line-mode)
		 ((dashboard-mode-hook
		   eshell-mode-hook) . (lambda () (setq-local global-hl-line-mode nil)))))


(leaf *cus-cursor
  :doc "Controls cursor blinking"
  :hook (emacs-startup-hook . blink-cursor-mode)
  :custom
  `((blink-cursor-blinks   . 0)
	(blink-cursor-interval . 0.3)
	(blink-cursor-delay    . 10))
  :init
  (leaf *hide-cursor
	:doc "Hide cursor in inactive window"
	:config
	(setq-default cursor-in-non-selected-windows nil)))


(leaf *paren
  :doc "Highlight matching parens"
  :hook (after-init-hook . show-paren-mode)
  :custom
  `((show-paren-style . 'parenthesis)
	(show-paren-when-point-inside-paren . t)
	(show-paren-when-point-in-periphery . t))
  :custom-face
  (show-paren-match . '((t (:background "#6272a4" :foreground "#f1fa8c" :weight bold)))))


(leaf volatile-highlights
  :doc "Hilight the pasted region"
  :url "https://github.com/k-talo/volatile-highlights.el"
  :ensure t
  :hook (after-init-hook . volatile-highlights-mode)
  :custom-face
  (vhl/default-face . '((t (:foreground "#FF3333" :background "#FFCDCD"))))
  :config
  (when (fboundp 'pulse-momentary-highlight-region)
	(defun my:vhl-pulse (beg end &optional _buf face)
	  "Pulse the changes."
	  (pulse-momentary-highlight-region beg end face))
	(advice-add #'vhl/.make-hl :override #'my:vhl-pulse)))


(leaf rainbow-delimiters
  :doc "Display brackets in rainbow"
  :url "https://www.emacswiki.org/emacs/RainbowDelimiters"
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))


(leaf rainbow-mode
  :doc "Color letter that indicate the color"
  :url "https://elpa.gnu.org/packages/rainbow-mode.html"
  :ensure t
  :bind ("C-c w" . rainbow-mode))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_highlight.el ends here
