;;; 07_highlight.el --- Highlighting configurations. -*- lexical-binding: t -*-
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


(leaf smartparens
  :doc "dealing with pairs in Emacs"
  :url "https://github.com/Fuco1/smartparens"
  :ensure t
  :hook (after-init-hook . smartparens-global-mode)
  :config (require 'smartparens-config))


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


(leaf *highlight-whitespace
  :doc "Highligh trailing whitespace"
  :hook (prog-mode-hook . my:enable-trailing-mode)
  :bind ("C-c C-c" . my:cleanup-for-spaces)
  :custom
  (show-trailing-whitespace . nil)
  :init
  (defun my:enable-trailing-mode ()
	"Show tail whitespace."
	(setq show-trailing-whitespace t))

  (defun my:cleanup-for-spaces ()
	"Remove contiguous line breaks at end of line + end of file."
	(interactive)
	(delete-trailing-whitespace)
	(save-excursion
	  (save-restriction
		(widen)
		(goto-char (point-max))
		(delete-blank-lines)))))


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


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 07_highlight.el ends here
