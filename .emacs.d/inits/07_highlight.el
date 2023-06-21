;;; 07_highlight.el --- Highlighting configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *paren
  :hook (after-init-hook . show-paren-mode)
  :custom
  `((show-paren-style . 'parenthesis)
	(show-paren-when-point-inside-paren . t)
	(show-paren-when-point-in-periphery . t)))


(leaf smartparens
  :doc "dealing with pairs in Emacs"
  :url "https://github.com/Fuco1/smartparens"
  :ensure t
  :hook ((after-init-hook . smartparens-global-mode)
		 (prog-mode-hook . turn-on-smartparens-mode))
  :config (require 'smartparens-config))


(leaf volatile-highlights
  :doc "Hilight the pasted region"
  :url "https://github.com/k-talo/volatile-highlights.el"
  :ensure t
  :hook (after-init-hook . volatile-highlights-mode))

(when (fboundp 'pulse-momentary-highlight-region)
  (defun my:vhl-pulse (beg end &optional _buf face)
	"Pulse the changes."
	(pulse-momentary-highlight-region beg end face))
  (advice-add #'vhl/.make-hl :override #'my:vhl-pulse))


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


;; (leaf rainbow-mode
;;   :doc "Color letter that indicate the color"
;;   :url "https://elpa.gnu.org/packages/rainbow-mode.html"
;;   :ensure t
;;   :hook (prog-mode-hook . rainbow-mode))


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


;; Custom set face
(custom-set-faces
 '(show-paren-match ((t (:background "#44475a" :foreground "#f1fa8c"))))
 '(vhl/default-face ((t (:foreground "#FF3333" :background "#FFCDCD"))))
 '(lsp-face-highlight-read ((t (:background "gray21" :underline t))))
 '(lsp-face-highlight-write ((t (:background "gray21" :underline t))))
 '(markdown-code-face ((t (:inherit nil))))
 '(markdown-pre-face ((t (:inherit font-lock-constant-face))))
 '(markup-meta-face ((t (:stipple nil :foreground "gray30" :inverse-video nil :box nil
								  :strike-through nil :overline nil :underline nil :slant normal
								  :weight normal :height 135 :width normal :foundry "unknown" :family "Monospace"))))
 '(symbol-overlay-default-face ((t (:background "gray21" :underline t)))))
 (put 'dired-find-alternate-file 'disabled nil)


 ;; Local Variables:
 ;; no-byte-compile: t
 ;; End:
;;; 07_highlight.el ends here
