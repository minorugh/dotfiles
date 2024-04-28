;;; 20_highlight.el --- Highlighting configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *hl-line
  :doc "Highlight the current line"
  :hook ((after-init-hook . global-hl-line-mode)
		 ((dashboard-mode-hook
		   eshell-mode-hook) . (lambda () (setq-local global-hl-line-mode nil)))))


(leaf *whitespace
  :doc "minor mode to visualize TAB, (HARD) SPACE, NEWLINE"
  :tag "Builtin"
  :hook (prog-mode-hook . (lambda () (setq show-trailing-whitespace t)))
  :bind ("C-c C-c" . my:cleanup-for-spaces)
  :custom ((show-trailing-whitespace . nil))
  :preface
  (defun my:cleanup-for-spaces ()
	"Remove contiguous line breaks at end of line + end of file."
	(interactive)
	(delete-trailing-whitespace)
	(save-excursion
      (save-restriction
		(widen)
		(goto-char (point-max))
		(delete-blank-lines)))))


(leaf *paren
  :doc "Highlight matching parens"
  :tag "builtin"
  :hook (after-init-hook . show-paren-mode)
  :custom
  `((show-paren-style . 'parenthesis)
	(show-paren-when-point-inside-paren . t)
	(show-paren-when-point-in-periphery . t))
  :custom-face
  (show-paren-match . '((t (:background "#6272a4" :foreground "#f1fa8c" :weight bold)))))


(leaf volatile-highlights
  :doc "Hilight the pasted region"
  :ensure t
  :url "https://github.com/k-talo/volatile-highlights.el"
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
  :ensure t
  :url "https://www.emacswiki.org/emacs/RainbowDelimiters"
  :hook prog-mode-hook)


(leaf rainbow-mode
  :doc "Color letter that indicate the color"
  :ensure t
  :url "https://elpa.gnu.org/packages/rainbow-mode.html"
  :bind ("C-c w" . rainbow-mode))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_highlight.el ends here
