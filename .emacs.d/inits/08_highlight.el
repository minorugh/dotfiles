;;; 08_highlight.el --- Highlighting configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; Paren
(leaf paren
  :hook (after-init-hook . show-paren-mode)
  :custom
  `((show-paren-style . 'parenthesis)
	(show-paren-when-point-inside-paren . t)
	(show-paren-when-point-in-periphery . t)))


;; Volatile-highlights
(leaf volatile-highlights
  :ensure t
  :hook (after-init-hook . volatile-highlights-mode))

(when (fboundp 'pulse-momentary-highlight-region)
  (defun my:vhl-pulse (beg end &optional _buf face)
	"Pulse the changes."
	(pulse-momentary-highlight-region beg end face))
  (advice-add #'vhl/.make-hl :override #'my:vhl-pulse))


;; Whitespace
(leaf whitespace
  :ensure nil
  :bind ("C-c C-c" . my:cleanup-for-spaces)
  :custom
  (show-trailing-whitespace . nil)
  :init
  (defun my:enable-trailing-mode ()
	"Show tail whitespace."
	(setq show-trailing-whitespace t))
  (add-hook 'prog-mode-hook 'my:enable-trailing-mode)

  (defun my:cleanup-for-spaces ()
	"Remove contiguous line breaks at end of line + end of file."
	(interactive)
	(delete-trailing-whitespace)
	(save-excursion
	  (save-restriction
		(widen)
		(goto-char (point-max))
		(delete-blank-lines)))))


;; Smartparens
(leaf smartparens
  :ensure t
  :hook ((after-init-hook . smartparens-global-mode)
		 (prog-mode-hook . turn-on-smartparens-mode))
  :config (require 'smartparens-config))


;; Rainbow-delimiters
(leaf rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))


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
 '(symbol-overlay-default-face ((t (:background "gray21" :underline t))))
 '(mozc-cand-posframe-normal-face ((t (:background "#1E2029" :foreground "#C7C9D1"))))
 '(mozc-cand-posframe-focused-face ((t (:background "#393F60" :foreground "#C7C9D1"))))
 '(mozc-cand-posframe-footer-face ((t (:background "#1E2029" :foreground "#454D73"))))
 )

(put 'dired-find-alternate-file 'disabled nil)


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 08_highlight.el ends here
