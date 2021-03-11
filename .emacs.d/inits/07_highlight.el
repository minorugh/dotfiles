;;; 07_highlight.el --- Highlighting configurations. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf hl-line
  :config
  (global-hl-line-mode 1)
  (make-variable-buffer-local 'global-hl-line-mode)
  (add-hook 'dashboard-mode-hook (lambda() (setq global-hl-line-mode nil))))


(leaf paren
  :config
  (show-paren-mode 1)
  (setq show-paren-delay 0)
  (setq show-paren-style 'mixed))


(leaf smartparens
  :ensure t
  :require smartparens-config
  :hook (prog-mode-hook . turn-on-smartparens-mode)
  :config
  (smartparens-global-mode t))


(leaf rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))


(leaf rainbow-mode
  :ensure t
  :bind ("C-c r" . rainbow-mode))


(leaf volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t)
  (with-no-warnings
	(when (fboundp 'pulse-momentary-highlight-region)
	  (defun my-vhl-pulse (beg end &optional _buf face)
		"Pulse the changes."
		(pulse-momentary-highlight-region beg end face))
	  (advice-add #'vhl/.make-hl :override #'my-vhl-pulse))))


(leaf whitespace
  :ensure t
  :bind ("C-c C-c" . my:cleanup-for-spaces)
  :hook (prog-mode-hook . my:enable-trailing-mode)
  :config
  (setq show-trailing-whitespace nil)
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


(custom-set-faces
 '(lsp-face-highlight-read ((t (:background "gray21" :underline t))))
 '(lsp-face-highlight-write ((t (:background "gray21" :underline t))))
 '(markdown-code-face ((t (:inherit nil))))
 '(markdown-pre-face ((t (:inherit font-lock-constant-face))))
 '(markup-meta-face ((t (:stipple nil :foreground "gray30" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "unknown" :family "Monospace"))))
 '(symbol-overlay-default-face ((t (:background "gray21" :underline t)))))
(put 'dired-find-alternate-file 'disabled nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 07_highlight.el ends here
