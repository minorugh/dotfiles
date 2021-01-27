;;; 07_highlight.el --- highlight tools  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf hl-line
  :config
  (make-variable-buffer-local 'global-hl-line-mode)
  (add-hook 'dashboard-mode-hook (lambda() (setq global-hl-line-mode nil)))
  :global-minor-mode global-hl-line-mode)

(leaf paren
  :global-minor-mode show-paren-mode
  :config
  (setq show-paren-delay 0)
  (setq show-paren-style 'mixed))

(leaf beacon
  :ensure t
  :global-minor-mode t
  :config
  (setq beacon-color "yellow"))

(leaf rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(leaf volatile-highlights
  :ensure t
  :global-minor-mode t
  :config
  (with-no-warnings
    (when (fboundp 'pulse-momentary-highlight-region)
      (defun my-vhl-pulse (beg end &optional _buf face)
		"Pulse the changes."
		(pulse-momentary-highlight-region beg end face))
      (advice-add #'vhl/.make-hl :override #'my-vhl-pulse))))

(leaf dimmer
  :ensure t
  :global-minor-mode t
  :config
  (setq dimmer-exclusion-regexp-list
		'(".*Minibuf.*" ".*which-key.*" "*direx:direx.*" "*Messages.*" ".*LV.*" ".*howm.*" ".*magit.*" ".*org.*" "*undo-tree*" "posframe")
		dimmer-fraction 0.5)
  (with-eval-after-load "dimmer"
	(defun dimmer-off ()
	  (dimmer-mode -1)
	  (dimmer-process-all))
	(defun dimmer-on ()
	  (dimmer-mode 1)
	  (dimmer-process-all))
	(add-hook 'focus-out-hook #'dimmer-off)
	(add-hook 'focus-in-hook #'dimmer-on)))

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
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mozc-cand-posframe-normal-face ((t (:background "#282D43" :foreground "#C7C9D1"))))
 '(mozc-cand-posframe-focused-face ((t (:background "#393F60" :foreground "#C7C9D1"))))
 '(mozc-cand-posframe-footer-face ((t (:background "#282D43" :foreground "#454D73"))))
 '(lsp-face-highlight-read ((t (:background "gray21" :underline t))))
 '(lsp-face-highlight-write ((t (:background "gray21" :underline t))))
 '(markdown-code-face ((t (:inherit nil))))
 '(markdown-pre-face ((t (:inherit font-lock-constant-face))))
 '(markup-meta-face ((t (:stipple nil :foreground "gray30" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "unknown" :family "Monospace"))))
 '(show-paren-match ((t (:foreground nil :background nil :underline "SkyBlue" :weight bold))))
 '(symbol-overlay-default-face ((t (:background "gray21" :underline t)))))
(put 'dired-find-alternate-file 'disabled nil)


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 07_highlight.el ends here
