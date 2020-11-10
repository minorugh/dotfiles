;;; 06_highlight.el --- highlight tools  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)

(leaf smartparens
  :ensure t
  :global-minor-mode smartparens-global-mode
  :config
  (sp-pair "`" nil :actions :rem)
  (sp-pair "'" nil :actions :rem)
  (sp-pair "[" nil :actions :rem))


(leaf aggressive-indent
  :ensure t
  :hook ((emacs-lisp-mode-hook css-mode-hook) . aggressive-indent-mode))


(leaf beacon
  :ensure t
  :global-minor-mode t
  :config
  (setq beacon-color "yellow"))


(leaf rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))


(leaf rainbow-mode
  :ensure t
  :bind ("C-c r" . rainbow-mode))


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


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 06_highlight.el ends here
