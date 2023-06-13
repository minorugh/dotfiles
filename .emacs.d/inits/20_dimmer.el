;;; 20_dimmer.el --- Dimmer mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)


(leaf dimmer
  :ensure t
  :chord (".." . my:toggle-dimmer)
  :config
  (defvar my:dimmer-mode t)
  (setq dimmer-buffer-exclusion-regexps '("^ \\*which-key\\|^ \\*LV\\|^ \\*.*posframe.*buffer.*\\*$"))
  (setq dimmer-fraction 0.3)
  :init
  (defun my:toggle-dimmer ()
	(interactive)
	(unless (one-window-p)
	  (if (setq my:dimmer-mode (not my:dimmer-mode))
		  (dimmer-on)
		(dimmer-off))))

  (defun dimmer-off ()
	(dimmer-process-all)
	(dimmer-mode -1))

  (defun dimmer-on ()
	(when my:dimmer-mode
	  (dimmer-mode 1)
	  (dimmer-process-all))))


;; Split window configuration with dimmer control
(leaf cus-window-command
  :bind (("C-q" . other-window-or-split)
		 ("C-x 1" . my:delete-other-windows)
		 ("C-x 0" . my:delete-window))
  :init
  (defun other-window-or-split ()
	"If there is one window, open split window.
If there are two or more windows, it will go to another window."
	(interactive)
	(when (one-window-p)
	  (if (evil-normal-state-p)
		  (follow-delete-other-windows-and-split)
		(split-window-horizontally))
	  (dimmer-mode 1))
	(other-window 1))

  (defun my:delete-other-windows ()
	"With turn off dimmer."
	(interactive)
	(delete-other-windows)
	(dimmer-mode -1)
	(follow-mode -1))

  (defun my:delete-window ()
	"With turn off dimmer."
	(interactive)
	(delete-window)
	(when (one-window-p)
	  (dimmer-mode -1)
	  (follow-mode -1))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_dimmer.el ends here
