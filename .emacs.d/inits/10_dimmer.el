;;; 10_dimmer.el --- Dimmer configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf dimmer
  :ensure t
  :chord (".." . my:toggle-dimmer)
  :config
  (defvar my:dimmer-mode 1)
  (setq dimmer-buffer-exclusion-regexps '("^ \\*which-key\\|^ \\*LV\\|^ \\*.*posframe.*buffer.*\\*$"))
  (setq dimmer-fraction 0.3)
  ;; Hide cursor in inactive window
  (setq-default cursor-in-non-selected-windows nil)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Split window configuration with dimmer control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf cus-sprit-window-key-bind
  :bind (("C-q" . other-window-or-split)
		 ("C-x 3" . my:split-window-right)
		 ("C-x 2" . my:split-window-below)
		 ("C-x 1" . my:delete-other-windows)
		 ("C-x 0" . my:delete-window))
  :init
  (defun other-window-or-split ()
	"If there is one window, open split window.
If there are two or more windows, it will go to another window."
	(interactive)
	(when (one-window-p)
	  (split-window-horizontally)
	  (dimmer-mode 1)
	  (hydra-ctrl-x/body))
	(other-window 1)
	(hydra-ctrl-x/body))

  (defun my:split-window-right ()
	"With turn on dimmer."
	(interactive)
	(split-window-right)
	(dimmer-mode 1))

  (defun my:split-window-below ()
	"With turn on dimmer."
	(interactive)
	(split-window-below)
	(dimmer-mode 1))

  (defun my:delete-other-windows ()
	"With turn off dimmer."
	(interactive)
	(delete-other-windows)
	(dimmer-mode -1))

  (defun my:delete-window ()
	"With turn off dimmer."
	(interactive)
	(delete-window)
	(when (one-window-p)
	  (dimmer-mode -1)))

  (defun kill-other-buffers ()
	"Kill all other buffers."
	(interactive)
	(mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

  (defhydra hydra-ctrl-x (ctl-x-map "" :pre (widen))
	"ctrl-x"
	("0" delete-window)
	("1" delete-other-windows)
	("2" split-window-below)
	("3" split-window-right)
	("o" other-window-or-split)
	(":" counsel-switch-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scroll deactive window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf cus-scrroll-window-key-bind
  :bind (("C-<next>" . my:scroll-other-window)
		 ("C-<prior>" . my:scroll-other-window-down))
  :init
  (defun my:scroll-other-window ()
	"If there are two windows, `scroll-other-window'."
	(interactive)
	(when (one-window-p)
	  (scroll-up))
	(scroll-other-window))

  (defun my:scroll-other-window-down ()
	"If there are two windows, `scroll-other-window-down'."
	(interactive)
	(when (one-window-p)
	  (scroll-down))
	(scroll-other-window-down)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 10_dimmer.el ends here
