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
  (setq dimmer-fraction 0.6)

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
	"With turn on dimmer."
	(interactive)
	(when (one-window-p)
	  (hydra-window/body)
	  (split-window-horizontally)
	  (follow-mode 1)
	  (dimmer-mode 1))
	(other-window 1))

  (defun my:split-window-right ()
	"With turn on dimmer."
	(interactive)
	(split-window-right)
	(follow-mode 1)
	(dimmer-mode 1))

  (defun my:split-window-below ()
	"With turn on dimmer."
	(interactive)
	(split-window-below)
	(follow-mode 1)
	(dimmer-mode 1))

  (defun my:delete-other-windows ()
	"With turn off dimmer."
	(interactive)
	(delete-other-windows)
	(follow-mode -1)
	(dimmer-mode -1))

  (defun my:delete-window ()
	"With turn off dimmer."
	(interactive)
	(delete-window)
	(when (one-window-p)
	  (follow-mode -1)
	  (dimmer-mode -1)))

  (defun kill-other-buffers ()
	"Kill all other buffers."
	(interactive)
	(mapc 'kill-buffer (delq (current-buffer) (buffer-list)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scroll deactive window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf cus-scrroll-window-key-bind
  :bind (("<next>" . my:scroll-other-window)
		 ("<prior>" . my:scroll-other-window-down))
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
	(scroll-other-window-down))

  (defhydra hydra-window
	(:columns nil :foreign-keys run)
	"  "("0" my:delete-window)
	("1" my:delete-other-windows)
	("/" kill-this-buffer)
	("o" other-window-or-split)
	("t" neotree)
	("SPC" nil)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 10_dimmer.el ends here
