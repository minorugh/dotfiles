;;; 07_window.el --- Window controle configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

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
	(scroll-other-window-down)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 07_window.el ends here
