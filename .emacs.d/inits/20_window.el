;;; 20_winndow.el --- Window utility configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf dimmer
  :doc "Visually highlight the selected buffer"
  :url "https://github.com/gonewest818/dimmer.el"
  :ensure t
  :hook ((minibuffer-setup-hook . dimmer-off)
		 (minibuffer-exit-hook  . dimmer-on))
  :chord (".." . my:toggle-dimmer)
  :config
  (defvar my:dimmer-mode nil)
  (setq dimmer-buffer-exclusion-regexps
		'("^ \\*which-key\\|^ \\*LV\\|\\*Go-Translate*\\|\\COMMIT_EDITMSG"))
  (setq dimmer-fraction 0.5)
  :init
  (defun my:dimmer-activate ()
	(setq my:dimmer-mode (dimmer-mode 1))
	(remove-hook 'window-configuration-change-hook #'my:dimmer-activate))
  (add-hook 'window-configuration-change-hook #'my:dimmer-activate)

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


(leaf *cus-window-functions
  :doc "Split window configuration with dimmer control"
  :bind ("C-q" . other-window-or-split)
  :init
  (defun other-window-or-split ()
	"If there is one window, open split window.
If there are two or more windows, it will go to another window."
	(interactive)
	(when (one-window-p)
	  (split-window-horizontally))
	(other-window 1)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_window.el ends here
