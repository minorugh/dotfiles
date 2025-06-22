;;; 09-frame.el --- Frame controle configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf dimmer :ensure t
  :defun my:dimmer-activate dimmer-process-all
  :doc "Visually highlight the selected buffer"
  :chord ("::"  . my:toggle-dimmer)
  :hook ((emacs-startup-hook . dimmer-excludes)
	 (minibuffer-setup-hook . dimmer-off)
	 (minibuffer-exit-hook  . dimmer-on)
	 (imenu-list-major-mode-hook . dimmer-off))
  :config
  (setq dimmer-buffer-exclusion-regexps
	'("^ \\*compilation*\\|\\*YaTeX-typesetting*\\|\\*Google Translate*\\|\\*Flycheck errors*\\|\\*Ilist*\\|\\*gt-result*"))
  (setq dimmer-fraction 0.5)

  (defun dimmer-excludes ()
    "Settings to suppress the dimmer."
    (interactive)
    (dimmer-configure-which-key)
    (dimmer-configure-magit)
    (dimmer-configure-hydra)
    (dimmer-configure-org))

  (defvar my:dimmer-mode nil)
  (defun my:dimmer-activate ()
    (setq my:dimmer-mode (dimmer-mode 1))
    (remove-hook 'window-configuration-change-hook #'my:dimmer-activate))
  (add-hook 'window-configuration-change-hook #'my:dimmer-activate)

  (defun my:toggle-dimmer ()
    "Toggle dimmer-mode on and off."
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

(leaf *cus-frame-funtions
  :bind  ("C-q" . other-window-or-split)
  :init
  (defun other-window-or-split ()
    "If there is one window, open split window.
If there are two or more windows, it will go to another window."
    (interactive)
    (when (one-window-p)
      (split-window-horizontally))
    (other-window 1))

  (defun handle-delete-frame (event)
    "Overwrite `handle-delete-frame` defined in `frame.el`.
If it's the last frame, minimize it without deleting it."
    (interactive "e")
    (let ((frame  (posn-window (event-start event)))
	  (numfrs (length (visible-frame-list))))
      (cond ((> numfrs 1) (delete-frame frame t))
	    ((iconify-frame))))))

(provide '09-frame)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 09-frame.el ends here
