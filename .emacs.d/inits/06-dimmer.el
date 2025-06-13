;;; 06-dimmer.el --- Dimmer configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf dimmer :ensure t
  :doc "Visually highlight the selected buffer"
  :chord ("::" . my:toggle-dimmer)
  :hook ((emacs-startup-hook . dimmer-excludes)
	 (minibuffer-setup-hook . dimmer-off)
	 (minibuffer-exit-hook  . dimmer-on)
	 (imenu-list-major-mode-hook . dimmer-off))
  :config
  (setq dimmer-buffer-exclusion-regexps
	'("^ \\*compilation*\\|\\*YaTeX-typesetting*\\|\\*Ilist*\\|\\*gt-result*"))
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
      (dimmer-process-all)))


  (leaf *cus-window-split
    :doc "Smart window splitting and moving"
    :bind ("C-q" . other-window-or-split)
    :init
    (defun other-window-or-split ()
      "If there is one window, open split window.
If there are two or more windows, it will go to another window."
      (interactive)
      (when (one-window-p)
	(split-window-horizontally))
      (other-window 1))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 06-dimmer.el ends here
