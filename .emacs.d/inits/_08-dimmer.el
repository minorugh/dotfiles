;;; 08-dimmer.el --- Dimmer configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf dimmer :ensure t
  :doc "Visually highlight the selected buffer."
  :chord ("::" . my-toggle-dimmer)
  :hook ((emacs-startup-hook       . dimmer-excludes)
	 (minibuffer-setup-hook     . dimmer-off)
	 (minibuffer-exit-hook      . dimmer-on)
	 (imenu-list-major-mode-hook . dimmer-off))
  :config
  (setq dimmer-buffer-exclusion-regexps
	'("^ \\*compilation\\*"
	  "\\*YaTeX-typesetting\\*"
	  "\\*Google Translate\\*"
	  "\\*Flycheck errors\\*"
	  "\\*Ilist\\*"
	  "\\*howmC\\*"
	  "\\*gt-result\\*"))
  (setq dimmer-fraction 0.5)

  (defun dimmer-excludes ()
    "Settings to suppress the dimmer."
    (interactive)
    (dimmer-configure-which-key)
    (dimmer-configure-hydra)
    (dimmer-configure-org))

  (defvar my-dimmer-enabled nil
    "Non-nil when dimmer is active.")

  (defun my-dimmer-activate ()
    (setq my-dimmer-enabled t)
    (dimmer-mode 1)
    (remove-hook 'window-configuration-change-hook #'my-dimmer-activate))
  (add-hook 'window-configuration-change-hook #'my-dimmer-activate)

  (defun my-toggle-dimmer ()
    "Toggle dimmer-mode on and off."
    (interactive)
    (unless (one-window-p)
      (if (setq my-dimmer-enabled (not my-dimmer-enabled))
	  (dimmer-on)
	(dimmer-off))))

  (defun dimmer-off ()
    (dimmer-process-all)
    (dimmer-mode -1))

  (defun dimmer-on ()
    (when my-dimmer-enabled
      (dimmer-mode 1)
      (dimmer-process-all))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 08-dimmer.el ends here
