;;; 07_dimmer.el --- Window utility configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf dimmer :ensure t
  :doc "Visually highlight the selected buffer"
  :chord ("::" . my:toggle-dimmer)
  :config
  (setq dimmer-buffer-exclusion-regexps
	'("^ \\*which-key\\|^ \\*Lv\\|\\*compilation*\\|\\*YaTeX-typesetting*\\|\\*Go-Translate*\\|\\magit\\|\\COMMIT_EDITMSG"))
  (setq dimmer-fraction 0.5)
  (defvar my:dimmer-mode nil)
  (defun my:dimmer-activate ()
    (setq my:dimmer-mode (dimmer-mode 1))
    (remove-hook 'window-configuration-change-hook #'my:dimmer-activate))
  (add-hook 'window-configuration-change-hook #'my:dimmer-activate)

  ;; for swiper/counsel
  (add-hook 'minibuffer-setup-hook 'dimmer-off)
  (add-hook 'minibuffer-exit-hook  'dimmer-on)

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
;; no-byte-compile: t
;; End:
;;; 07_dimmer.el ends here
