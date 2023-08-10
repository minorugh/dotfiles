;;; 07_dimmer.el --- Window utility configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf dimmer
  :doc "Visually highlight the selected buffer"
  :url "https://github.com/gonewest818/dimmer.el"
  :ensure t
  :defun (my:dimmer-activate dimmer-process-all dimmer-on dimmer-off)
  :chord (".." . my:toggle-dimmer)
  :custom
  '((dimmer-buffer-exclusion-regexps
	 . '("^ \\*which-key\\|^ \\*Lv\\|\\*compilation*\\|\\*YaTeX-typesetting*\\|\\*Go-Translate*\\|\\magit\\|\\COMMIT_EDITMSG"))
	(dimmer-fraction . 0.5))
  :config
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
	  (dimmer-process-all))))


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
	(other-window 1)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 07_dimmer.el ends here
