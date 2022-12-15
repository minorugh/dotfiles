;;; 40_evil-mode.el --- Evil local mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf evil
  :ensure t
  :if (display-graphic-p)
  :hook (prog-mode-hook . evil-mode)
  :chord ("::" . toggle-evil-mode)
  :bind (:evil-normal-state-map
		 ("b" . evil-scroll-up)
		 ("SPC" . evil-scroll-down)
		 ("M-." . hydra-quick/body)
		 ([home] . open-dashboard)
		 ([muhenkan] . keyboard-quit))
  :config
  ;; Insert state overrides Emacs settings
  (setcdr evil-insert-state-map nil)

  ;; Retain ESC function in insert-state
  (define-key evil-insert-state-map [escape] 'evil-normal-state)

  ;; Allow for escape even with muhenkan key.
  (define-key key-translation-map [muhenkan] 'evil-escape-or-quit)
  (define-key evil-operator-state-map [muheqkan] 'evil-escape-or-quit)

  (defun toggle-evil-mode ()
	"Toggle on and off evil local mode."
	(interactive)
	(if evil-mode (evil-mode 0)
	  (evil-mode 1)))

  (defun evil-escape-or-quit (&optional prompt)
	"Define the function when press Esc key."
	(interactive)
	;; Turn off IME when return to normal-state
	(deactivate-input-method)
	(cond
	 ((or (evil-normal-state-p) (evil-insert-state-p) (evil-visual-state-p)
		  (evil-replace-state-p) (evil-visual-state-p)) [escape])
	 ([muhenkan])))

  (defun my:unlock-evil-mode ()
	"Disable-evil-mode."
	(interactive)
	(evil-mode 0))

  ;; Hook up unlock-evil-mode collectively
  (defvar unlock-evil-hooks
	'(dashboard-mode-hook
	  magit-status-mode-hook
	  markdown-mode-hook
	  org-mode-hook
	  text-mode-hook
	  neotree-mode-hook))
  (cl-loop for hook in unlock-evil-hooks
		   do (add-hook hook 'my:unlock-evil-mode)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 40_evil-mode.el ends here
