;;; 40_evil-mode.el --- Evil local mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf evil
  :ensure t
  :hook ((after-init-hook . evil-mode)
		 (magit-status-mode-hook . my:unlock-evil-mode))
  :chord ("::" . toggle-evil-mode)
  :bind ((:key-translation-map
		  ("<muhenkan>" . evil-escape-or-quit))
		 (:evil-operator-state-map
		  ("<muhenkan>" . evil-escape-or-quit))
		 (:evil-normal-state-map
		  ("M-." . hydra-quick/body)
		  ("<home>" . open-dashboard)))
  :config
  ;; Insert state overrides Emacs settings
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)

  (defun my:unlock-evil-mode ()
	(when evil-mode
      (evil-mode 0)))

  (defun toggle-evil-mode ()
	"Toggle on and off evil mode in local buffer."
	(interactive)
	(if evil-mode
		(evil-mode 0)
	  (evil-mode 1)))

  (defun evil-escape-or-quit (&optional prompt)
	(interactive)
	(deactivate-input-method)
	(cond
	 ((or (evil-normal-state-p) (evil-insert-state-p) (evil-visual-state-p)
		  (evil-replace-state-p) (evil-visual-state-p)) [escape])
	 (t (kbd "<muhenkan>")))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 40_evil-mode.el ends here
