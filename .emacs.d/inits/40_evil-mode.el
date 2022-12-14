;;; 40_evil-mode.el --- Evil local mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf evil
  :ensure t
  :if (display-graphic-p)
  :hook (prog-mode-hook . evil-local-mode)
  :chord ("::" . toggle-evil-local-mode)
  :bind (:evil-normal-state-map
		 ("M-." . hydra-quick/body)
		 ([home] . open-dashboard)
		 ([muhenkan] . keyboard-quit))
  :config
  ;; Insert state overrides Emacs settings
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (define-key key-translation-map [muhenkan] 'evil-escape-or-quit)
  (define-key evil-operator-state-map [muheqkan] 'evil-escape-or-quit)

  (defun toggle-evil-local-mode ()
	"Toggle on and off evil local mode."
	(interactive)
	(if evil-local-mode
		(evil-local-mode 0)
	  (evil-local-mode 1)))

  (defun evil-escape-or-quit (&optional prompt)
	(interactive)
	(deactivate-input-method)
	(cond
	 ((or (evil-normal-state-p) (evil-insert-state-p) (evil-visual-state-p)
		  (evil-replace-state-p) (evil-visual-state-p)) [escape])
	 ([muhenkan]))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 40_evil-mode.el ends here
