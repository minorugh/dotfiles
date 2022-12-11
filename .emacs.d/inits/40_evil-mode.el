;;; 40_evil-mode.el --- Evil local mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf evil
  :ensure t
  :chord ("::" . toggle-evil-local-mode)
  :bind ((:key-translation-map
		  ("<muhenkan>" . evil-escape-or-quit))
		 (:evil-operator-state-map
		  ("<muhenkan>" . evil-escape-or-quit))
		 (:evil-normal-state-map
		  (";" . toggle-evil-local-mode)
		  ("M-." . hydra-quick/body)
		  ("<home>" . open-dashboard)
		  ([escape] . keyboard-quit)))
  :config
  (evil-mode 0)
  (defun toggle-evil-local-mode () (interactive)
		 (if evil-local-mode
			 (evil-local-mode 0)
		   (progn
			 (view-mode 0)
			 (evil-local-mode 1))))

  (defun evil-escape-or-quit (&optional prompt)
	(interactive)
	(cond
	 ((or (evil-normal-state-p) (evil-insert-state-p) (evil-visual-state-p)
          (evil-replace-state-p) (evil-visual-state-p)) [escape])
	 (t (kbd "<muhenkan>")))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 40_evil-mode.el ends here
