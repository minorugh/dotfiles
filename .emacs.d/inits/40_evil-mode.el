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
		 ("." . select-evil-command)
		 ("b" . evil-scroll-up)
		 ("SPC" . evil-scroll-down)
		 ("M-." . hydra-quick/body)
		 ([home] . open-dashboard)
		 ([muhenkan] . keyboard-quit))
  :config
  ;; Insert state overrides Emacs settings
  (setcdr evil-insert-state-map nil)
  ;; Retain ESC & "C-[" function in insert-state
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-[") 'evil-normal-state)

  ;; Allow for escape even with muhenkan key.
  (define-key key-translation-map [muhenkan] 'evil-escape-or-quit)
  (define-key evil-operator-state-map [muheqkan] 'evil-escape-or-quit)

  (defun toggle-evil-local-mode ()
	"Toggle on and off evil local mode."
	(interactive)
	(if evil-local-mode (evil-local-mode 0)
	  (evil-local-mode 1)))

  (defun evil-escape-or-quit (&optional prompt)
	"Define the function when press Esc key."
	(interactive)
	;; Turn off IME when return to normal-state
	(deactivate-input-method)
	(cond
	 ((or (evil-normal-state-p) (evil-insert-state-p) (evil-visual-state-p)
		  (evil-replace-state-p) (evil-visual-state-p)) [escape])
	 ([muhenkan])))

  (defun select-evil-command ()
	"Select evil commands."
	(interactive)
	(counsel-M-x "evil- ")))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 40_evil-mode.el ends here
