;;; 40_evil-mode.el --- Evil local mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf evil
  :ensure t
  :hook (prog-mode-hook . evil-local-mode)
  :chord ("::" . toggle-evil-local-mode)
  :bind (:evil-normal-state-map
		 ("b" . evil-scroll-up)
		 ("SPC" . evil-scroll-down)
		 ("M-." . hydra-quick/body)
		 ([home] . open-dashboard)
		 ([muhenkan] . keyboard-quit))
  :init
  (setq evil-undo-system 'undo-fu)
  :config
  ;; Insert state overrides Emacs settings
  (setcdr evil-insert-state-map nil)

  ;; Retain ESC & "C-[" function in insert-state
  (define-key evil-insert-state-map [escape] 'my:evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-[") 'my:evil-normal-state)

  ;; Allow for escape even with muhenkan key.
  (define-key key-translation-map [muhenkan] 'evil-escape-or-quit)
  (define-key evil-operator-state-map [muhenkan] 'evil-escape-or-quit)

  (defun toggle-evil-local-mode ()
	"Toggle on and off evil local mode."
	(interactive)
	(if evil-local-mode (evil-local-mode 0)
	  (evil-local-mode 1)))

  (defun turn-off-input-method ()
	"If input-method is on, turn it off."
	(interactive)
	(if current-input-method (deactivate-input-method)))

  (defun my:evil-normal-state ()
	(interactive)
	(turn-off-input-method)
	(evil-normal-state))

  (defun evil-escape-or-quit (&optional prompt)
	"Define the function when press Esc key."
	(interactive)
	(cond
	 ((or (evil-normal-state-p) (evil-insert-state-p) (evil-visual-state-p)
		  (evil-replace-state-p) (evil-visual-state-p)) [escape])
	 ([muhenkan]))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 40_evil-mode.el ends here
