;;; 40_evil-mode.el --- Evil local mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf evil
  :ensure t
  :hook (after-init-hook  . evil-mode)
  :chord ("::" . toggle-evil-mode)
  :bind ((:evil-normal-state-map
		  ("?" . evil-tutor-ja-start)
		  ("." . vim-cheat)
		  ("o" . other-window-or-vsplit)
		  ("C-e" . seq-end)
		  ("M-." . hydra-quick/body)
		  ([home] . open-dashboard)
		  ([muhenkan] . keyboard-quit)))
  :init
  ;; move to prev/next line when hl at the end of a line
  (setq evil-cross-lines t)

  ;; Use undo-fu for undo-system
  (setq evil-undo-system 'undo-fu)

  :config
  ;; Insert state overrides Emacs settings, but Esc makes it work
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'my:evil-normal-state)

  ;; Allow for escape even with muhenkan key.
  (define-key key-translation-map [muhenkan] 'evil-escape-or-quit)
  (define-key evil-operator-state-map [muhenkan] 'evil-escape-or-quit)

  ;; Initial state for major mode
  (evil-set-initial-state 'lisp-interaction-mode 'insert)
  (evil-set-initial-state 'fundamental-mode 'insert)
  (evil-set-initial-state 'text-mode 'insert)
  (evil-set-initial-state 'neotree-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs)

  (defun toggle-evil-mode ()
	"Toggle on and off evil mode."
	(interactive)
	(if evil-mode (evil-mode 0)
	  (evil-mode 1)))

  (defun turn-off-input-method ()
	"If input-method is on, turn it off."
	(interactive)
	(if current-input-method (deactivate-input-method)))

  (defun my:evil-normal-state ()
	"Turn off input-method and return to normal-state."
	(interactive)
	(turn-off-input-method)
	(evil-normal-state))

  (defun evil-escape-or-quit (&optional prompt)
	"If in evil any state to escape key, else muhenkan key."
	(interactive)
	(cond
	 ((or (evil-normal-state-p) (evil-insert-state-p) (evil-visual-state-p)
		  (evil-replace-state-p) (evil-visual-state-p)) [escape])
	 (t [muhenkan]))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 40_evil-mode.el ends here
