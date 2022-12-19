;;; 40_evil.el --- Evil mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf evil
  :ensure t
  :hook (after-init-hook  . evil-mode)
  :chord ("::" . toggle-evil-mode)
  :bind ((:evil-normal-state-map
		  ("." . chromium-vim-chert)
		  ("C-e" . seq-end)
		  ("M-." . nil)
		  ([home] . open-dashboard)
		  ([muhenkan] . keyboard-quit)))
  :init
  ;; options for Evil, must be written before (require 'evil)
  (setq evil-insert-state-cursor '(bar . 4))
  (setq evil-want-C-u-scroll t)
  (setq evil-cross-lines t)
  (setq evil-ex-search-vim-style-regexp nil)
  (setq evil-search-module 'evil-search)
  (setq evil-undo-system 'undo-fu)
  :config
  ;; Use emacs key bindings in insert state
  (setcdr evil-insert-state-map nil)

  ;; Go back to normal state with ESC
  (define-key evil-insert-state-map [escape] 'my:evil-normal-state)

  ;; Use muhenkan key as ESC
  (define-key key-translation-map [muhenkan] 'evil-escape-or-quit)
  (define-key evil-operator-state-map [muhenkan] 'evil-escape-or-quit)

  ;; Set the initial state for major mode
  (evil-set-initial-state 'lisp-interaction-mode 'insert)
  (evil-set-initial-state 'fundamental-mode 'insert)
  (evil-set-initial-state 'text-mode 'insert)
  (evil-set-initial-state 'easy-hugo-mode 'insert)

  ;; Set the major mode to run in emacs-state
  (add-to-list 'evil-emacs-state-modes 'neotree-mode)
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  (add-to-list 'evil-emacs-state-modes 'dashboard-mode)

  (defun toggle-evil-mode ()
	"Toggle on and off evil mode."
	(interactive)
	(if evil-mode (evil-mode 0)
	  (evil-mode 1)))

  (defun my:evil-normal-state ()
	"Turn off input-method then return to normal-state."
	(interactive)
	(if current-input-method (deactivate-input-method))
	(evil-normal-state))

  (defun evil-escape-or-quit (&optional prompt)
	"If in evil state to ESC, else muhenkan key."
	(interactive)
	(cond
	 ((or (evil-normal-state-p) (evil-insert-state-p) (evil-visual-state-p)
		  (evil-replace-state-p) (evil-visual-state-p)) [escape])
	 (t [muhenkan]))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 40_evil.el ends here
