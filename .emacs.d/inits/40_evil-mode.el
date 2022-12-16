;;; 40_evil-mode.el --- Evil local mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf evil
  :ensure t
  :hook (prog-mode-hook . evil-local-mode)
  :chord ("::" . toggle-evil-local-mode)
  :bind (:evil-normal-state-map
		 ("?" . vim-cheat)
		 ("C-e" . seq-end)
		 ("M-." . hydra-quick/body)
		 ([home] . open-dashboard)
		 ([muhenkan] . keyboard-quit))
  :init (setq evil-undo-system 'undo-fu)
  :config
  ;; Insert state overrides Emacs settings, but ESC makes it work
  (setcdr evil-insert-state-map nil)
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
	 (t [muhenkan])))

  :preface
  (leaf evil-leader
	:ensure t
	:after evil
	:config
	(setq evil-leader/in-all-states 1)
	(global-evil-leader-mode)
	(evil-leader/set-leader "SPC")
	(evil-leader/set-key
	  "SPC" 'keyboard-quit
	  ":" 'shell-command
	  "/" 'kill-this-buffer
	  "_" 'my:delete-other-windows
	  "s" 'swiper-thing-at-point
	  "t" 'gts-do-translate
	  "j" 'dired-jump
	  "e" 'my:eijiro
	  "w" 'my:weblio
	  "g" 'my:google
	  "k" 'my:koujien)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 40_evil-mode.el ends here
