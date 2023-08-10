;;; 08_evil.el --- Evil mode configurations.
;;; Commentary:
;;; code:
;; (setq debug-on-error t)

(leaf evil
  :doc "The extensible vi layer for Emacs"
  :url "https://github.com/emacs-evil/evil"
  :ensure t
  :defun (evil-swap-key ad:switch-to-buffer)
  :hook ((after-init-hook . evil-mode)
		 (find-file-hook  . my:evil-find-file))
  :bind ((:evil-normal-state-map
		  ("C-e"  . seq-end) ;; sequential-command
		  ("SPC"  . evil-insert)
		  ("C-@"  . er/expand-region)
		  ("M-."  . nil)
		  ("@"    . other-window-or-split)
		  ("?"    . chromium-vim-chert)
		  ([home] . open-dashboard))
		 (:evil-visual-state-map
		  ([muhenkan] . my:return-to-normal-state)
		  ("SPC"      . hydra-selected/body))
		 (:evil-emacs-state-map
		  ([muhenkan] . my:return-to-normal-state)
		  ([escape]   . my:return-to-normal-state)))
  :init
  (setq evil-cross-lines t)
  (setq evil-undo-system 'undo-fu)
  :config
  (eval-and-compile (require 'evil))
  (defalias 'evil-insert-state 'evil-emacs-state)
  ;; Force evil-emacs-state for major modes
  (dolist (mode '(lisp-interaction-mode
				  fundamental-mode dashboard-mode dired-mode
				  neotree-mode howm-mode yatex-mode
				  git-timemachine-mode easy-hugo-mode))
	(add-to-list 'evil-emacs-state-modes mode))

  ;; Force evil-emacs-state for minor modes
  (add-hook 'org-capture-mode-hook 'evil-emacs-state)
  (add-hook 'magit-blame-mode-hook 'evil-emacs-state)

  (defun my:return-to-normal-state ()
	"Turn off input-method then return to normal-state."
	(interactive)
	(if (use-region-p)(keyboard-escape-quit))
	(if current-input-method (deactivate-input-method))
	(evil-normal-state)
	(message "-- NORMAL --"))

  (defun my:evil-find-file ()
	"New files open in insert-state."
	(interactive)
	(unless (file-exists-p buffer-file-name)
      (evil-insert-state)))

  ( defun evil-swap-key (map key1 key2)
	"Swap KEY1 and KEY2 in MAP."
	(let ((def1 (lookup-key map key1))
		  (def2 (lookup-key map key2)))
	  (define-key map key1 def2)
	  (define-key map key2 def1)))
  (evil-swap-key evil-motion-state-map "j" "gj")
  (evil-swap-key evil-motion-state-map "k" "gk")

  (defun ad:switch-to-buffer (&rest _arg)
	"Set buffer for automatic `evil-insert-state'."
	(when (member (buffer-name) '("COMMIT_EDITMSG"))
      (evil-insert-state)))
  (advice-add 'switch-to-buffer :after #'ad:switch-to-buffer))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 08_evil.el ends here
