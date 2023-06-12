;;; 05_evil.el --- Evil mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; code:
;; (setq debug-on-error t)

(leaf evil
  :ensure t
  :hook ((after-init-hook . evil-mode)
		 (find-file-hook . my:evil-find-file))
  :bind ((:evil-normal-state-map
		  ("C-e" . seq-end) ;; sequential-command
		  ("SPC" . evil-insert)
		  ("M-." . nil)
		  ("@" . other-window-or-split)
		  ("?" . chromium-vim-chert)
		  ([home] . open-dashboard))
		 (:evil-emacs-state-map
		  ([muhenkan] . my:return-to-normal-state)
		  ([escape] . my:return-to-normal-state)))
  :preface
  (setq evil-cross-lines t)
  (setq evil-undo-system 'undo-fu)
  :config
  (defalias 'evil-insert-state 'evil-emacs-state)
  ;; Force evil-emacs-state-modes into major mode
  (dolist (mode '(lisp-interaction-mode
				  fundamental-mode
				  dashboard-mode
				  dired-mode
				  neotree-mode
				  howm-mode
				  git-timemachine-mode
				  easy-hugo-mode))
	(add-to-list 'evil-emacs-state-modes mode))

  ;; Force evil-emacs-state into minor mode
  (add-hook 'org-capture-mode-hook 'evil-emacs-state)
  (add-hook 'magit-blame-mode-hook 'evil-emacs-state)

  ;; User custom functions
  (defun my:return-to-normal-state ()
	"Turn off input-method then return to normal-state."
	(interactive)
	(if current-input-method (deactivate-input-method))
	(evil-normal-state)
	(if (use-region-p)(keyboard-quit)))

  (defun my:evil-find-file ()
    "New files open in insert-state."
    (interactive)
    (unless (file-exists-p buffer-file-name)
      (evil-insert-state)))

  (defun evil-swap-key (map key1 key2)
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
  (advice-add 'switch-to-buffer :after #'ad:switch-to-buffer)

  (defun chromium-vim-chert ()
    "Chromium vim chert sheet."
    (interactive)
    (browse-url "https://minorugh.github.io/emacs.d/vim-cheat.html")))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 05_evil.el ends here
