;;; 08_evil.el --- Evil mode configurations.
;;; Commentary:
;;; code:
;; (setq debug-on-error t)

(leaf evil
  :ensure t
  :doc "The extensible vi layer for Emacs"
  :hook after-init-hook
  :bind ((:evil-normal-state-map
	  ("M-."      . nil)
	  ("?"        . vim-cheat-sheet)
 	  ("SPC"      . hydra-pinky/body)
	  ([home]     . open-dashboard)
       	  ([muhenkan] . evil-insert))
	 (:evil-emacs-state-map
	  ([muhenkan] . my:return-to-normal-state)
	  ([escape]   . my:return-to-normal-state)))
  :init
  ;; At the end of a line, move to the previous/next line
  (setq evil-cross-lines t)
  ;; Use undo-fu for evil undo
  (setq evil-undo-system 'undo-fu)
  :config
  ;; Insert state is automatically changed to emacs state
  (defalias 'evil-insert-state 'evil-emacs-state)
  ;; Force evil-emacs-state for major modes
  (dolist (mode '(lisp-interaction-mode
		  fundamental-mode dashboard-mode dired-mode neotree-mode
		  mail-mode org-mode yatex-mode	git-timemachine-mode easy-hugo-mode))
    (add-to-list 'evil-insert-state-modes mode))
  ;; Force evil-emacs-state for minor modes
  (add-hook 'magit-blame-mode-hook 'evil-emacs-state)

  (defun vim-cheat-sheet ()
    "View vim cheat sheet online."
    (interactive)
    (browse-url "https://minorugh.github.io/emacs.d/vim-cheat.html"))

  (defun my:return-to-normal-state ()
    "Turn off input-method then return to normal-state."
    (interactive)
    (if (use-region-p)(keyboard-escape-quit))
    (if current-input-method (deactivate-input-method))
    (evil-normal-state)
    (message "-- NORMAL --"))

  (defun evil-find-file ()
    "New files open in insert-state."
    (interactive)
    (unless (file-exists-p buffer-file-name)
      (evil-insert-state)))
  (add-hook 'find-file-hooks 'evil-find-file)

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
;; no-byte-compile: t
;; End:
;;; 08_evil.el ends here
