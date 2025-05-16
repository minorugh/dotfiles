;;; 03_evil.el --- Evil mode configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; code:
;; (setq debug-on-error t)

(leaf evil :ensure t
  :doc "The extensible vi layer for Emacs"
  :hook after-init-hook
  :bind ((:evil-normal-state-map
	  ("M-."      . nil)      ;; Disable and assign to other use
 	  ("C-a"      . seq-home) ;; Overwrite to sequential-command
 	  ("C-e"      . seq-end)  ;; Overwrite to sequential-command
 	  ("@"        . er/expand-region)
 	  ("SPC"      . set-mark-command)
          ([muhenkan] . evil-insert)
	  ([home]     . open-dashboard))
	 (:evil-visual-state-map
	  ;; Single-char binding is performed when selected region
	  (";"        . comment-dwim)
	  ("c"        . clipboard-kill-ring-save)
	  ("g"        . my:google-this)
	  ("w"        . my:weblio)
	  ("d"        . deepl-translate)
	  ("t"        . gt-do-translate)
	  ([muhenkan] . my:return-to-normal-state))
	 (:evil-emacs-state-map
	  ([muhenkan] . my:return-to-normal-state)
	  ([escape]   . my:return-to-normal-state)))
  :init
  ;; At the end of a line, move to the previous/next line
  (setq evil-cross-lines t)
  ;; Use undo-fu for evil undo
  (setq evil-undo-system 'undo-fu)
  :config
  ;; Do not exit emacs with quit, close the buffer instead
  (evil-ex-define-cmd "q[uit]"  'my:kill-buffer)
  (evil-ex-define-cmd "wq[uit]" 'my:kill-buffer)

  ;; Insert state is automatically changed to emacs state
  (defalias 'evil-insert-state 'evil-emacs-state)

  ;; Force evil-emacs-me-mode-hook 'evil-emacs-state)
  (dolist (mode '(howm-view-summary-mode
		  easy-hugo-mode fundamental-mode yatex-mode
     		  org-mode neotree-mode git-timemachine-mode))
    (add-to-list 'evil-insert-state-modes mode))

  ;; Force evil-emacs-state for minor modes
  (add-hook 'magit-blame-mode-hook 'evil-emacs-state)

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
  (advice-add 'switch-to-buffer :after #'ad:switch-to-buffer))


(leaf evil-leader :ensure t
  :doc "Free keymap on evil-mode"
  :hook (after-init-hook . global-evil-leader-mode)
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "0" 'delete-window
    "1" 'delete-other-windows
    "2" 'split-window-below
    "3" 'split-window-right
    "n" 'make-frame
    "_" 'other-frame
    "/" 'delete-frame
    "S" 'window-swap-states
    "o" 'other-window-or-split
    "[" 'previous-buffer
    "]" 'next-buffer
    "l" 'recenter-top-bottom
    "j" 'diff-hl-next-hunk
    "k" 'diff-hl-previous-hunk
    "a" 'counsel-ag
    "s" 'swiper-thing-at-point
    ":" 'counsel-switch-buffer
    "r" 'avy-goto-word-1
    "," 'org-capture
    "." 'terminal-open
    "?" 'vim-cheat-sheet
    "q" 'keyboard-quit
    "w" 'kill-word-or-region
    "SPC" 'evil-visual-line)
  :init
  (defun vim-cheat-sheet ()
    "View vim cheat sheet online."
    (interactive)
    (browse-url "https://minorugh.github.io/vim-cheat/vim-cheat-sheet.html")))


;;; 03_evil.el ends here
