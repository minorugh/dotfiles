;;; 03-evil.el --- Evil mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf evil :ensure t
  :hook after-init-hook
  :bind ((:evil-normal-state-map
	  ("M-."      . nil) ;; This bind is for use other
	  ("C-a"      . seq-home)
 	  ("C-e"      . seq-end)
	  ("C-y"      . clipboard-yank)
  	  ("C-w"      . evil-delete-backward-word)
 	  ("SPC"      . set-mark-command)
	  ("_"        . evil-visual-line)
          ([muhenkan] . evil-insert)
	  ([home]     . dashboard-toggle))
	 (:evil-visual-state-map
	  (";"        . comment-dwim)
	  ("c"        . clipboard-kill-ring-save)
	  ("g"        . my:google-this)
	  ("d"        . deepl-translate)
	  ("t"        . google-translate-auto)
	  ([muhenkan] . my:return-to-normal-state))
	 (:evil-motion-state-map
	  ([muhenkan] . my:return-to-normal-state))
	 (:evil-replace-state-map
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
  ;; Insert state is automatically changed to emacs state
  (defalias 'evil-insert-state 'evil-emacs-state)

  ;; Ovewrite `evil-quit` with kill-buffer
  (evil-ex-define-cmd "q[uit]"  'kill-current-buffer)
  (evil-ex-define-cmd "wq[uit]" 'kill-current-buffer)

  ;; Force evil-emacs-state
  (dolist (mode '(howm-view-summary-mode
		  imenu-list-major-mode easy-hugo-mode neotree-mode
		  org-mode fundamental-mode git-timemachine-mode))
    (add-to-list 'evil-emacs-state-modes mode))
  (add-hook 'magit-blame-mode-hook 'evil-emacs-state) ;; for minor mode

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
      (evil-emacs-state)))
  (add-hook 'find-file-hook 'evil-find-file)

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
      (evil-emacs-state)))
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
    "h" 'hydra-diff/body
    "j" 'evil-join-whitespace
    "g" 'my:google-this
    "s" 'swiper-thing-at-point
    ":" 'counsel-switch-buffer
    "," 'org-capture
    "." 'terminal-open
    "?" 'vim-cheat-sheet
    "q" 'keyboard-quit
    "SPC" 'avy-goto-word-1)
  :hydra
  (hydra-diff
   (:color red :hint nil)
   "
    diff-hl-hunk  prev.next:_[_._]_  _s_how"
   ("]" diff-hl-next-hunk)
   ("[" diff-hl-previous-hunk)
   ("g" diff-hl-diff-goto-hunk)
   ("r" diff-hl-revert-hunk)
   ("s" diff-hl-show-hunk)
   ("<muhenkan>" nil))
  :init
  (defun vim-cheat-sheet ()
    "View vim cheat sheet online."
    (interactive)
    (browse-url "https://minorugh.github.io/vim-cheat/vim-cheat-sheet.html")))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 03-evil.el ends here
