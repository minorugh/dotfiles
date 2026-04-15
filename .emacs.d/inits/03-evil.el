;;; 03-evil.el --- Evil mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf evil :ensure t
  :hook (after-init-hook . evil-mode)
  ;; [muhenkan] の挙動はファイル冒頭を参照
  :bind ((:evil-normal-state-map
          ("M-."      . nil) ;; This bind is for use other
          ("C-a"      . seq-home)
          ("C-e"      . seq-end)
          ("C-w"      . evil-delete-backward-word)
          ("SPC"      . set-mark-command)
          ("_"        . evil-visual-line)
          ("?"        . vim-cheat-sheet)
          ([muhenkan] . my-muhenkan)
          ([home]     . dashboard-toggle))
         (:evil-visual-state-map
          (";"        . comment-dwim)
          ("c"        . clipboard-kill-ring-save)
          ("g"        . my-google-search)
          ("d"        . deepl-translate)
          ([muhenkan] . my-muhenkan))
         (:evil-motion-state-map
          ([muhenkan] . my-muhenkan))
         (:evil-replace-state-map
          ([muhenkan] . my-muhenkan))
         (:evil-emacs-state-map
          ([muhenkan] . my-muhenkan)
          ([escape]   . my-muhenkan)))
  :init
  ;; At the end of a line, move to the previous/next line
  (setq evil-cross-lines t)
  ;; Use undo-fu for evil undo
  (setq evil-undo-system 'undo-fu)
  :config
  ;; Insert state is automatically changed to emacs state
  (defalias 'evil-insert-state 'evil-emacs-state)

  ;; Overwrite `evil-quit' with kill-buffer
  (evil-ex-define-cmd "q[uit]"  'kill-current-buffer)
  (evil-ex-define-cmd "wq[uit]" 'kill-current-buffer)

  ;; Force evil-emacs-state for specific modes
  (dolist (mode '(howm-view-summary-mode
                  imenu-list-major-mode easy-hugo-mode neotree-mode
                  org-mode fundamental-mode git-timemachine-mode))
    (add-to-list 'evil-emacs-state-modes mode))
  ;; For minor modes
  (add-hook 'counsel-find-file-hook #'evil-emacs-state)
  (add-hook 'view-mode-hook         #'evil-emacs-state)

  (defun evil-swap-key (map key1 key2)
    "Swap KEY1 and KEY2 in MAP."
    (let ((def1 (lookup-key map key1))
          (def2 (lookup-key map key2)))
      (define-key map key1 def2)
      (define-key map key2 def1)))
  (evil-swap-key evil-motion-state-map "j" "gj")
  (evil-swap-key evil-motion-state-map "k" "gk")

  (defun ad:switch-to-buffer (&rest _arg)
    "Set buffer for automatic `evil-emacs-state'."
    (when (member (buffer-name) '("COMMIT_EDITMSG"))
      (evil-emacs-state)))
  (advice-add 'switch-to-buffer :after #'ad:switch-to-buffer)

  ;; Universal key definition to escape and switch in any situation (my-muhenkan)
  ;;
  ;; git-peek running     → force exit
  ;; minibuffer in use    → close minibuffer (if failed, use top-level to escape)
  ;; evil Normal state    → Switch to Emacs / insert state
  ;; region selected      → deselect
  ;; input method enabled → disable input method
  ;; other than above     → return to evil normal state
  (defun my-muhenkan ()
    "[muhenkan] key defined universally."
    (interactive)
    (cond
     ((get-buffer "*git-peek-commits*") (git-peek-emergency-quit))
     ((minibuffer-window-active-p (selected-window))
      (minibuffer-keyboard-quit)
      (when (minibuffer-window-active-p (selected-window))
        (top-level)))
     ((evil-normal-state-p) (evil-insert-state))
     ((use-region-p) (deactivate-mark))
     (current-input-method (deactivate-input-method))
     (t (evil-normal-state) (message "-- NORMAL --"))))

  (bind-key "<muhenkan>" #'my-muhenkan)

  (defun vim-cheat-sheet ()
    "View vim cheat sheet online."
    (interactive)
    (browse-url "https://minorugh.github.io/vim-cheat/vim-cheat-sheet.html")))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 03-evil.el ends here
