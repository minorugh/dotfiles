;;; 03-evil.el --- Evil mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf evil :ensure t
  :hook (after-init-hook . evil-mode)
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
         ([muhenkan]  . my-muhenkan)
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


(defun my-toggle-evil-normal-emacs ()
  "Toggle between evil Normal and Emacs state.
Deactivates input method first if active."
  (interactive)
  (when current-input-method (deactivate-input-method))
  (if (evil-normal-state-p) (evil-emacs-state) (evil-normal-state)))

(defun my-muhenkan ()
  "Universal escape key."
  ;; git-peek running           → force quit
  ;; minibuffer active          → close it (minibuffer-keyboard-quit)
  ;; minibuffer open, elsewhere → focus & abort
  ;;   (pressing twice: 1st moves focus to minibuffer, 2nd closes it)
  ;; evil Normal/Emacs state    → toggle (deactivate input method if active)
  ;; region selected            → deactivate mark
  ;; otherwise                  → evil Normal state
  (interactive)
  (cond
   ((get-buffer "*git-peek-commits*") (git-peek-emergency-quit))
   ((minibuffer-window-active-p (selected-window))
    (minibuffer-keyboard-quit))
   ((active-minibuffer-window)
    (select-window (active-minibuffer-window))
    (abort-recursive-edit))
   ((or (evil-normal-state-p) (evil-emacs-state-p))
    (my-toggle-evil-normal-emacs))
   ((use-region-p) (deactivate-mark))
   (t (evil-normal-state))))

  (defun vim-cheat-sheet ()
    "View vim cheat sheet online."
    (interactive)
    (browse-url "https://minorugh.github.io/vim-cheat/vim-cheat-sheet.html")))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 03-evil.el ends here
