;;; 03-evil.el --- Evil mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf evil :ensure t
  :hook ((after-init-hook . evil-mode)
	 (evil-normal-state-entry-hook . deactivate-input-method))
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

  ;; Emacs state only when creating new files
  (add-hook 'find-file-hook
            (lambda ()
              (unless (file-exists-p (buffer-file-name))
                (evil-emacs-state))))

  (defun evil-swap-key (map key1 key2)
    "Swap KEY1 and KEY2 in MAP."
    (let ((def1 (lookup-key map key1))
          (def2 (lookup-key map key2)))
      (define-key map key1 def2)
      (define-key map key2 def1)))
  (evil-swap-key evil-motion-state-map "j" "gj")
  (evil-swap-key evil-motion-state-map "k" "gk")

  (defun my-muhenkan ()
    "Universal escape key."
    (interactive)
    (cond
     ;; git-peek実行中なら強制終了
     ((get-buffer "*git-peek-commits*") (git-peek-emergency-quit))

     ;; Helpバッファが開いていれば閉じる
     ((get-buffer-window "*Help*")
      (delete-window (get-buffer-window "*Help*"))
      (kill-buffer "*Help*"))

     ;; ミニバッファ操作中なら中断
     ((minibuffer-window-active-p (selected-window))
      (abort-minibuffers))

     ;; 別のウィンドウでミニバッファが開いていれば、フォーカスして中断
     ((active-minibuffer-window)
      (select-window (active-minibuffer-window))
      (abort-recursive-edit))

     ;; 選択範囲（リージョン）があれば解除
     ((use-region-p) (deactivate-mark))

     ;; すでにNormalならEmacsへ、それ以外（Emacs/Insert等）ならNormalへ
     ((evil-normal-state-p) (evil-emacs-state))
     (t (evil-normal-state))))

  (defun vim-cheat-sheet ()
    "View vim cheat sheet online."
    (interactive)
    (browse-url "https://minorugh.github.io/vim-cheat/vim-cheat-sheet.html")))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 03-evil.el ends here
