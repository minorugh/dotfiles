;;; 03-evil.el --- Evil mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf evil
  :ensure t
  :hook ((after-init-hook . evil-mode)
	 (evil-normal-state-entry-hook . deactivate-input-method))
  :bind ((:evil-normal-state-map
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
                  org-mode fundamental-mode))
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
    (browse-url "https://minorugh.github.io/vim-cheat/vim-cheat-sheet.html"))

;;; --------------------------------------------------------------------------
;;; Normal-state でleader key ";" を使って編集コマンドを呼び出す。
;;; insert-stateを使わずNormal stateのまま軽微な編集を完結させるための仕組み。
;;; ESCでキャンセル、完了後もNormal stateに留まる。muhenkanでEmacs stateへ。
  ;; Normal-state leader key ( ; )
  (defvar my-normal-leader-map (make-sparse-keymap)
    "Prefix map triggered by ';' in evil-normal-state.")

  (define-key evil-normal-state-map ";" my-normal-leader-map)

  (let ((m my-normal-leader-map))
    (define-key m "w" #'save-buffer)            ; ;w → 保存
    (define-key m "s" #'swiper)                 ; ;s → Swiper検索
    (define-key m "b" #'consult-buffer)         ; ;b → バッファ切り替え
    (define-key m ";" #'comment-line)           ; ;; → コメントトグル
    (define-key m "d" #'duplicate-line)         ; ;d → 行の複製（Emacs29+）
    (define-key m "i" #'my-insert-one-char)     ; ;i → 一文字だけ挿入
    (define-key m "@" #'my-insert-maru))        ; ;@ → 行頭に◎挿入（俳句選者用）

;; (defun my-insert-one-char ()
;;   (interactive)
;;   (when (eq major-mode 'text-mode)
;;     (my-ime-on))
;;   (unwind-protect
;;       (let ((str (read-string "insert: ")))
;;         (unless (string-empty-p str)
;;           (insert str)))
    ;; (my-ime-off)))

  (defun my-insert-one-char ()
      "Insert only one character and stay in Normal state; ESC to cancel."
      (interactive)
      (let ((char (read-key "insert (ESC to cancel): ")))
        (unless (eq char 27)  ; 27 = ESC
          (insert char))))

  (defun my-insert-maru ()
    "行頭に◎を挿入する。カーソル位置は変わらず Normal state のまま完結。"
    (interactive)
    (save-excursion
      (beginning-of-line)
      (insert "◎"))))
;; 使い方: ;@ で◎挿入、j で次行、繰り返すだけ

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 03-evil.el ends here
