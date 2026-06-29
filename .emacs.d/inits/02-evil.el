;;; 02-evil.el --- Evil mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  Evil Core
;; ============================================================

(defvar-local my-evil--to-emacs-state nil
  "Non-nil when this buffer was manually switched Normal→Emacs state.")
(defvar my-evil--current-buffer nil
  "Buffer tracked for evil state restoration.")

(leaf evil
  :ensure t
  :require (my-evil-cheat-sheet)       ; custom Evil help buffer
  :hook ((after-init-hook . evil-mode))
  :bind ((:evil-normal-state-map
          ("C-a"      . my-seq-home)   ; Smart beginning-of-line (see 08-edit.el)
          ("C-e"      . my-seq-end)    ; Smart end-of-line (see 08-edit.el)
          ("C-w"      . evil-delete-backward-word)
          ("SPC"      . evil-scroll-page-down)
          ("b"        . evil-scroll-page-up)
          ("p"        . evil-paste-before)    ; paste at cursor position (emacs-like)
          ("P"        . evil-paste-after)     ; paste after cursor (needed at EOL)
          ("@"        . evil-visual-char)
          ("_"        . evil-visual-line)
          ("?"        . my-evil-cheat-sheet)
          ([muhenkan] . my-muhenkan)
          ([home]     . dashboard-toggle))
         (:evil-visual-state-map
          ([prior]    . er/expand-region)    ; Use PgUp to expand region
          ([next]     . er/contract-region)  ; Use PgDn to contract region
          (";"        . comment-dwim)
          ("c"        . clipboard-kill-ring-save)
          ("s"        . swiper-region)
          ("g"        . my-google-search)
          ("d"        . deepl-translate)
          ([insert]   . my-iedit-toggle)
          ([muhenkan] . my-muhenkan))
         (:evil-motion-state-map
          ([muhenkan] . my-muhenkan))
         (:evil-replace-state-map
          ([muhenkan] . my-muhenkan))
         (:evil-emacs-state-map
          ("C-a"      . my-seq-home)
          ("C-e"      . my-seq-end)
          ([insert]   . my-iedit-toggle)
          ([muhenkan] . my-muhenkan)
          ([escape]   . (lambda () (interactive) (evil-normal-state)))))
  :init
  (setq evil-cross-lines  t)           ; wrap to prev/next line at EOL/BOL
  (setq evil-undo-system 'undo-fu)     ; use undo-fu for undo/redo
  (setq evil-visual-char 'exclusive)   ; exclude cursor position from visual selection (emacs-like)
  :config
  ;; Route Insert state → Emacs state to enforce Emacs-state workflow
  (defalias 'evil-insert-state 'evil-emacs-state)

  ;; Disable edit-entry keys to prevent accidental Insert-state entry.
  ;; 編集トリガーを 'i' に限定し、Insert-state への誤進入を完全にシャットアウトする。
  (dolist (key '("I" "a" "A" "o" "O" "s" "S" "c" "C" "R"))
    (define-key evil-normal-state-map key #'ignore))

  ;; Map :q and :wq to kill-buffer instead of quitting Emacs
  (evil-ex-define-cmd "q[uit]"  'kill-current-buffer)
  (evil-ex-define-cmd "wq[uit]" 'kill-current-buffer)

  ;; Force Emacs state for special-purpose modes
  (dolist (mode '(howm-view-summary-mode easy-hugo-mode neotree-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  ;; Force Emacs state for named buffers (*init log*, *scratch*)
  ;; Checked on every buffer switch via buffer-list-update-hook.
  (add-hook 'buffer-list-update-hook
            (lambda ()
              (when (and (member (buffer-name) '("*init log*" "*scratch*"))
                         (fboundp 'evil-emacs-state)
                         (not (eq evil-state 'emacs)))
                (evil-emacs-state))))


  ;; ============================================================
  ;; Auto Restore Normal State
  ;; ============================================================

  ;; 手動で normal→emacs したときにフラグをたてる
  (add-hook 'evil-normal-state-exit-hook
            (lambda ()
              (when (eq evil-next-state 'emacs)
                (setq-local my-evil--to-emacs-state t))))

  ;; カレントバッファでなくなったら normal-state に戻す
  (add-hook 'post-command-hook
            (lambda ()
              (unless (eq (current-buffer) my-evil--current-buffer)
                (when (and my-evil--current-buffer
                           (buffer-live-p my-evil--current-buffer))
                  (with-current-buffer my-evil--current-buffer
                    (when (and my-evil--to-emacs-state
                               (eq evil-state 'emacs)
                               (not (apply #'derived-mode-p evil-emacs-state-modes))
                               (not (string-match-p "\\`\\*" (buffer-name))))
                      (setq-local my-evil--to-emacs-state nil)
                      (evil-normal-state))))
                (setq my-evil--current-buffer (current-buffer)))))


  ;; ============================================================
  ;; New files open in Emacs state
  ;; ============================================================

  (add-hook 'find-file-hook
            (lambda ()
              (when (and (buffer-file-name)
                         (not (file-exists-p (buffer-file-name))))
                (evil-emacs-state))))

  ;; ============================================================
  ;; Swap j/gj and k/gk so visual-line motion is the default
  ;; ============================================================

  (defun evil-swap-key (map key1 key2)
    "Swap KEY1 and KEY2 in MAP."
    (let ((def1 (lookup-key map key1))
          (def2 (lookup-key map key2)))
      (define-key map key1 def2)
      (define-key map key2 def1)))
  (evil-swap-key evil-motion-state-map "j" "gj")
  (evil-swap-key evil-motion-state-map "k" "gk")


  ;; ============================================================
  ;;  Universal Escape Key (muhenkan)
  ;; ============================================================
  (defun my-muhenkan ()
    "Universal escape key — context-sensitive quit/state switch."
    (interactive)
    (cond
     ;; iedit-mode中なら終了してnormal-stateへ
     ((bound-and-true-p iedit-mode)
      (iedit-mode -1)
      (evil-normal-state))
     ;; *Help* バッファが開いていれば閉じる
     ((get-buffer-window "*Help*")
      (delete-window (get-buffer-window "*Help*"))
      (kill-buffer "*Help*"))
     ;; ミニバッファ操作中なら中断
     ((minibuffer-window-active-p (selected-window))
      (abort-minibuffers))
     ;; 別ウィンドウのミニバッファにフォーカスして中断
     ((active-minibuffer-window)
      (select-window (active-minibuffer-window))
      (abort-recursive-edit))
     ;; リージョンがあれば解除
     ((use-region-p) (deactivate-mark))
     ;; Normal → Emacs、それ以外 → Normal
     ((evil-normal-state-p) (evil-emacs-state))
     (t (deactivate-input-method)
        (evil-normal-state)))))


;; ============================================================
;;  iedit
;; ============================================================

(leaf iedit
  :ensure t
  :after evil
  :config
  (defun my-iedit-toggle ()
    "Toggle `iedit-mode’.
Visual-state で範囲選択中ならその範囲を対象に iedit を起動する.
キーマップ競合を避けるため iedit 中は evil-emacs-state に切り替え、
終了時は evil-normal-state に戻る."
    (interactive)
    (if (bound-and-true-p iedit-mode)
        (progn
          (iedit-mode -1)
          (evil-normal-state))
      (let ((beg (and (use-region-p) (region-beginning)))
            (end (and (use-region-p) (region-end))))
        (evil-emacs-state)
        (when (and beg end)
          (set-mark beg)
          (goto-char end)
          (activate-mark))
        (iedit-mode)))))


;; ============================================================
;;  Normal-state Leader Key ";"
;;  Normal state のまま軽微な編集を完結させるための仕組み。
;;  ESC でキャンセル、完了後も Normal state に留まる。
;;  muhenkan で Emacs state へ。
;; ============================================================

(defvar-local my-evil--to-emacs-state nil
  "Non-nil when this buffer was manually switched Normal→Emacs state.
Set by \=`my-evil--mark-emacs-transition\='; cleared after restoring Normal state.")

(leaf evil-leader-map
  :doc "Normal-state leader key ';' for edit commands without leaving Normal state."
  :require (my-sen-cleanup)  ;; minoru_sen commands
  :after evil
  :config
  (setq echo-keystrokes 0)

  (defvar my-normal-leader-map (make-sparse-keymap)
    "Prefix map triggered by ';' in evil-normal-state.")

  (define-key evil-normal-state-map ";" my-normal-leader-map)

  (let ((m my-normal-leader-map))
    (define-key m "f" #'counsel-find-file)     ; ファイル検索
    (define-key m ":" #'counsel-switch-buffer) ; バッファ切替
    (define-key m "/" #'kill-current-buffer)   ; バッファを閉じる
    (define-key m ";" #'comment-line)          ; コメントトグル
    (define-key m "o" #'my-newline-above)      ; カーソル行の上に空行挿入
    (define-key m "c" #'my-sen-cleanup)        ; cleanup sen markers
    (define-key m "r" #'my-sen-restore)        ; restore sen markers
    (define-key m "w" #'my-darkroom-toggle)    ; darkroom 起動
    (define-key m "s" #'swiper)                ; swiper 検索
    (define-key m "@" #'my-insert-maru)        ; 行頭に ◎ 挿入（俳句選者用）
    (define-key m "i" #'my-emacs-state-mozc))  ; Emacs-state + mozc on

  ;;  Leader Key Helper Commands
  (defun my-newline-above ()
    "Insert a blank line above the current line without leaving Normal state."
    (interactive)
    (save-excursion
      (beginning-of-line)
      (open-line 1)))

  (defun my-emacs-state-mozc ()
    "Switch to Emacs state and activate Mozc input method."
    (interactive)
    (evil-emacs-state)
    (activate-input-method "japanese-mozc"))

  (defun my-insert-maru ()
    "Insert ◎ at the beginning of the current line.  Bound to ;@."
    (interactive)
    (save-excursion
      (beginning-of-line)
      (insert "◎"))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 02-evil.el ends here
