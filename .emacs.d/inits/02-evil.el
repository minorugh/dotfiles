;;; 02-evil.el --- Evil mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
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
  :hook ((after-init-hook . evil-mode)
         (find-file-hook  . my-evil-emacs-state-for-new-file))
  :bind (([muhenkan]  . my-quit-dwim)  ;   Universal escape (see below)
         (:evil-normal-state-map
          ("C-a"      . my-seq-home)   ; Smart beginning-of-line (see 08-edit.el)
          ("C-e"      . my-seq-end)    ; Smart end-of-line (see 08-edit.el)
          ("SPC"      . evil-scroll-page-down)
          ("b"        . evil-scroll-page-up)
          ("p"        . evil-paste-before)    ; paste at cursor position (emacs-like)
          ("P"        . evil-paste-after)     ; paste after cursor (needed at EOL)
          ("i"        . my-emacs-state-mozc)
          ("@"        . evil-visual-char)
          ("_"        . evil-visual-line)
          ("?"        . my-evil-cheat-sheet)
          ([escape]   . my-evil-toggle-state)
          ([home]     . dashboard-toggle))
         (:evil-visual-state-map
          ([prior]    . er/expand-region)    ; Use PgUp to expand region
          ([next]     . er/contract-region)  ; Use PgDn to contract region
          (";"        . comment-dwim)
          ("c"        . clipboard-kill-ring-save)
          ("s"        . swiper-region)
          ("g"        . my-google-search)
          ("d"        . deepl-translate)
          ([insert]   . my-iedit-toggle))
         (:evil-emacs-state-map
          ("C-a"      . my-seq-home)
          ("C-e"      . my-seq-end)
          ([insert]   . my-iedit-toggle)
          ([escape]   . my-evil-toggle-state)))
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

  ;; Force Emacs state for special-purpose major modes
  (dolist (mode '(howm-view-summary-mode easy-hugo-mode
                                         yatex-mode neotree-mode fundamental-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  ;; Open new files in Emacs state
  (defun my-evil-emacs-state-for-new-file ()
    "バッファが未存在ファイルなら evil-emacs-state に切り替える."
    (when (and (buffer-file-name)
               (not (file-exists-p (buffer-file-name))))
      (evil-emacs-state)))

  ;; Switch to Emacs state and enable Mozc.
  (defun my-emacs-state-mozc ()
    "If it's at the end of a line, shift it one character to the right and execute."
    (interactive)
    (when (and (not (bolp))
               (save-excursion (forward-char) (eolp)))
      (forward-char))
    (evil-emacs-state)
    (activate-input-method "japanese-mozc"))


  ;; ============================================================
  ;; Auto Restore Normal State
  ;; ============================================================

  (defun my-evil--flag-manual-to-emacs ()
    "手動で normal→emacs したときにフラグをたてる."
    (when (eq evil-next-state 'emacs)
      (setq-local my-evil--to-emacs-state t)))

  (defun my-evil--restore-normal-state-on-buffer-switch ()
    "カレントバッファでなくなったら normal-state に戻す."
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
      (setq my-evil--current-buffer (current-buffer))))

  (add-hook 'evil-normal-state-exit-hook #'my-evil--flag-manual-to-emacs)
  (add-hook 'post-command-hook #'my-evil--restore-normal-state-on-buffer-switch)


  ;; ============================================================
  ;; Swap j/gj and k/gk so visual-line motion is the default
  ;; ============================================================

  (defun my-evil-swap-key (map key1 key2)
    "Swap KEY1 and KEY2 in MAP."
    (let ((def1 (lookup-key map key1))
          (def2 (lookup-key map key2)))
      (define-key map key1 def2)
      (define-key map key2 def1)))
  (my-evil-swap-key evil-motion-state-map "j" "gj")
  (my-evil-swap-key evil-motion-state-map "k" "gk")


  ;; ============================================================
  ;; ESC: Toggle Normal/Emacs State
  ;; ============================================================

  (defun my-evil-toggle-state ()
    "ESC key act as toggles between normal-state and emacs-state."
    (interactive)
    (if (evil-normal-state-p)
        (progn
          (evil-force-normal-state) (evil-emacs-state))  ; Normal→Emacs
      (deactivate-input-method) (evil-normal-state)))    ; Emacs→Normal


  ;; ============================================================
  ;;  muhenkan: Universal escape & Quit
  ;; ============================================================
  ;; [muhenkan] key gives priority to various exit operations,
  ;; and if none of those operations apply, it functions as the ESC key.

  (defun my-quit-dwim ()
    "Context-sensitive quit / evil-state escape."
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
        (evil-normal-state))))

  ;; mozc起動中は mozc-modeが無変換キーを奪うため my-quit-dwimで上書きする
  (with-eval-after-load 'mozc
    (define-key mozc-mode-map [muhenkan] #'my-quit-dwim)))


;; ============================================================
;;  iedit
;; ============================================================

(leaf iedit
  :ensure t
  :after evil
  :config
  (defun my-iedit-toggle ()
    "Switch to emacs-state and run iedit. After exiting, return to normal-state."
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
;;  Normal state のまま軽微な編集を完結させるための仕組み。
;;  ESC でキャンセル、完了後も Normal state に留まる。
;; ============================================================

(leaf evil-leader-map
  :require (my-sen-cleanup)                    ; minoru_sen commands
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
    (define-key m "@" #'my-insert-maru))        ; 行頭に ◎ 挿入（俳句選者用）

  ;;  Leader Key Helper Commands
  (defun my-newline-above ()
    "Insert a blank line above the current line without leaving Normal state."
    (interactive)
    (save-excursion
      (beginning-of-line)
      (open-line 1)))

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
