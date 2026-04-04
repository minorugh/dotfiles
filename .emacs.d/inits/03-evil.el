;;; 03-evil.el --- Evil mode configurations. -*- lexical-binding: t -*-
;;; Commentary:

;; [muhenkan] キーについて:
;;   どんな状況でも脱出・切替できる万能キー (my-muhenkan で定義)。
;;
;;   git-peek 起動中        → 強制終了
;;   ミニバッファ使用中     → ミニバッファを閉じる (失敗時は top-level で強制脱出)
;;   evil ノーマル状態      → Emacs / insert ステートへ切替
;;   リージョン選択中       → 選択解除
;;   入力メソッド有効時     → 入力メソッドを無効化
;;   上記以外               → evil ノーマル状態へ戻る

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
          ([muhenkan] . my-muhenkan)
	  ([home]     . dashboard-toggle))
	 (:evil-visual-state-map
	  (";"        . comment-dwim)
	  ("c"        . clipboard-kill-ring-save)
	  ("g"        . my-google-this)
	  ("d"        . deepl-translate)
	  ("t"        . google-translate-auto)
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

  (defun my-muhenkan ()
    "Muhenkan key handler: toggle evil state or rescue from any situation.
In evil normal state: switch to insert state.
Otherwise: git-peek quit → minibuffer quit (top-level fallback)
→ deactivate mark → deactivate input method → return to evil normal state."
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

  (bind-key "<muhenkan>" #'my-muhenkan))


(leaf evil-leader :ensure t
  :doc "Free keymap on evil-mode."
  :hook (after-init-hook . global-evil-leader-mode)
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "0" 'delete-window
    "1" 'delete-other-windows
    "2" 'split-window-below
    "3" 'split-window-right
    "m" 'make-frame
    "n" 'neomutt
    "_" 'other-frame
    "/" 'delete-frame
    "w" 'window-swap-states
    "o" 'other-window-or-split
    "[" 'previous-buffer
    "]" 'next-buffer
    "l" 'recenter-top-bottom
    "h" 'hydra-diff/body
    "j" 'evil-join-whitespace
    "g" 'my-google-this
    ":" 'thunar-open
    "f" 'flycheck-list-errors
    "," 'org-capture
    "." 'thunderbird
    "?" 'vim-cheat-sheet
    "c" 'org-capture
    "q" 'keyboard-quit
    "SPC" 'avy-goto-word-1)
  :init
  (defun vim-cheat-sheet ()
    "View vim cheat sheet online."
    (interactive)
    (browse-url "https://minorugh.github.io/vim-cheat/vim-cheat-sheet.html"))

  (defun thunderbird ()
    "Open thunderbird mail-client for Gmail."
    (interactive)
    (start-process "thunderbird" nil "thunderbird"))

  (defun neomutt ()
    "Open terminal and ssh to xsrv."
    (interactive)
    (start-process-shell-command "neomutt" nil "neomutt.sh"))
  (setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))

  (defun mattermost ()
    "Open mattermost-desktop."
    (interactive)
    (start-process "mattermost" nil "mattermost-desktop")))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 03-evil.el ends here
