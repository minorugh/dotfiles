;;; 15-makefile.el --- Makefile support: targets, imenu, ivy, compile  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Makefile操作をまとめた個人設定。
;;
;; ## ターゲット記法
;;   mytarget: ## ここに書いた説明がivyに表示される
;;
;; ## キーバインド (makefile-mode / dired)
;;   @        : ivy でターゲット選択 (Enter=ジャンプ, ↑↓=プレビュー, C-c C-c=make実行)
;;   C-c C-e  : read-only トグル (makefile-mode のみ)
;;   qq       : 同上 (key-chord)
;;   f2       : imenu-list トグル (30-ui.el で設定済み)
;;
;;; Code:

;;; ----------------------------------------------------------------
;;; Core: Makefileの検索
;;; ----------------------------------------------------------------

(defun my-make--find-makefile ()
  "Return the Makefile path from the current context (Dired or buffer file or default-directory)."
  (let ((dir (cond
              ((derived-mode-p 'dired-mode) (dired-current-directory))
              ((and buffer-file-name
                    (string= (file-name-nondirectory buffer-file-name) "Makefile"))
               (file-name-directory buffer-file-name))
              (t default-directory))))
    (let ((mk (expand-file-name "Makefile" dir)))
      (when (file-exists-p mk) mk))))

;;; ----------------------------------------------------------------
;;; Imenu: ## コメント付きターゲットをインデックス化
;;; ----------------------------------------------------------------

(defun my-makefile-imenu-create-index ()
  "Indexing for Makefile: ## comment targets only."
  (let (index)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([^:# \t\n]+\\):.*?##[ \t]*\\(.*\\)$" nil t)
        (let ((target (match-string 1))
              (pos    (match-beginning 1)))
          (push (cons target pos) index))))
    (nreverse index)))

;;; ----------------------------------------------------------------
;;; Ivy: ターゲット選択 (makefile-mode / dired 両対応)
;;; ----------------------------------------------------------------

(defun my-make-ivy-integrated ()
  "Makefileターゲットをivyで選択・実行。
diredバッファおよびmakefile-modeバッファから呼び出し可能。

Enter   : その場所へジャンプして終了
↑↓     : リアルタイムプレビュー
C-c C-c : make 実行 (ivyは閉じる)"
  (interactive)
  (require 'ivy)
  (let ((makefile (my-make--find-makefile)))
    (unless makefile (user-error "Makefileが見つかりません"))
    (let ((candidates nil)
          (orig-buf   (current-buffer))
          (orig-point (point))
          (map        (copy-keymap ivy-minibuffer-map)))

      ;; リアルタイムプレビュー
      (define-key map (kbd "<down>") 'ivy-next-line-and-call)
      (define-key map (kbd "<up>")   'ivy-previous-line-and-call)

      ;; C-c C-c で make 実行
      (define-key map (kbd "C-c C-c")
        (lambda ()
          (interactive)
          (ivy-exit-with-action
           (lambda (x)
             (let ((target (cdr x)))
               (compile (format "make -C %s %s"
                                (file-name-directory makefile)
                                target)))))))

      ;; 候補を Makefile バッファからパース
      (with-current-buffer (find-file-noselect makefile)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^\\([^:# \t\n]+\\):.*?##[ \t]*\\(.*\\)$" nil t)
            (let* ((target     (match-string 1))
                   (desc       (match-string 2))
                   (pos        (match-beginning 1))
                   (target-fmt (propertize (format "%-20s" target) 'face 'font-lock-function-name-face))
                   (desc-fmt   (propertize desc 'face 'font-lock-comment-face)))
              (push (cons (concat target-fmt " " desc-fmt)
                          (propertize target 'pos pos 'makefile makefile))
                    candidates)))))

      (if (not candidates)
          (message "ターゲットが見つかりませんでした。")
        (ivy-read "Makefile Targets: "
                  (nreverse candidates)
                  :keymap map
                  :action (lambda (x)
                            (let ((pos (get-text-property 0 'pos (cdr x)))
                                  (mk  (get-text-property 0 'makefile (cdr x))))
                              (find-file mk)
                              (goto-char pos)
                              (recenter)))
                  :unwind (lambda ()
                            (unless (eq ivy-exit 'done)
                              (switch-to-buffer orig-buf)
                              (goto-char orig-point)
                              (recenter)))
                  :caller 'my-make-ivy-integrated)))))

;;; ----------------------------------------------------------------
;;; Utilities
;;; ----------------------------------------------------------------

(defun my-makefile-toggle-readonly ()
  "Toggle read-only mode of the current Makefile buffer."
  (interactive)
  (read-only-mode 'toggle)
  (message "Makefile: %s" (if buffer-read-only "read-only" "EDITABLE")))

(defun my-make-git ()
  "Run `make git' in the repository root."
  (interactive)
  (let* ((dir  (or buffer-file-name default-directory))
         (root (locate-dominating-file dir "Makefile")))
    (if root
        (let ((default-directory root))
          (compile "make git"))
      (message "Makefile not found"))))

;;; ----------------------------------------------------------------
;;; Hooks
;;; ----------------------------------------------------------------

(add-hook 'makefile-mode-hook
          (lambda ()
            (evil-local-set-key 'normal (kbd "@") #'my-make-ivy-integrated)
            (local-set-key (kbd "C-c C-e") #'my-makefile-toggle-readonly)
            (when (fboundp 'key-chord-define)
              (key-chord-define (current-local-map) "qq" #'my-makefile-toggle-readonly))
            (setq imenu-create-index-function #'my-makefile-imenu-create-index)))

(add-hook 'dired-mode-hook
          (lambda ()
            (evil-local-set-key 'normal (kbd "@") #'my-make-ivy-integrated)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 15-makefile.el ends here
