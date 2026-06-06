;;; my-fix-mojibake.el --- 句会データファイルの文字化け修復
;;
;; 使い方:
;;   1. このファイルを load-path の通った場所に置く
;;      例: ~/.emacs.d/lisp/my-fix-mojibake.el
;;
;;   2. init.el に以下を追加:
;;      (load "my-fix-mojibake")
;;      ;; キーバインドしたい場合（任意）:
;;      (global-set-key (kbd "C-c f") 'my-fix-mojibake)
;;
;;   3. 実行方法:
;;      - ファイルをバッファーで開いた状態で M-x my-fix-mojibake
;;      - dired で対象ファイルにカーソルを当てて M-x my-fix-mojibake
;;      - キーバインドを設定した場合は C-c f

(defvar my-fix-mojibake-script "/usr/local/bin/fix_mojibake.py"
  "fix_mojibake.py のパス。環境に合わせて変更してください。")

(defun my-fix-mojibake ()
  "現在のバッファーまたは dired のカーソル位置のファイルに対して
fix_mojibake.py を実行し、結果を *fix-mojibake* バッファーに表示する。"
  (interactive)
  (let ((filepath
         (cond
          ;; dired モードの場合: カーソル位置のファイル
          ((derived-mode-p 'dired-mode)
           (dired-get-filename nil t))
          ;; 通常バッファーの場合: そのファイル
          (buffer-file-name
           buffer-file-name)
          ;; それ以外
          (t
           (read-file-name "対象ファイル: ")))))

    (unless filepath
      (user-error "対象ファイルが特定できません"))

    (unless (file-exists-p my-fix-mojibake-script)
      (user-error "スクリプトが見つかりません: %s" my-fix-mojibake-script))

    ;; 確認プロンプト
    (when (yes-or-no-p (format "%s を修復しますか？ "
                               (file-name-nondirectory filepath)))

      ;; バッファーを保存してから実行（編集中の場合）
      (when (and buffer-file-name
                 (buffer-modified-p)
                 (not (derived-mode-p 'dired-mode)))
        (save-buffer))

      (let ((output-buf (get-buffer-create "*fix-mojibake*")))
        (with-current-buffer output-buf
          (setq buffer-read-only nil)
          (erase-buffer))

        (message "修復中: %s ..." (file-name-nondirectory filepath))

        (call-process "python3" nil output-buf t
                      my-fix-mojibake-script
                      (expand-file-name filepath))

        ;; 結果を表示
        (display-buffer output-buf)

        ;; バッファーで開いていたら最新の内容に更新
        (when (and buffer-file-name
                   (not (derived-mode-p 'dired-mode)))
          (revert-buffer t t t)
          (message "バッファーを更新しました: %s"
                   (file-name-nondirectory filepath)))

        ;; dired なら表示を更新
        (when (derived-mode-p 'dired-mode)
          (revert-buffer))))))
