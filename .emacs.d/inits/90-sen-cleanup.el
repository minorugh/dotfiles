;;; 91-sen-cleanup.el - みのる選：没句削除コマンド
;;
;; キーバインド（my-normal-leader-map）:
;;   ;c → sen_cleanup.pl 実行 → 成功でrevert-buffer・super-saveに委譲
;;   ;b → .tmp から復元して revert-buffer
;;
;; ファイル管理:
;;   infile.bak → bklog.pl が使う永続バックアップ（触らない）
;;   infile.tmp → ;b リバート用（実行のたびに上書き・放置OK）

(defvar my-sen-script "~/.emacs.d/elisp/bin/sen_cleanup.pl"
  "sen_cleanup.pl のパス。")

(defun my-sen-cleanup ()
  "sen_cleanup.pl を実行する。
成功時はrevert-bufferしてsuper-saveに委譲。
失敗時はエラーバッファに表示。"
  (interactive)
  (let* ((file     (buffer-file-name))
         (script   (expand-file-name my-sen-script))
         (outbuf   (get-buffer-create "*sen-cleanup*"))
         (exit-code (call-process "perl" nil outbuf nil script file)))
    (if (= exit-code 0)
        (progn
          (revert-buffer t t t)
          (set-buffer-modified-p t)
          (message "%s"
                   (with-current-buffer outbuf
                     (string-trim (buffer-string)))))
      (display-buffer outbuf))))

(defun my-sen-restore ()
  ".tmp ファイルから復元して revert-buffer する。"
  (interactive)
  (let* ((file (buffer-file-name))
         (tmp  (concat file ".tmp")))
    (if (not (file-exists-p tmp))
        (message "tmpファイルが見つかりません: %s" tmp)
      (when (yes-or-no-p (format "%s から復元しますか？" tmp))
        (copy-file tmp file t)
        (revert-buffer t t t)
        (set-buffer-modified-p t)
        (message "復元しました: %s" tmp)))))

;; キーバインド登録
(with-eval-after-load 'evil
  (define-key my-normal-leader-map "c" #'my-sen-cleanup)
  (define-key my-normal-leader-map "b" #'my-sen-restore))
