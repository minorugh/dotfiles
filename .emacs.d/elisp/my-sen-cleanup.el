;;; my-sen-cleanup.el --- Minoru's selection: command to delete the lost clause. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; キーバインド（my-normal-leader-map）:
;;   ;c → sen_cleanup.pl 実行 → 成功でrevert-buffer・super-saveに委譲
;;   ;b → .tmp から復元して revert-buffer
;;
;; *sen-cleanup* バッファー:
;;   実行中はストリーミング表示。終了後は view-mode になり q で閉じる。
;;   成功・失敗どちらもバッファーに結果を残す（super-saveによるecho上書き対策）。
;;
;; ファイル管理:
;;   infile.bak → bklog.pl が使う永続バックアップ（触らない）
;;   infile.tmp → ;b リバート用（実行のたびに上書き・放置OK）
;;; Code:

;; Include an executable perl script
(defvar my-sen-script "~/.emacs.d/elisp/bin/sen_cleanup.pl"
  "Path of sen_cleanup.pl.")

(defun my-sen-cleanup ()
  "Run sen_cleanup.pl.
Progress is streamed to *sen-cleanup* buffer.
On finish (success or failure), buffer enters `view-mode' so q closes it.
On success, `revert-buffer' is called before switching back to the source buffer."
  (interactive)
  (let* ((file   (or (buffer-file-name)
                     (user-error "バッファーはファイルに紐付いていません")))
         (script (expand-file-name my-sen-script))
         (srcbuf (current-buffer))
         (outbuf (get-buffer-create "*sen-cleanup*")))
    (with-current-buffer outbuf
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (let ((proc (start-process "sen-cleanup" outbuf "perl" script file)))
      (set-process-sentinel
       proc
       (lambda (p _event)
         (when (memq (process-status p) '(exit signal))
           (let ((code (process-exit-status p)))
             (when (= code 0)
               (with-current-buffer srcbuf
                 (let ((msg (with-current-buffer outbuf
                              (string-trim (buffer-string)))))
                   (revert-buffer t t t)
                   (pop-to-buffer srcbuf)        ; ← 追加: minoru_sen.txt に戻す
                   (set-buffer-modified-p t)
                   (letrec ((hook (lambda ()
                                    (remove-hook 'after-save-hook hook t)
                                    (message "%s" msg)
                                    (when (buffer-live-p outbuf)
                                      (kill-buffer outbuf)))))
                     (add-hook 'after-save-hook hook nil t)))))
             (when (/= code 0)
               (with-current-buffer outbuf
                 (let ((inhibit-read-only t))
                   (goto-char (point-max))
                   (insert "\n[失敗 — q で閉じる]"))
                 (view-mode 1)
                 (pop-to-buffer outbuf))))))))))

(defun my-sen-restore ()
  "Restore from .tmp file and `revert-buffer'."
  (interactive)
  (let* ((file (buffer-file-name))
         (tmp  (concat file ".tmp")))
    (if (not (file-exists-p tmp))
        (message "tmpファイルが見つかりません: %s" tmp)
      (when (yes-or-no-p (format "%s Do you want to restore from?" tmp))
        (copy-file tmp file t)
        (revert-buffer t t t)
        (set-buffer-modified-p t)
        (message "復元しました: %s" tmp)))))


(provide 'my-sen-cleanup)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my-sen-cleanup.el ends here
