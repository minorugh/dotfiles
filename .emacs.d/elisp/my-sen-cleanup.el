;;; my-sen-cleanup.el --- Minoru's selection: command to delete the lost clause. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; キーバインド（my-normal-leader-map）:
;;   ;c → sen_cleanup.pl 実行 → 成功でrevert-buffer・super-saveに委譲
;;   ;b → .tmp から復元して revert-buffer
;;
;; ファイル管理:
;;   infile.bak → bklog.pl が使う永続バックアップ（触らない）
;;   infile.tmp → ;b リバート用（実行のたびに上書き・放置OK）
;;; Code:

(defvar my-sen-script "~/.emacs.d/elisp/bin/sen_cleanup.pl"
  "Path of sen_cleanup.pl.")

(defun my-sen-cleanup ()
  "Run sen_cleanup.pl.
On success, `revert-buffer' and delegate to super-save.
On failure, display in error buffer."
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

;; See 03-evil.el for key bindings

(provide 'my-sen-cleanup)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my-sen-cleanup.el ends here
