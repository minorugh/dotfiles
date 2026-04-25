;; mozc カーソル色変更 (mozc-cursor-color パッケージ代替)
;; init.el に直接貼り付けて使う

(defvar my-mozc-cursor-color-alist
  '((direct       . "lime green")
    (read-only    . "lime green")
    (hiragana     . "#cc3333")
    (full-katakana  . "goldenrod")
    (half-ascii   . "dark orchid")
    (full-ascii   . "orchid")
    (half-katakana  . "dark goldenrod")))

(defvar-local my-mozc-current-input-mode 'hiragana)

;; mozc の入力モード変化を追跡
(advice-add 'mozc-session-execute-command :after
            (lambda (return-value &rest _)
              (when return-value
                (let ((mode (mozc-protobuf-get return-value 'mode)))
                  (when mode
                    (setq my/mozc-current-input-mode mode))))))

;; カーソル色を更新する関数
(defun my-mozc-cursor-color-update ()
  (set-cursor-color
   (or (cdr (assq (cond
                   ((and buffer-read-only (not inhibit-read-only)) 'read-only)
                   ((not mozc-mode) 'direct)
                   (t my/mozc-current-input-mode))
                  my/mozc-cursor-color-alist))
       (frame-parameter nil 'foreground-color))))

;; idle timer で定期更新 (0.1秒)
(run-with-idle-timer 0.1 t #'my/mozc-cursor-color-update)
