;;; 90-calendar.el --- Calendar and diary configuration.  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs calendar/diary の設定(キーバインド・face・フック配線)。
;; Google Calendarとの同期ロジック本体は elisp/my-gcal-diary.el に
;; 分離してあり、ここでは autoload して使うだけ。
;; kill-emacs-hookでEmacs終了時に自動同期される他、
;; 手動で M-x my-gcal-sync-to-diary を実行することもできる。
;;
;;; Code:

;; ============================================================
;;  Calendar / diary 本体
;; ============================================================

(leaf calendar
  :tag "builtin"
  :defvar calendar-holidays japanese-holidays
  :hook ((kill-emacs-hook . my-gcal-sync-on-exit)
         (calendar-mode-hook . my-calendar-cursor-type))  ; ← 追加
  :bind (("<f7>" . calendar)
         (:calendar-mode-map
          ("<f7>" . calendar-exit)))
  :config
  (autoload 'my-gcal-sync-to-diary "my-gcal-diary" nil t)

  ;; カーソルを「輪郭だけ」にして、下の日付の色を隠さないようにする
  (defun my-calendar-cursor-type ()
    (setq-local evil-motion-state-cursor '(hollow)))

  (defun my-gcal-sync-on-exit ()
    "Sync Google Calendar on Emacs exit, ignoring errors and timeouts."
    (with-timeout (10 (message "my-gcal-sync-to-diary: タイムアウトのためスキップ"))
      (ignore-errors (my-gcal-sync-to-diary))))

  ;; 手書き用diaryファイル(存在しなければ空で作成)
  (let ((diary (locate-user-emacs-file "tmp/diary")))
    (setq diary-file diary
          calendar-mark-diary-entries-flag t
          calendar-view-diary-initially-flag t)
    (unless (file-exists-p diary)
      (make-empty-file diary t)))

  (let ((gcal-diary (locate-user-emacs-file "tmp/diary-gcal")))
    (unless (file-exists-p gcal-diary)
      (make-empty-file gcal-diary t)))

  (add-hook 'diary-list-entries-hook #'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook #'diary-mark-included-diary-files)

  (add-hook 'calendar-today-visible-hook #'diary-mark-entries t)
  (add-hook 'calendar-today-invisible-hook #'diary-mark-entries t)

  ;; 日付の背景色(calendar-todayの指定はここ一箇所だけにする)
  (set-face-attribute 'diary nil :background "#d33682")          ; 予定がある日
  (set-face-attribute 'calendar-today nil
                      :background "#f2fa8c"
                      :foreground "#282a36"
                      :weight 'bold)                             ; 当日

  (with-eval-after-load 'japanese-holidays
    (setq calendar-holidays
          (append japanese-holidays holiday-local-holidays))))


;; ============================================================
;;  Japanese-Holidays
;; ============================================================

(leaf japanese-holidays :ensure t
  :after calendar
  :require t
  :hook ((calendar-today-visible-hook   . japanese-holiday-mark-weekend)
         (calendar-today-invisible-hook . japanese-holiday-mark-weekend)
         (calendar-today-visible-hook   . calendar-mark-today))
  :config
  (setq calendar-holidays
        (append japanese-holidays holiday-local-holidays holiday-other-holidays))
  (setq calendar-mark-holidays-flag t))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 90-calendar.el ends here
