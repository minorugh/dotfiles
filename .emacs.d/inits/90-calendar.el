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
  :defvar calendar-holidays japanese-holidays
  :hook (kill-emacs-hook . my-gcal-sync-on-exit)
  :bind (("<f7>" . calendar)
         (:calendar-mode-map
          ("<f7>" . calendar-exit)))
  :config
  (require 'my-gcal-diary)

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

  ;; Google Calendar同期用ファイル(存在しなければ空で作成)
  ;; my-diary-gcal-file は elisp/my-gcal-diary.el で定義
  (unless (file-exists-p my-diary-gcal-file)
    (make-empty-file my-diary-gcal-file t))

  ;; diary本体の先頭に以下を書いておくことで、diary-gcalの内容が
  ;; #include され、下記2フックによって一覧表示・マーク付けの両方に
  ;; 反映されるようになる。
  ;;   #include "/home/minoru/.emacs.d/tmp/diary-gcal"
  (add-hook 'diary-list-entries-hook #'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook #'diary-mark-included-diary-files)

  ;; 土日祝マーク(japanese-holidays)は calendar-today-visible-hook の
  ;; 先頭に入るため先に実行される。ここで diary-mark-entries を末尾(t)に
  ;; 追加し直すことで、diaryの色(ピンク)が土日祝色より後に塗られ、
  ;; 常にdiaryの色が優先されるようにしている。
  (add-hook 'calendar-today-visible-hook #'diary-mark-entries t)
  (add-hook 'calendar-today-invisible-hook #'diary-mark-entries t)

  ;; 予定がある日: ピンク背景・白文字
  (set-face-attribute 'diary nil
                      :foreground "white"
                      :background "#d33682"
                      :weight 'bold)

  ;; 当日: 緑背景・白文字(土日祝・diaryマークと被っても見分けがつくように)
  (set-face-attribute 'calendar-today nil
                      :foreground "white"
                      :background "#228b22"
                      :weight 'bold
                      :underline nil)

  (with-eval-after-load 'japanese-holidays
    (setq calendar-holidays
          (append japanese-holidays holiday-local-holidays))))

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
