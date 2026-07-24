;;; my-gcal-diary.el --- One-way sync from Google Calendar to Emacs diary.  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Google Calendar(複数カレンダー)から Emacs diary への一方向同期ロジック。
;; UI(calendar-mode、キーバインド、face等)には一切関与しない、
;; 「ダウンロード→変換→フィルタ→書き込み」だけを行う自己完結モジュール。
;; ;;;###autoload によって遅延読み込みされるため、利用側(90-calendar.el
;; など)で require する必要はない。
;;
;; ------------------------------------------------------------
;; 全体構成
;; ------------------------------------------------------------
;;   ~/.emacs.d/tmp/diary        … 手書き用。このファイルは直接編集する。
;;   ~/.emacs.d/tmp/diary-gcal   … Google Calendar由来。自動生成専用、
;;                                  手で編集しないこと(同期のたびに
;;                                  まるごと上書きされる「洗い替え」方式)。
;;
;; diary の先頭に以下の1行を追加することで、diary-gcal の内容が
;; #include され、calendar上でまとめて扱われる(この設定自体は
;; 利用側のcalendar設定ファイルで行う)。
;;
;;   #include "/home/minoru/.emacs.d/tmp/diary-gcal"
;;
;; ------------------------------------------------------------
;; 同期の仕組み(M-x my-gcal-sync-to-diary)
;; ------------------------------------------------------------
;;   対象カレンダーは `my-gcal-calendars' に (名前 . URLファイルパス) の
;;   リストとして登録する。各URLファイルには Google Calendarの
;;   「非公開URL」(secret address in iCal format)を1行だけ書いて
;;   ~/.env_source 配下に保存する(dotfilesには含めない)。
;;
;;   カレンダーごとに以下を繰り返し、結果をdiary-gcalへ追記していく:
;;   1. 非公開URLから .ics をダウンロードする(認証不要、読み取り専用)。
;;   2. `icalendar-import-file' で .ics を diary形式のテキストに変換する。
;;   3. `my-diary-filter-recent' で「直近 my-gcal-months-back ヶ月分より
;;      新しい予定」だけに絞り込む(全履歴を毎回持ち込むと肥大化するため)。
;;      日付が判定できない繰り返し予定(diary-float等)は安全側に倒して残す。
;;
;;   diary-gcalファイル自体は同期開始時に一旦空にしてから、各カレンダーの
;;   結果を順に追記する(=全カレンダー分をまとめて洗い替え)。
;;   カレンダーを増やしたい場合は `my-gcal-calendars' に1行追加するだけでよい。
;;
;;   自動実行(after-save-hook等)はあえて行っていない。ネットワーク越しの
;;   処理を保存のたびに走らせるのは事故のもとなので、diaryを編集した後は
;;   手動で M-x my-gcal-sync-to-diary を実行する運用とする。
;;
;; ------------------------------------------------------------
;; Google Calendar側で直接追加した予定について
;; ------------------------------------------------------------
;; 洗い替え方式のため、diary-gcalに対して手を加えても次回同期で消える。
;; Google Calendar側(スマホ等)で予定を追加する分には全く問題ないが、
;; Emacs側からGoogle Calendarへの書き込みは行わない(あくまで一方向)。
;;
;;; Code:

;; 同期対象カレンダーのリスト。(名前 . 非公開URLファイルパス) の形式。
;; URL自体は ~/.env_source で秘密管理し、dotfilesには含めない。
;; カレンダーを増やす場合はこのリストに1行追加するだけでよい。
(defvar my-gcal-calendars
  '(("private" . "~/.env_source/tokens/gcal-diary-url")
    ("kukai"   . "~/.env_source/tokens/gcal-kukai-url"))
  "Alist of (calendar-name . secret-ical-url-file).")

;; diary-gcalは同期のたびに丸ごと作り直される(洗い替え方式)ので、
;; 絶対に手で編集しないこと。
;; パス自体は軽量な文字列計算のみなので、autoloadで起動時に評価しておき、
;; 90-calendar.el側からファイル存在チェックに使えるようにしている。
(defvar my-diary-gcal-file
  (locate-user-emacs-file "tmp/diary-gcal")
  "Auto-generated diary file synced from Google Calendar.
Do not edit by hand.")

;; これより古い予定は同期時に除外される。
(defvar my-gcal-months-back 12
  "Number of past months to keep when syncing from Google Calendar.")

(defun my-gcal--read-url (file)
  "Read a secret iCal URL (single line) from FILE.
Return nil if FILE is missing."
  (let ((f (expand-file-name file)))
    (when (file-exists-p f)
      (with-temp-buffer
        (insert-file-contents f)
        (string-trim (buffer-string))))))

;; diaryのエントリは「日付行 + インデントされた継続行」を1グループとして
;; 扱い、グループ単位で残す/捨てるを判定する。日付が
;; - `M/D/YYYY ...' 形式
;; - `%%(and (diary-block M D Y M D Y)) ...' 形式(繰り返し・期間指定)
;; のどちらにもマッチしない場合は、日付判定不能とみなし安全側に倒して残す
;; (例: diary-float を使った曜日指定の繰り返し予定など)。
(defun my-diary-filter-recent (input-file output-file months-back)
  "Write entries from INPUT-FILE newer than MONTHS-BACK months to OUTPUT-FILE."
  (let* ((cutoff-abs (- (calendar-absolute-from-gregorian
                         (calendar-current-date))
                        (* months-back 30)))
         (lines (with-temp-buffer
                  (insert-file-contents input-file)
                  (split-string (buffer-string) "\n")))
         (groups '())
         (cur nil))
    ;; 行をエントリ単位(継続行込み)でグループ化する
    (dolist (line lines)
      (if (and cur (or (string= line "") (string-match-p "^[ \t]" line)))
          (setq cur (append cur (list line)))
        (when cur (push cur groups))
        (setq cur (if (string= line "") nil (list line)))))
    (when cur (push cur groups))
    (setq groups (nreverse groups))
    ;; 各グループの先頭行から日付を判定し、新しいものだけ残す
    (with-temp-buffer
      (dolist (g groups)
        (let* ((head (car g))
               (date-abs
                (cond
                 ;; 通常形式: M/D/YYYY ...
                 ((string-match "^\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)" head)
                  (calendar-absolute-from-gregorian
                   (list (string-to-number (match-string 1 head))
                         (string-to-number (match-string 2 head))
                         (string-to-number (match-string 3 head)))))
                 ;; sexp形式(diary-block): 終了日を採用
                 ((string-match "diary-block \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)" head)
                  (calendar-absolute-from-gregorian
                   (list (string-to-number (match-string 4 head))
                         (string-to-number (match-string 5 head))
                         (string-to-number (match-string 6 head)))))
                 (t nil))))
          (when (or (null date-abs) (>= date-abs cutoff-abs))
            (dolist (l g) (insert l "\n")))))
      (write-region (point-min) (point-max) output-file))))

;; 処理の流れ(カレンダーごとに繰り返す):
;;   ダウンロード → icalendar変換 → 直近分にフィルタ → diary-gcalへ追記
;; diary-gcal自体は同期開始時に一旦空にする(=全カレンダー分をまとめて
;; 洗い替え)。手書きのdiary本体には一切触れない。
;; URLファイルが見つからないカレンダーはエラーにせずスキップする。
;; 一時ファイルはunwind-protectで必ず削除される。
;; 呼び出された時点でこのファイル全体がロードされるので、icalendarも
;; その時に一緒に読み込まれる(require不要)。
(defun my-gcal-sync-to-diary ()
  "Sync all calendars in `my-gcal-calendars' into `my-diary-gcal-file'."
  (interactive)
  (require 'icalendar)
  (write-region "" nil my-diary-gcal-file) ; 洗い替え開始:一旦空にする
  (let ((count 0))
    (dolist (cal my-gcal-calendars)
      (let* ((name (car cal))
             (url (my-gcal--read-url (cdr cal))))
        (if (not url)
            (message "my-gcal-sync-to-diary: %s のURLが見つかりません(%s), skip"
                     name (cdr cal))
          (let* ((tmp-ics      (make-temp-file "gcal-sync-" nil ".ics"))
                 (tmp-raw      (make-temp-file "gcal-sync-raw-"))
                 (tmp-filtered (make-temp-file "gcal-sync-filtered-")))
            (unwind-protect
                (progn
                  ;; 1. ダウンロード
                  (url-copy-file url tmp-ics t)
                  ;; 2. icsをdiary形式へ変換(一時ファイルへ)
                  (when (file-exists-p tmp-raw) (delete-file tmp-raw))
                  (icalendar-import-file tmp-ics tmp-raw)
                  ;; 3. 日付でフィルタ
                  (my-diary-filter-recent tmp-raw tmp-filtered my-gcal-months-back)
                  ;; 4. diary-gcalへ追記
                  (write-region (with-temp-buffer
                                  (insert-file-contents tmp-filtered)
                                  (buffer-string))
                                nil my-diary-gcal-file t)
                  (setq count (1+ count)))
              (dolist (f (list tmp-ics tmp-raw tmp-filtered))
                (when (file-exists-p f) (delete-file f))))))))
    (message "Google Calendar → diary 同期完了: %d件のカレンダー (%s)"
             count (format-time-string "%Y-%m-%d %H:%M"))))

(provide 'my-gcal-diary)
;;; my-gcal-diary.el ends here
