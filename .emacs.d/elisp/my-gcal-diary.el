;;; my-gcal-diary.el --- One-way sync from Google Calendar to Emacs diary.  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Google Calendar(複数カレンダー)から Emacs diary への一方向同期ロジック。
;; UI(calendar-mode、キーバインド、face等)には一切関与しない、
;; 「ダウンロード→変換→フィルタ→書き込み」だけを行う自己完結モジュール。
;;
;; ------------------------------------------------------------
;; 全体構成
;; ------------------------------------------------------------
;;   ~/.emacs.d/tmp/diary-gcal   … Google Calendar由来。自動生成専用、
;;                                  手で編集しないこと(同期のたびに
;;                                  まるごと上書きされる「洗い替え」方式)。
;;
;; 予定はスマホ等からGoogle Calendarに登録する運用を前提としており、
;; Emacs側で手書きのdiaryを併用することは想定していない。そのため
;; このファイルをそのまま `diary-file' として使う(利用側の設定は
;; 利用側のcalendar設定ファイルで行う)。
;;
;; ------------------------------------------------------------
;; 同期の仕組み(M-x my-gcal-sync-to-diary)
;; ------------------------------------------------------------
;;   対象カレンダーは `my-gcal-calendars' に (名前 . URLファイルパス) の
;;   リストとして登録する。各URLファイルには Google Calendarの
;;   「非公開URL」(secret address in iCal format)を1行だけ書いて
;;   ~/.env_source 配下に保存する(dotfilesには含めない)。
;;
;;   カレンダーごとに以下を繰り返し、結果を一時diaryファイルへ
;;   追記していく:
;;   1. 非公開URLから .ics をダウンロードする(認証不要、読み取り専用)。
;;   2. `icalendar-import-file' で .ics を diary形式のテキストに変換する。
;;   3. `my-diary-filter-recent' で「直近 my-gcal-months-back ヶ月分より
;;      新しい予定」だけに絞り込む(全履歴を毎回持ち込むと肥大化するため)。
;;      日付が判定できない繰り返し予定(diary-float等)は安全側に倒して残す。
;;
;;   全カレンダー分の処理が終わってから、一時diaryファイルの中身を
;;   まとめて `my-diary-gcal-file' へ一括コピーする(=洗い替え)。
;;   途中でエラーやタイムアウトが起きても本番ファイルには一切手を
;;   付けないため、直前の(完全な)状態がそのまま保たれる。
;;   カレンダーを増やしたい場合は `my-gcal-calendars' に1行追加するだけでよい。
;;
;;   自動実行(after-save-hook等)はあえて行っていない。ネットワーク越しの
;;   処理を毎回自動で走らせるのは事故のもとなので、
;;   kill-emacs-hook(利用側で設定)による終了時同期か、
;;   手動での M-x my-gcal-sync-to-diary 実行を基本の運用とする。
;;
;;   `icalendar-import-file' は内部で入力(.ics)・出力(diary形式)の
;;   両ファイルを find-file 系でバッファに開くが、そのバッファ自体は
;;   killしてくれない。一時ファイルを消してもバッファだけ残ると
;;   ivy-switch-buffer等の候補が汚れるため、`my-gcal--kill-file-buffer'
;;   で一時ファイル削除の直前に visit中バッファも合わせてkillする。
;;
;; ------------------------------------------------------------
;; Google Calendar側で直接追加した予定について
;; ------------------------------------------------------------
;; 洗い替え方式のため、diary-gcalに対して手を加えても次回同期で消える。
;; Google Calendar側(スマホ等)で予定を追加する分には全く問題ないが、
;; Emacs側からGoogle Calendarへの書き込みは行わない(あくまで一方向)。
;;
;;; Code:
(require 'calendar)

;; 同期対象カレンダーのリスト。(名前 . 非公開URLファイルパス) の形式。
;; URL自体は ~/.env_source で秘密管理し、dotfilesには含めない。
;; カレンダーを増やす場合はこのリストに1行追加するだけでよい。
(defvar my-gcal-calendars
  '(("private" . "~/.env_source/tokens/gcal-diary-url")
    ("kukai"   . "~/.env_source/tokens/gcal-kukai-url"))
  "Alist of (calendar-name . secret-ical-url-file).")

;; diary-gcalは同期のたびに丸ごと作り直される(洗い替え方式)ので、
;; 絶対に手で編集しないこと。
;; このファイルのパス・存在保証は自分自身(my-gcal-diary.el)の責務とし、
;; 利用側(90-gcal-agenda.el)は `require' した上でこの変数を参照するだけにする。
(defvar my-diary-gcal-file
  (locate-user-emacs-file "tmp/diary-gcal")
  "Auto-generated diary file synced from Google Calendar.
Do not edit by hand.")

;; このファイルの所有者はここ(my-gcal-diary.el)なので、
;; 存在しない場合の初期化もここで面倒を見る。
;; 利用側(90-gcal-agenda.el)は `my-diary-gcal-file' を参照するだけでよい。
(unless (file-exists-p my-diary-gcal-file)
  (make-empty-file my-diary-gcal-file t))

;; これより古い予定は同期時に除外される。
(defvar my-gcal-months-back 12
  "Number of past months to keep when syncing from Google Calendar.")

;; ------------------------------------------------------------
;; 複数日イベントの終了日補正について
;; ------------------------------------------------------------
;; iCalendarの仕様(RFC5545)上、複数日イベントの `DTEND' は非包含
;; (exclusive)、すなわち「最終日の翌日」を指す約束になっている
;; (例: 9/1〜9/3の3日間イベントなら DTEND=9/4)。
;; `icalendar-import-file' はこれを diary-block(両端含む=inclusive)
;; に変換するが、実測したところ終日複数日イベントについて終了日が
;; 実際の最終日より短く出る挙動が確認されたため、ここで補正する。
;; 0にすれば補正なし。ズレの実測値が変わった場合はこの値を調整する。
(defcustom my-gcal-block-end-date-correction 1
  "diary-blockの終了日に加算する日数(実測でズレている日数)."
  :type 'integer :group 'calendar)

(defun my-gcal--fix-block-end-dates (input-file output-file days)
  "INPUT-FILE中のdiary-blockの終了日にDAYS日加算してOUTPUT-FILEへ書き出す.
DAYSが0の場合は補正せずそのままコピーする."
  (if (zerop days)
      (copy-file input-file output-file t)
    (with-temp-buffer
      (insert-file-contents input-file)
      (goto-char (point-min))
      (while (re-search-forward
              "\\(diary-block [0-9]+ [0-9]+ [0-9]+ \\)\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)"
              nil t)
        (let* ((m (string-to-number (match-string 2)))
               (d (string-to-number (match-string 3)))
               (y (string-to-number (match-string 4)))
               (fixed (calendar-gregorian-from-absolute
                       (+ (calendar-absolute-from-gregorian (list m d y)) days))))
          (replace-match (format "\\1%d %d %d"
                                  (nth 0 fixed) (nth 1 fixed) (nth 2 fixed))
                          t)))
      (write-region (point-min) (point-max) output-file))))

(defun my-gcal--read-url (file)
  "Read a secret iCal URL (single line) from FILE.
Return nil if FILE is missing."
  (let ((f (expand-file-name file)))
    (when (file-exists-p f)
      (with-temp-buffer
        (insert-file-contents f)
        (string-trim (buffer-string))))))

;; icalendar-import-file等が一時ファイルをvisitして残したバッファを、
;; ファイル削除の前にkillしておくためのヘルパー。
;; 未保存扱いのまま残っているとkill時に確認プロンプトが出て
;; kill-emacs-hook経由の自動同期が止まりかねないので、
;; killする前に明示的に「未保存ではない」ことにしておく。
(defun my-gcal--kill-file-buffer (file)
  "Kill any buffer visiting FILE, without a save prompt."
  (let ((buf (find-buffer-visiting file)))
    (when buf
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf))))

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
;;   ダウンロード → icalendar変換 → 直近分にフィルタ → 一時diaryへ追記
;; 全カレンダー処理後にまとめて本番ファイルへコピーする(=洗い替え)。
;; こうすることで、途中でタイムアウトやエラーが起きても本番ファイル
;; (my-diary-gcal-file)には一切手を付けないまま終われるので、
;; 「一部のカレンダー分だけ反映された中途半端な状態」が本番に
;; 残ることがない。
;; URLファイルが見つからないカレンダーはエラーにせずスキップする。
;; 一時ファイルとそれをvisitしていたバッファは、unwind-protectで必ず
;; 削除・killされる。
;; 呼び出された時点でこのファイル全体がロードされるので、icalendarも
;; その時に一緒に読み込まれる(require不要)。
(defun my-gcal-sync-to-diary ()
  "Sync all calendars in `my-gcal-calendars' into `my-diary-gcal-file'.
Builds the merged result in a temp file first, and only replaces
`my-diary-gcal-file' once every calendar has been processed
successfully, so a mid-sync timeout or error never leaves the real
diary file in a half-written state."
  (interactive)
  (require 'icalendar)
  (let ((count 0)
        (tmp-diary (make-temp-file "gcal-sync-diary-")))
    (unwind-protect
        (progn
          (dolist (cal my-gcal-calendars)
            (let* ((name (car cal))
                   (url (my-gcal--read-url (cdr cal))))
              (if (not url)
                  (message "my-gcal-sync-to-diary: %s のURLが見つかりません(%s), skip"
                           name (cdr cal))
                (let* ((tmp-ics      (make-temp-file "gcal-sync-" nil ".ics"))
                       (tmp-raw      (make-temp-file "gcal-sync-raw-"))
                       (tmp-fixed    (make-temp-file "gcal-sync-fixed-"))
                       (tmp-filtered (make-temp-file "gcal-sync-filtered-")))
                  (unwind-protect
                      (progn
                        ;; 1. ダウンロード
                        (url-copy-file url tmp-ics t)
                        ;; 2. icsをdiary形式へ変換(一時ファイルへ)
                        (when (file-exists-p tmp-raw) (delete-file tmp-raw))
                        (icalendar-import-file tmp-ics tmp-raw)
                        ;; 3. 複数日イベントの終了日を補正
                        (my-gcal--fix-block-end-dates
                         tmp-raw tmp-fixed my-gcal-block-end-date-correction)
                        ;; 4. 日付でフィルタ
                        (my-diary-filter-recent tmp-fixed tmp-filtered my-gcal-months-back)
                        ;; 5. 一時diaryへ追記(本番ファイルにはまだ触れない)
                        (write-region (with-temp-buffer
                                        (insert-file-contents tmp-filtered)
                                        (buffer-string))
                                      nil tmp-diary t)
                        (setq count (1+ count)))
                    (dolist (f (list tmp-ics tmp-raw tmp-fixed tmp-filtered))
                      (my-gcal--kill-file-buffer f)
                      (when (file-exists-p f) (delete-file f))))))))
          ;; 1件以上成功していれば、まとめて本番ファイルへ反映する
          (when (> count 0)
            (copy-file tmp-diary my-diary-gcal-file t)))
      (my-gcal--kill-file-buffer tmp-diary)
      (when (file-exists-p tmp-diary) (delete-file tmp-diary)))
    (message "Google Calendar → diary 同期完了: %d件のカレンダー (%s)"
             count (format-time-string "%Y-%m-%d %H:%M"))))

(provide 'my-gcal-diary)
;;; my-gcal-diary.el ends here
