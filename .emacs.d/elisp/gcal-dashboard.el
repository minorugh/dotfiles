;;; gcal-dashboard.el --- Sync Google Calendar into an org file, and wire it into dashboard's Agenda.  -*- lexical-binding: t -*-

;; Author: minorugh
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (dashboard "1.8.0"))
;; URL: https://github.com/minorugh/gcal-dashboard
;; Keywords: calendar, org, convenience

;;; Commentary:
;;
;; Google Calendar(複数カレンダー)から org ファイルへの一方向同期ロジック、
;; および同期結果をdashboardのAgendaウィジェットに表示するための連携設定。
;; 「ダウンロード→変換→フィルタ→org化→書き込み」の同期部分と、
;; 「org-agenda-filesへの登録・dashboard標準agendaウィジェットの
;; カスタマイズ」という表示連携部分の2つで構成されている
;; (ファイル冒頭〜`(provide ...)'手前までが同期、それ以降が表示連携)。
;;
;; ------------------------------------------------------------
;; 全体構成
;; ------------------------------------------------------------
;;   ~/.emacs.d/tmp/gcal.org … Google Calendar由来。自動生成専用、
;;                              手で編集しないこと(同期のたびに
;;                              まるごと上書きされる「洗い替え」方式)。
;;
;; 予定はスマホ等からGoogle Calendarに登録する運用を前提としており、
;; Emacs側で手書きのorgエントリをこのファイルに併用することは想定
;; していない。org-agenda-filesへの登録もこのファイル内で完結するので、
;; 利用側は `require' した上で `dashboard-items' に
;; `(gcal-agenda . N)' を追加するだけでよい(dashboard.el標準の
;; `agenda' とは別キーとして登録してあるため、両方を並べて
;; 使うこともできる)。
;;
;; ------------------------------------------------------------
;; 同期の仕組み(M-x gcal-dashboard-sync)
;; ------------------------------------------------------------
;;   対象カレンダーは `gcal-dashboard-calendars' に (名前 . URLファイルパス) の
;;   リストとして登録する。各URLファイルには Google Calendarの
;;   「非公開URL」(secret address in iCal format)を1行だけ書いて
;;   ~/.env_source 配下に保存する(dotfilesには含めない)。
;;
;;   全カレンダー分の処理が終わってから、一時orgファイルの中身を
;;   まとめて `gcal-dashboard-org-file' へ一括コピーする(=洗い替え)。
;;   途中でエラーやタイムアウトが起きても本番ファイルには一切手を
;;   付けないため、直前の(完全な)状態がそのまま保たれる。
;;   カレンダーを増やしたい場合は `gcal-dashboard-calendars' に1行追加するだけでよい。
;;
;;   自動実行(after-save-hook等)はあえて行っていない。ネットワーク越しの
;;   処理を毎回自動で走らせるのは事故のもとなので、
;;   kill-emacs-hook(このファイル内で登録)による終了時同期か、
;;   手動での M-x gcal-dashboard-sync 実行を基本の運用とする。
;;
;;   `icalendar-import-file' は内部で入力(.ics)・出力(diary形式)の
;;   両ファイルを find-file 系でバッファに開くが、そのバッファ自体は
;;   killしてくれない。一時ファイルを消してもバッファだけ残ると
;;   ivy-switch-buffer等の候補が汚れるため、`gcal-dashboard--kill-file-buffer'
;;   で一時ファイル削除の直前に visit中バッファも合わせてkillする。
;;
;; ------------------------------------------------------------
;; Google Calendar側で直接追加した予定について
;; ------------------------------------------------------------
;; 洗い替え方式のため、gcal.orgに対して手を加えても次回同期で消える
;; (org標準のTODO/DONE切り替えも同様に、次回同期で失われる)。
;; Google Calendar側(スマホ等)で予定を追加する分には全く問題ないが、
;; Emacs側からGoogle Calendarへの書き込みは行わない(あくまで一方向)。
;;
;;; Code:
(require 'calendar)

(defgroup gcal-dashboard nil
  "Sync Google Calendar into an org file and show it in dashboard's Agenda."
  :group 'applications
  :prefix "gcal-dashboard-")

;; 同期対象カレンダーのリスト。(名前 . 非公開URLファイルパス) の形式。
;; URL自体は ~/.env_source で秘密管理し、dotfilesには含めない。
;; このパスは一例であり、置き場所自体はどこでも構わない。
;; カレンダーを増やす場合はこのリストに1行追加するだけでよい。
(defcustom gcal-dashboard-calendars
  '(("private" . "~/.env_source/tokens/gcal-diary-url")
    ("kukai"   . "~/.env_source/tokens/gcal-kukai-url"))
  "Alist of (calendar-name . secret-ical-url-file)."
  :type '(alist :key-type string :value-type file)
  :group 'gcal-dashboard)

;; gcal.orgは同期のたびに丸ごと作り直される(洗い替え方式)ので、
;; 絶対に手で編集しないこと。
;; gcal.orgの生成・管理はこのファイルで行う。
;; ファイルが存在しない場合の初期化と `org-agenda-files' への登録も
;; ここで行う。利用側は `require' するだけでよい.
(defvar gcal-dashboard-org-file
  (locate-user-emacs-file "tmp/gcal.org")
  "Auto-generated org file synced from Google Calendar.
Do not edit by hand.")

(unless (file-exists-p gcal-dashboard-org-file)
  (make-empty-file gcal-dashboard-org-file t))

(defcustom gcal-dashboard-months-back 12
  "Number of past months to keep when syncing from Google Calendar."
  :type 'integer
  :group 'gcal-dashboard)

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
;; (この補正はdiary形式の段階で行うため、後段のorg変換では
;; 補正済みの終了日をそのまま使うだけでよい)
(defcustom gcal-dashboard-block-end-date-correction 1
  "Number of days to add to diary-block end dates."
  :type 'integer :group 'gcal-dashboard)

(defun gcal-dashboard--fix-block-end-dates (input-file output-file days)
  "Write INPUT-FILE to OUTPUT-FILE with DAYS added to block end dates."
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

(defun gcal-dashboard--read-url (file)
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
(defun gcal-dashboard--kill-file-buffer (file)
  "Kill any buffer visiting FILE, without a save prompt."
  (let ((buf (find-buffer-visiting file)))
    (when buf
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf))))

;; フィルタとorg変換の両方で使うため、グループ化処理だけ共通化する。
(defun gcal-dashboard--group-diary-lines (lines)
  "Group diary LINES by entries."
  (let (groups cur)
    (dolist (line lines)
      (if (and cur (or (string= line "") (string-match-p "^[ \t]" line)))
          (setq cur (append cur (list line)))
        (when cur (push cur groups))
        (setq cur (if (string= line "") nil (list line)))))
    (when cur (push cur groups))
    (nreverse groups)))

;; グループの先頭行が
;; - `M/D/YYYY ...' 形式
;; - `%%(and (diary-block M D Y M D Y)) ...' 形式(期間指定)
;; のどちらにもマッチしない場合は、日付判定不能とみなし安全側に倒して残す
;; (例: diary-cyclic を使った繰り返し予定など)。
(defun gcal-dashboard--filter-recent (input-file output-file months-back)
  "Write entries from INPUT-FILE newer than MONTHS-BACK months to OUTPUT-FILE."
  (let* ((cutoff-abs (- (calendar-absolute-from-gregorian
                         (calendar-current-date))
                        (* months-back 30)))
         (lines (with-temp-buffer
                  (insert-file-contents input-file)
                  (split-string (buffer-string) "\n")))
         (groups (gcal-dashboard--group-diary-lines lines)))
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

;; ------------------------------------------------------------
;; diary形式 → org形式 変換
;; ------------------------------------------------------------
;; icalendar-import-file の出力(diary形式)を直接書き換えるのではなく、
;; 既存のダウンロード・変換・補正・フィルタ処理はそのまま活かし、
;; 最後にorg形式へ変換する処理だけを追加する。
;;
;; 対応する見出し行のパターンは3種類:
;;   - `M/D/YYYY TEXT'                              … 単日の予定
;;   - `%%(and (diary-block M D Y M D Y)) TEXT'      … 複数日の予定
;;   - それ以外の `%%(SEXP) TEXT'(diary-cyclic等)    … 繰り返し予定
;;
;; 複数日の予定は、冒頭のCommentaryで述べた通り「日ごとに単日
;; エントリを1件ずつ展開する」方式に変換する
;; (dashboardのAgendaウィジェットが範囲タイムスタンプ
;; `<開始日>--<終了日>' の終了日を拾えないための対応。詳細は
;; ファイル冒頭のCommentary参照)。それ以外のsexp形式(繰り返し
;; 予定等)は、org が diary sexp をそのまま評価できる機能を使い、
;; `<%%(SEXP)>' の形で素通しする。
;;
;; どちらのパターンにも一致しない行は変換不能として読み飛ばし、
;; メッセージでログを残す(diary運用時の「安全側に倒して残す」とは
;; 異なり、org化できない情報を無理に残しても後段で解釈できないため)。

(defun gcal-dashboard--parse-sexp-line (line)
  "Parse LINE as a diary sexp entry."
  (when (string-match "\\`%+(" line)
    (let* ((start  (1- (match-end 0)))
           (parsed (read-from-string line start))
           (sexp   (car parsed))
           (end    (cdr parsed)))
      (cons sexp (string-trim (substring line end))))))

(defun gcal-dashboard--block-dates (sexp)
  "Return block dates from SEXP, or nil."
  (let ((inner (and (eq (car-safe sexp) 'and) (cadr sexp))))
    (when (eq (car-safe inner) 'diary-block)
      (cdr inner))))

(defun gcal-dashboard--org-timestamp (date)
  "Format DATE as an Org timestamp."
  (format-time-string "%Y-%m-%d %a"
                       (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))

(defun gcal-dashboard--format-org-entry (text date)
  "Format TEXT and DATE as an Org entry."
  (format "* %s\n  <%s>\n" text (gcal-dashboard--org-timestamp date)))

(defun gcal-dashboard--format-org-block-entries (text start end)
  "Format TEXT as daily Org entries from START through END."
  (let ((day  (calendar-absolute-from-gregorian start))
        (last (calendar-absolute-from-gregorian end))
        (out ""))
    (while (<= day last)
      (setq out (concat out (gcal-dashboard--format-org-entry
                              text (calendar-gregorian-from-absolute day))))
      (setq day (1+ day)))
    out))

(defun gcal-dashboard--format-org-sexp-entry (text sexp)
  "Format TEXT and SEXP as an Org diary entry."
  (format "* %s\n  <%%%%%S>\n" text sexp))

(defun gcal-dashboard--diary-group-to-org (group)
  "Convert GROUP from diary format to Org format."
  (let ((head (car group)))
    (cond
     ;; 単日: M/D/YYYY TEXT
     ((string-match "\\`\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\) *\\(.*\\)\\'" head)
      (gcal-dashboard--format-org-entry
       (match-string 4 head)
       (list (string-to-number (match-string 1 head))
             (string-to-number (match-string 2 head))
             (string-to-number (match-string 3 head)))))
     ;; sexp形式(diary-block / diary-cyclic 等)
     (t (let ((parsed (gcal-dashboard--parse-sexp-line head)))
          (when parsed
            (let* ((sexp  (car parsed))
                   (text  (cdr parsed))
                   (block (gcal-dashboard--block-dates sexp)))
              (if block
                  (gcal-dashboard--format-org-block-entries
                   text
                   (list (nth 0 block) (nth 1 block) (nth 2 block))
                   (list (nth 3 block) (nth 4 block) (nth 5 block)))
                (gcal-dashboard--format-org-sexp-entry text sexp)))))))))

(defun gcal-dashboard--diary-to-org (input-file output-file)
  "Convert INPUT-FILE to Org format in OUTPUT-FILE."
  (let* ((lines  (with-temp-buffer
                    (insert-file-contents input-file)
                    (split-string (buffer-string) "\n")))
         (groups (gcal-dashboard--group-diary-lines lines)))
    (with-temp-buffer
      (dolist (g groups)
        (let ((org-entry (gcal-dashboard--diary-group-to-org g)))
          (if org-entry
              (insert org-entry)
            (message "gcal-dashboard-sync: 未対応形式のためスキップ: %s" (car g)))))
      (write-region (point-min) (point-max) output-file))))

;; 全カレンダー処理後にまとめて本番ファイルへコピーする(=洗い替え)。
;; こうすることで、途中でタイムアウトやエラーが起きても本番ファイル
;; (gcal-dashboard-org-file)には一切手を付けないまま終われるので、
;; 「一部のカレンダー分だけ反映された中途半端な状態」が本番に
;; 残ることがない。
;; URLファイルが見つからないカレンダーはエラーにせずスキップする。
;; 一時ファイルとそれをvisitしていたバッファは、unwind-protectで必ず
;; 削除・killされる。
(defun gcal-dashboard-sync ()
  "Sync all calendars in `gcal-dashboard-calendars' into `gcal-dashboard-org-file'.
Builds the merged result in a temp file first, and only replaces
`gcal-dashboard-org-file' once every calendar has been processed
successfully, so a mid-sync timeout or error never leaves the real
org file in a half-written state."
  (interactive)
  (require 'icalendar)
  (let ((count 0)
        (tmp-org (make-temp-file "gcal-sync-org-")))
    (unwind-protect
        (progn
          (dolist (cal gcal-dashboard-calendars)
            (let* ((name (car cal))
                   (url (gcal-dashboard--read-url (cdr cal))))
              (if (not url)
                  (message "gcal-dashboard-sync: %s のURLが見つかりません(%s), skip"
                           name (cdr cal))
                (let* ((tmp-ics      (make-temp-file "gcal-sync-" nil ".ics"))
                       (tmp-raw      (make-temp-file "gcal-sync-raw-"))
                       (tmp-fixed    (make-temp-file "gcal-sync-fixed-"))
                       (tmp-filtered (make-temp-file "gcal-sync-filtered-"))
                       (tmp-org-part (make-temp-file "gcal-sync-org-part-")))
                  (unwind-protect
                      (progn
                        ;; 1. ダウンロード
                        (url-copy-file url tmp-ics t)
                        ;; 2. icsをdiary形式へ変換(一時ファイルへ)
                        (when (file-exists-p tmp-raw) (delete-file tmp-raw))
                        (icalendar-import-file tmp-ics tmp-raw)
                        ;; 3. 複数日イベントの終了日を補正
                        (gcal-dashboard--fix-block-end-dates
                         tmp-raw tmp-fixed gcal-dashboard-block-end-date-correction)
                        ;; 4. 日付でフィルタ
                        (gcal-dashboard--filter-recent tmp-fixed tmp-filtered gcal-dashboard-months-back)
                        ;; 5. org形式に変換
                        (gcal-dashboard--diary-to-org tmp-filtered tmp-org-part)
                        ;; 6. 一時orgへ追記(本番ファイルにはまだ触れない)
                        (write-region (with-temp-buffer
                                        (insert-file-contents tmp-org-part)
                                        (buffer-string))
                                      nil tmp-org t)
                        (setq count (1+ count)))
                    (dolist (f (list tmp-ics tmp-raw tmp-fixed tmp-filtered tmp-org-part))
                      (gcal-dashboard--kill-file-buffer f)
                      (when (file-exists-p f) (delete-file f))))))))
          ;; 1件以上成功していれば、まとめて本番ファイルへ反映する
          (when (> count 0)
            (copy-file tmp-org gcal-dashboard-org-file t)))
      (gcal-dashboard--kill-file-buffer tmp-org)
      (when (file-exists-p tmp-org) (delete-file tmp-org)))
    (message "Google Calendar → org 同期完了: %d件のカレンダー (%s)"
             count (format-time-string "%Y-%m-%d %H:%M"))))

;; ------------------------------------------------------------
;; dashboardのAgendaウィジェットとの連携
;; ------------------------------------------------------------
;; ここから下は「同期したorgファイルをdashboardにどう見せるか」という
;; 表示側の設定。同期ロジック本体とは関心事が違うが、利用側の設定を
;; 薄く保つため、gcal関連の設定としてこちらにまとめて持たせている。
;;
;; dashboard.el標準のagendaウィジェット(org-map-entriesベース)を
;; そのまま使い、以下の3点だけ差し替える:
;;   1. 表示日数を「今日/週」の2択から `gcal-dashboard-agenda-days'
;;      日数指定に拡張(標準の `dashboard-due-date-for-agenda' を再定義)
;;   2. 複数カレンダーを使うとファイル順のまま出て日付順にならないため、
;;      時刻順に明示的にソート
;;   3. 見出しの "gcal:" のようなカテゴリ表示は不要なので消す
(require 'org)
(require 'org-agenda)
(add-to-list 'org-agenda-files gcal-dashboard-org-file)

;; gcal.orgは外部同期で書き換えられるため、
;; visitしたバッファでauto-revertのメッセージを表示しない。
(with-current-buffer (find-file-noselect gcal-dashboard-org-file)
  (setq-local auto-revert-verbose nil))

;; `dashboard-insert-section' はマクロ(内部で `el' を暗黙に束縛する
;; anaphoricマクロ)なので、バイトコンパイル時にもこのマクロが
;; 見えている必要がある。実行時だけの `require' だと、コンパイル時に
;; マクロ未定義のまま「普通の関数」として誤ってコンパイルされ、
;; 引数(下の `dashboard-insert-agenda' 内の `el' を使う式)が
;; 先に評価されて "Symbol's value as variable is void: el" になる。
(eval-and-compile (require 'dashboard-widgets))

(defcustom gcal-dashboard-agenda-days 30
  "Number of days to show in the dashboard Agenda."
  :type 'integer :group 'gcal-dashboard)

;; dashboard.el標準の `dashboard-due-date-for-agenda' は
;; `dashboard-week-agenda' の「今日/週」の2択しか選べない。
;; 生の関数再定義ではなく `advice-add' で差し替えることで、
;; 何が差し替えたかを `describe-function' で追跡でき、
;; `(advice-remove 'dashboard-due-date-for-agenda
;;                  #'gcal-dashboard--due-date-for-agenda)'
;; で元の挙動に戻せるようにしてある。
(defun gcal-dashboard--due-date-for-agenda ()
  "Return the upper time limit for the Agenda widget.
Overrides dashboard.el's own day/week choice with
`gcal-dashboard-agenda-days' via advice (see `gcal-dashboard-mode')."
  (time-add (current-time) (* 86400 (1+ gcal-dashboard-agenda-days))))

(advice-add 'dashboard-due-date-for-agenda :override
            #'gcal-dashboard--due-date-for-agenda)

;; dashboard.el標準の `dashboard-insert-agenda' を直接上書きするのではなく、
;; 別名で定義して `dashboard-item-generators' に新しい項目
;; (`gcal-agenda') として追加登録する。利用側は `dashboard-items' に
;; `(agenda . N)' ではなく `(gcal-agenda . N)' と書けば、標準のAgenda
;; ウィジェットと共存できる(標準の方は上書きされずそのまま残る)。
(defun gcal-dashboard-insert-agenda (list-size)
  "Insert the coming `gcal-dashboard-agenda-days' days of Agenda entries for LIST-SIZE items.
A near-verbatim copy of dashboard.el's own `dashboard-insert-agenda',
registered under a separate `gcal-agenda' key so the standard `agenda'
item generator is left untouched."
  (require 'org-agenda)
  (dashboard-insert-section
   (format "Agenda for the coming %d days:" gcal-dashboard-agenda-days)
   (dashboard-agenda--sorted-agenda)
   list-size
   'gcal-agenda
   (dashboard-get-shortcut 'gcal-agenda)
   `(lambda (&rest _)
      (let ((file (get-text-property 0 'dashboard-agenda-file ,el))
            (point (get-text-property 0 'dashboard-agenda-loc ,el)))
        (funcall dashboard-agenda-action file point)))
   (format "%s" el)))

(add-to-list 'dashboard-item-generators
             '(gcal-agenda . gcal-dashboard-insert-agenda))

;; `gcal-agenda' はdashboard.el組み込みの `agenda' とは別のキーなので、
;; ショートカットキーは既定では割り当てられない。必要であれば
;; (add-to-list 'dashboard-item-shortcuts '(gcal-agenda . "a"))
;; のように利用側で追加できる。

;; 曜日をロケールに依存させず漢字1文字で表示するための対応表(calendar-day-of-week/
;; format-time-string "%w" と同じく 0=日曜始まり)。
(defcustom gcal-dashboard-weekday-kanji ["日" "月" "火" "水" "木" "金" "土"]
  "Kanji weekday names indexed from Sunday."
  :type '(vector string string string string string string string)
  :group 'gcal-dashboard)

;; dashboard-widgets標準の同名関数を直接上書きせず、adviceで差し替える
;; (`gcal-dashboard--due-date-for-agenda' と同じ理由)。
;; `dashboard-agenda-time-string-format' はそのまま日付部分のフォーマットに使う。
(defun gcal-dashboard--formatted-time ()
  "Format the agenda time with a Japanese weekday suffix."
  (when-let* ((time (or (org-get-scheduled-time (point))
                         (org-get-deadline-time (point))
                         (dashboard-agenda--entry-timestamp (point)))))
    (let ((dow (string-to-number (format-time-string "%w" time))))
      (concat (format-time-string dashboard-agenda-time-string-format time)
              (format "（%s）" (aref gcal-dashboard-weekday-kanji dow))))))

(advice-add 'dashboard-agenda--formatted-time :override
            #'gcal-dashboard--formatted-time)

;; 複数カレンダー(=複数org)をファイル順のまま並べると順序が
;; バラバラになるため、時刻順に明示的にソートする。
(setq dashboard-agenda-sort-strategy '(time-up))
;; 既定の "%-12:c"(カテゴリ名, 例 "gcal:")のプレフィックスは不要。
(setq dashboard-agenda-prefix-format "%s  ")

;; Emacs終了時にGoogle Calendarと同期する(タイムアウト・エラーは無視).
;; 無名関数ではなく名前付きにしておくことで、
;; `(remove-hook 'kill-emacs-hook #'gcal-dashboard--sync-on-exit)'
;; で誰でも簡単に無効化できるようにしてある。
(defun gcal-dashboard--sync-on-exit ()
  "Sync Google Calendar on exit, ignoring errors and timeouts."
  (with-timeout (10 (message "gcal-dashboard-sync: タイムアウトのためスキップ"))
    (ignore-errors (gcal-dashboard-sync))))

(add-hook 'kill-emacs-hook #'gcal-dashboard--sync-on-exit)

(provide 'gcal-dashboard)
;;; gcal-dashboard.el ends here
