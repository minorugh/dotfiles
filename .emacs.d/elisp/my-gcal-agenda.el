;;; my-gcal-agenda.el --- Sync Google Calendar into an org file, and wire it into dashboard's Agenda.  -*- lexical-binding: t -*-
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
;; 利用側(01-dashboard.el)は `require' するだけでよい。
;;
;; ------------------------------------------------------------
;; 同期の仕組み(M-x my-gcal-sync-to-org)
;; ------------------------------------------------------------
;;   対象カレンダーは `my-gcal-calendars' に (名前 . URLファイルパス) の
;;   リストとして登録する。各URLファイルには Google Calendarの
;;   「非公開URL」(secret address in iCal format)を1行だけ書いて
;;   ~/.env_source 配下に保存する(dotfilesには含めない)。
;;
;;   カレンダーごとに以下を繰り返し、結果を一時orgファイルへ
;;   追記していく:
;;   1. 非公開URLから .ics をダウンロードする(認証不要、読み取り専用)。
;;   2. `icalendar-import-file' で .ics を diary形式のテキストに変換する
;;      (org形式への直接変換は行わず、実績のあるdiary変換を経由する)。
;;   3. `my-gcal--fix-block-end-dates' で複数日イベントの終了日を補正する
;;      (iCalendarのDTENDは非包含のため)。
;;   4. `my-diary-filter-recent' で「直近 my-gcal-months-back ヶ月分より
;;      新しい予定」だけに絞り込む(全履歴を毎回持ち込むと肥大化するため)。
;;      日付が判定できない繰り返し予定(diary-cyclic等)は安全側に倒して残す。
;;   5. `my-gcal--diary-to-org' で、ここまでできたdiary形式のテキストを
;;      org形式(見出し+タイムスタンプ)に変換する。
;;      - 単日の予定 → 単一タイムスタンプ `<Y-M-D Day>'
;;      - 複数日の予定(diary-block) → 日ごとに単日エントリを1件ずつ
;;        並べる(M-x org-agenda自体は範囲タイムスタンプ
;;        `<開始日>--<終了日>' を (1/3) のような進捗表示で正しく
;;        扱えるが、dashboardパッケージのagendaウィジェットは
;;        独自簡易実装(org-map-entries + `org-entry-get' の
;;        特殊プロパティ"TIMESTAMP")で開始日しか拾えず、範囲の
;;        終了日を無視してしまう。dashboard表示を優先し、
;;        diary運用時と同じ「日ごとに展開」する方式に戻す)
;;      - それ以外のsexp形式(diary-cyclic等の繰り返し予定) →
;;        org は diary sexp をそのまま評価できるため、
;;        `<%%(SEXP)>' の形で素通しする(手動でorgのrepeater構文に
;;        変換する必要はない)
;;
;;   全カレンダー分の処理が終わってから、一時orgファイルの中身を
;;   まとめて `my-gcal-org-file' へ一括コピーする(=洗い替え)。
;;   途中でエラーやタイムアウトが起きても本番ファイルには一切手を
;;   付けないため、直前の(完全な)状態がそのまま保たれる。
;;   カレンダーを増やしたい場合は `my-gcal-calendars' に1行追加するだけでよい。
;;
;;   自動実行(after-save-hook等)はあえて行っていない。ネットワーク越しの
;;   処理を毎回自動で走らせるのは事故のもとなので、
;;   kill-emacs-hook(このファイル内で登録)による終了時同期か、
;;   手動での M-x my-gcal-sync-to-org 実行を基本の運用とする。
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
;; 洗い替え方式のため、gcal.orgに対して手を加えても次回同期で消える
;; (org標準のTODO/DONE切り替えも同様に、次回同期で失われる)。
;; Google Calendar側(スマホ等)で予定を追加する分には全く問題ないが、
;; Emacs側からGoogle Calendarへの書き込みは行わない(あくまで一方向)。
;;
;;; Code:
(require 'calendar)

;; 同期対象カレンダーのリスト。(名前 . 非公開URLファイルパス) の形式。
;; URL自体は ~/.env_source で秘密管理し、dotfilesには含めない。
;; このパスは一例であり、置き場所自体はどこでも構わない。
;; カレンダーを増やす場合はこのリストに1行追加するだけでよい。
(defvar my-gcal-calendars
  '(("private" . "~/.env_source/tokens/gcal-diary-url")
    ("kukai"   . "~/.env_source/tokens/gcal-kukai-url"))
  "Alist of (calendar-name . secret-ical-url-file).")

;; gcal.orgは同期のたびに丸ごと作り直される(洗い替え方式)ので、
;; 絶対に手で編集しないこと。
;; このファイルのパス・存在保証、および `org-agenda-files' への登録も
;; すべて自分自身(my-gcal-agenda.el)の責務とする。
;; 利用側(01-dashboard.el)は `require' するだけでよい。
(defvar my-gcal-org-file
  (locate-user-emacs-file "tmp/gcal.org")
  "Auto-generated org file synced from Google Calendar.
Do not edit by hand.")

;; このファイルの所有者はここ(my-gcal-diary.el)なので、
;; 存在しない場合の初期化もここで面倒を見る。
(unless (file-exists-p my-gcal-org-file)
  (make-empty-file my-gcal-org-file t))

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
;; (この補正はdiary形式の段階で行うため、後段のorg変換では
;; 補正済みの終了日をそのまま使うだけでよい)
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
;; 扱う。フィルタとorg変換の両方で使うため、グループ化処理だけ共通化する。
(defun my-gcal--group-diary-lines (lines)
  "LINES(diary形式のファイル内容を行分割したもの)を、
「日付行 + インデントされた継続行」のグループ単位でまとめて返す.
各要素は行のリスト(先頭が日付行)."
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
(defun my-diary-filter-recent (input-file output-file months-back)
  "Write entries from INPUT-FILE newer than MONTHS-BACK months to OUTPUT-FILE."
  (let* ((cutoff-abs (- (calendar-absolute-from-gregorian
                         (calendar-current-date))
                        (* months-back 30)))
         (lines (with-temp-buffer
                  (insert-file-contents input-file)
                  (split-string (buffer-string) "\n")))
         (groups (my-gcal--group-diary-lines lines)))
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
;; 複数日の予定だけは、org のタイムスタンプ範囲
;; `<開始日>--<終了日>' に変換する。それ以外のsexp形式(繰り返し予定等)は、
;; org が diary sexp をそのまま評価できる機能を使い、
;; `<%%(SEXP)>' の形で素通しする。
;;
;; どちらのパターンにも一致しない行は変換不能として読み飛ばし、
;; メッセージでログを残す(diary運用時の「安全側に倒して残す」とは
;; 異なり、org化できない情報を無理に残しても後段で解釈できないため)。

(defun my-gcal--parse-sexp-line (line)
  "diary sexpエントリ行(\"%%(...) TEXT\")をパースする.
戻り値は (SEXP . TEXT) のcons。sexp形式でなければnil."
  (when (string-match "\\`%+(" line)
    (let* ((start  (1- (match-end 0)))
           (parsed (read-from-string line start))
           (sexp   (car parsed))
           (end    (cdr parsed)))
      (cons sexp (string-trim (substring line end))))))

(defun my-gcal--block-dates (sexp)
  "SEXP が `(and (diary-block M1 D1 Y1 M2 D2 Y2))' 形式なら
(M1 D1 Y1 M2 D2 Y2) を返す。そうでなければnil."
  (let ((inner (and (eq (car-safe sexp) 'and) (cadr sexp))))
    (when (eq (car-safe inner) 'diary-block)
      (cdr inner))))

(defun my-gcal--org-timestamp (date)
  "DATE(calendar形式の (M D Y))をorgタイムスタンプの中身の文字列
(例: \"2026-09-01 Tue\")に変換する."
  (format-time-string "%Y-%m-%d %a"
                       (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))

(defun my-gcal--format-org-entry (text date)
  "TEXTを見出しとして、DATEの日付をタイムスタンプに持つ
org形式の単日エントリ文字列(見出し+タイムスタンプの2行)を返す."
  (format "* %s\n  <%s>\n" text (my-gcal--org-timestamp date)))

(defun my-gcal--format-org-block-entries (text start end)
  "TEXTを見出しとして、START〜END(両端含む)の日付ぶん、
単日エントリを1日1件ずつ並べた文字列を返す(dashboardのagenda
ウィジェットが範囲タイムスタンプの終了日を無視してしまうための
回避策。M-x org-agendaでは同じ予定が日数分並ぶだけで実用上問題ない)."
  (let ((day  (calendar-absolute-from-gregorian start))
        (last (calendar-absolute-from-gregorian end))
        (out ""))
    (while (<= day last)
      (setq out (concat out (my-gcal--format-org-entry
                              text (calendar-gregorian-from-absolute day))))
      (setq day (1+ day)))
    out))

(defun my-gcal--format-org-sexp-entry (text sexp)
  "TEXTを見出しとして、SEXPをそのままdiary sexpタイムスタンプとして
埋め込んだorg形式のエントリ文字列を返す(繰り返し予定用)."
  (format "* %s\n  <%%%%%S>\n" text sexp))

(defun my-gcal--diary-group-to-org (group)
  "diaryのエントリ1件分(GROUP, 継続行込みの行リスト)をorg形式の
文字列に変換する。認識できない形式であればnilを返す(=読み飛ばす)."
  (let ((head (car group)))
    (cond
     ;; 単日: M/D/YYYY TEXT
     ((string-match "\\`\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\) *\\(.*\\)\\'" head)
      (my-gcal--format-org-entry
       (match-string 4 head)
       (list (string-to-number (match-string 1 head))
             (string-to-number (match-string 2 head))
             (string-to-number (match-string 3 head)))))
     ;; sexp形式(diary-block / diary-cyclic 等)
     (t (let ((parsed (my-gcal--parse-sexp-line head)))
          (when parsed
            (let* ((sexp  (car parsed))
                   (text  (cdr parsed))
                   (block (my-gcal--block-dates sexp)))
              (if block
                  (my-gcal--format-org-block-entries
                   text
                   (list (nth 0 block) (nth 1 block) (nth 2 block))
                   (list (nth 3 block) (nth 4 block) (nth 5 block)))
                (my-gcal--format-org-sexp-entry text sexp)))))))))

(defun my-gcal--diary-to-org (input-file output-file)
  "INPUT-FILE(diary形式、変換・補正・フィルタ済み)をorg形式に
変換してOUTPUT-FILEへ書き出す."
  (let* ((lines  (with-temp-buffer
                    (insert-file-contents input-file)
                    (split-string (buffer-string) "\n")))
         (groups (my-gcal--group-diary-lines lines)))
    (with-temp-buffer
      (dolist (g groups)
        (let ((org-entry (my-gcal--diary-group-to-org g)))
          (if org-entry
              (insert org-entry)
            (message "my-gcal-sync-to-org: 未対応形式のためスキップ: %s" (car g)))))
      (write-region (point-min) (point-max) output-file))))

;; 処理の流れ(カレンダーごとに繰り返す):
;;   ダウンロード → icalendar変換 → 終了日補正 → 直近分にフィルタ
;;   → org形式に変換 → 一時orgへ追記
;; 全カレンダー処理後にまとめて本番ファイルへコピーする(=洗い替え)。
;; こうすることで、途中でタイムアウトやエラーが起きても本番ファイル
;; (my-gcal-org-file)には一切手を付けないまま終われるので、
;; 「一部のカレンダー分だけ反映された中途半端な状態」が本番に
;; 残ることがない。
;; URLファイルが見つからないカレンダーはエラーにせずスキップする。
;; 一時ファイルとそれをvisitしていたバッファは、unwind-protectで必ず
;; 削除・killされる。
(defun my-gcal-sync-to-org ()
  "Sync all calendars in `my-gcal-calendars' into `my-gcal-org-file'.
Builds the merged result in a temp file first, and only replaces
`my-gcal-org-file' once every calendar has been processed
successfully, so a mid-sync timeout or error never leaves the real
org file in a half-written state."
  (interactive)
  (require 'icalendar)
  (let ((count 0)
        (tmp-org (make-temp-file "gcal-sync-org-")))
    (unwind-protect
        (progn
          (dolist (cal my-gcal-calendars)
            (let* ((name (car cal))
                   (url (my-gcal--read-url (cdr cal))))
              (if (not url)
                  (message "my-gcal-sync-to-org: %s のURLが見つかりません(%s), skip"
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
                        (my-gcal--fix-block-end-dates
                         tmp-raw tmp-fixed my-gcal-block-end-date-correction)
                        ;; 4. 日付でフィルタ
                        (my-diary-filter-recent tmp-fixed tmp-filtered my-gcal-months-back)
                        ;; 5. org形式に変換
                        (my-gcal--diary-to-org tmp-filtered tmp-org-part)
                        ;; 6. 一時orgへ追記(本番ファイルにはまだ触れない)
                        (write-region (with-temp-buffer
                                        (insert-file-contents tmp-org-part)
                                        (buffer-string))
                                      nil tmp-org t)
                        (setq count (1+ count)))
                    (dolist (f (list tmp-ics tmp-raw tmp-fixed tmp-filtered tmp-org-part))
                      (my-gcal--kill-file-buffer f)
                      (when (file-exists-p f) (delete-file f))))))))
          ;; 1件以上成功していれば、まとめて本番ファイルへ反映する
          (when (> count 0)
            (copy-file tmp-org my-gcal-org-file t)))
      (my-gcal--kill-file-buffer tmp-org)
      (when (file-exists-p tmp-org) (delete-file tmp-org)))
    (message "Google Calendar → org 同期完了: %d件のカレンダー (%s)"
             count (format-time-string "%Y-%m-%d %H:%M"))))

;; ------------------------------------------------------------
;; dashboardのAgendaウィジェットとの連携
;; ------------------------------------------------------------
;; ここから下は「同期したorgファイルをdashboardにどう見せるか」という
;; 表示側の設定。同期ロジック本体とは関心事が違うが、01-dashboard.el
;; (01-dashboard.el)を薄く保つため、gcal関連の設定としてこちらに
;; まとめて持たせている。
;;
;; dashboard.el標準のagendaウィジェット(org-map-entriesベース)を
;; そのまま使い、以下の3点だけ差し替える:
;;   1. 表示日数を「今日/週」の2択から `my-dashboard-agenda-days'
;;      日数指定に拡張(標準の `dashboard-due-date-for-agenda' を再定義)
;;   2. 複数カレンダーを使うとファイル順のまま出て日付順にならないため、
;;      時刻順に明示的にソート
;;   3. 見出しの "gcal:" のようなカテゴリ表示は不要なので消す
(require 'org)
(add-to-list 'org-agenda-files my-gcal-org-file)

;; `dashboard-insert-section' はマクロ(内部で `el' を暗黙に束縛する
;; anaphoricマクロ)なので、バイトコンパイル時にもこのマクロが
;; 見えている必要がある。実行時だけの `require' だと、コンパイル時に
;; マクロ未定義のまま「普通の関数」として誤ってコンパイルされ、
;; 引数(下の `dashboard-insert-agenda' 内の `el' を使う式)が
;; 先に評価されて "Symbol's value as variable is void: el" になる。
(eval-and-compile (require 'dashboard-widgets))

(defcustom my-dashboard-agenda-days 30
  "DashboardのAgendaウィジェットで何日先までの予定を表示するか."
  :type 'integer :group 'dashboard)

(defun dashboard-due-date-for-agenda ()
  "Agendaに含める予定の上限日時(`my-dashboard-agenda-days' 日後)を返す.
dashboard.el標準の「今日/週」の2択(`dashboard-week-agenda')の
代わりに、日数を自由に指定できるようにするための再定義."
  (time-add (current-time) (* 86400 (1+ my-dashboard-agenda-days))))

(defun dashboard-insert-agenda (list-size)
  "直近 `my-dashboard-agenda-days' 日分のAgendaを一覧表示する.
dashboard.el標準の `dashboard-insert-agenda' を、見出し文言だけ
日数に合わせて差し替えたもの(本体の処理は標準のものをそのまま利用)."
  (require 'org-agenda)
  (dashboard-insert-section
   (format "Agenda for the coming %d days:" my-dashboard-agenda-days)
   (dashboard-agenda--sorted-agenda)
   list-size
   'agenda
   (dashboard-get-shortcut 'agenda)
   `(lambda (&rest _)
      (let ((file (get-text-property 0 'dashboard-agenda-file ,el))
            (point (get-text-property 0 'dashboard-agenda-loc ,el)))
        (funcall dashboard-agenda-action file point)))
   (format "%s" el)))

;; 複数カレンダー(=複数org)をファイル順のまま並べると順序が
;; バラバラになるため、時刻順に明示的にソートする。
(setq dashboard-agenda-sort-strategy '(time-up))
;; 既定の "%-12:c"(カテゴリ名, 例 "gcal:")のプレフィックスは不要。
(setq dashboard-agenda-prefix-format " %s ")

;; Emacs終了時にGoogle Calendarと同期する(タイムアウト・エラーは無視).
(add-hook 'kill-emacs-hook
          (lambda ()
            (with-timeout (10 (message "my-gcal-sync-to-org: タイムアウトのためスキップ"))
              (ignore-errors (my-gcal-sync-to-org)))))

(provide 'my-gcal-agenda)
;;; my-gcal-agenda.el ends here
