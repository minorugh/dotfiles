;;; 90-gcal-agenda.el --- Google Calendar連携のAgenda表示設定.  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; コンセプト: calendar-mode(月表示バッファ)もdiaryへの手書きも一切
;; しない。予定はスマホ等からGoogle Calendarに登録する運用を前提とし、
;; Emacs側はそれを `diary-list-entries' で読み取ってdashboardの
;; Agendaウィジェットとして表示するだけの、読み取り専用ビューア。
;;
;; Google Calendarとの同期ロジック本体は elisp/my-gcal-diary.el に
;; 分離してある(ネットワーク越しの同期処理とUI/表示を疎結合に保つため)。
;; diary-gcalファイルのパス・存在保証は my-gcal-diary.el 側の責務なので、
;; ここでは `require' して変数・関数を使うだけ。
;; kill-emacs-hookでEmacs終了時に自動同期される他、
;; 手動で M-x my-gcal-sync-to-diary を実行することもできる。
;;
;; dashboard側 (01-dashboard.el) は `my-calendar-agenda-insert' を
;; 呼び出す薄いラッパーを置くだけで、Agendaの実体はすべてここに
;; 集約してある。
;;
;;; Code:

(leaf calendar
  :tag "builtin"
  :hook (kill-emacs-hook . my-gcal-sync-on-exit)
  :config
  (require 'diary-lib)
  (require 'my-gcal-diary)

  (defun my-gcal-sync-on-exit ()
    "Sync Google Calendar on Emacs exit, ignoring errors and timeouts."
    (with-timeout (10 (message "my-gcal-sync-to-diary: タイムアウトのためスキップ"))
      (ignore-errors (my-gcal-sync-to-diary))))

  ;; 手書きdiaryは使わない。gcal同期ファイルをdiary-fileそのものにする
  ;; ことで、"#include"によるマージという間接層を無くしている。
  (setq diary-file my-diary-gcal-file)

  ;; ============================================================
  ;;  Dashboard向け Agenda データ (直近の予定リスト)
  ;; ============================================================
  ;; カレンダーの月表示ではなく、今日を起点に直近 N 日分の diary
  ;; (Google Calendar 同期分を含む) をリスト表示するためのロジック。
  ;; 描画そのものは dashboard に依存させたくないので、見出しを描画
  ;; する関数 (dashboard-insert-heading 等) を引数として受け取る形にし、
  ;; dashboard 側 (01-dashboard.el) は薄いラッパーを置くだけにする。

  (defcustom my-calendar-agenda-days 60
    "Agenda ウィジェットで先読みする日数."
    :type 'integer :group 'calendar)

  (defcustom my-calendar-agenda-limit 8
    "Agenda ウィジェットに表示する予定の最大件数."
    :type 'integer :group 'calendar)

  (defcustom my-calendar-agenda-soon-days 3
    "この日数以内の予定を「近日」として太字強調する."
    :type 'integer :group 'calendar)

  (defface my-calendar-agenda-today
    '((t :background "#f2fa8c" :foreground "#282a36" :weight bold))
    "Agenda で今日の行を強調するフェイス.
calendar-mode の `calendar-today' とは独立させてあり、
calendar-mode を使わない構成でも影響を受けない." :group 'calendar)

  (defface my-calendar-agenda-saturday
    '((t :foreground "#8be9fd"))
    "Agenda で土曜日の日付を表示するフェイス." :group 'calendar)

  (defface my-calendar-agenda-sunday
    '((t :foreground "#ff5555"))
    "Agenda で日曜日の日付を表示するフェイス." :group 'calendar)

  (defface my-calendar-agenda-soon
    '((t :weight bold :foreground "#f8f8f2"))
    "Agenda で近日中(`my-calendar-agenda-soon-days' 以内)の予定名を
強調するフェイス." :group 'calendar)

  ;; `calendar-day-name' の英語省略形(Mon/Tue...)ではなく、
  ;; 日本語の曜日一文字(月火水木金土日)で表示するためのヘルパー。
  ;; `calendar-day-name-array' 等のロケール設定に依存させず、
  ;; ここだけで完結させる。
  (defconst my-calendar-agenda--day-name-ja ["日" "月" "火" "水" "木" "金" "土"]
    "`calendar-day-of-week' (0=日曜)に対応する日本語の曜日一文字表記.")

  (defun my-calendar-agenda--day-name (date)
    "DATE(calendar形式の (MONTH DAY YEAR))の曜日を日本語一文字で返す."
    (aref my-calendar-agenda--day-name-ja (calendar-day-of-week date)))

  (defun my-calendar-agenda-entries ()
    "今日から `my-calendar-agenda-days' 日分の diary 予定を取得する.
戻り値は ((MONTH DAY YEAR) STRING . ...) のリストで、日付順."
    (diary-list-entries (calendar-current-date) my-calendar-agenda-days t))

  ;; ── 複数日イベントの1行集約 ──────────────────────────────────
  ;; `diary-list-entries' は複数日にまたがる予定(iCalの複数日イベントは
  ;; icalendar-import-file により `%%(and (diary-block ...))' 形式で
  ;; diaryに変換される)を「該当する日ごとに1エントリ」として展開して
  ;; 返してくる。これをそのまま Agenda に出すと、たとえば3泊の出張が
  ;; `my-calendar-agenda-limit' の枠を3つ消費してしまい、件数制限に
  ;; 引っかかった際に最終日だけ表示から欠落する、といった不自然な
  ;; 切れ方をする。そこで、同一 diary-block 由来(specifierが同一)の
  ;; 連続エントリを1件の (START END TEXT) にまとめてから表示する。
  ;;
  ;; `diary-cyclic'(週次等の繰り返し予定)は specifier が
  ;; "%%(and (diary-cyclic ...))" で `diary-block' とは異なるため、
  ;; この集約の対象にはならず、従来通り毎回の発生日ごとに個別表示
  ;; される(これらは元々「別の日の予定」なので、まとめてしまうと
  ;; 逆に情報が失われるため意図的にそのまま残している)。

  (defun my-calendar-agenda--block-spec-p (spec)
    "SPEC が `diary-block' 由来(複数日イベント)の specifier かどうか.
`diary-list-entries' は、そのエントリが diary ファイルの先頭行に
来た場合に限り、返す specifier 先頭の \"%%\" を \"%\" 一個に落として
返してしまうことがある(diary-lib.el 側の既知の癖で、ファイル先頭
以外では正しく \"%%\" のまま返る)。`tmp/diary-gcal' は同期のたびに
丸ごと作り直される洗い替え方式のため、たまたま先頭に来た予定が
複数日イベントだと将来このケースに当たりうる。\"%%\" 固定ではなく
\"%\" 1個以上を許容することで、このケースでも正しく検出できるように
してある。"
    (and (stringp spec)
         (string-match-p "\\`%+(and (diary-block " spec)))

  (defun my-calendar-agenda--merge-blocks (entries)
    "ENTRIES(`diary-list-entries' の返り値, 日付昇順を前提)のうち、
`diary-block' 由来で specifier が同一(=同一イベント)の連続エントリ
を1件の (START-DATE END-DATE TEXT) にまとめて返す.
単日の予定や `diary-cyclic' はそのまま (DATE DATE TEXT) として通過する."
    (let (result)
      (while entries
        (let* ((entry (pop entries))
               (date  (nth 0 entry))
               (text  (nth 1 entry))
               (spec  (nth 2 entry))
               (end-date date))
          (when (my-calendar-agenda--block-spec-p spec)
            (while (and entries (equal (nth 2 (car entries)) spec))
              (setq end-date (nth 0 (car entries)))
              (pop entries)))
          (push (list date end-date text) result)))
      (nreverse result)))

  (defun my-calendar-agenda-insert (heading-insert-fn)
    "直近の予定を一覧挿入する.
HEADING-INSERT-FN は見出しを描画する関数(例: `dashboard-insert-heading')。
呼び出し元(dashboard等)に描画部分だけ委譲し、ここでは dashboard への
依存を持たない.

今日の予定(複数日イベントの期間中を含む)は専用のfaceで強調し、
それ以外は開始日の曜日で色分けし(土曜=水色, 日曜=赤)、
`my-calendar-agenda-soon-days' 以内に開始する予定は太字で強調する。
複数日にまたがる予定(`diary-block' 由来)は
`my-calendar-agenda--merge-blocks' により1行にまとめて
\"9/1-9/3 (Tue-Thu)\" のような期間表示にする."
    (funcall heading-insert-fn
             "Agenda:" nil
             (and (boundp 'dashboard-set-heading-icons)
                  dashboard-set-heading-icons
                  (fboundp 'nerd-icons-octicon)
                  (nerd-icons-octicon "nf-oct-calendar" :height 1.0 :face 'nerd-icons-purple)))
    (insert "\n\n")
    (let* ((entries (my-calendar-agenda--merge-blocks (my-calendar-agenda-entries)))
           (today     (calendar-current-date))
           (today-abs (calendar-absolute-from-gregorian today))
           (shown     (seq-take entries my-calendar-agenda-limit))
           (rest      (- (length entries) (length shown))))
      (if (null entries)
          (insert (propertize "  予定はありません" 'face 'font-lock-comment-face))
        (dolist (entry shown)
          (let* ((start      (nth 0 entry))
                 (end        (nth 1 entry))
                 (text       (nth 2 entry))
                 (start-abs  (calendar-absolute-from-gregorian start))
                 (end-abs    (calendar-absolute-from-gregorian end))
                 (days-away  (- start-abs today-abs))
                 ;; 複数日イベントは「今日がその期間に含まれているか」で
                 ;; today 扱いにする(開始日と一致するかだけでは、
                 ;; 開始2日目・3日目に今日を迎えたケースを見逃すため)。
                 (is-today   (and (<= start-abs today-abs) (<= today-abs end-abs)))
                 (is-soon    (and (not is-today) (> days-away 0)
                                   (<= days-away my-calendar-agenda-soon-days)))
                 (dow        (calendar-day-of-week start))
                 (date-str
                  (if (equal start end)
                      (format "%2d/%-2d (%s)"
                              (calendar-extract-month start)
                              (calendar-extract-day start)
                              (my-calendar-agenda--day-name start))
                    (format "%d/%d-%d/%d (%s-%s)"
                            (calendar-extract-month start) (calendar-extract-day start)
                            (calendar-extract-month end)   (calendar-extract-day end)
                            (my-calendar-agenda--day-name start)
                            (my-calendar-agenda--day-name end))))
                 (date-face  (let ((weekday-face (cond (is-today 'my-calendar-agenda-today)
                                                        ((= dow 0) 'my-calendar-agenda-sunday)
                                                        ((= dow 6) 'my-calendar-agenda-saturday)
                                                        (t 'font-lock-comment-face))))
                               (if is-soon
                                   (list 'my-calendar-agenda-soon weekday-face)
                                 weekday-face)))
                 (text-face  (cond (is-today 'my-calendar-agenda-today)
                                   (is-soon  'my-calendar-agenda-soon))))
            (insert (propertize (format "  %-16s " date-str) 'face date-face))
            (insert (propertize text 'face text-face))
            (insert "\n")))
        (when (> rest 0)
          (insert (propertize (format "  …他 %d 件" rest) 'face 'font-lock-comment-face))
          (insert "\n"))))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 90-gcal-agenda.el ends here
