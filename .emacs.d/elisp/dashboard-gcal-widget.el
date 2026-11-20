;;; dashboard-gcal-widget.el --- Show Google Calendar in dashboard's Agenda.  -*- lexical-binding: t -*-

;; Author: minorugh

;;; Commentary:
;;
;; `gcal-sync' が生成する gcal.org を、dashboard.el 標準の agenda
;; ウィジェットに表示するための設定(表示日数・曜日表示・ソート・
;; 当日分のフィルタ補正)をまとめる。
;; gcal.org の生成と終了時同期は gcal-sync.el の担当。
;;
;;; Code:
(require 'gcal-sync)

(defgroup gcal-widget nil
  "Show Google Calendar in dashboard's Agenda."
  :group 'applications
  :prefix "gcal-widget-")

;; ------------------------------------------------------------
;; dashboardのAgendaウィジェットとの連携
;; ------------------------------------------------------------
;; 公開版としての汎用性のため、独自の item-generator(旧: `gcal-agenda')
;; は作らず、dashboard.el標準の `agenda' アイテムをそのまま使う。
;; 独自シンボルを新設すると、
;;   - `dashboard-heading-icons' が recents/bookmarks/projects/agenda/
;;     registers の5種類にしかアイコンを対応付けていないため、
;;     見出しアイコンが表示されなくなる
;;   - `dashboard-item-shortcuts' のショートカット("a"等)も
;;     利用側で登録し直しが必要になる
;; など、標準ウィジェットの恩恵を失うだけになるため。
;; 表示日数・並び順・見出し文言は、すべてdashboard.elが公開している
;; カスタマイズ変数経由で調整する。
(require 'org)
(require 'org-agenda)
(add-to-list 'org-agenda-files gcal-sync-org-file)

;; gcal.orgは外部同期で書き換えられるため、
;; visitしたバッファでauto-revertのメッセージを表示しない。
(with-current-buffer (find-file-noselect gcal-sync-org-file)
  (setq-local auto-revert-verbose nil))

(eval-and-compile (require 'dashboard-widgets))

(defcustom gcal-widget-agenda-days 30
  "Number of days to show in the dashboard Agenda.
Change with a plain `setq'; the value is re-read every time the
dashboard buffer is refreshed (see `gcal-widget--sync-agenda-heading')."
  :type 'integer :group 'gcal-widget)

;; 標準は「今日/週」の2択しかないため差し替える。生の関数再定義ではなく
;; advice-addにするのは、describe-functionで追跡でき
;; advice-removeで元に戻せるようにするため(以下のadvice-add共通の理由)。
;; `dashboard-due-date-for-agenda' はdashboard.el側で"--"の付かない
;; 通常の関数名で提供されており(READMEでも `dashboard-week-agenda' 経由で
;; 言及される準公開的な関数)、上書き対象として比較的安定している。
(defun gcal-widget--due-date-for-agenda ()
  "Return the upper time limit for the Agenda widget.
Overrides dashboard.el's own day/week choice with
`gcal-widget-agenda-days' via advice."
  (time-add (current-time) (* 86400 (1+ gcal-widget-agenda-days))))

(advice-add 'dashboard-due-date-for-agenda :override
            #'gcal-widget--due-date-for-agenda)

;; 見出し文言("Agenda for the coming week:")を、公式の
;; `dashboard-item-names'(見出し文字列の置換辞書)経由で
;; 日数入りの文言に差し替える。dashboard-refresh-buffer実行前に
;; 毎回計算し直すことで、`gcal-widget-agenda-days' を単純な
;; `setq' で変更しても(customize経由の:set関数を使わなくても)
;; 次回のdashboard表示・更新時に反映される。
(defun gcal-widget--sync-agenda-heading ()
  "Sync the dashboard Agenda heading text with `gcal-widget-agenda-days'."
  (setq dashboard-week-agenda t)
  (setq dashboard-item-names
        (cons (cons "Agenda for the coming week:"
                    (format "Agenda for the coming %d days:"
                            gcal-widget-agenda-days))
              (assoc-delete-all "Agenda for the coming week:"
                                 dashboard-item-names))))

(advice-add 'dashboard-refresh-buffer :before
            (lambda (&rest _) (gcal-widget--sync-agenda-heading)))

;; ロケール非依存の曜日表示用(format-time-string "%w" と同じく0=日曜)。
(defcustom gcal-widget-weekday-kanji ["日" "月" "火" "水" "木" "金" "土"]
  "Kanji weekday names indexed from Sunday."
  :type '(vector string string string string string string string)
  :group 'gcal-widget)

(defun gcal-widget--formatted-time ()
  "Format the agenda time with a Japanese weekday suffix."
  (when-let* ((time (or (org-get-scheduled-time (point))
                         (org-get-deadline-time (point))
                         (dashboard-agenda--entry-timestamp (point)))))
    (let ((dow (string-to-number (format-time-string "%w" time))))
      (concat (format-time-string dashboard-agenda-time-string-format time)
              (format "（%s）" (aref gcal-widget-weekday-kanji dow))))))

(advice-add 'dashboard-agenda--formatted-time :override
            #'gcal-widget--formatted-time)

;; dashboard.elの既定フィルタ(dashboard-filter-agenda-by-time)は、
;; SCHEDULED/DEADLINEを持たないプレーンなタイムスタンプのエントリ
;; (このウィジェットがorg変換で出力する形式)について
;; 「now < entry-timestamp」を要求する。タイムスタンプに時刻指定が
;; ないと00:00として解釈されるため、当日はどの時刻に実行しても
;; 既にその00:00を過ぎている扱いになり、当日分のエントリだけが
;; フィルタで弾かれてしまう(翌日以降は00:00がまだ未来なので通る)。
;; 下限を「now」ではなく「当日0時」にしたフィルタへ差し替えて回避する。
;; SCHEDULED/DEADLINEを持つ他のorgファイルのエントリの扱いは変えない。
(defun gcal-widget--filter-agenda-by-time ()
  "Like `dashboard-filter-agenda-by-time', but don't drop today's
plain-timestamp entries just because their implicit midnight has
already passed."
  (let* ((scheduled-time (org-get-scheduled-time (point)))
         (deadline-time (org-get-deadline-time (point)))
         (entry-timestamp (dashboard-agenda--entry-timestamp (point)))
         (due-date (dashboard-due-date-for-agenda))
         (today-start (apply #'encode-time
                              (append '(0 0 0) (nthcdr 3 (decode-time (current-time)))))))
    (unless (and (not (org-entry-is-done-p))
                 (not (org-in-archived-heading-p))
                 (or (and scheduled-time
                          (time-less-p scheduled-time due-date))
                     (and deadline-time
                          (time-less-p deadline-time due-date))
                     (and entry-timestamp
                          (not (time-less-p entry-timestamp today-start))
                          (time-less-p entry-timestamp due-date))))
      (point))))

(setq dashboard-filter-agenda-entry #'gcal-widget--filter-agenda-by-time)

;; 複数カレンダー(=複数org)をファイル順のまま並べると順序が
;; バラバラになるため、時刻順に明示的にソートする。
(setq dashboard-agenda-sort-strategy '(time-up))
;; 既定の "%-12:c"(カテゴリ名, 例 "gcal:")のプレフィックスは不要。
(setq dashboard-agenda-prefix-format "%s  ")

(provide 'dashboard-gcal-widget)
;;; dashboard-gcal-widget.el ends here
