;;; dashboard-gcal-widget.el --- Sync Google Calendar into an org file, and wire it into dashboard's Agenda.  -*- lexical-binding: t -*-

;; Author: minorugh
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (dashboard "1.8.0"))
;; URL: https://github.com/minorugh/dashboard-widget-extensions
;; Keywords: calendar, org, convenience

;;; Commentary:
;;
;; Google Calendar(複数可)を org ファイルへ一方向同期し、dashboard.el の
;; Agendaウィジェットに `gcal-agenda' として表示する。使い方はREADME参照。
;;
;; 構成: `(provide ...)' 手前までが同期処理(ダウンロード→diary変換→
;; 終了日補正→期間フィルタ→org変換→洗い替え書き込み)、それ以降が
;; dashboard連携(表示日数・曜日表示・ソートのカスタマイズ)。
;;
;; gcal.orgは洗い替え方式の自動生成ファイルなので手編集不可。
;; 自動実行(after-save-hook等)は事故のもとになるためあえて行わず、
;; kill-emacs-hookでの終了時同期か手動の M-x gcal-dashboard-sync を使う。
;;
;;; Code:
(require 'calendar)

(defgroup gcal-dashboard nil
  "Sync Google Calendar into an org file and show it in dashboard's Agenda."
  :group 'applications
  :prefix "gcal-dashboard-")

;; URL自体はdotfiles外(例: ~/.env_source)で秘密管理する。パス例は任意。
(defcustom gcal-dashboard-calendars
  '(("private" . "~/.env_source/tokens/gcal-diary-url")
    ("kukai"   . "~/.env_source/tokens/gcal-kukai-url"))
  "Alist of (calendar-name . secret-ical-url-file)."
  :type '(alist :key-type string :value-type file)
  :group 'gcal-dashboard)

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

;; RFC5545のDTENDは非包含(最終日の翌日)だが、icalendar-import-fileの
;; diary-block変換では終日複数日イベントの終了日が実際より短く出る
;; ため、日数を足して補正する。0で補正なし。
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

;; icalendar-import-file等がvisitしたまま残すバッファをkillする。
;; 未保存扱いのままだと確認プロンプトでkill-emacs-hookが止まりうる。
(defun gcal-dashboard--kill-file-buffer (file)
  "Kill any buffer visiting FILE, without a save prompt."
  (let ((buf (find-buffer-visiting file)))
    (when buf
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf))))

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

;; 日付判定できない行(diary-cyclic等)は安全側に倒してそのまま残す。
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
;; 見出し行のパターンは3種類: 単日(M/D/YYYY)・複数日(diary-block、
;; dashboardが範囲タイムスタンプを扱えないため日ごとに展開)・
;; それ以外のsexp(diary-cyclic等、`<%%(SEXP)>' のまま素通し)。
;; どれにも一致しない行は変換不能としてログを残し読み飛ばす。

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

;; URLファイルが見つからないカレンダーはエラーにせずスキップする。
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
(add-to-list 'org-agenda-files gcal-dashboard-org-file)

;; gcal.orgは外部同期で書き換えられるため、
;; visitしたバッファでauto-revertのメッセージを表示しない。
(with-current-buffer (find-file-noselect gcal-dashboard-org-file)
  (setq-local auto-revert-verbose nil))

(eval-and-compile (require 'dashboard-widgets))

(defcustom gcal-dashboard-agenda-days 30
  "Number of days to show in the dashboard Agenda.
Change with a plain `setq'; the value is re-read every time the
dashboard buffer is refreshed (see `gcal-dashboard--sync-agenda-heading')."
  :type 'integer :group 'gcal-dashboard)

;; 標準は「今日/週」の2択しかないため差し替える。生の関数再定義ではなく
;; advice-addにするのは、describe-functionで追跡でき
;; advice-removeで元に戻せるようにするため(以下のadvice-add共通の理由)。
;; `dashboard-due-date-for-agenda' はdashboard.el側で"--"の付かない
;; 通常の関数名で提供されており(READMEでも `dashboard-week-agenda' 経由で
;; 言及される準公開的な関数)、上書き対象として比較的安定している。
(defun gcal-dashboard--due-date-for-agenda ()
  "Return the upper time limit for the Agenda widget.
Overrides dashboard.el's own day/week choice with
`gcal-dashboard-agenda-days' via advice."
  (time-add (current-time) (* 86400 (1+ gcal-dashboard-agenda-days))))

(advice-add 'dashboard-due-date-for-agenda :override
            #'gcal-dashboard--due-date-for-agenda)

;; 見出し文言("Agenda for the coming week:")を、公式の
;; `dashboard-item-names'(見出し文字列の置換辞書)経由で
;; 日数入りの文言に差し替える。dashboard-refresh-buffer実行前に
;; 毎回計算し直すことで、`gcal-dashboard-agenda-days' を単純な
;; `setq' で変更しても(customize経由の:set関数を使わなくても)
;; 次回のdashboard表示・更新時に反映される。
(defun gcal-dashboard--sync-agenda-heading ()
  "Sync the dashboard Agenda heading text with `gcal-dashboard-agenda-days'."
  (setq dashboard-week-agenda t)
  (setq dashboard-item-names
        (cons (cons "Agenda for the coming week:"
                    (format "Agenda for the coming %d days:"
                            gcal-dashboard-agenda-days))
              (assoc-delete-all "Agenda for the coming week:"
                                 dashboard-item-names))))

(advice-add 'dashboard-refresh-buffer :before
            (lambda (&rest _) (gcal-dashboard--sync-agenda-heading)))

;; ロケール非依存の曜日表示用(format-time-string "%w" と同じく0=日曜)。
(defcustom gcal-dashboard-weekday-kanji ["日" "月" "火" "水" "木" "金" "土"]
  "Kanji weekday names indexed from Sunday."
  :type '(vector string string string string string string string)
  :group 'gcal-dashboard)

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

;; 無名関数ではなく名前付きにして remove-hook で無効化できるようにする。
(defun gcal-dashboard--sync-on-exit ()
  "Sync Google Calendar on exit, ignoring errors and timeouts."
  (with-timeout (10 (message "gcal-dashboard-sync: タイムアウトのためスキップ"))
    (ignore-errors (gcal-dashboard-sync))))

(add-hook 'kill-emacs-hook #'gcal-dashboard--sync-on-exit)

(provide 'dashboard-gcal-widget)
;;; dashboard-gcal-widget.el ends here
