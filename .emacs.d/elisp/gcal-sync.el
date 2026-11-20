;;; gcal-sync.el --- Sync Google Calendar into an org file.  -*- lexical-binding: t -*-

;; Author: minorugh

;;; Commentary:
;;
;; Google Calendar(複数可)の秘密 iCal URL から予定を取得し、
;; `gcal-sync-org-file' (既定: ~/.emacs.d/tmp/gcal.org) へ一方向同期する。
;; このファイルは「gcal.org を作る」ことだけを担当する共通部品で、
;; 利用側(dashboard の Agenda、calendar の予定マークなど)はそれぞれ
;; (require 'gcal-sync) して gcal.org を読む。
;;
;; 利用側: dashboard-gcal-widget.el / inits/99-calendar.el
;;
;; 処理: ダウンロード→diary変換→終了日補正→期間フィルタ→org変換→
;;       洗い替え書き込み。
;;
;; gcal.org の形式(利用側との約束事):
;;   * 予定のタイトル
;;     <YYYY-MM-DD 曜 [HH:MM[-HH:MM]]>
;; 複数日の予定は1日ごとのエントリに展開される。
;; diary-cyclic 等の繰り返し予定だけは `<%%(SEXP)>' の形のまま残る。
;;
;; gcal.org は洗い替え方式の自動生成ファイルなので手編集不可。
;; 自動実行(after-save-hook等)は事故のもとになるためあえて行わず、
;; kill-emacs-hook での終了時同期か手動の M-x gcal-sync を使う。
;;
;;; Code:
;;; Code:
(require 'calendar)

(defgroup gcal-sync nil
  "Sync Google Calendar into an org file."
  :group 'applications
  :prefix "gcal-sync-")

;; URL自体はdotfiles外(例: ~/.env_source)で秘密管理する。パス例は任意。
(defcustom gcal-sync-calendars
  '(("private" . "~/.env_source/tokens/gcal-diary-url")
    ("kukai"   . "~/.env_source/tokens/gcal-kukai-url"))
  "Alist of (calendar-name . secret-ical-url-file)."
  :type '(alist :key-type string :value-type file)
  :group 'gcal-sync)

(defvar gcal-sync-org-file
  (locate-user-emacs-file "tmp/gcal.org")
  "Auto-generated org file synced from Google Calendar.
Do not edit by hand.")

(unless (file-exists-p gcal-sync-org-file)
  (make-empty-file gcal-sync-org-file t))

(defcustom gcal-sync-months-back 12
  "Number of past months to keep when syncing from Google Calendar."
  :type 'integer
  :group 'gcal-sync)

;; RFC5545のDTENDは非包含(最終日の翌日)だが、icalendar-import-fileの
;; diary-block変換では終日複数日イベントの終了日が実際より短く出る
;; ため、日数を足して補正する。0で補正なし。
(defcustom gcal-sync-block-end-date-correction 1
  "Number of days to add to diary-block end dates."
  :type 'integer :group 'gcal-sync)

(defun gcal-sync--fix-block-end-dates (input-file output-file days)
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

(defun gcal-sync--read-url (file)
  "Read a secret iCal URL (single line) from FILE.
Return nil if FILE is missing."
  (let ((f (expand-file-name file)))
    (when (file-exists-p f)
      (with-temp-buffer
        (insert-file-contents f)
        (string-trim (buffer-string))))))

;; icalendar-import-file等がvisitしたまま残すバッファをkillする。
;; 未保存扱いのままだと確認プロンプトでkill-emacs-hookが止まりうる。
(defun gcal-sync--kill-file-buffer (file)
  "Kill any buffer visiting FILE, without a save prompt."
  (let ((buf (find-buffer-visiting file)))
    (when buf
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf))))

(defun gcal-sync--group-diary-lines (lines)
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
(defun gcal-sync--filter-recent (input-file output-file months-back)
  "Write entries from INPUT-FILE newer than MONTHS-BACK months to OUTPUT-FILE."
  (let* ((cutoff-abs (- (calendar-absolute-from-gregorian
                         (calendar-current-date))
                        (* months-back 30)))
         (lines (with-temp-buffer
                  (insert-file-contents input-file)
                  (split-string (buffer-string) "\n")))
         (groups (gcal-sync--group-diary-lines lines)))
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

(defun gcal-sync--parse-sexp-line (line)
  "Parse LINE as a diary sexp entry."
  (when (string-match "\\`%+(" line)
    (let* ((start  (1- (match-end 0)))
           (parsed (read-from-string line start))
           (sexp   (car parsed))
           (end    (cdr parsed)))
      (cons sexp (string-trim (substring line end))))))

(defun gcal-sync--block-dates (sexp)
  "Return block dates from SEXP, or nil."
  (let ((inner (and (eq (car-safe sexp) 'and) (cadr sexp))))
    (when (eq (car-safe inner) 'diary-block)
      (cdr inner))))

(defun gcal-sync--org-timestamp (date &optional time)
  "Format DATE as an Org timestamp, with TIME if given.
TIME is a string such as \"10:00\" or \"10:00-11:00\"."
  (concat (format-time-string "%Y-%m-%d %a"
                              (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))
          (and time (concat " " time))))

(defun gcal-sync--format-org-entry (text date &optional time)
  "Format TEXT, DATE and optional TIME as an Org entry."
  (format "* %s\n  <%s>\n" text (gcal-sync--org-timestamp date time)))

(defun gcal-sync--format-org-block-entries (text start end)
  "Format TEXT as daily Org entries from START through END."
  (let ((day  (calendar-absolute-from-gregorian start))
        (last (calendar-absolute-from-gregorian end))
        (out ""))
    (while (<= day last)
      (setq out (concat out (gcal-sync--format-org-entry
                              text (calendar-gregorian-from-absolute day))))
      (setq day (1+ day)))
    out))

(defun gcal-sync--format-org-sexp-entry (text sexp)
  "Format TEXT and SEXP as an Org diary entry."
  (format "* %s\n  <%%%%%S>\n" text sexp))

(defun gcal-sync--diary-group-to-org (group)
  "Convert GROUP from diary format to Org format."
  (let ((head (car group)))
    (cond
     ;; 単日: M/D/YYYY [HH:MM[-HH:MM]] TEXT
     ((string-match "\\`\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\) *\\(.*\\)\\'" head)
      (let ((date (list (string-to-number (match-string 1 head))
                        (string-to-number (match-string 2 head))
                        (string-to-number (match-string 3 head))))
            (text (match-string 4 head)))
        (gcal-sync--format-org-entry
         text date
         (and (string-match "\\`[0-9]+:[0-9]+\\(?:-[0-9]+:[0-9]+\\)?" text)
              (match-string 0 text)))))
     ;; sexp形式(diary-block / diary-cyclic 等)
     (t (let ((parsed (gcal-sync--parse-sexp-line head)))
          (when parsed
            (let* ((sexp  (car parsed))
                   (text  (cdr parsed))
                   (block (gcal-sync--block-dates sexp)))
              (if block
                  (gcal-sync--format-org-block-entries
                   text
                   (list (nth 0 block) (nth 1 block) (nth 2 block))
                   (list (nth 3 block) (nth 4 block) (nth 5 block)))
                (gcal-sync--format-org-sexp-entry text sexp)))))))))

(defun gcal-sync--diary-to-org (input-file output-file)
  "Convert INPUT-FILE to Org format in OUTPUT-FILE."
  (let* ((lines  (with-temp-buffer
                    (insert-file-contents input-file)
                    (split-string (buffer-string) "\n")))
         (groups (gcal-sync--group-diary-lines lines)))
    (with-temp-buffer
      (dolist (g groups)
        (let ((org-entry (gcal-sync--diary-group-to-org g)))
          (if org-entry
              (insert org-entry)
            (message "gcal-sync: 未対応形式のためスキップ: %s" (car g)))))
      (write-region (point-min) (point-max) output-file))))

;; URLファイルが見つからないカレンダーはエラーにせずスキップする。
(defun gcal-sync ()
  "Sync all calendars in `gcal-sync-calendars' into `gcal-sync-org-file'.
Builds the merged result in a temp file first, and only replaces
`gcal-sync-org-file' once every calendar has been processed
successfully, so a mid-sync timeout or error never leaves the real
org file in a half-written state."
  (interactive)
  (require 'icalendar)
  (let ((count 0)
        (tmp-org (make-temp-file "gcal-sync-org-")))
    (unwind-protect
        (progn
          (dolist (cal gcal-sync-calendars)
            (let* ((name (car cal))
                   (url (gcal-sync--read-url (cdr cal))))
              (if (not url)
                  (message "gcal-sync: %s のURLが見つかりません(%s), skip"
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
                        (gcal-sync--fix-block-end-dates
                         tmp-raw tmp-fixed gcal-sync-block-end-date-correction)
                        ;; 4. 日付でフィルタ
                        (gcal-sync--filter-recent tmp-fixed tmp-filtered gcal-sync-months-back)
                        ;; 5. org形式に変換
                        (gcal-sync--diary-to-org tmp-filtered tmp-org-part)
                        ;; 6. 一時orgへ追記(本番ファイルにはまだ触れない)
                        (write-region (with-temp-buffer
                                        (insert-file-contents tmp-org-part)
                                        (buffer-string))
                                      nil tmp-org t)
                        (setq count (1+ count)))
                    (dolist (f (list tmp-ics tmp-raw tmp-fixed tmp-filtered tmp-org-part))
                      (gcal-sync--kill-file-buffer f)
                      (when (file-exists-p f) (delete-file f))))))))
          ;; 1件以上成功していれば、まとめて本番ファイルへ反映する
          (when (> count 0)
            (copy-file tmp-org gcal-sync-org-file t)))
      (gcal-sync--kill-file-buffer tmp-org)
      (when (file-exists-p tmp-org) (delete-file tmp-org)))
    (message "Google Calendar → org 同期完了: %d件のカレンダー (%s)"
             count (format-time-string "%Y-%m-%d %H:%M"))))

;; ------------------------------------------------------------
;; 終了時同期
;; ------------------------------------------------------------
;; 無名関数ではなく名前付きにして remove-hook で無効化できるようにする。
(defun gcal-sync--sync-on-exit ()
  "Sync Google Calendar on exit, ignoring errors and timeouts."
  (with-timeout (10 (message "gcal-sync: タイムアウトのためスキップ"))
    (ignore-errors (gcal-sync))))

(add-hook 'kill-emacs-hook #'gcal-sync--sync-on-exit)

(provide 'gcal-sync)
;;; gcal-sync.el ends here
