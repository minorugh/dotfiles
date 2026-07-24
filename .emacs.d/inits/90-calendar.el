;;; 90-calendardar.el --- Calendar configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;;
;;
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  Calendar
;; ============================================================
(defvar my-gcal-diary-url
  (let ((f (expand-file-name "~/.env_source/tokens/gcal-diary-url")))
    (when (file-exists-p f)
      (with-temp-buffer
        (insert-file-contents f)
        (string-trim (buffer-string)))))
  "Google Calendar非公開iCal URL.  ~/.env_sourceで秘密管理.")

(defvar my-diary-gcal-file
  (locate-user-emacs-file "tmp/diary-gcal")
  "Google Calendar由来の予定を書き込む専用ファイル。手で編集しないこと.")

(defvar my-gcal-months-back 12
  "Diary-gcalに残す過去分の月数.")

(defun my-diary-filter-recent (input-file output-file months-back)
  "INPUT-FILE の予定のうち、今日から MONTHS-BACK ヶ月分より新しいものだけを OUTPUT-FILE に書き出す."
  (let* ((cutoff-abs (- (calendar-absolute-from-gregorian
                         (calendar-current-date))
                        (* months-back 30)))
         (lines (with-temp-buffer
                  (insert-file-contents input-file)
                  (split-string (buffer-string) "\n")))
         (groups '())
         (cur nil))
    (dolist (line lines)
      (if (and cur (or (string= line "") (string-match-p "^[ \t]" line)))
          (setq cur (append cur (list line)))
        (when cur (push cur groups))
        (setq cur (if (string= line "") nil (list line)))))
    (when cur (push cur groups))
    (setq groups (nreverse groups))
    (with-temp-buffer
      (dolist (g groups)
        (let* ((head (car g))
               (date-abs
                (cond
                 ((string-match "^\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)" head)
                  (calendar-absolute-from-gregorian
                   (list (string-to-number (match-string 1 head))
                         (string-to-number (match-string 2 head))
                         (string-to-number (match-string 3 head)))))
                 ((string-match "diary-block \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)" head)
                  (calendar-absolute-from-gregorian
                   (list (string-to-number (match-string 4 head))
                         (string-to-number (match-string 5 head))
                         (string-to-number (match-string 6 head)))))
                 (t nil))))
          (when (or (null date-abs) (>= date-abs cutoff-abs))
            (dolist (l g) (insert l "\n")))))
      (write-region (point-min) (point-max) output-file))))

(defun my-gcal-sync-to-diary ()
  "Google Calendar(マイカレンダー)の予定をdiary-gcalへ一方向同期する.
diary-gcalは毎回まるごと作り直す(洗い替え)。手書きのdiary本体には触れない."
  (interactive)
  (unless my-gcal-diary-url
    (user-error "my-gcal-diary-url が設定されていません"))
  (let* ((tmp-ics (make-temp-file "gcal-sync-" nil ".ics"))
         (tmp-raw  (make-temp-file "gcal-sync-raw-")))
    (unwind-protect
        (progn
          (url-copy-file my-gcal-diary-url tmp-ics t)
          (when (file-exists-p tmp-raw) (delete-file tmp-raw))
          (icalendar-import-file tmp-ics tmp-raw)
          (my-diary-filter-recent tmp-raw my-diary-gcal-file my-gcal-months-back)
          (message "Google Calendar → diary 同期完了 (%s)"
                   (format-time-string "%Y-%m-%d %H:%M")))
      (dolist (f (list tmp-ics tmp-raw))
        (when (file-exists-p f) (delete-file f))))))

(leaf calendar
  :defvar calendar-holidays japanese-holidays
  :bind (("<f7>"   . calendar)
         (:calendar-mode-map
          ("<f7>" . calendar-exit)))
  :config
  (let ((diary (locate-user-emacs-file "tmp/diary")))
    (setq diary-file diary
          calendar-mark-diary-entries-flag t
          calendar-view-diary-initially-flag t)
    (unless (file-exists-p diary)
      (make-empty-file diary t)))
  (unless (file-exists-p my-diary-gcal-file)
    (make-empty-file my-diary-gcal-file t))

  (add-hook 'diary-list-entries-hook #'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook #'diary-mark-included-diary-files)
  (add-hook 'calendar-today-visible-hook #'diary-mark-entries t)
  (add-hook 'calendar-today-invisible-hook #'diary-mark-entries t)

  (set-face-attribute 'diary nil
                      :foreground "white"
                      :background "#d33682"
                      :weight 'bold)
  (set-face-attribute 'calendar-today nil
                      :foreground "white"
                      :background "#228b22"
                      :weight 'bold
                      :underline nil)

  (with-eval-after-load 'japanese-holidays
    (setq calendar-holidays
          (append japanese-holidays holiday-local-holidays))))


;; ============================================================
;;  Japanese Holidays
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
;; byte-compile-warnings: (not free-vars unresolved make-local)
;; End:
;;; 90-calendar.el ends here
