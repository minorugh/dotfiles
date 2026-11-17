;;; 99-calendar.el --- Calendar with Japanese holidays and Google Calendar marks. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; F7 toggles the calendar (see 08-functions.el).
;; Saturdays, Sundays and Japanese holidays are colored.
;; Dates that have entries in the synced gcal.org are shown in bold underline
;; (see dashboard-gcal-widget.el).
;;
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  Calendar
;; ============================================================
(leaf calendar
  :hook ((calendar-today-visible-hook   . calendar-mark-today)
         (calendar-today-visible-hook   . my-calendar-mark-events)
         (calendar-today-invisible-hook . my-calendar-mark-events))
  :custom-face
  (calendar-today . '((t (:box t))))
  :config
  (keymap-set calendar-mode-map "<mouse-1>" #'my-calendar-open-gcal)
  (keymap-set calendar-mode-map "RET" #'my-calendar-open-gcal)
  (evil-define-key 'motion calendar-mode-map (kbd "RET") #'my-calendar-open-gcal)
  :preface
  (defface my-calendar-sunday
    '((t :foreground "#ff5555"))
    "Face for Sunday dates in the calendar."
    :group 'calendar)

  (defface my-calendar-saturday
    '((t :foreground "#8be9fd"))
    "Face for Saturday dates in the calendar."
    :group 'calendar)

  (defface my-calendar-event
    '((t :foreground "#f1fa8c" :weight bold))
    "Face for the dot marking dates with Google Calendar events."
    :group 'calendar)

  (defun my-calendar-mark-events ()
    "Mark today and later dates that have entries in the synced Google Calendar org file."
    (when (and (boundp 'gcal-widget-org-file)
               (file-exists-p gcal-widget-org-file))
      (let ((today (calendar-absolute-from-gregorian (calendar-current-date)))
            dates)
        (with-temp-buffer
          (insert-file-contents gcal-widget-org-file)
          (while (re-search-forward
                  "<\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" nil t)
            (let ((date (list (string-to-number (match-string 2))
                              (string-to-number (match-string 3))
                              (string-to-number (match-string 1)))))
              (unless (member date dates)
                (push date dates)))))
        (dolist (date dates)
          (when (and (calendar-date-is-visible-p date)
                     (>= (calendar-absolute-from-gregorian date) today))
            (calendar-mark-visible-date
             date (propertize "." 'face 'my-calendar-event)))))))

  (defun my-calendar-open-gcal (&optional event)
    "Open the Google Calendar day view of the date at point, or clicked in EVENT."
    (interactive (list (and (mouse-event-p last-command-event)
                            last-command-event)))
    (let ((date (calendar-cursor-to-date nil event)))
      (when date
        (browse-url-chromium
         (format "https://calendar.google.com/calendar/r/day/%d/%d/%d"
                 (nth 2 date) (nth 0 date) (nth 1 date)))
        (calendar-exit))))

  (defun my-calendar-toggle ()
    "Show the calendar, or hide it when it is already displayed."
    (interactive)
    (if (get-buffer-window "*Calendar*")
        (calendar-exit)
      (calendar))))

;; ============================================================
;;  Japanese Holidays
;; ============================================================
(leaf japanese-holidays
  :ensure t
  :after calendar
  :require t
  :config
  (setq calendar-holidays
        (append japanese-holidays holiday-local-holidays holiday-other-holidays))
  (setq calendar-mark-holidays-flag t)
  (setq japanese-holiday-weekend-marker
        '(my-calendar-sunday nil nil nil nil nil my-calendar-saturday))
  (add-hook 'calendar-today-visible-hook   #'japanese-holiday-mark-weekend)
  (add-hook 'calendar-today-invisible-hook #'japanese-holiday-mark-weekend))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 99-calendar.el ends here
