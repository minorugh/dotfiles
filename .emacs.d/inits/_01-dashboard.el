;;; 01-dashboard.el --- Dashboard configurations.    -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  Dashboard
;; ============================================================

;; Replacement for page-break-lines: render widget separators as a
;; colored horizontal rule without depending on the ^L character.
(defface my-dashboard-rule
  '((t :foreground "#6272a4" :bold nil))
  "The face of the separator line." :group 'dashboard)

(defun my-dashboard-separator ()
  "Return a full-width horizontal rule string for `dashboard-page-separator'."
  (let* ((width (- (window-total-width) 2))
         (line  (propertize (make-string width ?─) 'face 'my-dashboard-rule)))
    (concat "\n\n" line "\n\n")))


(leaf dashboard
  :ensure t
  :doc "An extensible startup screen."
  :if (display-graphic-p)
  :hook ((emacs-startup-hook  . open-dashboard)
         (dashboard-mode-hook
          . (lambda () (set-window-margins (selected-window) 2 2))))
  :bind ([home] . dashboard-toggle)

  :init
  ;; ── Icons ────────────────────────────────────────────────────
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons    t)
  (setq dashboard-icon-type        'nerd-icons)

  :config
  ;; ── Banner & title ───────────────────────────────────────────
  (setq dashboard-startup-banner  "~/.emacs.d/emacs.png")
  (setq dashboard-banner-logo-title
        (let* ((uname  (split-string (shell-command-to-string "uname -rn")))
               (debian (string-trim (shell-command-to-string "cat /etc/debian_version"))))
          (format "GNU Emacs %s kernel %s Debian %s x86_64 GNU/Linux"
                  emacs-version (cadr uname) debian)))

  ;; ── Layout ───────────────────────────────────────────────────
  ;; Content left-aligned (haiku centering handled in seiho-haiku.el)
  (setq dashboard-center-content nil)
  (setq dashboard-week-agenda    t)
  (defvar my-dashboard-agenda-days 60)

  (defun dashboard-due-date-for-agenda ()
    (time-add (current-time) (* 86400 my-dashboard-agenda-days)))

  ;; ── Separator ────────────────────────────────────────────────
  ;; Initialize separator; recomputed on each refresh via advice below.
  (setq dashboard-page-separator (my-dashboard-separator))

  ;; Recompute separator width before each refresh to follow window size.
  (advice-add 'dashboard-refresh-buffer :before
              (lambda (&rest _)
                (setq dashboard-page-separator (my-dashboard-separator))))

  ;; ── Widgets & items ──────────────────────────────────────────
  (defun dashboard-insert-haiku (_list-size)
    "今日の一句を dashboard に挿入する. 表示設定は seiho-haiku.el で調整."
    (require 'seiho-haiku)   ;; see ~/.emacs.d/elisp/seiho-haiku.el
    (seiho-haiku-insert-today #'dashboard-insert-heading))

  (add-to-list 'dashboard-item-generators
               '(haiku . dashboard-insert-haiku))

  ;; ── Agenda widget（直近の予定リスト） ─────────────────────────
  ;; Google Calendar → org の同期ロジックは my-gcal-diary.el に
  ;; 集約してある。表示は自作せず、dashboard標準の org-agenda連携
  ;; (組み込みの `dashboard-insert-agenda')にそのまま任せる。
  ;; ここでは同期先ファイルを `org-agenda-files' に登録するのと、
  ;; Emacs終了時の自動同期の設定だけを行う。
  (require 'my-gcal-diary)
  (require 'org)

  (setq org-agenda-files (list my-gcal-org-file))

  (defun my-gcal-sync-on-exit ()
    "Sync Google Calendar on Emacs exit, ignoring errors and timeouts."
    (with-timeout (10 (message "my-gcal-sync-to-org: タイムアウトのためスキップ"))
      (ignore-errors (my-gcal-sync-to-org))))

  (add-hook 'kill-emacs-hook #'my-gcal-sync-on-exit)

  ;; Items: main machine shows haiku + agenda; other machines show haiku only
  ;; (agenda項目は dashboard パッケージ組み込みの生成子を使う。
  ;;  独自の generator 登録は不要。)
  (if my-main-machine-p
      (setq dashboard-items '((haiku . 1) (agenda . 5)))
    (setq dashboard-items '((haiku . 1))))

  ;; ── Footer ───────────────────────────────────────────────────
  (setq dashboard-footer-messages '("God Bless Our Home And All Who Enter Here."))
  (setq dashboard-footer-icon
        (nerd-icons-octicon "nf-oct-home" :height 1.0 :face 'nerd-icons-lred))


  ;; ============================================================
  ;;  Dashboard Helper Commands
  ;; ============================================================

  (defun dashboard-toggle ()
    "Toggle between *dashboard* and the previous buffer."
    (interactive)
    (if (not (string= "*dashboard*" (buffer-name)))
        (open-dashboard)
      (previous-buffer)))

  (defun open-dashboard ()
    "Open *dashboard* and jump to the first widget."
    (interactive)
    (setq default-directory (expand-file-name "~/"))
    (delete-other-windows)
    (switch-to-buffer (get-buffer-create "*dashboard*"))
    (dashboard-refresh-buffer)
    (delete-other-windows))


  ;; ============================================================
  ;;  Startup Time Display
  ;; ============================================================

  (advice-add 'emacs-init-time :filter-return
              (lambda (_)
                (format "%.3f seconds"
                        (float-time (time-subtract after-init-time
                                                   before-init-time))))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 01-dashboard.el ends here
