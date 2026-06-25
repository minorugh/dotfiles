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
    (let* ((width (- (window-total-width) 1))
           (line  (propertize (make-string width ?─) 'face 'my-dashboard-rule)))
      (concat "\n\n" line "\n\n")))


(leaf dashboard
  :ensure t
  :doc "An extensible startup screen."
  :if (display-graphic-p)
  :hook ((emacs-startup-hook  . open-dashboard)
         (dashboard-mode-hook . (lambda ()
				  (set-window-margins (selected-window) 2 2))))
  :bind ([home] . dashboard-toggle)
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons    t)
  (setq dashboard-icon-type        'nerd-icons)

  (defun dashboard-insert-haiku (_list-size)
    "今日の一句を dashboard に挿入する. 表示設定は seiho-haiku.el で調整."
    (require 'seiho-haiku)   ;; see ~/.emacs.d/elisp/seiho-haiku.el
    (seiho-haiku-insert-today #'dashboard-insert-heading))

  :config
  ;; Initialize separator; recomputed on each refresh via advice below.
  (setq dashboard-page-separator (my-dashboard-separator))

  ;; Recompute separator width before each refresh to follow window size.
  (advice-add 'dashboard-refresh-buffer :before
              (lambda (&rest _)
                (setq dashboard-page-separator (my-dashboard-separator))))

  ;; Layout — content left-aligned (haiku centering handled in seiho-haiku.el)
  (setq dashboard-center-content nil)

  ;; Widget registration
  (add-to-list 'dashboard-item-generators
               '(haiku . dashboard-insert-haiku))

  ;; Items: P1 shows haiku + recents; other machines show recents only
  (if (string-match "P1" (system-name))
      (setq dashboard-items '((haiku . 1) (recents . 5)))
    (setq dashboard-items '((recents . 5))))

  ;; Title: combine uname + Debian version into one shell call
  (setq dashboard-banner-logo-title
        (let* ((uname  (split-string (shell-command-to-string "uname -rn")))
               (debian (string-trim (shell-command-to-string "cat /etc/debian_version"))))
          (format "GNU Emacs %s kernel %s Debian %s x86_64 GNU/Linux"
                  emacs-version (cadr uname) debian)))

  ;; Banner & layout
  (setq dashboard-startup-banner  "~/.emacs.d/emacs.png")
  (setq dashboard-page-separator "\n\n")
  ;; (setq dashboard-page-separator  "\n\f\f\n")
  (setq dashboard-week-agenda     t)

  ;; Footer
  (setq dashboard-footer-messages '("God Bless Our Home And All Who Enter Here."))
  (setq dashboard-footer-icon
        (nerd-icons-octicon "nf-oct-home" :height 1.0 :face 'nerd-icons-lred))


  ;; ============================================================
  ;;  Dashboard Helper Commands
  ;; ============================================================

  (defun dashboard-goto-recent-files ()
    "Jump to the recent-files widget."
    (interactive)
    (let ((func (local-key-binding "r")))
      (and func (funcall func))))

  (defun dashboard-toggle ()
    "Toggle between *dashboard* and the previous buffer."
    (interactive)
    (if (not (string= "*dashboard*" (buffer-name)))
        (open-dashboard)
      (previous-buffer)))

  (defun open-dashboard ()
    "Open *dashboard* and jump to the first widget."
    (interactive)
    (setq default-directory "~/")
    (delete-other-windows)
    (switch-to-buffer (get-buffer-create "*dashboard*"))
    (dashboard-refresh-buffer)
    (dashboard-goto-recent-files)
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
