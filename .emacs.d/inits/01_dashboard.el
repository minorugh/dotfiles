;;; 01_dashboard.el --- Dashboard configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

(leaf dashboard :ensure t
  :doc "An extracted startup screen"
  :if (display-graphic-p)
  :hook (after-init-hook . dashboard-setup-startup-hook)
  :bind ([home] . dashboard-toggle)
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-icon-type 'nerd-icons)
  (leaf page-break-lines :ensure t
    :doc "Display ^L page breaks as tidy horizontal lines"
    :after dashboard
    :global-minor-mode t)
  :config
  ;; Set the items
  (if (string-match "P1" (shell-command-to-string "uname -n"))
      (setq dashboard-items '((recents . 8)(agenda . 5)))
    (setq dashboard-items '((recents . 5))))
  ;; Set the title
  (setq dashboard-banner-logo-title
	(concat "GNU Emacs " emacs-version " kernel "
		(car (split-string (shell-command-to-string "uname -r")))  " Debian "
		(car (split-string (shell-command-to-string "cat /etc/debian_version"))) " 86_64 GNU/Linux"))
  ;; Set the banner
  (setq dashboard-startup-banner "~/.emacs.d/emacs.png")
  (setq dashboard-page-separator "\n\f\f\n")
  (setq show-week-agenda-p t)
  ;; Set the footer
  (setq dashboard-footer-messages
	'("Rejoice always. Pray without ceasing. In everything give thanks. (1Thes.5.16-18)"))
  (setq dashboard-footer-icon (nerd-icons-octicon "nf-oct-heart" :height 1.0 :face 'nerd-icons-lred))

  (defun dashboard-goto-recent-files ()
    "Go to recent files."
    (interactive)
    (let ((func (local-key-binding "r")))
      (and func (funcall func))))

  (defun dashboard-toggle ()
    "Switch buffer for dashboard and previous buffer."
    (interactive)
    (if (not (string= "*dashboard*" (buffer-name)))
	(open-dashboard)
      (previous-buffer)))

  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    (setq default-directory "~/")
    (delete-other-windows)
    (switch-to-buffer "*dashboard*")
    (dashboard-refresh-buffer)
    (dashboard-goto-recent-files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 01_dashboard.el ends here
