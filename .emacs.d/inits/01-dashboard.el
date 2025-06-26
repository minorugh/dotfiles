;;; 01-dashboard.el --- Dashboard configurations.    -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

(leaf dashboard :ensure t
  :doc "An extracted startup screen"
  :if (display-graphic-p)
  :defun nerd-icons-octicon open-dashboard dashboard-refresh-buffer dashboard-goto-recent-files
  :hook ((after-init-hook . dashboard-setup-startup-hook)
	 (dashboard-mode-hook . (lambda () (set-window-margins (selected-window) 1 1))))
  :bind ([home] . dashboard-toggle)
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-icon-type 'nerd-icons)
  :config
  (leaf page-break-lines :ensure t
    :doc "Display ^L page breaks as tidy horizontal lines"
    :global-minor-mode t)
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
    (dashboard-goto-recent-files))

  (defun emacs-init-time ()
    "Overwrite `emacs-init-time' defined in time.el."
    (interactive)
    (let ((str
	   (format "%.3f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))))
      (if (called-interactively-p 'interactive)
	  (message "%s" str)
	str))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 01-dashboard.el ends here
