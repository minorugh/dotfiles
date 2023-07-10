;; 01_dashboard.el --- Dashboard configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf dashboard
  :doc "An extensible emacs startup screen"
  :url "https://github.com/emacs-dashboard/emacs-dashboard"
  :ensure t
  :if (display-graphic-p)
  :hook ((after-init-hook    . dashboard-setup-startup-hook)
		 (after-init-hook    . global-page-break-lines-mode)
		 (emacs-startup-hook . open-dashboard))
  :bind (([home] . open-dashboard)
		 (:dashboard-mode-map
		  ("c" . chromium-calendar)
		  ("n" . chromium-nhk-news)
		  ("w" . chromium-weather)
		  ("h" . chromium-homepage)
		  ("m" . sylpheed)
		  ("s" . slack)
		  ("." . hydra-browse/body)
		  ([home] . quit-dashboard)))
  :init
  (leaf page-break-lines :ensure t)
  (leaf nerd-icons :ensure t)
  (setq dashboard-icon-type 'nerd-icons)
  :config
  (setq dashboard-banner-logo-title
		(concat "GNU Emacs " emacs-version " kernel "
				(car (split-string (shell-command-to-string "uname -r")))  " Debian "
				(car (split-string (shell-command-to-string "cat /etc/debian_version"))) " 86_64 GNU/Linux"))
  (setq dashboard-startup-banner "~/.emacs.d/emacs.png")
  (setq dashboard-page-separator "\n\f\f\n")
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq show-week-agenda-p t)
  (setq dashboard-items '((recents . 8)(agenda . 5)))
  (setq dashboard-footer-messages
		'("Be joyful always. Pray constantly. Be thankful for everything."))
  (setq dashboard-footer-icon (nerd-icons-octicon "nf-oct-heart" :height 1.0 :face 'nerd-icons-lred))
  (defun dashboard-insert-custom (list-size)
	"Insert custom itemes LIST-SIZE."
	(interactive)
	(insert " GH: (h)    WX: (w)   Cal: (c)    News: (n)    Mail: (m)    Slack: (s)    (.)"))
  (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
  (add-to-list 'dashboard-items '(custom) t)

  (defvar dashboard-recover-layout-p nil
	"Wether recovers the layout.")

  (defun open-dashboard ()
	"Open the *dashboard* buffer and jump to the first widget."
	(interactive)
	(setq dashboard-recover-layout-p t)
	(delete-other-windows)
	(dashboard-refresh-buffer)
	(dashboard-goto-recent-files))

  (defun quit-dashboard ()
	"Quit dashboard window."
	(interactive)
	(quit-window t)
	(when (dashboard-recover-layout-p)
	  (setq dashboard-recover-layout-p nil)))

  (defun dashboard-goto-recent-files ()
	"Go to recent files."
	(interactive)
	(let ((func (local-key-binding "r")))
	  (and func (funcall func))))

  (defun sylpheed ()
	"Open sylpheed."
	(interactive)
	(compile "sylpheed")
	(delete-other-windows))

  (defun slack ()
	"Open sylpheed."
	(interactive)
	(compile "slack")
	(delete-other-windows)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 01_dashboard.el ends here
