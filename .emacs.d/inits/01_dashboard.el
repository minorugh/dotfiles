;;; 01_dashboard.el --- Dashboard configurations.
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

(leaf dashboard
  :ensure t
  :if (display-graphic-p)
  :defun ((nerd-icons-octicon dashboard-refresh-buffer dashboard-goto-recent-files)
		  (restart-server server-running-p))
  :hook ((emacs-startup-hook . open-dashboard)
		 (dashboard-mode-hook . (lambda () (setq left-margin-width 1))))
  :bind (([home] . open-dashboard)
		 (:dashboard-mode-map
		  ("c" . chromium-calendar)
		  ("n" . chromium-nhk-news)
		  ("w" . chromium-weather)
		  ("h" . chromium-homepage)
		  ("m" . (lambda () (interactive) (compile "sylpheed")))
		  ("s" . (lambda () (interactive) (compile "slack")))
		  ("." . hydra-browse/body)))
  :init
  (leaf page-break-lines :ensure t :after dashboard :global-minor-mode t)
  (setq dashboard-icon-type 'nerd-icons)
  :config
  ;; Set the title
  (setq dashboard-banner-logo-title
		(concat "GNU Emacs " emacs-version " kernel "
				(car (split-string (shell-command-to-string "uname -r")))  " Debian "
				(car (split-string (shell-command-to-string "cat /etc/debian_version"))) " 86_64 GNU/Linux"))
  ;; Set the banner
  (setq dashboard-startup-banner "~/.emacs.d/emacs.png")
  (setq dashboard-page-separator "\n\f\f\n")
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq show-week-agenda-p t)
  (setq dashboard-items '((recents . 8)(agenda . 5)))
  ;; Set the footer
  (setq dashboard-footer-messages
		'("Be joyful always. Pray constantly. Be thankful for Everything."))
  (setq dashboard-footer-icon (nerd-icons-octicon "nf-oct-heart" :height 1.0 :face 'nerd-icons-lred))
  ;; Set the insert custom
  (defun dashboard-insert-custom (list-size)
	"Insert custom itemes."
	(interactive)
	(insert " GH: (h)    WX: (w)   Cal: (c)    News: (n)    Mail: (m)    Slack: (s)    (.)"))
  (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
  (add-to-list 'dashboard-items '(custom) t)

  (defun dashboard-goto-recent-files ()
	"Go to recent files."
	(interactive)
	(let ((func (local-key-binding "r")))
	  (and func (funcall func))))

  (defun open-dashboard ()
	"Open the *dashboard* buffer and jump to the first widget."
	(interactive)
	(setq default-directory "~/")
	(delete-other-windows)
	(dashboard-refresh-buffer)
	(dashboard-goto-recent-files)
	(restart-server))

  (defun restart-server ()
	"Server Start."
	(interactive)
	(require 'server)
	(unless (server-running-p)
	  (server-start))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 01_dashboard.el ends here
