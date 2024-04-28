;;; 01_dashboard.el --- Dashboard configurations.
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

(leaf dashboard :ensure t
  :if (display-graphic-p)
  :hook ((emacs-startup-hook . open-dashboard)
		 (dashboard-mode-hook . (lambda () (setq left-margin-width 1))))
  :bind (([home] . open-dashboard)
		 (:dashboard-mode-map
		  ("c" . (lambda () (interactive) (browse-url "https://calendar.google.com/calendar/r")))
		  ("n" . (lambda () (interactive) (browse-url "https://www.nhk.or.jp/news/")))
		  ("x" . (lambda () (interactive) (browse-url "https://twitter.com/gospelhaiku")))
		  ("w" . (lambda () (interactive) (browse-url "https://tenki.jp/week/6/31/")) )
		  ("h" . (lambda () (interactive) (browse-url "https://gospel-haiku.com/")))
		  ("g" . (lambda () (interactive) (browse-url "https://github.com/minorugh")))
		  ("m" . (lambda () (interactive) (compile "thunderbird")))
		  ("s" . (lambda () (interactive) (compile "slack")))
		  ("." . hydra-browse/body)))
  :init
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
  (if (string-match "P1" (shell-command-to-string "uname -n"))
	  (setq dashboard-items '((recents . 10)))
	(setq dashboard-items '((recents . 3))))
  ;; Set the footer
  (setq dashboard-footer-messages
		'("Be joyful always. Pray constantly. Be thankful for Everything."))
  (setq dashboard-footer-icon (nerd-icons-octicon "nf-oct-heart" :height 1.0 :face 'nerd-icons-lred))
  ;; Set the insert custom
  (defun dashboard-insert-custom (list-size)
	"Insert custom itemes."
	(interactive)
	(insert " GH(h)   WX(w)   Cal(c)    News(n)    Mail(m)    (x)   (g)   (s)    (.)"))
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

  (defun ad:emacs-init-time ()
	"Advice `emacs-init-time'."
	(interactive)
	(let ((str
		   (format "%.1f seconds"
				   (float-time
					(time-subtract after-init-time before-init-time)))))
	  (if (called-interactively-p 'interactive)
		  (message "%s" str)
		str)))
  (advice-add 'emacs-init-time :override #'ad:emacs-init-time))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 01_dashboard.el ends here
