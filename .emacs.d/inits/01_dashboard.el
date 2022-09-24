;;; 01_dashboard.el --- Dashboard configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf dashboard
  :ensure t
  :hook (after-init-hook . dashboard-setup-startup-hook)
  :defun (dashboard-setup-startup-hook)
  :bind (("<home>" . open-dashboard)
		 (:dashboard-mode-map
		  ("c" . chromium-calendar)
		  ("A" . (lambda ()(interactive)(org-agenda nil "a")))
		  ("p" . keepassxc)
		  ("l" . logout)
		  ("y" . chromium-yahoo-japan)
		  ("n" . chromium-nhk-news)
		  ("w" . chromium-weather)
		  ("t" . chromium-tweetdeck)
		  ("h" . chromium-homepage)
		  ("m" . sylpheed)
		  ("s" . slack)
		  ("." . hydra-browse/body)
		  ("<home>" . quit-dashboard)))
  :config
  ;; Set the title
  (setq dashboard-banner-logo-title
		(concat "GNU Emacs " emacs-version " kernel "
				(car (split-string (shell-command-to-string "uname -r")))  " Debian "
				(car (split-string (shell-command-to-string "cat /etc/debian_version"))) " 86_64 GNU/Linux"))

  ;; Set the banner
  (setq dashboard-startup-banner (expand-file-name "emacs.png" user-emacs-directory)
		dashboard-page-separator "\n\f\f\n"
		dashboard-set-heading-icons t
		dashboard-set-file-icons t
		show-week-agenda-p t
		dashboard-items '((recents  . 5)
						  (agenda . 5)))

  ;; Set the footer
  (setq dashboard-footer-messages
		'("Always be joyful. Never stop praying. Be thankful in all circumstances."))
  (setq dashboard-footer-icon
		(all-the-icons-octicon "dashboard" :height 1.0 :v-adjust -0.05 :face 'font-lock-keyword-face))

  (defun dashboard-insert-custom (list-size)
	"Insert custom itemes LIST-SIZE."
	(interactive)
	(insert " GH: (h)    Cal: (c)    News: (n)    Mail: (m)    Slack: (s)    Twit: (t)    (p)   (l)   (.)"))

  ;; Insert custom item
  (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
  (add-to-list 'dashboard-items '(custom) t)
  :init
  (leaf page-break-lines
	:ensure t
	:hook (dashboard-mode-hook . page-break-lines-mode))

  (defun dashboard-goto-recent-files ()
	"Go to recent files."
	(interactive)
	(let ((func (local-key-binding "r")))
      (and func (funcall func))))

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
	(when (and dashboard-recover-layout-p
			   (bound-and-true-p winner-mode))
      (winner-undo)
      (setq dashboard-recover-layout-p nil)))

  ;; Return a string giving the duration of the Emacs initialization
  (defun ad:emacs-init-time ()
	"Advice `emacs-init-time'."
	(interactive)
	(let ((str
		   (format "%.3f seconds"
				   (float-time
					(time-subtract after-init-time before-init-time)))))
	  (if (called-interactively-p 'interactive)
		  (message "%s" str)
		str)))

  (advice-add 'emacs-init-time :override #'ad:emacs-init-time)

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
;; no-byte-compile: t
;; End:
;;; 01_dashboard.el ends here
