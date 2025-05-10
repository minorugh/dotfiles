;;; 01_dashboard.el --- Dashboard configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

(leaf dashboard :ensure t
  :doc "An extracted startup screen"
  :if (display-graphic-p)
  :hook (after-init-hook . dashboard-setup-startup-hook)
  :bind (([home] . open-dashboard)
	 (:dashboard-mode-map
	  ([home] . previous-buffer)
	  ("a"    . org-agenda)
	  ("c"    . org-capture)
	  ("m"    . neomutt)
	  ("s" . (lambda () (interactive) (compile "slack")))
	  ("x" . (lambda () (interactive) (browse-url "https://x.com/minoruGH")))
	  ("n" . (lambda () (interactive) (browse-url "https://news.yahoo.co.jp/")))
	  ("w" . (lambda () (interactive) (browse-url "https://tenki.jp/week/6/31/")))
	  ("h" . (lambda () (interactive) (browse-url "https://gospel-haiku.com/")))))
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-icon-type 'nerd-icons)
  :config
  ;; Set the items
  (if (string-match "P1" (shell-command-to-string "uname -n"))
      (setq dashboard-items '((recents . 10)))
    (setq dashboard-items '((recents . 3))))
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
  ;; Set the insert custom
  (defun dashboard-insert-custom (list-size)
    "Insert custom itemes."
    (interactive)
    (insert " GH(h)   Agenda(a)    Capture(c)    Mutt(m)    Slack(s)    Twit(x)    Weather(w)    News(n)"))
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
    (dashboard-goto-recent-files))

  (defun neomutt ()
    "Open terminal and ssh to xsrv."
    (interactive)
    (compile "neomutt.sh"))
  (setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))

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


;;; 01_dashboard.el ends here
