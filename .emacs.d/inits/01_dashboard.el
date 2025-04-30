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
	  ("."    . hydra-browse/body)
	  ("m"    . neomutt)
	  ("x" . (lambda () (interactive) (compile "/home/minoru/.webcatalog/X/X")))
	  ("s" . (lambda () (interactive) (compile "slack")))
	  ("h" . (lambda () (interactive) (browse-url "https://gospel-haiku.com/")))))
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-icon-type 'nerd-icons)
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

  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    (setq default-directory "~/")
    (delete-other-windows)
    (dashboard-refresh-buffer)
    (dashboard-goto-recent-files))

  (defun emacs-init-time ()
    "Overwrite `emacs-init-time'."
    (interactive)
    (let ((str
	   (format "%.3f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))))
      (if (called-interactively-p 'interactive)
	  (message "%s" str)
	str))))


;;; 01_dashboard.el ends here
