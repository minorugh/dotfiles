;;; 01_dashboard.el --- Dashboard configurations.  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf dashboard
  :ensure t
  :when window-system
  :hook (emacs-startup-hook . dashboard-setup-startup-hook)
  :config
  (with-eval-after-load 'dashboard
    (bind-key "<home>" 'open-dashboard)
    (bind-key "b" 'chromium-bible dashboard-mode-map)
    (bind-key "c" 'chromium-calendar dashboard-mode-map)
    (bind-key "w" 'chromium-weather dashboard-mode-map)
    (bind-key "n" 'chromium-google-news dashboard-mode-map)
    (bind-key "k" 'chromium-keep dashboard-mode-map)
    (bind-key "K" 'chromium-keep-new dashboard-mode-map)
    (bind-key "m" 'chromium-gmail dashboard-mode-map)
	(bind-key "t" 'chromium-tweetdeck dashboard-mode-map)
    (bind-key "s" 'chromium-slack dashboard-mode-map)
    (bind-key "h" 'chromium-homepage dashboard-mode-map)
    (bind-key "p" 'chromium-pocket dashboard-mode-map)
    (bind-key "." 'hydra-browse/body dashboard-mode-map)
    (bind-key "<home>" 'quit-dashboard dashboard-mode-map)
	(open-dashboard))
  ;; Set the title
  (setq dashboard-banner-logo-title
		(concat "GNU Emacs " emacs-version " kernel "
				(car (split-string (shell-command-to-string "uname -r")))  " Debian "
				(car (split-string (shell-command-to-string "cat /etc/debian_version"))) " 86_64 GNU/Linux"))
  (global-page-break-lines-mode)
  ;; Set the banner
  (setq dashboard-startup-banner (expand-file-name "emacs.png" user-emacs-directory))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq show-week-agenda-p t)
  (setq dashboard-items '((recents  . 5)
  						  (agenda . 5)))
  ;; Set the footer
  (setq dashboard-footer-icon
		(all-the-icons-octicon "dashboard" :height 1.1 :v-adjust -0.05 :face 'font-lock-keyword-face))
  (setq dashboard-footer-messages '("Always be joyful. Never stop praying. Be thankful in all circumstances!"))
  ;; Insert custom item
  (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
  (add-to-list 'dashboard-items '(custom) t)
  :init
  (defun dashboard-goto-recent-files ()
	"Go to recent files."
	(interactive)
	(funcall (local-key-binding "r")))

  (defun dashboard-insert-custom (list-size)
	"Insert custom and set LIST-SIZE."
	(interactive)
	(insert (if (display-graphic-p)
				(all-the-icons-faicon "google" :height 1.2 :v-adjust -0.05 :face 'dashboard-heading) " "))
	(insert "   GH: (h)    Calendar: (c)   ⛅ Weather: (w)   📰 News: (n)    Mail: (m)    Twitter: (t)    Pocket: (p)    Slack: (s) "))

  (defvar dashboard-recover-layout-p nil
	"Wether recovers the layout.")

  (defun restore-previous-session ()
	"Restore the previous session."
	(interactive)
	(when (bound-and-true-p persp-mode)
	  (restore-session persp-auto-save-fname)))

  (defun restore-session (fname)
	"Restore the specified session."
	(interactive (list (read-file-name "Load perspectives from a file: "
									   persp-save-dir)))
	(when (bound-and-true-p persp-mode)
	  (message "Restoring session...")
	  (quit-window t)
	  (condition-case-unless-debug err
		  (persp-load-state-from-file fname)
		(error "Error: Unable to restore session -- %s" err))
	  (message "Done")))

  (defun open-dashboard ()
	"Open the *dashboard* buffer and jump to the first widget."
	(interactive)
	(delete-other-windows)
	(setq default-directory "~/")
	;; Refresh dashboard buffer
	(if (get-buffer dashboard-buffer-name)
		(kill-buffer dashboard-buffer-name))
	(dashboard-insert-startupify-lists)
	(switch-to-buffer dashboard-buffer-name)
	;; Jump to the first section
	(goto-char (point-min))
	(dashboard-goto-recent-files))

  (defun quit-dashboard ()
	"Quit dashboard window."
	(interactive)
	(quit-window t)
	(when (and dashboard-recover-layout-p
			   (bound-and-true-p winner-mode))
	  (winner-undo)
	  (setq dashboard-recover-layout-p nil)))

  (defun open-sylpheed ()
	"Open sylpheed."
	(interactive)
	(compile "sylpheed")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 01_dashboard.el ends here
