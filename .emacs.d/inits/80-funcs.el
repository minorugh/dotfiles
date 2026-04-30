;;; 80-funcs.el --- External tools & SSH launchers.  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Interactive commands for launching external tools and managing SSH connections.
;; Covers: gnome-terminal, Thunar, FileZilla, KeePassXC, and xserver access.
;; Keybindings (F3/F4/F6/F10) are registered through `leaf external-functions'.
;;
;;; Code:
;; (setq debug-on-error t)

(leaf external-functions
  :bind (("<f3>" . terminal-open)
         ("<f4>" . xsrv-ssh)
         ("<f6>" . thunar-open)
         ("<f10>" . toggle-scratch-buffer))
  :init
  (defun terminal-open ()
    "Open gnome-terminal at current dir on adjacent display."
    (interactive)
    (let ((dir (directory-file-name default-directory)))
      (start-process-shell-command
       "gnome-terminal" nil
       (concat "gnome-terminal --working-directory " dir))
      (run-with-timer
       0.5 nil
       (lambda ()
	 (shell-command
          "xdotool search --sync --onlyvisible --class gnome-terminal windowmove 0 0")))))

  (defun thunar-open ()
    "Open Thunar at current dir on adjacent display."
    (interactive)
    (start-process-shell-command
     "thunar" nil
     (concat "thunar " default-directory))
    (run-with-timer
     0.5 nil
     (lambda ()
       (shell-command
	"xdotool search --sync --onlyvisible --class thunar windowmove 0 0"))))

  (defun xsrv-ssh ()
    "Open xserver gospel-haiku.com.
Optionally edit passwd files via TRAMP."
    (interactive)
    (let* ((candidates '("" "exec vim (xsrv)" "edit wmember" "edit dmember"))
           (choice (completing-read
                    "xsrv-ssh [Enter=terminal]: "
                    candidates)))
      (cond
       ((string-prefix-p "exec" choice)
	(start-process-shell-command "xsrv-vim" nil "gnome-terminal --maximize -- ssh -t xsrv 'exec vim'"))
       ((string-prefix-p "edit d" choice)
	(find-file "/ssh:xsrv-GH:gospel-haiku.com/passwd/dmember.cgi")
	(text-mode)
	(setq-local super-save-mode nil))
       ((string-prefix-p "edit w" choice)
	(find-file "/ssh:xsrv-GH:gospel-haiku.com/passwd/wmember.cgi")
	(text-mode)
	(setq-local super-save-mode nil))
       (t
	(start-process-shell-command "xsrv-gh" nil "gnome-terminal --maximize -- ssh xsrv-GH")))))

  (defun fzilla-GH ()
    "Open Filezilla with `gospel-haiku.com'."
    (interactive)
    (start-process-shell-command "filezilla" nil "filezilla --site='0/gospel-haiku.com'"))

  (defun fzilla-minoruGH ()
    "Open Filezilla with `minorugh.com'."
    (interactive)
    (start-process-shell-command "filezilla" nil "filezilla --site='0/minorugh.com'"))

  (defun fzilla-s ()
    "Open Filezilla with list of connections."
    (interactive)
    (start-process-shell-command "filezilla" nil "filezilla -s"))

  (defun keepassxc ()
    "Open keepassxc with auto passwd input."
    (interactive)
    (start-process-shell-command "keepass" nil "keepass.sh"))

  (defun toggle-scratch-buffer ()
    "Toggle *scratch* buffer."
    (interactive)
    (if (string= (buffer-name) "*scratch*")
	(switch-to-buffer (other-buffer))
      (switch-to-buffer "*scratch*")))

  (defun xsrv-gh-edit (member)
    "Open gospel-haiku.com member file via SSH.
Specify \"d\"(dmember.cgi) or \"w\"(wmember.cgi) for MEMBER.
Disable super-save-mode in buffer-local."
    (interactive "sMember (d/w): ")
    (let ((file (cond
		 ((string= member "d") "dmember.cgi")
		 ((string= member "w") "wmember.cgi")
		 (t (error "%s" "use 'd' or 'w'")))))
      (find-file (format "/ssh:xsrv-GH:gospel-haiku.com/passwd/%s" file))
      (setq-local super-save-mode nil)
      (message "Opened %s (super-save disabled)" file))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 80-funcs.el ends here
