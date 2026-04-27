;;; 80-funcs.el --- External tools & SSH launchers.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(bind-key "<f3>" 'terminal-open)
(bind-key "<f4>" 'xsrv-ssh)
(bind-key "<f6>" 'thunar-open)
(bind-key "<f8>" 'toggle-scratch-buffer)

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
  (interactive "sMember (d/w): ")
  (let ((file (cond
               ((string= member "d") "dmember.cgi")
               ((string= member "w") "wmember.cgi")
               (t (error "use 'd' or 'w'")))))
    (find-file (format "/ssh:xsrv-GH:gospel-haiku.com/passwd/%s" file))
    (setq-local super-save-mode nil)
    (message "Opened %s (super-save disabled)" file)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 80-funcs.el ends here
