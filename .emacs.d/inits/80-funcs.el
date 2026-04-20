;;; 80-funcs.el --- External tools & SSH launchers.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(bind-key "<f3>" 'terminal-open)
(bind-key "<f4>" 'xsrv-gh)
(bind-key "<f6>" 'thunar-open)

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

(defun xsrv-gh ()
  "Open the xserver gospel-haiku.com in a terminal."
  (interactive)
  (start-process-shell-command "xsrv-gh" nil "gnome-terminal --maximize -- ssh xsrv-GH"))

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


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 80-funcs.el ends here
