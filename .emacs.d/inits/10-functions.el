;;; 10-funcs.el --- External tools & SSH launchers.  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Interactive commands for launching external tools and managing SSH connections.
;; Covers: gnome-terminal, Thunar, FileZilla, KeePassXC, and xserver access.
;; F1-F12 bindings are centrally managed in 10-funcs.el
;;
;;; Code:
;; (setq debug-on-error t)

(leaf external-functions
  :doc "External functions & SSH launchers."
  :bind (("<f1>"  . help-command)              ;; built-in
         ("<f2>"  . consult-imenu)             ;; see 04-consult.el
         ("<f3>"  . terminal-open-this)        ;; see :init
         ("<f4>"  . xsrv-ssh-access)           ;; see :init
         ("<f5>"  . quickrun)                  ;; see 30-utilities.el
         ("<f6>"  . thunar-open-this)          ;; see :init
         ("<f7>"  . neotree-toggle)            ;; see 50-neotree.el
         ("<f8>"  . my-darkroom-toggle)        ;; see 80-darkroom.el
         ("<f9>"  . display-line-numbers-mode) ;; see 30-ui.el
         ("<f10>" . toggle-scratch-buffer)     ;; see :init
         ("<f11>" . toggle-frame-fullscreen)   ;; built-in
         ("<f12>" . toggle-emacs))             ;; user shell script
  :init
  (defun terminal-open-this ()
    "Open gnome-terminal at current dir on adjacent display."
    (interactive)
    (let* ((dir (directory-file-name default-directory))
           (cmd (concat "gnome-terminal --working-directory " dir))
           (move "xdotool search --sync --onlyvisible --class gnome-terminal windowmove 0 0"))
      (start-process-shell-command "gnome-terminal" nil cmd)
      (run-with-timer 0.5 nil (lambda () (shell-command move)))))

  (defun thunar-open-this ()
    "Open Thunar at current dir on adjacent display."
    (interactive)
    (let* ((cmd  (concat "thunar " default-directory))
           (move "xdotool search --sync --onlyvisible --class thunar windowmove 0 0"))
      (start-process-shell-command "thunar" nil cmd)
      (run-with-timer 0.5 nil (lambda () (shell-command move)))))

  ;; Access xsrv gospel-haiku.com: terminal, vim, or TRAMP edit.
  (defun xsrv-ssh-access ()
    "Open xserver gospel-haiku.com."
    (interactive)
    (let* ((candidates '("" "exec vim (xsrv)" "edit wmember" "edit dmember"))
           (choice (completing-read "xsrv-ssh [Enter=terminal]: " candidates))
           (vim-cmd "gnome-terminal --maximize -- ssh -t xsrv 'exec vim'")
           (ssh-cmd "gnome-terminal --maximize -- ssh xsrv-GH")
           (dm-file "/ssh:xsrv-GH:gospel-haiku.com/passwd/dmember.cgi")
           (wm-file "/ssh:xsrv-GH:gospel-haiku.com/passwd/wmember.cgi"))
      (cond
       ((string-prefix-p "exec"   choice) (start-process-shell-command "xsrv-vim" nil vim-cmd))
       ((string-prefix-p "edit d" choice) (find-file dm-file) (text-mode) (setq-local super-save-mode nil))
       ((string-prefix-p "edit w" choice) (find-file wm-file) (text-mode) (setq-local super-save-mode nil))
       (t (start-process-shell-command "xsrv-gh" nil ssh-cmd)))))

  (defun fzilla-GH ()
    "Open Filezilla with `gospel-haiku.com'."
    (interactive)
    (start-process-shell-command
     "filezilla" nil "filezilla --site='0/gospel-haiku.com'"))

  (defun fzilla-minoruGH ()
    "Open Filezilla with `minorugh.com'."
    (interactive)
    (start-process-shell-command
     "filezilla" nil "filezilla --site='0/minorugh.com'"))

  (defun fzilla-s ()
    "Open Filezilla with list of connections."
    (interactive)
    (start-process-shell-command
     "filezilla" nil "filezilla -s"))

  (defun keepassxc ()
    "Open keepassxc with auto passwd input."
    (interactive)
    (start-process-shell-command
     "keepass" nil "keepass.sh"))

  (defun toggle-scratch-buffer ()
    "Toggle *scratch* buffer."
    (interactive)
    (if (string= (buffer-name) "*scratch*")
	(switch-to-buffer (other-buffer))
      (switch-to-buffer "*scratch*")))

  ;; Open member file via TRAMP.
  ;; Disables super-save-mode in the buffer.
  (defun xsrv-gh-edit (member)
    "Edit gospel-haiku.com MEMBER file via SSH."
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
;;; 10-functions.el ends here
