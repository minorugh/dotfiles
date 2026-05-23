;;; 10-funcs.el --- External tools & SSH launchers.  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Interactive commands for launching external tools and managing SSH connections.
;; Covers: gnome-terminal, Thunar, KeePassXC, and xserver SSH access.
;; F1-F12 bindings are centrally managed in 10-functions.el
;;
;;; Code:
;; (setq debug-on-error t)

(leaf external-functions
  :doc "External functions & SSH launchers."
  :bind (("<f1>"  . help-command)              ;; built-in
         ("<f2>"  . counsel-imenu)             ;; see 04-counsel.el
         ("<f3>"  . terminal-open-this)        ;; see :init
         ("<f4>"  . xsrv-ssh-cd-this)          ;; see :init
	 ("<f5>"  . xsrv-ssh-cd-select)        ;; see :init
         ("<f6>"  . quickrun)                  ;; see 30-utilities.el
	 ("<f7>"  . thunar-open-this)          ;; see :init
         ("<f8>"  . neotree-toggle)            ;; see 50-neotree.el
         ("<f9>"  . display-line-numbers-mode) ;; see 30-ui.el
         ("<f10>" . toggle-scratch-buffer)     ;; see :init
         ("<f11>" . toggle-frame-fullscreen)   ;; built-in
         ("<f12>" . toggle-emacs))             ;; see: toggle-emacs.sh (below)
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

  (defun xsrv-ssh-cd-this ()
    "Open gnome-terminal, SSH to xserver at the corresponding remote dir."
    (interactive)
    (let* ((local-root  "/home/minoru/Dropbox/GH/")
           (remote-root "/home/minorugh/gospel-haiku.com/public_html/")
           (cur (directory-file-name default-directory))
           (rel (file-relative-name cur local-root))
           (remote-dir (concat remote-root rel))
           (cmd (format "gnome-terminal --maximize -- ssh -t xsrv 'cd %s && bash -il'"
			remote-dir)))
      (start-process-shell-command "ssh-cd" nil cmd)))

  (defun xsrv-ssh-cd-select ()
    "Select remote dir and open gnome-terminal with SSH."
    (interactive)
    (let* ((home-root "/home/minorugh/")
           (srv-root  (concat home-root "gospel-haiku.com/"))
           (pub-root  (concat srv-root "public_html/"))
           (dirs `(("home/minorugh"     . ("ls l" . ,home-root))
                   ("gospel-haiku" . ("ls l" . ,srv-root))
                   ("public_html"  . ("ls l" . ,pub-root))
                   ("passwd"       . ("vim" . ,(concat srv-root "passwd/")))
                   ("d_kukai/data" . ("vim" . ,(concat pub-root "d_kukai/data/")))
                   ("w_kukai/data" . ("vim" . ,(concat pub-root "w_kukai/data/")))
                   ("s_kukai/data" . ("vim" . ,(concat pub-root "s_kukai/data/")))
                   ("m_kukai/data" . ("vim" . ,(concat pub-root "m_kukai/data/")))))
	   (choice (completing-read "xsrv dir: " (mapcar #'car dirs) nil t "^"))
           (entry   (cdr (assoc choice dirs)))
           (action  (car entry))
           (dir     (cdr entry))
           (cmd (if (string= action "vim")
                    (format "gnome-terminal --maximize -- ssh -t xsrv 'exec vim %s'" dir)
                  (format "gnome-terminal --maximize -- ssh -t xsrv 'cd %s && ls -a && $SHELL -il'" dir))))
      (start-process-shell-command "ssh-cd" nil cmd)))

  (defun xsrv-ssh-fzf ()
    "Select SSH host from ~/.ssh/config and connect (excluding github)."
    (interactive)
    (let* ((hosts (split-string
                   (shell-command-to-string
                    "grep -iE '^Host ' ~/.ssh/config | awk '{print $2}' | grep -viE '[*?]|github'")
                   "\n" t))
           (host (completing-read "SSH: " hosts)))
      (when (and host (not (string-empty-p host)))
	(start-process-shell-command
	 "ssh" nil (format "gnome-terminal --maximize -- ssh %s" host)))))

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
      (switch-to-buffer "*scratch*"))))


;; toggle-emacs.sh
;; #!/bin/bash
;; for wid in $(xdotool search --class emacs 2>/dev/null); do
;;     if xprop -id "$wid" _NET_WM_STATE 2>/dev/null | grep -q HIDDEN; then
;;         xdotool windowmap --sync "$wid"
;;         xdotool windowactivate "$wid"
;;         exit
;;     fi
;; done
;; wid=$(xdotool search --class emacs 2>/dev/null | tail -n1)
;; xdotool windowminimize "$wid"

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 10-functions.el ends here
