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
	 ("<f4>"  . my-remote-select)             ;; see :init
         ("<f5>"  . quickrun)                  ;; see 30-utilities.el
	 ("<f6>"  . thunar-open-this)          ;; see :init
         ("<f7>"  . neotree-toggle)            ;; see 50-neotree.el
	 ("<f8>"  . my-darkroom-toggle)        ;; see 90-darkroom.el
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

(defun my-remote-select ()
  "Select remote dir and open gnome-terminal with SSH."
  (interactive)
  (let* ((home-root "/home/minorugh/")
         (gh-root   (concat home-root "gospel-haiku.com/public_html/"))
         (mn-root   (concat home-root "minorugh.com/public_html/"))
         (dirs `(("home-root"     . ("ls"     . ,home-root))
                 ("gospel-haiku"  . ("ls"     . ,gh-root))
                 ("minorugh.com"  . ("ls"     . ,mn-root))
                 ("docker/httpd"  . ("docker" . "docker exec -it httpd /bin/bash"))
                 ("passwd"        . ("vim"    . ,(concat home-root "gospel-haiku.com/passwd/")))
                 ("d_kukai/data"  . ("vim"    . ,(concat gh-root "d_kukai/data/")))
                 ("w_kukai/data"  . ("vim"    . ,(concat gh-root "w_kukai/data/")))
                 ("s_kukai/data"  . ("vim"    . ,(concat gh-root "s_kukai/data/")))
                 ("m_kukai/data"  . ("vim"    . ,(concat gh-root "m_kukai/data/")))))
         (choice (completing-read "remote: " (mapcar #'car dirs) nil t "^"))
         (entry  (cdr (assoc choice dirs)))
         (action (car entry))
         (dir    (cdr entry))
         (cmd (cond
               ((string= action "docker")
                (format "gnome-terminal --maximize -- %s" dir))
               ((string= action "vim")
                (format "gnome-terminal --maximize -- ssh -t xsrv 'exec vim %s'" dir))
               (t
                (format "gnome-terminal --maximize -- ssh -t xsrv 'cd %s && bash -il'" dir)))))
    (start-process-shell-command "ssh-cd" nil cmd)))

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
