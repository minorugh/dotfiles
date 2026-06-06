;;; 07-funcs.el --- External tools & SSH launchers.  -*- lexical-binding: t -*-
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
	 ("<f4>"  . xsrv-open-this)            ;; see :init
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

  (defun xsrv-open-this ()
    "Open gnome-terminal via SSH at the xserver dir matching current buffer."
    (interactive)
    (let* ((local-gh  (expand-file-name "~/Dropbox/GH/"))
           (local-mn  (expand-file-name "~/Dropbox/minorugh.com/"))
           (remote-gh "/home/minorugh/gospel-haiku.com/public_html/")
           (remote-mn "/home/minorugh/minorugh.com/public_html/")
           (cur (expand-file-name
		 (if (derived-mode-p 'dired-mode)
                     (let ((f (dired-get-filename nil t)))
                       (if (and f (file-directory-p f))
                           f
			 (file-name-directory (or f default-directory))))
                   default-directory)))
           (dir (cond
		 ((string-prefix-p local-gh cur)
                  (concat remote-gh (file-relative-name cur local-gh)))
		 ((string-prefix-p local-mn cur)
                  (concat remote-mn (file-relative-name cur local-mn)))
		 (t "/home/minorugh/")))
           (cmd (format "gnome-terminal --maximize -- ssh -t xsrv 'cd %s && exec $SHELL -il'" dir)))
      (start-process-shell-command "ssh" nil cmd)))

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
;;; 07-functions.el ends here
