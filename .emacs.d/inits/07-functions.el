;;; 07-funcs.el --- External tools & SSH launchers.  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Interactive commands for launching external tools and managing SSH connections.
;; Covers: gnome-terminal, Thunar, KeePassXC, and xserver SSH access.
;; F1-F12 bindings are centrally managed here.
;;
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  F-key Bindings
;; ============================================================

(leaf external-functions
  :doc "External tools & SSH launchers."
  :bind (("<f1>"  . help-command)              ; built-in
         ("<f2>"  . my-remote-select)          ; see below
         ("<f3>"  . terminal-open-this)        ; see below
         ("<f4>"  . xsrv-open-this)            ; see below
         ("<f5>"  . quickrun)                  ; see 20-utils.el
         ("<f6>"  . thunar-open-this)          ; see below
         ("<f7>"  . neotree-toggle)            ; see 80-neotree.el
         ("<f8>"  . my-darkroom-toggle)        ; see 90-darkroom.el
         ("<f9>"  . display-line-numbers-mode)
         ("<f10>" . toggle-scratch-buffer)     ; see below
         ("<f11>" . toggle-frame-fullscreen)   ; built-in
         ("<f12>" . toggle-emacs))             ; see toggle-emacs.sh below


  ;; ============================================================
  ;;  External App Launchers
  ;; ============================================================

  :init
  (defun my-remote-select ()
    "Select remote directory and open gnome-terminal via SSH."
    (interactive)
    (let* ((home-root "/home/minorugh/")
           (gh-root   (concat home-root "gospel-haiku.com/public_html/"))
           (mn-root   (concat home-root "minorugh.com/public_html/"))
           (dirs `(("home-root"    . ("ls" . ,home-root))
                   ("gospel-haiku" . ("ls" . ,gh-root))
                   ("minorugh.com" . ("ls" . ,mn-root))
                   ("docker/httpd" . ("docker" . "docker exec -it httpd /bin/bash"))))
           (choice (completing-read "remote: " (mapcar #'car dirs) nil t "^"))
           (entry  (cdr (assoc choice dirs)))
           (action (car entry))
           (dir    (cdr entry))
           (cmd (cond
                 ((string= action "docker")
                  (format "gnome-terminal -- %s" dir))
		 (t
		  (let ((display-dir (replace-regexp-in-string "public_html/$" "" dir)))
		    (format "gnome-terminal -- ssh -t xsrv 'cd %s && printf \"%s\\n\" && bash -il'"
			    dir display-dir))))))
      (start-process-shell-command "ssh-cd" nil cmd)))

  (defun terminal-open-this ()
    "Open gnome-terminal at current directory."
    (interactive)
    (let* ((dir (directory-file-name default-directory))
           (cmd (concat "gnome-terminal --working-directory " dir)))
      (start-process-shell-command "gnome-terminal" nil cmd)))

  (defun thunar-open-this ()
    "Open Thunar file manager at current directory."
    (interactive)
    (let* ((cmd (concat "thunar " default-directory)))
      (start-process-shell-command "thunar" nil cmd)))


  ;; ============================================================
  ;;  SSH Launcher  (xsrv)
  ;;
  ;;  Maps local Dropbox paths to remote server paths:
  ;;    ~/Dropbox/GH/           → gospel-haiku.com/public_html/
  ;;    ~/Dropbox/minorugh.com/ → minorugh.com/public_html/
  ;; ============================================================

  (defun xsrv-open-this ()
    "Open gnome-terminal via SSH at the xserver directory
matching current buffer."
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


  ;; ============================================================
  ;;  Buffer Toggles
  ;; ============================================================

  (defun toggle-scratch-buffer ()
    "Toggle between *scratch* and the previous buffer."
    (interactive)
    (if (string= (buffer-name) "*scratch*")
        (switch-to-buffer (other-buffer))
      (switch-to-buffer "*scratch*"))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 07-functions.el ends here
