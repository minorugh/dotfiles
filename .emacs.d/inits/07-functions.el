;;; 07-functions.el --- External tools & SSH launchers.  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Interactive commands for launching external tools and managing SSH connections.
;; Covers: gnome-terminal, Thunar, and xserver SSH access.
;; F1-F12 bindings are centrally managed here.
;;
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  F-key Bindings
;; ============================================================

(leaf *function-keys
  :bind (("<f1>"  . help-command)              ; built-in
         ("<f2>"  . neotree)                   ; see 70-neotree.el
         ("<f3>"  . terminal-open-this)        ; see below
         ("<f4>"  . xsrv-open-this)            ; see below
         ("<f5>"  . my-quickrun)               ; see 30-utils.el
         ("<f6>"  . thunar-open-this)          ; see below
         ("<f7>"  . calendar)                  ; see 90-calendar.el
         ("<f8>"  . my-darkroom-toggle)        ; see 90-darkroom.el
         ("<f9>"  . display-line-numbers-mode) ; built-in
         ("<f10>" . toggle-scratch-buffer)     ; see below
         ("<f11>" . toggle-frame-fullscreen)   ; built-in
         ("<f12>" . toggle-emacs))             ; see below

  :preface
  (defun toggle-emacs ()
    "Show/hide the Emacs window via toggle-emacs.sh."
    (interactive)
    (start-process-shell-command "toggle-emacs" nil "toggle-emacs.sh"))

  (defun my-external-monitor-geometry ()
    "Return the \"+X+Y\" position of the first connected external monitor,
  or nil if none is found (e.g. only the internal eDP display is active)."
    (with-temp-buffer
      (call-process "xrandr" nil t nil "--query")
      (goto-char (point-min))
      (let (result)
        (while (and (not result)
                    (re-search-forward
                     "^\\([A-Za-z0-9-]+\\) connected \\(?:primary \\)?[0-9]+x[0-9]+\\(\\+[0-9]+\\+[0-9]+\\)"
                     nil t))
          (let ((name (match-string 1))
                (pos (match-string 2)))
            (unless (string-prefix-p "eDP" name)
              (setq result pos))))
        result)))

  (defun my-launch-gnome-terminal (&rest args)
    "Launch gnome-terminal with ARGS, placed on the external monitor
  when one is connected (via `my-external-monitor-geometry')."
    (let ((geo (my-external-monitor-geometry)))
      (apply #'start-process
             "gnome-terminal" nil
             "gnome-terminal"
             `(,@(when geo (list (concat "--geometry=" geo)))
               ,@args))))

  (defun terminal-open-this ()
    "Open gnome-terminal at current directory.
  Placed on the external monitor when one is connected."
    (interactive)
    (let ((dir (directory-file-name (expand-file-name default-directory))))
      (my-launch-gnome-terminal "--working-directory" dir)))

  (defvar xsrv-mirror-map
    (list (cons (expand-file-name "~/Dropbox/GH/")
                "/home/minorugh/gospel-haiku.com/public_html/")
          (cons (expand-file-name "~/Dropbox/minorugh.com/")
                "/home/minorugh/minorugh.com/public_html/"))
    "ローカルミラーディレクトリ→xsrv側の対応する公開ディレクトリのalist.
`xsrv-open-this' はこのいずれかの配下の dired バッファでのみ動作する.")

  (defun xsrv-open-this ()
    "Open gnome-terminal via SSH at the xserver directory matching current buffer.
Only valid in a `dired-mode' buffer whose directory is under one of
`xsrv-mirror-map' keys; otherwise signal a `user-error'."
    (interactive)
    (unless (derived-mode-p 'dired-mode)
      (user-error "xsrv-open-this: Dired バッファでのみ実行できます"))
    (let* ((cur (expand-file-name
                 (let ((f (dired-get-filename nil t)))
                   (if (and f (file-directory-p f))
                       f
                     (file-name-directory (or f default-directory))))))
           (match (seq-find (lambda (pair) (string-prefix-p (car pair) cur)) xsrv-mirror-map)))
      (unless match
        (user-error "xsrv-open-this: 対象外のディレクトリです: %s" cur))
      (let* ((local-root (car match))
             (remote-root (cdr match))
             (remote-dir (concat remote-root (substring cur (length local-root)))))
        (my-launch-gnome-terminal "--" "ssh" "-t" "xsrv"
                                  (format "cd '%s' && exec $SHELL -il" remote-dir)))))

  (defun thunar-open-this ()
    "Open Thunar file manager at current directory."
    (interactive)
    (let* ((cmd (concat "thunar " default-directory)))
      (start-process-shell-command "thunar" nil cmd))))


;; ============================================================
;;  Scratch Buffer Persistence
;; ============================================================

(leaf *scratch-buffer
  :hook ((after-init-hook . restore-scratch-buffer)
         (kill-emacs-hook . save-scratch-buffer))
  :preface
  (defun save-scratch-buffer ()
    "Write *scratch* contents to disk."
    (with-current-buffer "*scratch*"
      (write-region (point-min) (point-max)
                    (locate-user-emacs-file "tmp/scratch"))))

  (defun restore-scratch-buffer ()
    "Restore *scratch* contents from disk if the file exists."
    (let ((f (locate-user-emacs-file "tmp/scratch")))
      (when (file-exists-p f)
        (with-current-buffer "*scratch*"
          (erase-buffer)
          (insert-file-contents f)))))

  (defun toggle-scratch-buffer ()
    "Toggle between *scratch* and the previous buffer."
    (interactive)
    (if (string= (buffer-name) "*scratch*")
        (switch-to-buffer (other-buffer))
      (switch-to-buffer "*scratch*"))))


;; ============================================================
;;  toggle-emacs.sh
;; ============================================================

;; toggle-emacs.sh
;;   #!/bin/bash
;;   for wid in $(xdotool search --class emacs 2>/dev/null); do
;;       if xprop -id "$wid" _NET_WM_STATE 2>/dev/null | grep -q HIDDEN; then
;;           xdotool windowmap --sync "$wid"
;;           xdotool windowactivate "$wid"
;;           exit
;;       fi
;;   done
;;   wid=$(xdotool search --class emacs 2>/dev/null | tail -n1)
;;   xdotool windowminimize "$wid"


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 07-functions.el ends here
