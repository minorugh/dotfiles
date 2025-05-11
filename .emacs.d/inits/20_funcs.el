;;; 20_funcs.el --- User functions configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *user-define-functions
  :bind	(([f3]    . thunar-open)
	 ([f4]    . terminal-open)
	 ([f5]    . ssh-gh)
	 ("M-w"   . clipboard-kill-ring-save)
	 ("C-w"   . kill-whole-line-or-region)
	 ("M-/"   . my:kill-buffer)
	 ("C-M-/" . delete-this-file)
	 ("s-c"   . clipboard-kill-ring-save) ;; Like macOS,eq Win 'C-c'
	 ("s-v"   . clipboard-yank)           ;; Like macOS,eq Win 'C-v'
	 ([muhenkan] . my:keyboard-quit))
  :init
  (defun thunar-open ()
    "Open thunar with current dir."
    (interactive)
    (compile (concat "thunar " default-directory)))

  (defun terminal-open ()
    "Open termninal with current dir."
    (interactive)
    (let ((dir (directory-file-name default-directory)))
      (when (and (eq system-type 'gnu/linux)
		 (string-match-p "Microsoft" (shell-command-to-string "uname -r")))
	(shell-command (concat "xfce4-terminal --maximize --working-directory " dir)))
      (compile (concat "gnome-terminal --working-directory " dir))))

  (defun ssh-gh ()
    "Open terminal and ssh to xsrv."
    (interactive)
    (compile "gnome-terminal --maximize -- ssh xsrv-GH"))

  (defun my:keyboard-quit ()
    (interactive)
    (if (not (use-region-p))
	(minibuffer-keyboard-quit)
      (keyboard-quit)))

  (defun my:kill-buffer (arg)
    (interactive "P")
    (if arg
	(call-interactively 'kill-buffer)
      (kill-buffer)))

  (defun delete-this-file ()
    "Delete the current file, and kill the buffer."
    (interactive)
    (unless (buffer-file-name)
      (error "No file is currently being edited"))
    (when (yes-or-no-p (format "Really delete '%s'?"
			       (file-name-nondirectory buffer-file-name)))
      (delete-file (buffer-file-name))
      (my:kill-buffer)))

  (defun kill-whole-line-or-region ()
    "If the region is active, to kill region.
  If the region is inactive, to kill whole line."
    (interactive)
    (if (use-region-p)
	(clipboard-kill-region (region-beginning) (region-end))
      (kill-whole-line)))

  (defun handle-delete-frame (event)
    "Overwrite `handle-delete-frame` defined in `frame.el`.
  If it's the last frame, minimize it without deleting it."
    (interactive "e")
    (let ((frame  (posn-window (event-start event)))
	  (numfrs (length (visible-frame-list))))
      (cond ((> numfrs 1) (delete-frame frame t))
	    ((iconify-frame))))))


;;; 20_funcs.el ends here
