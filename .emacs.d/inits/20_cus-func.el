;;; 20_cus-func.el --- Cstomized user configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *user-define-functions
  :doc "User custom key bind"
  :bind	(([f3]  . thunar-open)
	 ([f4]  . terminal-open)
	 ([f5]  . ssh-gh)
	 ("M-w"   . clipboard-kill-ring-save)
	 ("C-w"   . kill-whole-line-or-region)
	 ("M-/"   . kill-this-buffer)
	 ("C-M-/" . delete-this-file)
	 ("M-,"   . xref-find-definitions)
	 ("s-c"   . clipboard-kill-ring-save) ;; Like macOS,eq Win 'C-c'
	 ("s-v"   . clipboard-yank)           ;; Like macOS,eq Win 'C-v'
	 ("C-x b" . ibuffer)
	 ("C-x C-c" . iconify-frame)
	 ([muhenkan] . my:muhenkan))
  :init
  (defun my:upcase-word (arg)
    "convert previous word (or ARG words) to upper case."
    (interactive "p")
    (upcase-word (- arg)))

  (defun my:downcase-word (arg)
    "Convert previous word (or ARG words) to down case."
    (interactive "p")
    (downcase-word (- arg)))

  (defun my:capitalize-word (arg)
    "Convert previous word (or ARG words) to capitalize."
    (interactive "p")
    (capitalize-word (- arg)))

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

  (defun my:muhenkan ()
    (interactive)
    (if (not (use-region-p))
	(minibuffer-keyboard-quit)
      (keyboard-quit)))

  (defun kill-whole-line-or-region ()
    "If the region is active, to kill region.
  If the region is inactive, to kill whole line."
    (interactive)
    (if (use-region-p)
	(clipboard-kill-region (region-beginning) (region-end))
      (kill-whole-line)))

  (defun delete-this-file ()
    "Delete the current file, and kill the buffer."
    (interactive)
    (unless (buffer-file-name)
      (error "No file is currently being edited"))
    (when (yes-or-no-p (format "Really delete '%s'?"
			       (file-name-nondirectory buffer-file-name)))
      (delete-file (buffer-file-name))
      (kill-this-buffer))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_cus-func.el ends here
