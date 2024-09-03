;;; 20_funcs.el --- Define functions.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf compile
  :doc "run compiler as inferior of Emacs"
  :tag "Builtin"
  :config
  (add-to-list 'auto-mode-alist '("\\.mak\\'" . makefile-mode))
  (setq compilation-scroll-output t)
  (setq compilation-always-kill t)
  (setq compilation-finish-functions 'compile-autoclose)
  :init
  (defun compile-autoclose (buffer string)
    "Automatically close the compilation."
    (cond ((string-match "compilation" (buffer-name buffer))
	   (string-match "finished" string)
	   (delete-other-windows)
	   (message "Compile successful."))
	  (t (message "Compilation exited abnormally: %s" string)))))


(leaf *define-functions
  :doc "User custom key bind"
  :bind	(([f3]  . thunar-open)
	 ([f4]  . terminal-open)
	 ([f5]  . ssh-xsrv)
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
  (defun thunar-open ()
    "Open thunar with current dir."
    (interactive)
    (shell-command (concat "thunar " default-directory)))

  (defun terminal-open ()
    "Open termninal with current dir."
    (interactive)
    (let ((dir (directory-file-name default-directory)))
      (when (and (eq system-type 'gnu/linux)
		 (string-match-p "Microsoft" (shell-command-to-string "uname -r")))
	(shell-command (concat "xfce4-terminal --maximize --working-directory " dir)))
      (shell-command (concat "gnome-terminal --working-directory " dir))))

  (defun ssh-xsrv ()
    "Open terminal and ssh to xsrv."
    (interactive)
    (shell-command "gnome-terminal --maximize -- ssh xsrv-emacs"))
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


(leaf *user-gist-commands
  :doc "Gist upload from current buffer or region"
  :tag "Be configured to be able to use gist on the command line from the terminal"
  :init
  (defun gist-description ()
    "Add gist description."
    (shell-quote-argument (read-from-minibuffer "Add gist description: ")))

  (defun gist-filename ()
    "The character string entered in minibuffer is used as file-name.
If enter is pressed without file-name, that's will be buffer-file-neme."
    (interactive)
    (let ((file (file-name-nondirectory (buffer-file-name (current-buffer)))))
      (read-from-minibuffer (format "File name (%s): " file) file)))

  (defun gist-region-or-buffer ()
    "If region is selected, post from the region.
If region isn't selected, post from the buffer."
    (interactive)
    (let ((file (buffer-file-name)))
      (if (not (use-region-p))
	  (compile (concat "gist -od " (gist-description) " " file))
	(compile (concat "gist -oPd " (gist-description) " -f " (gist-filename)))))
    (delete-other-windows))

  (defun dired-do-gist ()
    "Dired-get-filename do gist and open in browser."
    (interactive)
    (let ((file (dired-get-filename nil t)))
      (compile (concat "gist -od " (gist-description) " " file)))
    (delete-other-windows)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_funcs.el ends here
