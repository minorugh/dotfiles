;;; 20_custom.el --- User custom functions. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; User custom functions
(leaf cus-functions
  :chord ("jk" . my:delete-this-file)
  :bind	(([f3] . thunar-open)
		 ([f4] . terminal-open)
		 ([f5] . ssh-xsrv)
		 ([f8] . toggle-menu-bar-mode-from-frame)
		 ("C-c h" . chromium-tegaki)
		 ([muhenkan] . my:muhenkan)
		 ("C-c <left>" . winner-undo)
		 ("C-c <right>" . winner-redo))
  :init
  (defun my:muhenkan ()
	(interactive)
	(if (not (use-region-p))
		(minibuffer-keyboard-quit)
	  (keyboard-quit)))

  (defun ssh-xsrv ()
	"Open terminal and ssh to xsrv."
	(interactive)
	(shell-command "gnome-terminal -- ssh xsrv"))

  (defun thunar-open ()
	"Open thunar with current dir."
	(interactive)
	(shell-command (concat "xdg-open " default-directory)))

  (defun terminal-open ()
	"Open termninal with current dir."
	(interactive)
	(let ((dir (directory-file-name default-directory)))
	  (when (and (eq system-type 'gnu/linux)
				 (string-match-p "Microsoft" (shell-command-to-string "uname -r")))
		(shell-command (concat "xfce4-terminal --maximize --working-directory " dir)))
	  (shell-command (concat "gnome-terminal --working-directory " dir))))

  (defun my:delete-this-file ()
	"Delete the current file, and kill the buffer."
	(interactive)
	(unless (buffer-file-name)
	  (error "No file is currently being edited"))
	(when (yes-or-no-p (format "Really delete '%s'?"
							   (file-name-nondirectory buffer-file-name)))
	  (delete-file (buffer-file-name))
	  (kill-this-buffer))))


;; Fast full-text search
(leaf cus-counsel-ag
  :init
  (defun ad:counsel-ag (f &optional initial-input initial-directory extra-ag-args ag-prompt caller)
	(apply f (or initial-input
				 (and (not (thing-at-point-looking-at "^\\*+"))
					  (ivy-thing-at-point)))
		   (unless current-prefix-arg
			 (or initial-directory default-directory))
		   extra-ag-args ag-prompt caller))

  (with-eval-after-load "counsel"
	(require 'thingatpt nil t)
	(advice-add 'counsel-ag :around #'ad:counsel-ag)
	;; Make search trigger even with 2 characters
	(add-to-list 'ivy-more-chars-alist '(counsel-ag . 2))
	(ivy-add-actions
	 'counsel-ag
	 '(("r" my:counsel-ag-in-dir "search in directory")))

	(defun my:counsel-ag-in-dir (_arg)
	  "Search again with new root directory."
	  (let ((current-prefix-arg '(4)))
		(counsel-ag ivy-text nil "")))))


;; gist
(leaf cus-gist
  :bind (("s-g p" . gist-region-or-buffer)
		 ("s-g c" . my:chromium-gist))
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
;;; 20_custom.el ends here
