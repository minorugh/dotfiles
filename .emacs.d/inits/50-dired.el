;;; 50-dired.el --- Dired configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf dired
  :hook (dired-mode-hook  . my-dired-omit-mode)
  :bind (:dired-mode-map
	 ("^"   . my-dired-up)
	 ("RET" . my-dired-open)
	 ("w"   . wdired-change-to-wdired-mode)
	 ("s"   . my-sudo-reopen)
	 ("o"   . dired-open-file)
	 ("["   . dired-hide-details-mode)
	 ("a"   . dired-omit-mode)
	 ("."   . my-open-tig)
	 ("i"   . my-sxiv))
  :config
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  (setq dired-recursive-copies  'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-listing-switches "-AFl")
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-dirs-first t)
  (setq dired-omit-files "^\\.$\\|^\\.[^\\.].*$\\|\\.elc$")
  (put 'dired-find-alternate-file 'disabled nil)
  (defun my-dired-omit-mode ()
    "Disable omit mode only in `dotfiles' directory."
    (dired-omit-mode
     (if (equal (expand-file-name "~/src/github.com/minorugh/dotfiles/")
		(expand-file-name default-directory))
	 -1
       1)))

  (defun my-dired-open ()
    "Open file or directory at point."
    (interactive)
    (let ((file (dired-get-filename)))
      (if (file-directory-p file)
          (find-alternate-file file)
	(find-file file))))

  (defun my-dired-up ()
    "Go to parent directory in the same buffer."
    (interactive)
    (find-alternate-file ".."))

  (defun dired-open-file ()
    "In dired, open the file in associated application."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file)))

  (defun my-sudo-reopen ()
    "Reopen current file with sudo privileges via TRAMP."
    (interactive)
    (let ((pos (point)))
      (find-alternate-file (concat "/sudo:localhost:" (buffer-file-name)))
      (goto-char pos)))

  (defun my-open-tig ()
    "Run tig for current context in gnome-terminal."
    (interactive)
    (let* ((path (or (and (derived-mode-p 'dired-mode)
                          (dired-get-filename nil t))
                     (buffer-file-name)
                     default-directory))
           (dir  (if (file-directory-p path)
                     path
                   (file-name-directory path)))
           (root (locate-dominating-file dir ".git")))
      (if root
          (start-process
           "tig" nil
           "gnome-terminal"
           "--maximize"
           "--working-directory" dir
           "--"
           "bash" "-c"
           (format "tig %s" (shell-quote-argument path)))
	(message "Not in a Git repo"))))

  (defun my-sxiv ()
    "Open images in current directory with sxiv (fullscreen)."
    (interactive)
    (let* ((files (directory-files default-directory nil
                                   "\\.\\(jpe?g\\|png\\|gif\\|bmp\\)$"))
           (cmd (format "sxiv -t -f %s"
			(mapconcat #'shell-quote-argument files " "))))
      (start-process-shell-command "sxiv" nil cmd))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 50-dired.el ends here
