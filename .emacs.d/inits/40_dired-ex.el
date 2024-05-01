;;; 40_dired-ex.el --- Dired expand configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf nerd-icons-dired
  :ensure t
  :if (display-graphic-p)
  :hook (dired-mode-hook . nerd-icons-dired-mode)
  :custom (nerd-icons-scale-factor . 0.8))


(defun call-sxiv ()
  "Show all images in the directory with sxiv.
see https://gist.github.com/kobapan/28908b564b610bd3e6f3fae78637ac8b"
  (interactive)
  (let ((image-files
		 (delq nil
			   (mapcar
				(lambda (f)
				  (when (string-match "\.\\(jpe?g\\|png\\|gif\\|bmp\\)$" f)
					f))
				(directory-files default-directory)))))
	(start-process-shell-command
	 "sxiv" nil
	 (format "sxiv -f -t -n %s %s"
			 (length image-files)
			 (mapconcat 'identity image-files " ")))))


(defun gitk-open ()
  "Open gitk with current dir.
see https://riptutorial.com/git/example/18336/gitk-and-git-gui"
  (interactive)
  (shell-command "gitk &")
  (delete-other-windows))


(defun git-gui-open ()
  "Tools for creating commits."
  (interactive)
  (shell-command "git gui &")
  (delete-other-windows))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 40_dired-ex.el ends here
