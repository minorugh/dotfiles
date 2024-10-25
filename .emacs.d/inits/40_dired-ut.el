;;; 41_dired-ut.el --- Ddired utility configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *dired-ut
  :config
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
    (compile "gitk")
    (delete-other-windows)))


;;; 40_dired-ut.el ends here
