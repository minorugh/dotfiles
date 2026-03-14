;;; 50-dired.el --- Dired configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf dired
  :defun dired-get-filename dired-find-alternate-file dired-find-file
  dired-current-directory dired-goto-subdir dired-goto-file
  :hook ((after-init-hook . (lambda () (require 'ls-lisp)))
	 (dired-mode-hook  . dired-omit-mode))
  :bind (:dired-mode-map
	 ("<left>"  . dired-up-alternate-directory)
	 ("<right>" . dired-open-in-accordance-with-situation)
	 ("RET"     . dired-open-in-accordance-with-situation)
	 ("w"       . wdired-change-to-wdired-mode)
	 ("s"       . sudo-edit)
	 ("o"       . dired-open-file)
	 ("["       . dired-hide-details-mode)
	 ("a"       . dired-omit-mode)
	 ("i"       . call-sxiv))
  :config
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  (setq dired-recursive-copies  'always)
  (setq dired-recursive-deletes 'always)
  ;; ls-lisp を使う場合は --group-directories-first は使えないので除去
  (setq dired-listing-switches "-AFl")
  (setq ls-lisp-use-insert-directory-program nil)
  ;; ls-lisp でディレクトリを先頭に表示
  (setq ls-lisp-dirs-first t)
  (setq dired-omit-files "^\\.$\\|^\\.[^\\.].*$\\|\\.elc$")
  (put 'dired-find-alternate-file 'disabled nil)

  (defun dired-open-in-accordance-with-situation ()
    "Files are opened in separate buffers, directories are opened in the same buffer."
    (interactive)
    (let ((file (dired-get-filename)))
      (if (file-directory-p file)
	  (dired-find-alternate-file)
	(dired-find-file))))

  (defun dired-up-alternate-directory ()
    "Move to higher directory without make new buffer."
    (interactive)
    (let* ((dir (dired-current-directory))
	   (up (file-name-directory (directory-file-name dir))))
      (or (dired-goto-file (directory-file-name dir))
	  ;; Only try dired-goto-subdir if buffer has more than one dir.
	  (and (cdr dired-subdir-alist)
	       (dired-goto-subdir up))
	  (progn
	    (find-alternate-file up)
	    (dired-goto-file dir)))))

  (defun dired-open-file ()
    "In dired, open the file in associated application."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file)))

  (defun call-sxiv ()
    "Show all images in the directory with sxiv.
see https://gist.github.com/kobapan/28908b564b610bd3e6f3fae78637ac8b"
    (interactive)
    (let ((image-files
	   (delq nil
		 (mapcar
		  (lambda (f)
		    (when (string-match "\\.\\(jpe?g\\|png\\|gif\\|bmp\\)$" f)
		      f))
		  (directory-files default-directory)))))
      (start-process-shell-command
       "nsxiv" nil
       (format "sxiv -p -f -t -n %s %s"
	       (length image-files)
	       (mapconcat 'identity image-files " "))))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 50-dired.el ends here
