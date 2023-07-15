;;; 40_dired.el --- Dired configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf all-the-icons-dired
  :ensure t
  :if (display-graphic-p)
  :hook (dired-mode-hook . all-the-icons-dired-mode)
  :custom
  `((all-the-icons-dired-monochrome . nil)
	(all-the-icons-scale-factor . 0.9)))


(leaf dired
  :hook ((after-init-hook . (lambda () (require 'dired-x)))
		 (dired-mode-hook . (lambda () (dired-omit-mode 1))))
  :bind (:dired-mode-map
		 ("<" . beginning-of-buffer)
		 (">" . end-of-buffer)
		 ("r" . wdired-change-to-wdired-mode)
		 ("s" . sudo-edit)
		 ("o" . dired-open-file)
		 ("a" . dired-omit-mode)
		 ("i" . call-sxiv)
		 ("@" . dired-do-gist))
  :custom
  `((dired-dwim-target . t)
	(delete-by-moving-to-trash . t)
	(dired-recursive-copies . 'always)
	(dired-recursive-deletes . 'always)
	(dired-listing-switches . "-AFl --group-directories-first")
	(ls-lisp-use-insert-directory-program . nil)
	(ls-lisp-dirs-first . t))
  :config
  (setq dired-omit-files "^\\.$\\|^\\.[^\\.].*$\\|\\.elc$")
  (put 'dired-find-alternate-file 'disabled nil)

  (defun dired-open-file ()
	"In dired, open the file in associated application."
	(interactive)
	(let* ((file (dired-get-filename nil t)))
	  (call-process "xdg-open" nil 0 nil file)))

  (defun my:dired-sort ()
	"Sort dired listings with directories first."
	(save-excursion
	  (let (buffer-read-only)
		(forward-line 2) ;; beyond dir. header
		(sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
	  (set-buffer-modified-p nil)))

  (defadvice dired-readin
	  (after dired-after-updating-hook first () activate)
	"Sort dired listings with directories first before adding mark."
	(my:dired-sort))

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
			   (mapconcat 'identity image-files " "))))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 40_dired.el ends here
