;;; 40_dired.el --- Dired configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; (leaf nerd-icons-dired
;;   :ensure t
;;   :if (display-graphic-p)
;;   :hook (dired-mode-hook . nerd-icons-dired-mode)
;;   :custom (nerd-icons-scale-factor . 0.9))
(leaf all-the-icons-dired :ensure t
  :if (display-graphic-p)
  :hook (dired-mode-hook . all-the-icons-dired-mode)
  :custom `((all-the-icons-dired-monochrome . nil)
			(all-the-icons-scale-factor . 0.9)))


(leaf dired
  :after dired
  :hook ((after-init-hook . (lambda () (require 'ls-lisp)))
		 (dired-load-hook . (lambda () (require 'dired-x)))
		 (dired-mode-hook . (lambda () (dired-omit-mode 1))))
  :bind (:dired-mode-map
		 ("<left>" . dired-up-alternate-directory)
		 ("<right>" . dired-open-in-accordance-with-situation)
		 ("RET" . dired-open-in-accordance-with-situation)
		 ("<" . beginning-of-buffer)
		 (">" . end-of-buffer)
		 ("r" . wdired-change-to-wdired-mode)
		 ("s" . sudo-edit)
		 ("o" . dired-open-file)
		 ("[" . dired-hide-details-mode)
		 ("a" . dired-omit-mode)
		 ("i" . call-sxiv)
		 ("." . gitk-open)
		 ("@" . dired-do-gist))
  :custom
  `((dired-dwim-target . t)
	(delete-by-moving-to-trash . t)
	(dired-recursive-copies . 'always)
	(dired-recursive-deletes . 'always)
	(dired-listing-switches . "-AFl")
	(ls-lisp-use-insert-directory-program . nil))
  :config
  (setq dired-omit-files "^\\.$\\|^\\.[^\\.].*$\\|\\.elc$")
  (put 'dired-find-alternate-file 'disabled nil)
  (leaf sudo-edit :ensure t)

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

  (defun my:dired-sort ()
	"Sort dired listings with directories first.
  see https://www.emacswiki.org/emacs/DiredSortDirectoriesFirst?utm_source=pocket_saves"
	(save-excursion
	  (let (buffer-read-only)
		(forward-line 2) ;; beyond dir. header
		(sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
	  (set-buffer-modified-p nil)))
  (add-hook 'dired-after-readin-hook 'my:dired-sort)

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
