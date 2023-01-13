;;; 40_dired.el --- Dired configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf dired
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
		 ("a" . toggle-dired-listing-switches)
		 ("q" . dired-dwim-quit-window)
		 ("i" . call-sxiv)
		 ("." . gitk-open)
		 ("@" . dired-do-gist))
  :custom
  `((dired-dwim-target . t)
	(delete-by-moving-to-trash . t)
	(dired-recursive-copies . 'always)
	(dired-recursive-deletes . 'always)
	(dired-listing-switches . "-lgGhF")
	(ls-lisp-use-insert-directory-program . nil)
	(ls-lisp-dirs-first . t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custum functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun toggle-dired-listing-switches ()
  "Toggle `dired-mode' switch between with and without 'A' option to show or hide dot files."
  (interactive)
  (progn
	(if (string-match "[Aa]" dired-listing-switches)
		(setq dired-listing-switches "-lgGhF")
	  (setq dired-listing-switches "-lgGhFA"))
	(reload-current-dired-buffer)))
(defun reload-current-dired-buffer ()
  "Reload current `dired-mode' buffer."
  (let* ((dir (dired-current-directory)))
	(progn (kill-buffer (current-buffer))
		   (dired dir))))

(defun dired-my-append-buffer-name-hint ()
  "Append a auxiliary string [Dir] to a name of dired buffer."
  (when (eq major-mode 'dired-mode)
	(let* ((dir (expand-file-name list-buffers-directory))
		   ;; Add a drive letter for Windows
		   (drive (if (and (eq 'system-type 'windows-nt)
						   (string-match "^\\([a-zA-Z]:\\)/" dir))
					  (match-string 1 dir) "")))
	  (rename-buffer (concat (buffer-name) " [" drive "dir]") t))))
(add-hook 'dired-mode-hook 'dired-my-append-buffer-name-hint)


(defun dired-dwim-quit-window ()
  "`quit-window 'according to screen division."
  (interactive)
  (quit-window (not (delq (selected-window) (get-buffer-window-list)))))

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

;; https://gist.github.com/kobapan/28908b564b610bd3e6f3fae78637ac8b
(defun call-sxiv ()
  "Show all images in the directory with sxiv."
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


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 40_dired.el ends here
