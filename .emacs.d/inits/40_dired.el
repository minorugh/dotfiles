;;; 40_dired.el --- Ddired configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf dired
  :hook ((after-init-hook . (lambda () (require 'ls-lisp)))
	 (dired-load-hook . (lambda () (require 'dired-x)))
	 (dired-mode-hook . dired-omit-mode))
  :bind (:dired-mode-map
	 ("<left>" . dired-up-alternate-directory)
	 ("<right>" . dired-open-in-accordance-with-situation)
	 ("RET" . dired-open-in-accordance-with-situation)
	 ("<" . beginning-of-buffer)
	 (">" . end-of-buffer)
	 ("r" . wdired-change-to-wdired-mode)
	 ("s" . sudo-edit) ;; Load from package
	 ("o" . dired-open-file)
	 ("[" . dired-hide-details-mode)
	 ("a" . dired-omit-mode)
	 ("i" . call-sxiv) ;; defun in dired-ut.el
	 ("." . gitk-open) ;; defun in dired-ut.el
	 ("@" . dired-do-gist)) ;; defun in gist.el
  :config
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  (setq dired-recursive-copies  'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-listing-switches "-AFl --group-directories-first")
  (setq ls-lisp-use-insert-directory-program nil)
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
      (call-process "xdg-open" nil 0 nil file))))


;;; 40_dired.el ends here
