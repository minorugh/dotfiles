;;; 20_make.el --- Make command configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *cus-compilation
  :config
  (add-to-list 'auto-mode-alist '("\\.mak\\'" . makefile-mode))
  (setq compilation-scroll-output t)
  (setq compilation-always-kill t)
  (setq compilation-finish-functions 'compile-autoclose)
  (defun compile-autoclose (buffer string)
	"Automatically close the compilation."
	(cond ((string-match "compilation" (buffer-name buffer))
		   (string-match "finished" string)
		   (delete-other-windows)
		   (message "Compile successful."))
		  (t (message "Compilation exited abnormally: %s" string)))))


(leaf *make-function
  :config
  (defun my:make-k ()
	"Make k."
	(interactive)
	(compile "make -k"))

  (defun my:make-draft ()
	"Make kinnei draft."
	(interactive)
	(compile "make df"))

  (defun my:make-upsftp ()
	"Make upfstp."
	(interactive)
	(compile "make up"))

  (defun my:make-move ()
	"Make move."
	(interactive)
	(compile "make mv"))

  (defun my:make-bklog ()
	"Make bklog."
	(interactive)
	(compile "make bk"))

  (defun my:make-git ()
	"Make git."
	(interactive)
	(compile "make git"))

  (defun my:make-sort ()
	"Make sort for filelist."
	(interactive)
	(compile "make sort")
	(find-file "~/Dropbox/GH/upsftp/filelist.txt")
	(goto-char (point-min)))

  (defun my:make-clean ()
	"Make clean."
	(interactive)
	(compile "make clean")))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 20_make.el ends here
