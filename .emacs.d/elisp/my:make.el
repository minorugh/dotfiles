;;; my:make.el --- User make functions.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf my:make-function
  :doc "User make functions"
  :init
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


(provide 'my:make)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; my:make.el ends here
