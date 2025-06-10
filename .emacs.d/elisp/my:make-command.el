;;; my:make-command.el --- User make command configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(with-eval-after-load 'compile
  (defun my:make-k ()
    "Make k."
    (interactive)
    (compile "make -k"))

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

  (defun my:make-clean ()
    "Make clean."
    (interactive)
    (compile "make clean"))

  (defun make-backup ()
    "Backup all."
    (interactive)
    (let* ((default-directory (expand-file-name "~/Dropbox")))
      (compile "make -k")))

  (defun make-ghuser ()
    "Sync GH data by rsync."
    (interactive)
    (let* ((default-directory (expand-file-name "~/Dropbox")))
      (compile "make rsync-user")))

  (defun make-commit ()
    "Auto commit."
    (interactive)
    (let* ((default-directory (expand-file-name "~/Dropbox")))
      (compile "make git-commit"))))

(provide 'my:make-command)

;; End:
;;; my:make-command.el ends here
