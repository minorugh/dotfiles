;;; 60_gistc.el --- Gist configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *define-gist-commands
  :config
  (defun gist-description ()
    "Add gist description."
    (shell-quote-argument (read-from-minibuffer "Add gist description: ")))

  (defun gist-filename ()
    "The character string entered in minibuffer is used as file-name.
If enter is pressed without file-name, that's will be buffer-file-neme."
    (interactive)
    (let ((file (file-name-nondirectory (buffer-file-name (current-buffer)))))
      (read-from-minibuffer (format "File name (%s): " file) file)))

  (defun gist-region-or-buffer ()
    "If region is selected, post from the region.
If region isn't selected, post from the buffer."
    (interactive)
    (let ((file (buffer-file-name)))
      (if (not (use-region-p))
		  (compile (concat "gist -od " (gist-description) " " file))
		(compile (concat "gist -oPd " (gist-description) " -f " (gist-filename)))))
    (delete-other-windows))

  (defun dired-do-gist ()
    "Dired-get-filename do gist and open in browser."
    (interactive)
    (let ((file (dired-get-filename nil t)))
      (compile (concat "gist -od " (gist-description) " " file)))
    (delete-other-windows)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 60_gist.el ends here
