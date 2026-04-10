;;; 09-funcs.el --- Define functions.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *my-makefile
  :doc "ivy-based Makefile target selector."
  :require (my-makefile)
  :bind ("M-:" . my-open-cron-makefile)
  :hook ((makefile-mode-hook dired-mode-hook)
	 . (lambda () (evil-local-set-key 'normal (kbd "@") #'my-make-ivy)))
  :init
  (defun my-open-cron-makefile ()
    "Open ~/src/github.com/minorugh/dotfiles/cron/Makefile and invoke my-make-ivy."
    (interactive)
    (let ((file (expand-file-name "~/src/github.com/minorugh/dotfiles/cron/Makefile")))
      (find-file file)
      (evil-local-set-key 'normal (kbd "@") #'my-make-ivy)
      (run-at-time 0.1 nil #'my-make-ivy))))

(leaf compilation
  :doc "Auto-close compilation window on success after 1 second."
  :chord (("::" . my-switch-to-compilation))
  :config
  (setq compilation-scroll-output t)
  (setq compilation-always-kill   t)
  :init
  (defun my-switch-to-compilation ()
    (interactive)
    (if-let ((buf (get-buffer "*compilation*")))
	(progn
	  (switch-to-buffer buf)
	  (local-set-key (kbd "q") #'quit-window))
      (message "*compilation* buffer does not exist.")))

  (defun compile-autoclose (buffer string)
    "Auto-close compile window if BUFFER finished successfully.
Echo the last @echo output line to the minibuffer."
    (if (and (string-match "compilation" (buffer-name buffer))
             (string-match "finished" string))
	(let ((msg (with-current-buffer buffer
                     (save-excursion
                       (goto-char (point-max))
                       (if (re-search-backward "^##>\\(.*\\)$" nil t)
                           (match-string 1)
			 "Compile successful.")))))
          (message "%s" msg)
          (if (string-equal msg "")
              (run-at-time 0.1 nil (lambda ()
                                     (switch-to-buffer buffer)
                                     (delete-other-windows)))
            (delete-windows-on buffer)))
      (message "Compilation exited abnormally: %s" string)))
  (setq compilation-finish-functions #'compile-autoclose))

(leaf *gist
  :doc "Post region or buffer to gist via compile."
  :config
  (defun gist-description ()
    "Add gist description."
    (shell-quote-argument (read-from-minibuffer "Add gist description: ")))

  (defun gist-filename ()
    "The character string entered in minibuffer is used as file-name.
If enter is pressed without file-name, that's will be buffer file name."
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
    (delete-other-windows)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; 09-funcs.el ends here

