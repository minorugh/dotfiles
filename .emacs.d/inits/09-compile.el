;;; 09-compile.el --- Compilation functions.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf my-makefile
  :doc "ivy-based Makefile target selector."
  :require t
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

(defun my-make-git ()
  "Run `make git' in the repository root of the current buffer."
  (interactive)
  (let* ((dir (or buffer-file-name default-directory))
         (root (locate-dominating-file dir "Makefile")))
    (if root
        (let ((default-directory root))
          (compile "make git"))
      (message "Makefile not found"))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 09-compile.el ends here
