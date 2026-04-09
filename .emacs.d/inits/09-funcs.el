;;; 09-funcs.el --- Define functions.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *my-makefile
  :doc "ivy-based Makefile target selector; my-makefile.el lives in ~/.emacs.d/elisp/."
  :require (my-makefile)
  :hook
  (makefile-mode-hook . (lambda ()
                          (evil-local-set-key 'normal (kbd "@") #'my-make-ivy)))
  (dired-mode-hook    . (lambda ()
                          (evil-local-set-key 'normal (kbd "@") #'my-make-ivy)))
  :init
  (defun my-open-cron-makefile ()
    "Open ~/src/github.com/minorugh/dotfiles/cron/Makefile and invoke my-make-ivy."
    (interactive)
    (let ((file (expand-file-name "~/src/github.com/minorugh/dotfiles/cron/Makefile")))
      (find-file file)
      (evil-local-set-key 'normal (kbd "@") #'my-make-ivy)
      (run-at-time 0.1 nil #'my-make-ivy)))

  (defun my-open-cron-log ()
    "Open /tmp/cron.log fullscreen with cursor at bottom."
    (interactive)
    (find-file "/tmp/cron.log")
    (goto-char (point-max))(recenter -30)(delete-other-windows))

  (defun my-open-xsrv-log ()
    "Open /tmp/xsrv-backup.log fullscreen with cursor at bottom."
    (interactive)
    (find-file "/tmp/xsrv-backup.log")
    (goto-char (point-max))(delete-other-windows)))

(leaf compilation
  :doc "Auto-close compilation window on success; delay only for my-make-ivy."
  :init
  ;; my-make-ivy-called: when t, wait 2 seconds before closing so the result
  ;; can be confirmed; otherwise close immediately.
  (defvar my-make-ivy-called nil "Flag to indicate my-make-ivy triggered compilation.")
  :config
  (setq compilation-scroll-output t)
  (setq compilation-always-kill   t)
  (defun compile-autoclose (buffer string)
    "Close compile window if BUFFER finished successfully, report STRING otherwise."
    (if (and (string-match "compilation" (buffer-name buffer))
             (string-match "finished" string))
        (progn
          (message "Compile successful.")
          (if my-make-ivy-called
              (run-at-time 2 nil (lambda ()
                                   (setq my-make-ivy-called nil)
                                   (when (buffer-live-p buffer)
                                     (delete-windows-on buffer)
                                     (kill-buffer buffer))))
            (when (buffer-live-p buffer)
              (delete-windows-on buffer)
              (kill-buffer buffer))))
      (message "Compilation exited abnormally: %s" string)))
  (setq compilation-finish-functions #'compile-autoclose))

(leaf ps-print
  :doc "PostScript printing with Japanese support."
  :url "https://tam5917.hatenablog.com/entry/20120914/1347600433"
  :if (executable-find "lpr")
  :config
  (setq ps-multibyte-buffer 'non-latin-printer)
  (setq ps-paper-type       'a4)
  (setq ps-printer-name      nil)
  (setq ps-print-header      nil)
  (setq ps-print-footer      nil)
  (setq ps-font-size         9)
  (setq ps-font-family      'Courier)
  (setq ps-line-number-font 'Courier)
  (setq ps-line-number       t)
  (setq ps-show-n-of-n       t)
  (defalias 'ps-mule-header-string-charsets 'ignore))

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
