;;; 09-funcs.el --- Define functions.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;; compilation (builtin)

(defun compile-autoclose (buffer string)
  "Close compile window if BUFFER finished successfully, report STRING otherwise."
  (if (and (string-match "compilation" (buffer-name buffer))
	   (string-match "finished" string))
      (progn
	(delete-other-windows)
	(message "Compile successful."))
    (message "Compilation exited abnormally: %s" string)))

(add-to-list 'auto-mode-alist '("\\.mak\\'" . makefile-mode))
(setq compilation-scroll-output t)
(setq compilation-always-kill t)
(setq compilation-finish-functions #'compile-autoclose)


;;; ps-print / ps-mule (builtin)
;; https://tam5917.hatenablog.com/entry/20120914/1347600433
(when (executable-find "lpr")
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


;;; gist (external command)
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
  (delete-other-windows))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 09-funcs.el ends here
