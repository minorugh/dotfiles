;;; 20-funcs.e,l --- Define functions.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf compile
  :doc "run compiler as inferior of Emacs"
  :tag "Builtin"
  :require my:compile  ;; Lode user make configurations
  :config
  (add-to-list 'auto-mode-alist '("\\.mak\\'" . makefile-mode))
  (setq compilation-scroll-output t)
  (setq compilation-always-kill t)
  (setq compilation-finish-functions 'compile-autoclose)
  :init
  (defun compile-autoclose (buffer string)
    "Automatically close the compilation."
    (cond ((string-match "compilation" (buffer-name buffer))
	   (string-match "finished" string)
	   (delete-other-windows)
	   (message "Compile successful."))
	  (t (message "Compilation exited abnormally: %s" string)))))

(leaf *cus-frame-funtions
  :bind  ("C-q" . other-window-or-split)
  :init
  (defun other-window-or-split ()
    "If there is one window, open split window.
If there are two or more windows, it will go to another window."
    (interactive)
    (when (one-window-p)
      (split-window-horizontally))
    (other-window 1))

  (defun handle-delete-frame (event)
    "Overwrite `handle-delete-frame` defined in `frame.el`.
If it's the last frame, minimize it without deleting it."
    (interactive "e")
    (let ((frame  (posn-window (event-start event)))
	  (numfrs (length (visible-frame-list))))
      (cond ((> numfrs 1) (delete-frame frame t))
	    ((iconify-frame))))))

(leaf *user-gist-commands
  :doc "Gist upload from current buffer or region"
  :defun gist-filename gist-description dired-get-filename
  :init
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
    (delete-other-windows)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 20-funcs.el ends here
