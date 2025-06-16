;;; 00-cus-user.el --- User custom configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *cus-user-configrations
  :defun minibuffer-keyboard-quit
  :load-path "~/.emacs.d/elisp/"
  :require my:dired my:template my:make my:evil
  :bind (("C-x C-c" . server-edit)  ;; Server editing buffers exist. Replace "C-x #"
	 ("C-x b"   . ibuffer)      ;; Overwrite switch-to-buffer
	 ("M-,"     . xref-find-definitions)
	 ("M-w"     . clipboard-kill-ring-save)
	 ("C-w"     . kill-word-or-region)
	 ("C-q"     . other-window-or-split)
	 ("M-/"     . kill-current-buffer)
	 ("C-M-/"   . delete-this-file)
	 ("s-c"     . clipboard-kill-ring-save) ;; Like macOS,eq Win 'C-c'
	 ("s-v"     . clipboard-yank)           ;; Like macOS,eq Win 'C-v'
	 ([muhenkan] . my:keyboard-quit))
  :init
  (defun my:upcase-word (arg)
    "Convert previous word (or ARG words) to upper case."
    (interactive "p")
    (upcase-word (- arg)))

  (defun my:downcase-word (arg)
    "Convert previous word (or ARG words) to down case."
    (interactive "p")
    (downcase-word (- arg)))

  (defun my:capitalize-word (arg)
    "Convert previous word (or ARG words) to capitalize."
    (interactive "p")
    (capitalize-word (- arg)))

  (defun my:keyboard-quit ()
    (interactive)
    (if (not (use-region-p))
	(minibuffer-keyboard-quit)
      (keyboard-quit)))

  (defun delete-this-file ()
    "Delete the current file, and kill the buffer."
    (interactive)
    (unless (buffer-file-name)
      (error "No file is currently being edited"))
    (when (yes-or-no-p (format "Really delete '%s'?"
			       (file-name-nondirectory buffer-file-name)))
      (delete-file (buffer-file-name))
      (kill-current-buffer)))

  (defun kill-word-or-region ()
    "If the region is active, `clipboard-kill-region'.
If the region is inactive, `backward-kill-word'."
    (interactive)
    (if (use-region-p)
	(clipboard-kill-region (region-beginning) (region-end))
      (backward-kill-word 1)))

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

;;; 00-cus-user.el ends here
