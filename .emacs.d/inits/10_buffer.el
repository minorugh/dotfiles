;;; 10_buffer.el --- Buffer Utility configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; auto-save-buffers
(leaf auto-save-buffers-enhanced
  :ensure t
  :custom
  (auto-save-buffers-enhanced-exclude-regexps . '("^/ssh:" "^/scp:" "/sudo:" "*.gpg $"))
  (auto-save-buffers-enhanced-quiet-save-p . t)
  ;; Disable to prevent freeze in tramp-mode
  (auto-save-buffers-enhanced-include-only-checkout-path . nil)
  :config
  (auto-save-buffers-enhanced t))


;; Scratch for sticky-memo
(leaf *scratch-memo
  :after auto-save-buffers-enhanced
  :bind ("S-<return>" . toggle-scratch)
  :custom
  (auto-save-buffers-enhanced-save-scratch-buffer-to-file-p . t)
  (auto-save-buffers-enhanced-file-related-with-scratch-buffer . "~/.emacs.d/tmp/scratch")
  :init
  (defun toggle-scratch ()
	"Toggle current buffer and *scratch* buffer."
	(interactive)
	(if (not (string= "*scratch*" (buffer-name)))
		(progn
		  (setq toggle-scratch-prev-buffer (buffer-name))
		  (switch-to-buffer "*scratch*")
		  (display-line-numbers-mode 0)
		  (nyan-mode 0))
	  (switch-to-buffer toggle-scratch-prev-buffer)
	  (nyan-mode 1)))

  (defun read-scratch-data ()
	(let ((file "~/.emacs.d/tmp/scratch"))
	  (when (file-exists-p file)
		(set-buffer (get-buffer "*scratch*"))
		(erase-buffer)
		(insert-file-contents file))))
  (read-scratch-data))


;; automatically kill unnecessary buffers
(leaf tempbuf
  :el-get (tempbuf :url "http://www.emacswiki.org/emacs/download/tempbuf.el")
  :hook ((find-file-hook . my:find-file-tempbuf-hook)
		 (dired-mode-hook . turn-on-tempbuf-mode)
		 (magit-mode-hook . turn-on-tempbuf-mode))
  :custom
  `((tempbuf-kill-message . nil)
	(my:tempbuf-ignore-files . '("~/Dropbox/org/task.org")))
  :init
  (defun my:find-file-tempbuf-hook ()
	(let ((ignore-file-names (mapcar 'expand-file-name my:tempbuf-ignore-files)))
	  (unless (member (buffer-file-name) ignore-file-names)
		(turn-on-tempbuf-mode)))))


;; Set buffer that can not be killed
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*"
  (emacs-lock-mode 'kill))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 10_buffer.el ends here
