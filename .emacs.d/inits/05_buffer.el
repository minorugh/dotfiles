;;; 05_buffer.el --- Buffer Utility configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf auto-save-buffers-enhanced
  :doc "Automatically save buffers"
  :url "https://github.com/kentaro/auto-save-buffers-enhanced/tree/master"
  :ensure t
  :custom
  (auto-save-buffers-enhanced-exclude-regexps . '("^/ssh:" "^/scp:" "/sudo:" "*.gpg $"))
  (auto-save-buffers-enhanced-quiet-save-p . t)
  ;; Disable to prevent freeze in tramp-mode
  (auto-save-buffers-enhanced-include-only-checkout-path . nil)
  :config
  (auto-save-buffers-enhanced t))


(leaf *cus-scratch-memo
  :doc "Scratch for sticky-memo"
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
		  (display-line-numbers-mode 0))
	  (switch-to-buffer toggle-scratch-prev-buffer)))

  (defun read-scratch-data ()
	(let ((file "~/.emacs.d/tmp/scratch"))
	  (when (file-exists-p file)
		(set-buffer (get-buffer "*scratch*"))
		(erase-buffer)
		(insert-file-contents file))))
  (read-scratch-data))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 05_buffer.el ends here
