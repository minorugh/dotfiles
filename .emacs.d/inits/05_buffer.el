;;; 05_buffer.el --- Buffer Utility configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf super-save
  :ensure t
  :hook (after-init-hook . super-save-mode)
  :config
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration 1)
  (setq super-save-exclude '(".gpg"))
  (defun clear-message ()
	(message nil))
  (advice-add 'save-buffer :after 'clear-message)
  (defun my:super-save-buffers-command ()
	"Save the buffer if needed."
	(save-excursion
      (dolist (buf (buffer-list))
		(set-buffer buf)
		(when (and buffer-file-name
                   (buffer-modified-p (current-buffer))
                   (file-writable-p buffer-file-name)
                   (if (file-remote-p buffer-file-name)
                       super-save-remote-files t))
          (save-buffer)))))
  (advice-add 'super-save-command :override #'my:super-save-buffers-command))


(leaf persistent-scratch
  :doc "Save scratch buffer state to file and restore from file"
  :url "https://github.com/Fanael/persistent-scratch"
  :ensure t
  :hook (after-init-hook . persistent-scratch-autosave-mode)
  :bind ("S-<return>" . toggle-scratch)
  :custom (persistent-scratch-save-file . "~/.emacs.d/tmp/.persistent-scratch")
  :init
  (defun toggle-scratch ()
	"Toggle current buffer and *scratch* buffer."
	(interactive)
	(if (not (string= "*scratch*" (buffer-name)))
		(progn
		  (setq toggle-scratch-prev-buffer (buffer-name))
		  (switch-to-buffer-other-window "*scratch*")
		  (display-line-numbers-mode 0))
	  (switch-to-buffer toggle-scratch-prev-buffer)
	  (delete-other-windows))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 05_buffer.el ends here
