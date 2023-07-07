;;; 06_buffer.el --- Buffer Utility configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf super-save
  :doc "Smart auto save buffers"
  :url "https://github.com/bbatsov/super-save"
  :ensure t
  :hook (after-init-hook . super-save-mode)
  :custom
  (super-save-auto-save-when-idle . t)
  (super-save-idle-duration       . 1)
  (super-save-remote-files        . nil)
  (super-save-exclude             . '(".gpg"))
  :config
  (defun my:super-save-command ()
	"Save the buffer if needed.
see https://takaxp.github.io/init.html#orgde08dbd8"
	(save-excursion
	  (dolist (buf (buffer-list))
		(set-buffer buf)
		(when (and buffer-file-name
				   (buffer-modified-p (current-buffer))
				   (file-writable-p buffer-file-name)
				   (if (file-remote-p buffer-file-name)
					   super-save-remote-files t))
		  (save-buffer)(message nil)))))
  (advice-add 'super-save-command :override #'my:super-save-command))


(leaf persistent-scratch
  :doc "Save scratch buffer state to file and restore from file"
  :url "https://github.com/Fanael/persistent-scratch"
  :ensure t
  :hook (after-init-hook . persistent-scratch-autosave-mode)
  :bind ("S-<return>" . toggle-scratch)
  :custom (persistent-scratch-save-file . "~/.emacs.d/tmp/.scratch")
  :init
  (defun toggle-scratch ()
	"Toggle current buffer and *scratch* buffer."
	(interactive)
	(if (not (string= "*scratch*" (buffer-name)))
		(progn
		  (setq toggle-scratch-prev-buffer (buffer-name))
		  (switch-to-buffer-other-window "*scratch*")
		  (display-line-numbers-mode 0)
		  (dimmer-off))
	  (switch-to-buffer toggle-scratch-prev-buffer)
	  (delete-other-windows)
	  (dimmer-on))))


(leaf *emacs-lock-mode
  :doc "Set buffer that can not be killed"
  :config
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))
  (with-current-buffer "*Messages*"
    (emacs-lock-mode 'kill)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 06_buffer.el ends here
