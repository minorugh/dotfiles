;;; 06_buffer.el --- Buffer Utility configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf super-save :ensure t
  :doc "Smart auto save buffers"
  :hook (after-init-hook . super-save-mode)
  :config
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration       1)
  (setq super-save-remote-files        nil)
  (setq super-save-exclude             '(".gpg")))


(leaf persistent-scratch :ensure t
  :doc "Save scratch buffer state to file and restore from file"
  :hook (after-init-hook . persistent-scratch-autosave-mode)
  :bind ("S-<return>" . toggle-scratch)
  :config
  (setq persistent-scratch-save-file "~/.emacs.d/tmp/scratch")
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
  :tag "builtin"
  :hook (after-init-hook . my:lock-mode)
  :init
  (defun my:lock-mode ()
    (interactive)
    (with-current-buffer "*scratch*"
      (emacs-lock-mode 'kill))
    (with-current-buffer "*Messages*"
      (emacs-lock-mode 'kill))))


(leaf bs
  :doc "Menu for selecting and displaying buffers"
  :tag "builtin"
  :bind (("M-]" . bs-cycle-next)
	 ("M-[" . bs-cycle-previous)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 06_buffer.el ends here
