;;; 07_buffer.el --- Buffer Utility configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf super-save :ensure t
  :doc "Smart auto save buffers"
  :config
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration       1)
  (setq super-save-remote-files        nil)
  (setq super-save-exclude             '(".gpg"))
  :hook after-init-hook)


(leaf persistent-scratch :ensure t
  :doc "Save *scratch* buffer state to file and restore from file"
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
	  (switch-to-buffer "*scratch*")
	  (display-line-numbers-mode 0))
      (switch-to-buffer toggle-scratch-prev-buffer))))


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


;; Directly copied from frame.el but minimize it without deleting it
;; it when last frame will be closed
(leaf *ad:handle-delete-frame
  :url "https://tinyurl.com/23rah56r"
  :config
  (defun my:handle-delete-frame (event)
    "If it's the last frame, minimize it without deleting it."
    (interactive "e")
    (let ((frame   (posn-window (event-start event)))
          (numfrs  (length (visible-frame-list))))
      (cond ((> numfrs 1) (delete-frame frame t))
            ((iconify-frame)))))

  (advice-add 'handle-delete-frame :override
	      #'my:handle-delete-frame))

(leaf bs
  :doc "Menu for selecting and displaying buffers"
  :tag "builtin"
  :bind (("M-]" . bs-cycle-next)
	 ("M-[" . bs-cycle-previous)))


;;; 07_buffer.el ends here
