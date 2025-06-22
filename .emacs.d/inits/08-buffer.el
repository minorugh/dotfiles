;;; 08-buffer.el --- Buffer utilities configulation. -*- lexical-binding: t -*-
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
	  (switch-to-buffer "*scratch*"))
      (switch-to-buffer toggle-scratch-prev-buffer))))

(leaf tempbuf
  :doc "kill unused buffers in the background"
  :url "http://www.emacswiki.org/cgi-bin/wiki.pl?TempbufMode"
  :vc (:url "https://github.com/minorugh/tempbuf")
  :hook ((magit-mode-hook dired-mode-hook compilation-mode-hook)
	 . turn-on-tempbuf-mode)
  :config
  (setq tempbuf-kill-message nil))

(leaf bs :tag "builtin"
  :doc "Menu for selecting and displaying buffers"
  :bind (("M-]" . bs-cycle-next)
	 ("M-[" . bs-cycle-previous)))

(leaf compile
  :doc "run compiler as inferior of Emacs"
  :tag "Builtin"
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

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 08-buffer.el ends here
