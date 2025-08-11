;;; 30-utils.el --- Initialize ultilities.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf which-key :ensure nil
  :tag "builtin"
  :doc "Display available keybindings in popup"
  :config
  (setq which-key-max-description-length 40)
  (setq which-key-delay 0.0)
  :hook after-init-hook)

(leaf key-chord
  :vc (:url "https://github.com/minorugh/key-chord-20240910.1441")
  :config
  :chord (("df" . counsel-descbinds)
	  ("l;" . init-loader-show-log))
  :hook after-init-hook)

(leaf counsel-tramp :ensure t
  :config
  (setq tramp-persistency-file-name "~/.emacs.d/tmp/tramp")
  (setq tramp-default-method "scp")
  (setq counsel-tramp-custom-connections
	'(/scp:xsrv:/home/minorugh/gospel-haiku.com/public_html/)))

(leaf popwin :ensure t
  :doc "popup window manager for Emacs"
  :hook after-init-hook)

(leaf tempbuf
  :doc "Auto kill unused buffers in the background"
  :vc (:url "https://github.com/minorugh/tempbuf")
  :hook	((find-file-hook dired-mode-hook magit-mode-hook compilation-mode-hook)
         . turn-on-tempbuf-mode)
  :config
  (setq tempbuf-kill-message nil))

(leaf bs :ensure nil
  :tag "builtin"
  :doc "Menu for selecting and displaying buffers"
  :bind (("M-]" . bs-cycle-next)
	     ("M-[" . bs-cycle-previous)))

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

(leaf quickrun :ensure t
  :bind ([f5] . quickrun))

(leaf projectile :ensure t
  :doc "Manage and navigate projects in Emacs"
  :config
  (setq projectile-known-projects-file "~/.emacs.d/tmp/projectile.eld")
  :hook after-init-hook)

(leaf sequential-command
  :vc (:url "https://github.com/HKey/sequential-command")
  :doc "Move to first and last line of buffer"
  :config
  (leaf sequential-command-config
    :hook (after-init-hook . sequential-command-setup-keys)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 30-utils.el ends here
