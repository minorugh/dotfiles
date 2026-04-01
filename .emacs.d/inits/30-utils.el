;;; 30-utils.el --- Initialize utilities.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf which-key
  :ensure nil
  :tag "builtin"
  :doc "Display available keybindings in popup."
  :hook (after-init-hook . which-key-mode)
  :config
  (setq which-key-max-description-length 40)
  (setq which-key-delay 0.0))

(leaf key-chord
  :doc "map pairs of simultaneously pressed keys to commands.
        Placed in elisp/key-chord/. load-path is set in init.el."
  :hook (after-init-hook . key-chord-mode)
  :chord (("df" . counsel-descbinds)
	  ("l;" . init-loader-show-log)))

(leaf counsel-tramp
  :ensure t
  :config
  (setq tramp-persistency-file-name (locate-user-emacs-file "tmp/tramp"))
  (setq tramp-default-method "scp")
  (setq counsel-tramp-custom-connections
	'(/scp:xsrv:/home/minorugh/gospel-haiku.com/public_html/)))

;; (leaf viewer
;;   :ensure t
;;   :after view
;;   :hook (view-mode-hook . viewer-change-modeline-color-setup)
;;   :config
;;   (setq viewer-modeline-color-view "#852941"))

(leaf popwin
  :ensure t
  :doc "Popup window manager for Emacs."
  :hook (after-init-hook . popwin-mode))

(leaf tempbuf
  :doc "Auto kill unused buffers in the background.
        Placed in elisp/tempbuf/. load-path is set in init.el."
  :hook ((find-file-hook
	  dired-mode-hook
	  magit-mode-hook
	  compilation-mode-hook) . turn-on-tempbuf-mode)
  :config
  (setq tempbuf-kill-message nil))

(leaf bs
  :ensure nil
  :tag "builtin"
  :doc "Menu for selecting and displaying buffers."
  :bind (("M-]" . bs-cycle-next)
	 ("M-[" . bs-cycle-previous)))

(leaf persistent-scratch
  :ensure t
  :doc "Save scratch buffer state to file and restore from file."
  :hook (after-init-hook . persistent-scratch-autosave-mode)
  :bind ("S-<return>" . toggle-scratch)
  :init
  (defvar toggle-scratch-prev-buffer nil
    "Buffer name before switching to *scratch*.")
  :config
  (setq persistent-scratch-save-file (locate-user-emacs-file "tmp/scratch"))
  (defun toggle-scratch ()
    "Toggle current buffer and *scratch* buffer."
    (interactive)
    (if (not (string= "*scratch*" (buffer-name)))
	(progn
	  (setq toggle-scratch-prev-buffer (buffer-name))
	  (switch-to-buffer "*scratch*"))
      (switch-to-buffer toggle-scratch-prev-buffer))))

(leaf quickrun
  :ensure t
  :bind ([f5] . quickrun))

(leaf projectile
  :ensure t
  :doc "Manage and navigate projects in Emacs."
  :hook (after-init-hook . projectile-mode)
  :config
  (setq projectile-known-projects-file (locate-user-emacs-file "tmp/projectile.eld")))

(leaf sequential-command
  :doc "Move to first and last line of buffer.
        Placed in elisp/sequential-command/. load-path is set in init.el."
  :config
  (leaf sequential-command-config
    :hook (after-init-hook . sequential-command-setup-keys)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 30-utils.el ends here
