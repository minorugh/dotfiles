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
  :doc "map pairs of simultaneously pressed keys to commands."
  :vc (:url "https://github.com/minorugh/key-chord")
  :hook (after-init-hook . key-chord-mode)
  :chord (("df" . counsel-descbinds)
	  ("l;" . init-loader-show-log)))

(leaf popwin
  :ensure t
  :doc "Popup window manager for Emacs."
  :hook (after-init-hook . popwin-mode))

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
  :doc "Move to first and last line of buffer."
  :vc (:url "https://github.com/minorugh/sequential-command")
  :config
  (leaf sequential-command-config
    :hook (after-init-hook . sequential-command-setup-keys)))

(leaf ps-print
  :doc "PostScript printing with Japanese support."
  :url "https://tam5917.hatenablog.com/entry/20120914/1347600433"
  :if (executable-find "lpr")
  :config
  (setq ps-multibyte-buffer 'non-latin-printer)
  (setq ps-paper-type       'a4)
  (setq ps-printer-name      nil)
  (setq ps-print-header      nil)
  (setq ps-print-footer      nil)
  (setq ps-font-size         9)
  (setq ps-font-family      'Courier)
  (setq ps-line-number-font 'Courier)
  (setq ps-line-number       t)
  (setq ps-show-n-of-n       t)
  (defalias 'ps-mule-header-string-charsets 'ignore))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 30-utils.el ends here
