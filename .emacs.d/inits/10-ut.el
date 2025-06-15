;;; 10-ut.el --- Utilities configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf counsel-tramp :ensure t
  :config
  (setq tramp-persistency-file-name "~/.emacs.d/tmp/tramp")
  (setq tramp-default-method "scp")
  (setq counsel-tramp-custom-connections
	'(/scp:xsrv:/home/minorugh/gospel-haiku.com/public_html/)))

(leaf sequential-command
  :vc (:url "https://github.com/HKey/sequential-command")
  :doc "Move to first and last line of buffer"
  :config
  (leaf sequential-command-config
    :hook (after-init-hook . sequential-command-setup-keys)))

(leaf imenu-list :ensure t
  :doc "Show imenu entries in a separate buffer"
  :bind (([f2]  . imenu-list-smart-toggle)
	 (:imenu-list-major-mode-map
	  ("j"   . next-line)
	  ("k"   . previous-line)))
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t)
  (setq imenu-list-position 'left)
  :init
  (leaf counsel-css :ensure t
    :after counsel
    :hook (css-mode-hook . counsel-css-imenu-setup)))

(leaf ediff
  :doc "Edit while viewing the difference"
  :hook (ediff-mode-hook . dimmer-off)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
	ediff-split-window-function 'split-window-horizontally
	ediff-diff-options "-twB"))

(leaf ps-mule :tag "Builtin"
  :doc "provide multi-byte character facility to ps-print"
  :if (executable-find "lpr")
  :url "https://tam5917.hatenablog.com/entry/20120914/1347600433"
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

(leaf flymake :tag "builtin"
  :doc "A universal on-the-fly syntax checker"
  :bind ("C-c f" . flymake-show-buffer-diagnostics)
  :hook ((prog-mode-hook . flymake-mode)
	 (lisp-interaction-mode-hook
	  . (lambda () (interactive)(flymake-mode 0)))))


;; The tempbuf-mode is a minor mode that automatically deletes buffers.
;; after-change-major-mode-hook should be set last.
;; because it's executed at the end of a properly written major-mode command.
(leaf tempbuf
  :doc "https://www.emacswiki.org/emacs/TempbufMode"
  :vc (:url "https://github.com/minorugh/tempbuf")
  :hook (((lisp-interaction-mode-hook emacs-lock-mode-hook) . turn-off-tempbuf-mode)
	 (after-change-major-mode-hook . turn-on-tempbuf-mode))
  :config
  (setq tempbuf-kill-message nil))

(leaf bs :tag "builtin"
  :doc "Menu for selecting and displaying buffers"
  :bind (("M-]" . bs-cycle-next)
	 ("M-[" . bs-cycle-previous)))

(leaf emacs-lock-mode :tag "builtin"
  :doc "Set buffer that can not be killed"
  :hook (after-init-hook . my:lock-mode)
  :init
  (defun my:lock-mode ()
    (interactive)
    (with-current-buffer "*scratch*"
      (emacs-lock-mode 'kill))
    (with-current-buffer "*Messages*"
      (emacs-lock-mode 'kill))))

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
	  (switch-to-buffer "*scratch*"))
      (switch-to-buffer toggle-scratch-prev-buffer))))

(leaf *user-gist-commands
  :doc "Gist upload from current buffer or region"
  :tag "Be configured to be able to use gist on the command line from the terminal"
  :defun gist-filename gist-description dired-get-filename
  :init
  (defun gist-description ()
    "Add gist description."
    (shell-quote-argument (read-from-minibuffer "Add gist description: ")))

  (defun gist-filename ()
    "The character string entered in minibuffer is used as file-name.
If enter is pressed without file-name, that's will be buffer-file-neme."
    (interactive)
    (let ((file (file-name-nondirectory (buffer-file-name (current-buffer)))))
      (read-from-minibuffer (format "File name (%s): " file) file)))

  (defun gist-region-or-buffer ()
    "If region is selected, post from the region.
If region isn't selected, post from the buffer."
    (interactive)
    (let ((file (buffer-file-name)))
      (if (not (use-region-p))
	  (compile (concat "gist -od " (gist-description) " " file))
	(compile (concat "gist -oPd " (gist-description) " -f " (gist-filename)))))
    (delete-other-windows))

  (defun dired-do-gist ()
    "Dired-get-filename do gist and open in browser."
    (interactive)
    (let ((file (dired-get-filename nil t)))
      (compile (concat "gist -od " (gist-description) " " file)))
    (delete-other-windows)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 10-ut.el ends here
