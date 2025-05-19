;;; 10_ut.el --- Utilities configurations. -*- no-byte-compile: t; -*-
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
  :bind ([f2]  . imenu-list-smart-toggle)
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t)
  (setq imenu-list-position 'left)
  :init
  (leaf counsel-css :ensure t
    :hook (css-mode-hook . counsel-css-imenu-setup)))

(leaf web-mode :ensure t
  :doc "Web template editing mode for emacs"
  :mode ("\\.js?\\'" "\\.html?\\'" "\\.php?\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset    2)
  (setq web-mode-code-indent-offset   2))

(leaf ediff
  :doc "Edit while viewing the difference"
  :hook (ediff-mode-hook . dimmer-off)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
	ediff-split-window-function 'split-window-horizontally
	ediff-diff-options "-twB"))

(leaf ps-mule
  :doc "provide multi-byte character facility to ps-print"
  :tag "Builtin"
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

(leaf flycheck :ensure t
  :doc "On-the-fly syntax checking"
  :hook (((after-init-hook prog-mode-hook) . flycheck-mode)
	 (lisp-interaction-mode-hook . (lambda () (interactive)(flycheck-mode 0))))
  :bind (("M-n" . flycheck-next-error)
	 ("M-p" . flycheck-previous-error)))

(leaf which-key :tag "builtin"
  :doc "Display available keybindings in popup"
  :hook (after-init-hook . which-key-mode)
  :config
  (setq which-key-max-description-length 40)
  (setq which-key-delay 0.0))

(leaf iedit :ensure t
  :doc "Edit multiple occurrences in the same way simultaneously"
  :config
  (defun my:iedit-mode ()
    (interactive)
    (when evil-mode
      (evil-insert-state))
    (iedit-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Various settings for buffer control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


(leaf *emacs-lock-mode :tag "builtin"
  :doc "Set buffer that can not be killed"
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIST configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf *user-gist-commands
  :doc "Gist upload from current buffer or region"
  :tag "Be configured to be able to use gist on the command line from the terminal"
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


;;; 10_ut.el ends here
