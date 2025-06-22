;;; 20-ut.el --- Utilities configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

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
  (leaf counsel-css :ensure t
    :after counsel
    :hook (css-mode-hook . counsel-css-imenu-setup)))

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

(leaf *user-gist-commands
  :doc "Gist upload from current buffer or region"
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
;;; 20-ut.el ends here
