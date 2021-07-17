;;; 09_utils.el --- Misc utils configuration. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf auto-save-buffers-enhanced
  :ensure t
  :config
  (setq auto-save-buffers-enhanced-exclude-regexps '("^/ssh:" "^/scp:" "/sudo:"))
  (setq auto-save-buffers-enhanced-quiet-save-p t)
  (setq auto-save-buffers-enhanced-save-scratch-buffer-to-file-p t)
  (setq auto-save-buffers-enhanced-file-related-with-scratch-buffer "~/.emacs.d/tmp/scratch")
  (auto-save-buffers-enhanced t)
  (defun read-scratch-data ()
	(let ((file "~/.emacs.d/tmp/scratch"))
	  (when (file-exists-p file)
		(set-buffer (get-buffer "*scratch*"))
		(erase-buffer)
		(insert-file-contents file))))
  (read-scratch-data))


(leaf undohist
  :ensure t
  :hook (emacs-startup-hook . undohist-initialize)
  :config
  (setq undohist-directory "~/.emacs.d/tmp/undohist")
  (setq undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG")))


(leaf undo-tree
  :ensure t
  :chord ("uu" . undo-tree-visualize)
  :config
  (global-undo-tree-mode t)
  (make-variable-buffer-local 'undo-tree-visualizer-diff)
  (setq-default undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-enable-undo-in-region nil)
  (setq undo-tree-auto-save-history nil))


(leaf migemo
  :ensure t
  :if (executable-find "cmigemo")
  :hook (emacs-startup-hook . migemo-init)
  :config
  (setq migemo-command (executable-find "cmigemo"))
  (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
  (autoload 'migemo-init "migemo" nil t))


(leaf imenu-list
  :ensure t
  :bind ("<f2>" . imenu-list-smart-toggle)
  :config
  (setq imenu-list-size 30)
  (setq imenu-list-position 'left)
  (setq imenu-list-focus-after-activation t))


(add-hook 'emacs-startup-hook
		  (lambda ()
			(leaf sequential-command
			  :el-get HKey/sequential-command
			  :require sequential-command-config
			  :config
			  (sequential-command-setup-keys))))


(leaf ps-print-setting
  :init
  (defalias 'ps-mule-header-string-charsets 'ignore)
  (setq ps-multibyte-buffer 'non-latin-printer)
  (setq ps-paper-type 'a4)
  (setq ps-font-size 9)
  (setq ps-font-family 'Helvetica)
  (setq ps-line-number-font 'Courier)
  (setq ps-printer-name nil)
  (setq ps-print-header nil)
  (setq ps-show-n-of-n t)
  (setq ps-line-number t)
  (setq ps-print-footer nil))


(leaf pdfout-from-emacs
  :config
  (setq my:pdfout-command-format "nkf -e | e2ps -a4 -p -nh -line | ps2pdf - %s")
  :init
  (defun pdfout-select ()
	"PDF out select menu."
	(interactive)
	(counsel-M-x "my:pdfout "))
  (defun my:pdfout-buffer ()
	"PDF out from buffer."
	(interactive)
	(my:pdfout-region (point-min) (point-max)))
  (defun my:pdfout-region (begin end)
	"PDF out from BEGIN to END of region."
	(interactive "r")
	;; (shell-command-on-region begin end my:pdfout-command-format)))
	(shell-command-on-region begin end (format my:pdfout-command-format
											   (concat (read-from-minibuffer "File name:") ".pdf")))))


(leaf user-define-functions
  :config
  (bind-key
   "M-p"
   (defun open-screenshooter ()
	 "Narrow the only espy command in M-x."
	 (interactive)
	 (shell-command "xfce4-screenshooter")))

  (defun kill-other-buffers ()
	"Kill all other buffers."
	(interactive)
	(mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
	(when (get-buffer "*tramp/scp xsrv*")
	  (counsel-tramp-quit))
	(message "Killed Other Buffers!"))

  (defun toggle-scratch ()
	"Toggle current buffer and *scratch* buffer."
	(interactive)
	(if (not (string= "*scratch*" (buffer-name)))
		(progn
		  (setq toggle-scratch-prev-buffer (buffer-name))
		  (switch-to-buffer "*scratch*"))
	  (switch-to-buffer toggle-scratch-prev-buffer)))
  (bind-key "S-<return>" 'toggle-scratch)

  (defun filer-current-dir-open ()
	"Open filer in current dir."
	(interactive)
	(shell-command (concat "nautilus " default-directory)))
  (bind-key "<f3>" 'filer-current-dir-open)

  (defun term-current-dir-open ()
	"Open terminal application in current dir."
	(interactive)
	(let ((dir (directory-file-name default-directory)))
	  (shell-command (concat "gnome-terminal --working-directory " dir))))
  (bind-key "<f4>" 'term-current-dir-open)

  (defun my:delete-file-if-no-contents ()
	"Automatic deletion for empty files (Valid in all modes)."
	(when (and (buffer-file-name (current-buffer))
			   (= (point-min) (point-max)))
	  (delete-file
	   (buffer-file-name (current-buffer)))))
  (if (not (memq 'my:delete-file-if-no-contents after-save-hook))
	  (setq after-save-hook
			(cons 'my:delete-file-if-no-contents after-save-hook))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 09_utils.el ends here
