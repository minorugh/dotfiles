;;; 20-edit.el --- Editing configurations.      -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf super-save
  :ensure t
  :doc "Smart auto save buffers."
  :hook (after-init-hook . super-save-mode)
  :config
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration       1)
  (setq super-save-remote-files        nil)
  (setq super-save-exclude             '(".gpg")))

(leaf save-scratch
  :doc "Save contents of scratch at shutdown, restore at startup."
  :hook((kill-emacs-hook . save-scratch-buffer)
	(after-init-hook . restore-scratch-buffer))
  :init
  (defun save-scratch-buffer ()
    (with-current-buffer "*scratch*"
      (write-region (point-min) (point-max)
                    (locate-user-emacs-file "tmp/scratch"))))

  (defun restore-scratch-buffer ()
    (let ((f (locate-user-emacs-file "tmp/scratch")))
      (when (file-exists-p f)
	(with-current-buffer "*scratch*"
          (erase-buffer)
          (insert-file-contents f))))))

(leaf ediff
  :tag "builtin"
  :doc "Edit while viewing the difference."
  :hook (ediff-mode-hook . dimmer-off)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-diff-options "-twB"))

(leaf elec-pair
  :tag "builtin"
  :doc "Automatic parenthesis pairing. Disabled in text-mode (use yasnippet instead)."
  :hook ((after-init-hook  . electric-pair-mode)
         (text-mode-hook   . (lambda () (electric-pair-local-mode -1)))))

(leaf electric-indent
  :tag "builtin"
  :doc "Standard indentation at line breaks. Already ON but written for administrative purposes."
  :hook (after-init-hook . electric-indent-mode))

(leaf indent-helper
  :bind* ("C-c i" . indent-region-or-buffer)
  :init
  (defun indent-region-or-buffer ()
    "If there is a selection, indent there; if not, indent the entire buffer."
    (interactive)
    (save-excursion
      (if (use-region-p)
          (indent-region (region-beginning) (region-end))
	(indent-region (point-min) (point-max)))
      (message "Indented selected region or buffer."))))

(leaf flymake
  :doc "On-the-fly syntax checking."
  :tag "builtin"
  :hook ((prog-mode-hook     . flymake-mode)
         (markdown-mode-hook . flymake-mode)
	 (lisp-interaction-mode-hook . (lambda () (flymake-mode 0))))
  :config
  ;; Suppress "untrusted content" warning in flymake-log
  (with-eval-after-load 'elisp-mode
    (advice-add 'elisp-flymake-byte-compile :around
		(lambda (orig-fun report-fn &rest args)
                  (when buffer-file-name
                    (apply orig-fun report-fn args))))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 20-edit.el ends here
