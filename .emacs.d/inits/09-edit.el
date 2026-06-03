;;; 09-edit.el --- Editing configurations.      -*- lexical-binding: t -*-
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

(leaf undo-fu
  :ensure t
  :bind (("C-_" . undo-fu-only-undo)
         ("C-/" . undo-fu-only-redo)))

(leaf undohist
  :ensure t
  :doc "Persistent undo history."
  :hook (after-init-hook . undohist-initialize)
  :config
  (setq undohist-directory     (locate-user-emacs-file "tmp/undohist"))
  (setq undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG")))

(leaf iedit
  :ensure t
  :after evil
  :doc "Edit multiple occurrences in the same way simultaneously."
  :config
  (defun my-iedit-toggle ()
    "Toggle `iedit-mode' in visual-state, restrict to selected region."
    (interactive)
    (cond
     ((evil-visual-state-p)
      (let ((beg (region-beginning))
            (end (region-end)))
	(evil-emacs-state)
	(set-mark beg)
	(goto-char end)
	(iedit-mode)
	(add-hook 'iedit-mode-end-hook #'my-iedit-end-to-normal nil t)))
     (t
      (iedit-mode))))

  (defun my-iedit-end-to-normal ()
    "Return to Normal-state after the end of iedit."
    (evil-normal-state)
    (remove-hook 'iedit-mode-end-hook #'my-iedit-end-to-normal t)))

(leaf expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))

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
  :bind* ("C-c i" . indent-buffer)
  :init
  (defun indent-buffer ()
    "Indent the entire buffer."
    (interactive)
    (save-excursion
      (indent-region (point-min) (point-max))
      (message "Indented buffer."))))

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
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 09-edit.el ends here
