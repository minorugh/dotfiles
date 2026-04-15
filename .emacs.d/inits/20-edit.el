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

(leaf imenu-list
  :ensure t
  :doc "Show imenu entries in a separate buffer."
  :bind (([f2] . imenu-list-smart-toggle)
         (:imenu-list-major-mode-map
          ("j" . next-line)
          ("k" . previous-line)))
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t)
  (setq imenu-list-position 'left))

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
  :ensure nil
  :tag "builtin"
  :doc "Edit while viewing the difference."
  :hook (ediff-mode-hook . dimmer-off)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-diff-options "-twB"))

(leaf elec-pair
  :ensure nil
  :tag "builtin"
  :doc "Automatic parenthesis pairing."
  :hook (after-init-hook . electric-pair-mode))

(leaf iedit
  :ensure t
  :doc "Edit multiple occurrences in the same way simultaneously."
  :bind ("<insert>" . iedit-mode))

(leaf expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))

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


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 20-edit.el ends here
