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

(defun my-sudo-reopen ()
  "Reopen current file with sudo privileges via TRAMP."
  (interactive)
  (let ((pos (point)))
    (find-alternate-file (concat "/sudo:localhost:" (buffer-file-name)))
    (goto-char pos)))

;; (leaf sudo-edit :ensure t
;;   :doc "Edit currently visited file as another user.")


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 20-edit.el ends here
