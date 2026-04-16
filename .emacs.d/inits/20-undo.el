;;; 20-undo.el --- Undo and editing operation configurations.      -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

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
  :doc "Edit multiple occurrences in the same way simultaneously."
  :bind ("<insert>" . iedit-mode))

(leaf expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 20-undo.el ends here
