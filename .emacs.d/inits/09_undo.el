;;; 09_undo.el --- Undo/Redo utility configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf undo-fu
  :doc "Redo and Undo operations"
  :url "https://codeberg.org/ideasman42/emacs-undo-fu"
  :ensure t
  :bind (("C-_" . undo-fu-only-undo)
		 ("C-/" . undo-fu-only-redo)))


(leaf undohist
  :doc "Persistent undo history"
  :url "https://github.com/emacsorphanage/undohist"
  :ensure t
  :hook (after-init-hook . undohist-initialize)
  :custom
  `((undohist-directory     . "~/.emacs.d/tmp/undohist")
	(undohist-ignored-files . '("/tmp/" "COMMIT_EDITMSG"))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 09_undo.el ends here
