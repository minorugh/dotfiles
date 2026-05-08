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
  :after evil
  :doc "Edit multiple occurrences in the same way simultaneously."
  :config
  (defun my-iedit-toggle ()
    "Visual-stateならEmacs-stateに切り替えてiedit-mode起動、終了後Normal-stateへ。
Emacs-stateなら通常通りiedit-modeをon/off。"
    (interactive)
    (cond
     ((evil-visual-state-p)
      (evil-emacs-state)
      (iedit-mode)
      (add-hook 'iedit-mode-end-hook #'my-iedit-end-to-normal nil t))
     (t
      (iedit-mode))))

  (defun my-iedit-end-to-normal ()
    "Return to Normal-state after the end of iedit."
    (evil-normal-state)
    (remove-hook 'iedit-mode-end-hook #'my-iedit-end-to-normal t)))


(leaf expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved))
;; End:
;;; 20-undo.el ends here
