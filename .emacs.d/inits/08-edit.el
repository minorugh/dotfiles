;;; 08-edit.el --- Editing configurations.      -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  Expand Region
;; ============================================================

(leaf expand-region
  :ensure t
  :bind (("C-@"   . er/expand-region)
         ("C-M-@" . er/contract-region)))


;; ============================================================
;;  Auto Save
;; ============================================================

(leaf super-save
  :ensure t
  :doc "Smart auto-save buffers on focus loss and idle."
  :hook (after-init-hook . super-save-mode)
  :config
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration       1)
  (setq super-save-remote-files        nil)
  (setq super-save-exclude             '(".gpg")))


;; ============================================================
;;  Undo
;; ============================================================

(leaf undo-fu
  :ensure t
  :bind (("C-_" . undo-fu-only-undo)
         ("C-/" . undo-fu-only-redo)))

(leaf undo-fu-session
  :ensure t
  :hook (after-init-hook . undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-directory (locate-user-emacs-file "tmp/undo-session")))


;; ============================================================
;;  Diff / Ediff
;; ============================================================

(leaf ediff
  :tag "builtin"
  :doc "Side-by-side diff editing."
  :hook (ediff-mode-hook . dimmer-off)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options          "-twB"))


;; ============================================================
;;  Electric Modes
;; ============================================================

(leaf elec-pair
  :tag "builtin"
  :doc "Auto-pair parentheses.  Disabled in text-mode (yasnippet handles it)."
  :hook ((after-init-hook . electric-pair-mode)
         (text-mode-hook  . (lambda () (electric-pair-local-mode -1)))))

(leaf electric-indent
  :tag "builtin"
  :doc "Auto-indent on newline.  Already ON; declared for documentation."
  :hook (after-init-hook . electric-indent-mode))

(leaf my-indent-buffer
  :bind* ("C-c i" . my-indent-buffer)
  :init
  (defun my-indent-buffer ()
    "Untabify and indent the entire buffer."
    (interactive)
    (save-excursion
      (untabify (point-min) (point-max))
      (indent-region (point-min) (point-max))
      (message "Untabified and indented buffer."))))


;; ============================================================
;;  Revert Buffer
;; ============================================================

(defun my-revert-buffer ()
  "Revert current buffer without confirmation."
  (interactive)
  (unless (buffer-modified-p)
    (revert-buffer :ignore-auto :noconfirm)))

(key-chord-define-global "jk" #'my-revert-buffer)


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 08-edit.el ends here
