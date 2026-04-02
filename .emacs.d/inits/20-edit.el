;;; 20-edit.el --- Editing configurations.	-*- lexical-binding: t -*-
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

(leaf atomic-chrome
  :ensure t
  :doc "Edit text areas of the browser in Emacs."
  :hook (after-init-hook . atomic-chrome-start-server)
  :config (setq atomic-chrome-buffer-open-style 'full))

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

(leaf sudo-edit :ensure t
  :doc "Edit currently visited file as another user.")

(defun my-auto-follow-mode ()
  "Automatically enable `follow-mode' when the same buffer is split side-by-side."
  (walk-windows
   (lambda (win)
     (with-current-buffer (window-buffer win)
       (let* ((same (seq-filter
                     (lambda (w) (eq (window-buffer w) (current-buffer)))
                     (window-list)))
              (side-by-side
               (and (> (length same) 1)
                    (seq-some
                     (lambda (w)
                       (and (not (eq w win))
                            (= (window-top-line w) (window-top-line win))
                            (/= (window-left-column w) (window-left-column win))))
                     same))))
         (if side-by-side
             (unless (bound-and-true-p follow-mode) (follow-mode 1))
           (when (bound-and-true-p follow-mode) (follow-mode -1))))))))

(add-hook 'window-configuration-change-hook #'my-auto-follow-mode)


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 20-edit.el ends here
