;;; 02-git.el --- Git configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf magit :ensure t
  :doc "A Git porcelain inside Emacs"
  :defun my:magit-insert-timestamp magit-display-buffer-fullframe-status-v1
  :bind (("C-x g" . magit-status)
	 ("M-g"   . gitk-open))
  :config
  (setq magit-refs-show-commit-count 'all)
  (setq magit-log-buffer-file-locked t)
  (setq magit-revision-show-gravatars nil)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  :preface
  (defun gitk-open ()
    "Open gitk with current dir.
see https://riptutorial.com/git/example/18336/gitk-and-git-gui"
    (interactive)
    (start-process "gitk" nil "gitk")
    (delete-other-windows)))

(leaf browse-at-remote :ensure t
  :doc "Open github page from Emacs"
  :config
  (setq browse-at-remote-prefer-symbolic nil))

(leaf diff-hl :ensure t
  :doc "Highlight uncommitted changes using VC"
  :hook ((after-init-hook . global-diff-hl-mode)
	 (after-init-hook . diff-hl-margin-mode))
  :config
  (custom-set-faces
   '(diff-hl-change ((t (:background "#8adf80" :foreground "#333"))))
   '(diff-hl-delete ((t (:background "#ff8f88" :foreground "#333"))))
   '(diff-hl-insert ((t (:background "#bfc9ff" :foreground "#333"))))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 02-git.el ends here
