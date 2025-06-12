;; 02-git.el  --- Git configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-erro t)

(leaf magit :ensure t
  :doc "A Git porcelain inside Emacs"
  :defun magit-display-buffer-fullframe-status-v1
  :bind '(("C-x g" . magit-status)
	  ("M-g"   . hydra-magit/body))
  :hydra
  (hydra-magit
   (:hint nil :exit t)
   "
    _m_agit-status  _b_lame  _c_heckout  _l_og  _g_itk-open _t_imemachine
  "
   ("m" magit-status)
   ("b" magit-blame)
   ("c" magit-file-checkout)
   ("l" magit-log-buffer-file)
   ("g" gitk-open)
   ("t" git-timemachine)
   ("<muhenkan>" nil))				;
  :config
  (setq magit-refs-show-commit-count 'all)
  (setq magit-log-buffer-file-locked t)
  (setq magit-revision-show-gravatars nil)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (defun gitk-open ()
    "Open gitk with current dir.
see https://riptutorial.com/git/example/18336/gitk-and-git-gui"
    (interactive)
    (compile "gitk")
    (delete-other-windows)))

(leaf transient :ensure t
  :config
  (setq transient-history-file "~/.emacs.d/tmp/transient/history.el"))

(leaf git-timemachine :ensure t
  :doc "Walk through git revisions of a file")

(leaf browse-at-remote :ensure t
  :doc "Open github page from Emacs"
  :config
  (setq browse-at-remote-prefer-symbolic nil))

(leaf diff-hl :ensure t
  :doc "Highlight uncommitted changes using VC"
  :hook ((after-init-hook . global-diff-hl-mode)
	 (after-init-hook . diff-hl-margin-mode))
  :custom-face
  `((diff-hl-change . '((t (:background "#8adf80" :foreground "#333"))))
    (diff-hl-delete . '((t (:background "#ff8f88" :foreground "#333"))))
    (diff-hl-insert . '((t (:background "#bfc9ff" :foreground "#333"))))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 02-git.el ends here
