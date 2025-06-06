;; 02-git.el  --- Git configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-erro t)

(leaf magit :ensure t
  :doc "A Git porcelain inside Emacs"
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
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq transient-history-file "~/.emacs.d/tmp/transient/history.el")
  :preface
  (leaf git-timemachine :ensure t
    :doc "Walk through git revisions of a file"
    :after magit)

  (leaf browse-at-remote :ensure t
    :doc "Open github page from Emacs"
    :config
    (setq browse-at-remote-prefer-symbolic nil))

  (leaf diff-hl :ensure t
    :doc "Highlight uncommitted changes using VC"
    :hook ((after-init-hook . global-diff-hl-mode)
	   (after-init-hook . diff-hl-margin-mode)
	   (magit-pre-refresh-hook  . diff-hl-magit-pre-refresh)
	   (magit-post-refresh-hook . diff-hl-magit-post-refresh))
    :custom-face
    `((diff-hl-change . '((t (:background "#8adf80" :foreground "#333"))))
      (diff-hl-delete . '((t (:background "#ff8f88" :foreground "#333"))))
      (diff-hl-insert . '((t (:background "#bfc9ff" :foreground "#333"))))))

  (defun gitk-open ()
    "Open gitk with current dir.
see https://riptutorial.com/git/example/18336/gitk-and-git-gui"
    (interactive)
    (compile "gitk")
    (delete-other-windows)))

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 02-git.el ends here
