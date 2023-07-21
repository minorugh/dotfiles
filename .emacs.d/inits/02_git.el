;; 02_git.el  --- Git configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-erro t)

(leaf diff-hl
  :doc "Highlight uncommitted changes"
  :url "https://github.com/dgutov/diff-hl"
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))


(leaf git-timemachine
  :doc "Git time machine"
  :url "https://gitlab.com/pidu/git-timemachine"
  :ensure t)


(leaf browse-at-remote
  :doc "Open github page from Emacs"
  :url "https://github.com/rmuslimov/browse-at-remote"
  :ensure t
  :custom	(browse-at-remote-prefer-symbolic . nil))


(leaf magit
  :doc "A Git Porcelain inside Emacs"
  :url "https://github.com/magit/magit"
  :ensure t
  :bind ("C-x g" . magit-status)
  :custom
  (transient-history-file . "~/.emacs.d/tmp/transient-history")
  ;; Do not split window
  (magit-display-buffer-function . 'magit-display-buffer-fullframe-status-v1))


(leaf *gitk
  :doc ""
  :url ""
  :config
  (defun gitk-open ()
	"Open gitk with current dir."
	(interactive)
	(shell-command "gitk &")
	(delete-other-windows))

  (defun git-gui-open ()
	"Open gitk with current dir."
	(interactive)
	(shell-command "git gui &")
	(delete-other-windows)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 02_git.el ends here
