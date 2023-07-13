;; 02_git.el  --- Git configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-erro t)

;; -------------------------------------------------------------------
;; Interface to the version control system Git
;; -------------------------------------------------------------------
(leaf magit
  :doc "Git Porcelain inside Emacs"
  :url "https://github.com/magit/magit"
  :ensure t
  :bind '(("C-x g" . magit-status )
		  ("M-g"   . hydra-git/body))
  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :hydra
  (hydra-git
   (:color red :hint nil)
   "
    magit: _s_tatus  _b_lame  _c_heckout  _l_og  _g_itk  _t_imemachine
  "
   ("s" magit-status)
   ("b" magit-blame-addition)
   ("c" magit-file-checkout)
   ("l" magit-log-buffer-file)
   ("g" gitk-open)
   ("t" git-timemachine-toggle)
   ("<muhenkan>" nil))
  :custom
  (transient-history-file . "~/.emacs.d/tmp/transient-history")
  ;; Do not split window
  (magit-display-buffer-function . 'magit-display-buffer-fullframe-status-v1)
  :init
  (leaf diff-hl
    :ensure t
    :hook '((after-init-hook . global-diff-hl-mode)
			(after-init-hook . diff-hl-margin-mode)))
  (leaf git-timemachine	:ensure t)
  (leaf browse-at-remote
    :ensure t
    :custom (browse-at-remote-prefer-symbolic . nil))
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
