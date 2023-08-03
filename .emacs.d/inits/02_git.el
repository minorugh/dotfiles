;; 02_git.el  --- Git configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-erro t)
(eval-when-compile (leaf-keywords-init))

(leaf diff-hl
  :doc "Highlight uncommitted changes"
  :url "https://github.com/dgutov/diff-hl"
  :ensure t
  :hook
  ((after-init-hook . global-diff-hl-mode)
   (after-init-hook . diff-hl-margin-mode)
   (magit-pre-refresh-hook .diff-hl-magit-pre-refresh)
   (magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :custom-face
  (diff-hl-change . '((t (:background "#8adf80" :foreground "#333"))))
  (diff-hl-delete . '((t (:background "#ff8f88" :foreground "#333"))))
  (diff-hl-insert . '((t (:background "#bfc9ff" :foreground "#333")))))


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
  :defun ((hydra-default-pre)(hydra-keyboard-quit)
		  (hydra--call-interactively-remap-maybe)(hydra-show-hint)(hydra-set-transient-map))
  :bind '(("C-x g" . magit-status)
		  ("M-g" . hydra-git/body))
  :custom
  (transient-history-file . "~/.emacs.d/tmp/transient-history")
  ;; Do not split window
  (magit-display-buffer-function . 'magit-display-buffer-fullframe-status-v1)
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
  :config
  ;; Graphical history viewer for Git
  ;; https://riptutorial.com/git/example/18336/gitk-and-git-gui
  (defun gitk-open ()
	"Open gitk with current dir."
	(interactive)
	(shell-command "gitk &")
	(delete-other-windows))

  (defun git-gui-open ()
	"Tools for creating commits."
	(interactive)
	(shell-command "git gui &")
	(delete-other-windows)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 02_git.el ends here
