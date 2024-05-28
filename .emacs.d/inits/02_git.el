;; 02_git.el  --- Git configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-erro t)

(leaf diff-hl :ensure t
  :doc "Highlight uncommitted changes using VC"
  :hook
  ((after-init-hook . global-diff-hl-mode)
   (after-init-hook . diff-hl-margin-mode)
   (magit-pre-refresh-hook .diff-hl-magit-pre-refresh)
   (magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :custom-face
  `((diff-hl-change . '((t (:background "#8adf80" :foreground "#333"))))
    (diff-hl-delete . '((t (:background "#ff8f88" :foreground "#333"))))
    (diff-hl-insert . '((t (:background "#bfc9ff" :foreground "#333"))))))


(leaf magit :ensure t
  :doc "A Git porcelain inside Emacs"
  :bind '(("C-x g" . magit-status)
	  ("M-g" . hydra-git/body))
  :config
  (setq transient-history-file "~/.emacs.d/tmp/transient-history")
  ;; Do not split window
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
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
  (leaf git-timemachine :ensure t)
  (leaf browse-at-remote :ensure t
    :config
    (setq browse-at-remote-prefer-symbolic nil)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 02_git.el ends here
