;; 02_git.el  --- Git configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-erro t)

(leaf magit :ensure t
  :doc "A Git porcelain inside Emacs"
  :bind '(("C-x g" . magit-status)
	  ("M-g"   . hydra-magit/body))
  :hydra
  (hydra-magit
   (:color red :hint nil)
   "
    magit-_s_tatus  _b_lame  _c_heckout  _l_og  _g_itk-open _t_imemachine
  "
   ("s" magit-status)
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
  ;; Walk through git revisions of a file
  (leaf git-timemachine :ensure t)
  ;; Open github page from Emacs
  (leaf browse-at-remote :ensure t
    :config
    (setq browse-at-remote-prefer-symbolic nil))
  ;; Highlight uncommitted changes using VC
  (leaf diff-hl :ensure t
    :hook ((after-init-hook . global-diff-hl-mode)
	   (after-init-hook . diff-hl-margin-mode)
	   (magit-pre-refresh-hook  . diff-hl-magit-pre-refresh)
	   (magit-post-refresh-hook . diff-hl-magit-post-refresh))
    :custom-face
    `((diff-hl-change . '((t (:background "#8adf80" :foreground "#333"))))
      (diff-hl-delete . '((t (:background "#ff8f88" :foreground "#333"))))
      (diff-hl-insert . '((t (:background "#bfc9ff" :foreground "#333")))))))


;;; 02_git.el ends here
