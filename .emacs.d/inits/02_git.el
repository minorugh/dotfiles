;; 02_git.el  --- Git configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-erro t)

(leaf magit :ensure t
  :doc "A Git porcelain inside Emacs"
  :bind '(("C-x g" . magit-status)
	  ("M-g b" . magit-blame)
	  ("M-g c" . magit-file-checkout)
	  ("M-g l" . magit-log-buffer-file)
	  ("M-g k" . gitk-open)
	  ("M-g t" . git-timemachine-toggle))
  :config
  (setq magit-refs-show-commit-count 'all
        magit-log-buffer-file-locked t
        magit-revision-show-gravatars nil
        ;; magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
	;; Do not split window
	magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
	)
  (setq transient-history-file "~/.emacs.d/tmp/transient/history.el")
  ;; Walk through git revisions of a file
  (leaf git-timemachine :ensure t)
  ;; Open github page from Emacs
  (leaf browse-at-remote :ensure t
    :config
    (setq browse-at-remote-prefer-symbolic nil)))


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


;;; 02_git.el ends here
