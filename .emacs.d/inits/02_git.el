;; 02_git.el  --- Git configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-erro t)

(leaf magit
  :doc "Git Porcelain inside Emacs"
  :url "https://github.com/magit/magit"
  :ensure t
  :bind '(("C-x g" . magit-status )
		  ("M-g"   . hydra-git/body))
  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :custom
  (transient-history-file . "~/.emacs.d/tmp/transient-history")
  ;; Do not split window
  (magit-display-buffer-function . 'magit-display-buffer-fullframe-status-v1)
  :init
  (leaf diff-hl
    :ensure t
    :hook ((after-init-hook . global-diff-hl-mode)
		   (after-init-hook . diff-hl-margin-mode)))
  (leaf git-timemachine	:ensure t)
  (leaf browse-at-remote
    :ensure t
    :custom
	(browse-at-remote-prefer-symbolic . nil))
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


(leaf *define-gist-commands
  :doc "Gist upload from current buffer or region"
  :config
  (defun gist-description ()
    "Add gist description."
    (shell-quote-argument (read-from-minibuffer "Add gist description: ")))

  (defun gist-filename ()
    "The character string entered in minibuffer is used as file-name.
If enter is pressed without file-name, that's will be buffer-file-neme."
    (interactive)
    (let ((file (file-name-nondirectory (buffer-file-name (current-buffer)))))
      (read-from-minibuffer (format "File name (%s): " file) file)))

  (defun gist-region-or-buffer ()
    "If region is selected, post from the region.
If region isn't selected, post from the buffer."
    (interactive)
    (let ((file (buffer-file-name)))
      (if (not (use-region-p))
		  (compile (concat "gist -od " (gist-description) " " file))
		(compile (concat "gist -oPd " (gist-description) " -f " (gist-filename)))))
    (delete-other-windows))

  (defun dired-do-gist ()
    "Dired-get-filename do gist and open in browser."
    (interactive)
    (let ((file (dired-get-filename nil t)))
      (compile (concat "gist -od " (gist-description) " " file)))
    (delete-other-windows)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 02_git.el ends here
