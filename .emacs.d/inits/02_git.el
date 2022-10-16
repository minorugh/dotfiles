;; 02_git.el  --- Git configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-erro t)

;; Mgit configuration
(leaf magit
  :ensure t
  :bind (("M-g s" . magit-status)
		 ("M-g l" . magit-log-buffer-file)
		 ("M-g b" . magit-blame-addition)
		 ("M-g t" . git-timemachine-toggle))
  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :custom
  (transient-history-file . "~/.emacs.d/tmp/transient-history")
  :init
  (leaf diff-hl	:ensure t
	:hook ((after-init-hook . global-diff-hl-mode)
		   (after-init-hook . diff-hl-margin-mode)))

  (leaf git-timemachine	:ensure t)

  (leaf browse-at-remote :ensure t
	:custom (browse-at-remote-prefer-symbolic . nil)))


;; Gist configuration
(leaf gist
  :bind (("s-g p" . gist-region-or-buffer)
		 ("s-g c" . my:chromium-gist))
  :init
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
;; no-byte-compile: t
;; End:
;;; 02_git.el ends here
